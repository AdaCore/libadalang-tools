-- CE2411A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- OBJECTIVE:
--     CHECK THAT INDEX RETURNS THE CORRECT INDEX POSITION AND THAT
--     SET_INDEX CORRECTLY SETS THE INDEX POSITION IN A DIRECT FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT FILES.

-- HISTORY:
--     TBN 10/01/86
--     JLH 08/07/87  REVISED EXTERNAL FILE NAME, REMOVED CHECK FOR
--                   NAME_ERROR ON OPEN CALLS, AND REMOVED
--                   UNNECESSARY CODE.

with Direct_Io;
with Report; use Report;
procedure Ce2411a is

   package Dir_Io is new Direct_Io (Integer);
   use Dir_Io;

   File1 : File_Type;
   Incomplete : exception;

begin
   Test
     ("CE2411A",
      "CHECK THAT INDEX RETURNS THE CORRECT INDEX " &
      "POSITION AND THAT SET_INDEX CORRECTLY SETS " &
      "THE INDEX POSITION IN A DIRECT FILE");

   -- INITIALIZE TEST FILE

   begin
      Create (File1, Out_File, Legal_File_Name);
   exception
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED DURING CREATE " &
            "WITH OUT_FILE MODE FOR DIR_IO");
         raise Incomplete;
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED DURING CREATE " &
            "WITH OUT_FILE MODE FOR DIR_IO");
         raise Incomplete;
      when others =>
         Failed ("UNKNOWN EXCEPTION RAISED DURING CREATE");
         raise Incomplete;
   end;

   begin
      if Index (File1) /= 1 then
         Failed ("STARTING INDEX POSITION IS INCORRECT - 1");
         raise Incomplete;
      end if;
      for I in 1 .. 10 loop
         Write (File1, I);
      end loop;
      if Index (File1) /= 11 then
         Failed ("INDEX DOES NOT RETURN CORRECT POSITION - 2");
      end if;
      Write (File1, 20, 20);
      if Index (File1) /= 21 then
         Failed ("INDEX DOES NOT RETURN CORRECT POSITION - 3");
      end if;
      Set_Index (File1, 11);
      if Index (File1) /= 11 then
         Failed ("SET_INDEX DOES NOT CORRECTLY SET POSITION - 4");
      end if;
      Write (File1, 11);
      if Index (File1) /= 12 then
         Failed ("INDEX DOES NOT RETURN CORRECT POSITION - 5");
      end if;
   end;

   Close (File1);

   begin
      Open (File1, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED DURING OPEN INFILE " & "FOR DIR_IO");
         raise Incomplete;
      when others =>
         Failed ("UNKNOWN EXCEPTION RAISED DURING OPEN INFILE");
         raise Incomplete;
   end;

   declare
      Num : Integer;
   begin
      if Index (File1) /= 1 then
         Failed ("STARTING INDEX POSITION IS INCORRECT - 7");
         raise Incomplete;
      end if;
      for I in 1 .. 10 loop
         Read (File1, Num);
         if Num /= I then
            Failed ("FILE CONTAINS INCORRECT DATA - 8");
         end if;
         if Index (File1) /= Positive_Count (I + 1) then
            Failed ("INDEX DOES NOT RETURN THE CORRECT " & "POSITION - 9");
         end if;
      end loop;
      Set_Index (File1, 20);
      if Index (File1) /= 20 then
         Failed ("SET_INDEX DOES NOT CORRECTLY SET POSITION - " & "10");
      end if;
      Read (File1, Num, 20);
      if Num /= 20 then
         Failed ("FILE CONTAINS INCORRECT DATA - 11");
      end if;
      if Index (File1) /= 21 then
         Failed ("INDEX DOES NOT RETURN CORRECT POSITION - 12");
      end if;
      Set_Index (File1, 1);
      if Index (File1) /= 1 then
         Failed ("SET_INDEX DOES NOT CORRECTLY SET POSITION - " & "13");
      end if;
   end;

   Close (File1);

   begin
      Open (File1, Inout_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED DURING OPEN " & "INOUT_FILE FOR DIR_IO");
         raise Incomplete;
      when others =>
         Failed ("UNKNOWN EXCEPTION RAISED DURING OPEN INOUT");
         raise Incomplete;
   end;

   declare
      Num : Integer;
   begin
      if Index (File1) /= 1 then
         Failed ("STARTING INDEX POSITION IS INCORRECT - 15");
         raise Incomplete;
      end if;
      for I in 1 .. 10 loop
         Read (File1, Num);
         if Num /= I then
            Failed ("FILE CONTAINS INCORRECT DATA - 16");
         end if;
         if Index (File1) /= Positive_Count (I + 1) then
            Failed ("INDEX DOES NOT RETURN THE CORRECT " & "POSITION - 17");
         end if;
      end loop;
      Set_Index (File1, 20);
      if Index (File1) /= 20 then
         Failed ("SET_INDEX DOES NOT CORRECTLY SET POSITION - " & "18");
      end if;
      Write (File1, 12, 12);
      if Index (File1) /= 13 then
         Failed ("INDEX DOES NOT RETURN CORRECT POSITION - 19");
      end if;
      Set_Index (File1, 1);
      if Index (File1) /= 1 then
         Failed ("SET_INDEX DOES NOT CORRECTLY SET POSITION - " & "20");
      end if;
   end;

   begin
      Delete (File1);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;
end Ce2411a;
