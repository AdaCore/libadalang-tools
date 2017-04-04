-- CE2208B.ADA

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
--     CHECK THAT DATA CAN BE OVERWRITTEN IN THE SEQUENTIAL FILE AND THE
--     CORRECT VALUES CAN LATER BE READ.  ALSO CHECK THAT OVERWRITING
--     TRUNCATES THE FILE TO THE LAST ELEMENT WRITTEN.

-- APPLICABILITY CRITERIA:
--      THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--      THE CREATING AND OPENING OF SEQUENTIAL FILES.

-- HISTORY:
--     TBN  09/30/86  CREATED ORIGINAL TEST.
--     GMT  07/24/87  ADDED CHECKS FOR USE_ERROR AND REMOVED SOME CODE.
--     BCB  10/03/90  CHANGED CODE TO CHECK THAT OVERWRITING TRUNCATES
--                    INSTEAD OF WHETHER IT TRUNCATES.

with Sequential_Io;
with Report; use Report;
procedure Ce2208b is

   package Seq_Io is new Sequential_Io (Integer);
   use Seq_Io;

   File1 : File_Type;
   Incomplete : exception;

begin
   Test
     ("CE2208B",
      "CHECK THAT DATA CAN BE OVERWRITTEN IN THE SEQUENTIAL " &
      "FILE AND THE CORRECT VALUES CAN LATER BE READ.  ALSO " &
      "CHECK THAT OVERWRITING TRUNCATES THE FILE.");

   -- INITIALIZE TEST FILE

   begin
      Create (File1, Out_File, Legal_File_Name);
   exception
      when Name_Error =>
         Not_Applicable ("NAME_ERROR RAISED DURING CREATE");
         raise Incomplete;
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED DURING CREATE");
         raise Incomplete;
      when others =>
         Failed ("UNKNOWN EXCEPTION RAISED DURING CREATE");
         raise Incomplete;
   end;

   begin
      for I in 1 .. 25 loop
         Write (File1, I);
      end loop;
   exception
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED DURING WRITE");
         raise Incomplete;
   end;

   begin
      Close (File1);
   exception
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED DURING CLOSE");
         raise Incomplete;
   end;

   begin
      Open (File1, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("OPEN WITH  OUT_FILE  MODE NOT " &
            "SUPPORTED FOR SEQUENTIAL FILES");
         raise Incomplete;
      when others =>
         Failed ("EXCEPTION RAISED DURING OPEN");
         raise Incomplete;
   end;

   begin
      for I in 26 .. 36 loop
         Write (File1, I);
      end loop;
   exception
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED DURING OVERWRITE");
         raise Incomplete;
   end;

   begin
      Close (File1);
   exception
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED DURING 2ND CLOSE");
         raise Incomplete;
   end;

   begin
      Open (File1, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("OPEN WITH  IN_FILE  MODE NOT " &
            "SUPPORTED FOR SEQUENTIAL FILES");
         raise Incomplete;
      when others =>
         Failed ("EXCEPTION RAISED DURING SECOND OPEN");
         raise Incomplete;
   end;

   declare
      End_Reached : Boolean := False;
      Count       : Integer := 26;
      Num         : Integer;
   begin
      while Count <= 36 and not End_Reached loop
         begin
            Read (File1, Num);
            if Num /= Count then
               Failed
                 ("INCORRECT RESULTS READ FROM FILE " & Integer'Image (Num));
            end if;
            Count := Count + 1;
         exception
            when End_Error =>
               End_Reached := True;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED DURING " & "READING - 1");
               raise Incomplete;
         end;
      end loop;
      if Count <= 36 then
         Failed ("FILE WAS INCOMPLETE");
         raise Incomplete;
      else
         begin
            Read (File1, Num);
            Failed ("END_ERROR NOT RAISED BY ATTEMPT TO READ");
         exception
            when End_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED " & "DURING READING - 2");
               raise Incomplete;
         end;
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

end Ce2208b;
