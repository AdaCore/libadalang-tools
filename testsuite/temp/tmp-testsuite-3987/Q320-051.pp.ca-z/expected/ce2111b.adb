-- CE2111B.ADA

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
--     CHECK THAT A SUCCESSFUL RESET POSITIONS THE INDEX CORRECTLY
--     TO THE START OF THE FILE FOR DIRECT IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     RESET FOR DIRECT FILES.

-- HISTORY:
--     DLD 08/13/82
--     JBG 03/24/83
--     EG  05/29/85
--     JLH 07/23/87  ADDED CHECKS FOR USE_ERROR WHEN FILE IS RESET.

with Report; use Report;
with Direct_Io;

procedure Ce2111b is

   package Dir_Io is new Direct_Io (Integer);
   use Dir_Io;
   Test_File_One : Dir_Io.File_Type;
   Datum         : Integer;
   Incomplete : exception;

begin

   Test
     ("CE2111B",
      "CHECK THAT SUCCESSFUL RESETS POSITION THE " & "INDEX CORRECTLY");

-- CREATE AND INITIALIZE TEST FILE

   begin
      Create (Test_File_One, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable ("NAME_ERROR RAISED ON CREATE");
         raise Incomplete;
   end;

   Write (Test_File_One, 5);
   Write (Test_File_One, 6);
   Write (Test_File_One, 7);
   Write (Test_File_One, 8);

-- CHECK THAT RESET POSITIONS INDEX CORRECTLY FOR OUT_FILE

   begin
      Reset (Test_File_One);
      if Index (Test_File_One) /= 1 then
         Failed ("RESET INCORRECTLY POSITIONED FILE FOR " & "OUT_FILE");
      end if;
   exception
      when Use_Error =>
         Not_Applicable ("RESET NOT SUPPORTED FOR OUT_FILE");
         raise Incomplete;
   end;

-- WRITE MORE DATA

   Write (Test_File_One, 2);
   Close (Test_File_One);

-- NOW CHECK TO SEE THAT RESET WORKED FOR OUT_FILE

   begin
      Open (Test_File_One, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("DIR_IO NOT SUPPORTED FOR IN_FILE OPEN");
         raise Incomplete;
   end;
   Read (Test_File_One, Datum);
   if Datum /= 2 then
      Failed ("RESET FAILED FOR OUT_FILE");
   end if;

-- POSITION POINTER APPROPRIATELY FOR IN_FILE RESET

   Read (Test_File_One, Datum);

-- RESET IN_FILE

   begin
      Reset (Test_File_One);
      if Index (Test_File_One) /= 1 then
         Failed ("RESET INCORRECTLY POSITIONED FILE " & "FOR IN_FILE");
      end if;
   exception
      when Use_Error =>
         Not_Applicable ("RESET NOT SUPPORTED FOR IN_FILE");
         raise Incomplete;
   end;

-- VALIDATE IN_FILE RESET

   Read (Test_File_One, Datum);
   if Datum /= 2 then
      Failed ("RESET FAILED FOR IN_FILE");
   end if;

-- VALIDATE RESET FOR IN_OUT FILE

   Close (Test_File_One);
   begin
      Open (Test_File_One, Inout_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("DIR_IO NOT SUPPORTED FOR INOUT_FILE " & "OPEN");
         raise Incomplete;
   end;

-- WRITE NEW DATA

   Write (Test_File_One, 3);

-- RESET INOUT_FILE

   begin
      Reset (Test_File_One);
      if Index (Test_File_One) /= 1 then
         Failed ("RESET INCORRECTLY POSITIONED FILE " & "FOR INOUT_FILE");
      end if;
   exception
      when Use_Error =>
         Not_Applicable ("RESET NOT SUPPORTED FOR INOUT_FILE");
         raise Incomplete;
   end;

-- VALIDATE RESET

   Read (Test_File_One, Datum);
   if Datum /= 3 then
      Failed ("RESET FAILED FOR INOUT_FILE");
   end if;

-- DELETE TEST FILE

   begin
      Delete (Test_File_One);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce2111b;
