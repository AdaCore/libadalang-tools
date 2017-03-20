-- CE2111F.ADA

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
--     CHECK THAT A SUCCESSFUL RESET POSITIONS THE FILE CORRECTLY
--     TO THE START OF THE FILE FOR SEQUENTIAL IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     RESET FOR SEQUENTIAL FILES.

-- HISTORY:
--     JLH 08/03/87 CREATED ORIGINAL TEST.

with Report; use Report;
with Sequential_Io;

procedure Ce2111f is

   package Seq_Io is new Sequential_Io (Integer);
   use Seq_Io;
   Test_File_One : Seq_Io.File_Type;
   Datum         : Integer;
   Incomplete : exception;

begin
   Test
     ("CE2111F",
      "CHECK THAT SUCCESSFUL RESET POSITIONS THE " & "FILE CORRECTLY");

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

-- CHECK THAT RESET POSITIONS INDEX CORRECTLY FOR OUT_FILE

   begin
      Reset (Test_File_One);
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
         Not_Applicable ("SEQ_IO NOT SUPPORTED FOR IN_FILE OPEN");
         raise Incomplete;
   end;

   Read (Test_File_One, Datum);

   if Datum /= 2 then
      Failed ("RESET INCORRECTLY POSITIONED FILE FOR OUT_FILE");
   end if;

-- RESET IN_FILE

   begin
      Reset (Test_File_One);
   exception
      when Use_Error =>
         Not_Applicable ("RESET NOT SUPPORTED FOR IN_FILE");
         raise Incomplete;
   end;

-- VALIDATE IN_FILE RESET

   Read (Test_File_One, Datum);

   if Datum /= 2 then
      Failed ("RESET INCORRECTLY POSITIONED FILE FOR IN_FILE");
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

end Ce2111f;
