-- CE2111I.ADA

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
--     CHECK THAT A SUPPLIED MODE PARAMETER IN A RESET CHANGES
--     THE MODE OF A GIVEN FILE.  IF NO PARAMETER IS SUPPLIED
--     THE MODE REMAINS THE SAME.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     RESET FOR SEQUENTIAL FILES.

-- HISTORY:
--     JLH 07/23/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Sequential_Io;

procedure Ce2111i is

   package Seq_Io is new Sequential_Io (Integer);
   use Seq_Io;
   Seq_File : Seq_Io.File_Type;
   Seq_Mode : Seq_Io.File_Mode;
   Incomplete : exception;
   Var1 : Integer := 5;

begin

   Test
     ("CE2111I",
      "CHECK THAT A SUPPLIED MODE PARAMETER SETS " &
      "THE MODE OF THE GIVEN FILE APPROPRIATELY");

-- CREATE SEQUENTIAL TEST FILE

   begin
      Create (Seq_File, Out_File, Legal_File_Name);
      Write (Seq_File, Var1);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable ("NAME_ERROR RAISED ON CREATE");
         raise Incomplete;
   end;

-- RESET TO DEFAULT

   begin
      Seq_Mode := In_File;
      Reset (Seq_File);
      Seq_Mode := Mode (Seq_File);
      if Seq_Mode /= Out_File then
         Failed ("DEFAULT RESET CHANGED MODE - SEQ");
      end if;
   exception
      when Use_Error =>
         Not_Applicable ("RESET NOT SUPPORTED FOR SEQ OUT_FILE " & "MODE");
         raise Incomplete;
   end;

-- RESET TO IN_FILE

   begin
      Seq_Mode := Out_File;
      Reset (Seq_File, In_File);
      Seq_Mode := Mode (Seq_File);
      if Seq_Mode /= In_File then
         Failed ("RESET TO IN_FILE FAILED - SEQ");
      end if;
   exception
      when Use_Error =>
         Not_Applicable
           ("RESET FROM OUT_FILE TO IN_FILE MODE " &
            "NOT SUPPORTED FOR SEQ FILES");
         raise Incomplete;
   end;

   begin
      Delete (Seq_File);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce2111i;
