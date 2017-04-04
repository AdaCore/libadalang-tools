-- CE3705B.ADA

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
--     IF WIDTH IS ZERO, CHECK THAT END_ERROR IS RAISED IF THE ONLY
--     REMAINING CHARACTERS IN THE FILE CONSIST OF LINE TERMINATORS,
--     PAGE TERMINATORS, SPACES, AND HORIZONTAL TABULATION CHARACTERS.
--     AFTER END_ERROR IS RAISED, THE FILE SHOULD BE POSITIONED BEFORE
--     THE FILE TERMINATOR AND END_OF_FILE SHOULD BE TRUE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS THAT SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 07/15/88  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3705b is

   package Iio is new Integer_Io (Integer);
   use Iio;

   File : File_Type;
   Item : Integer;
   Incomplete : exception;

begin

   Test
     ("CE3705B",
      "IF WIDTH IS ZERO, CHECK THAT END_ERROR IS " &
      "RAISED IF THE ONLY REMAINING CHARACTERS IN " &
      "THE FILE CONSIST OF LINE TERMINATORS, PAGE " &
      "TERMINATORS, SPACES, AND HORIZONTAL TAB " &
      "CHARACTERS. AFTER END_ERROR IS RAISED, THE " &
      "FILE SHOULD BE POSITIONED BEFORE THE FILE " &
      "TERMINATOR AND END_OF_FILE SHOULD BE TRUE");

   begin

      begin
         Create (File, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE " & "WITH MODE OUT_FILE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE " & "WITH MODE OUT_FILE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
            raise Incomplete;
      end;

      Put (File, 2);
      New_Line (File);
      Put (File, 3);
      New_Line (File);
      New_Page (File);
      Put (File, Ascii.Ht);
      New_Line (File);
      New_Line (File);
      New_Page (File);
      Put (File, ' ');
      Put (File, Ascii.Ht);
      Put (File, ' ');

      Close (File);

      begin
         Open (File, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED ON OPEN WITH " & "MODE IN_FILE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON OPEN");
            raise Incomplete;
      end;

      Get (File, Item);
      if Item /= 2 then
         Failed ("INCORRECT VALUE READ - 1");
      end if;

      Get (File, Item);
      if Item /= 3 then
         Failed ("INCORRECT VALUE READ - 2");
      end if;

      begin
         Get (File, Item, Width => 0);
         Failed ("END_ERROR NOT RAISED FOR GET");
      exception
         when End_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON GET");
      end;

      if not End_Of_File (File) then
         Failed ("END_OF_FILE NOT TRUE AFTER RAISING EXCEPTION");
      end if;

      begin
         Delete (File);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3705b;
