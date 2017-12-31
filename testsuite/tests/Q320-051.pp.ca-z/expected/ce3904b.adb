-- CE3904B.ADA

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
--     CHECK THAT END_ERROR IS RAISED BY GET WITH AN ENUMERATION TYPE
--     WHEN THE ONLY REMAINING CHARACTERS IN THE FILE ARE SPACES,
--     HORIZONTAL TABULATION CHARACTERS, LINE TERMINATORS, AND PAGE
--     TERMINATORS.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS THAT SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 07/15/88  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3904b is

   type Color is (Red, Blue, Green);
   package Color_Io is new Enumeration_Io (Color);
   use Color_Io;

   File : File_Type;
   Item : Color;
   Incomplete : exception;

begin

   Test
     ("CE3904B",
      "CHECK THAT END_ERROR IS RAISED BY GET WITH " &
      "AN ENUMERATION TYPE WHEN THE ONLY REMAINING " &
      "CHARACTERS IN THE FILE ARE SPACES, HORIZONTAL " &
      "TABULATION CHARACTERS, LINE TERMINATORS, AND " & "PAGE TERMINATORS");

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

      Put (File, Red);
      New_Line (File);
      New_Line (File);
      New_Page (File);
      Put (File, Ascii.Ht);
      Put (File, Green);
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
      if Item /= Red then
         Failed ("INCORRECT VALUE READ - 1");
      end if;

      Get (File, Item);
      if Item /= Green then
         Failed ("INCORRECT VALUE READ - 2");
      end if;

      begin
         Get (File, Item);
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

end Ce3904b;
