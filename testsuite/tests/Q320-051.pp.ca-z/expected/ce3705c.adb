-- CE3705C.ADA

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
--     CHECK THAT THE LAST CHARACTER IN A FILE MAY BE READ WITHOUT
--     RAISING END_ERROR, AND THAT AFTER THE LAST CHARACTER OF THE
--     FILE HAS BEEN READ, ANY ATTEMPT TO READ FURTHER CHARACTERS
--     WILL RAISE END_ERROR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 07/18/88  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3705c is

   package Iio is new Integer_Io (Integer);
   use Iio;

   File : File_Type;
   Item : Integer;
   Incomplete : exception;

begin

   Test
     ("CE3705C",
      "CHECK THAT THE LAST CHARACTER IN A FILE MAY " &
      "BE READ WITHOUT RAISING END_ERROR, AND THAT " &
      "AFTER THE LAST CHARACTER OF THE FILE HAS BEEN " &
      "READ, ANY ATTEMPT TO READ FURTHER CHARACTERS " &
      "WILL RAISE END_ERROR");

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
      Put (File, 3);
      New_Line (File);
      New_Page (File);
      Put (File, 5);

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
      Get (File, Item);

      begin
         Get (File, Item);
         if Item /= 5 then
            Failed ("INCORRECT VALUE READ");
         end if;
         begin
            Get (File, Item);
            Failed
              ("END_ERROR NOT RAISED AFTER LAST " &
               "CHARACTER OF FILE HAS BEEN READ");
         exception
            when End_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED ON GET");
         end;
      exception
         when End_Error =>
            Failed
              ("END_ERROR RAISED WHEN READING LAST " & "CHARACTER OF FILE");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON GET - 2");
      end;

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

end Ce3705c;
