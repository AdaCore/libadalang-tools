-- CE3106A.ADA

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
--     CHECK THAT CLOSING A FILE HAS THE FOLLOWING EFFECT:
--          1) IF THERE IS NO LINE TERMINATOR, A LINE TERMINATOR, PAGE
--             TERMINATOR, AND FILE TERMINATOR ARE WRITTEN AT THE END
--             OF THE FILE.
--          2) IF THERE IS A LINE TERMINATOR BUT NO PAGE TERMINATOR, A
--             PAGE TERMINATOR AND A FILE TERMINATOR ARE WRITTEN.
--          3) IF THERE IS A PAGE TERMINATOR, A FILE TERMINATOR IS
--             WRITTEN.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 07/08/88  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3106a is

   Incomplete : exception;
   File1, File2, File3 : File_Type;
   Item                : Character;

begin

   Test
     ("CE3106A",
      "CHECK THAT CLOSING A FILE HAS THE CORRECT " &
      "EFFECT ON THE FILE CONCERNING LINE, PAGE, " &
      "AND FILE TERMINATORS");

   begin

      begin
         Create (File1, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE " & "WITH MODE OUT_FILE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE" & "WITH MODE OUT_FILE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
            raise Incomplete;
      end;

      Put (File1, 'A');
      New_Line (File1);
      Put (File1, 'B');

      Close (File1);

      begin
         Open (File1, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH MODE IN_FILE");
            raise Incomplete;
      end;

      Get (File1, Item);

      if Line (File1) /= 1 then
         Failed ("INCORRECT LINE NUMBER - 1");
      end if;

      Get (File1, Item);
      if Item /= 'B' then
         Failed ("INCORRECT VALUE READ - 1");
      end if;

      if Line (File1) /= 2 then
         Failed ("INCORRECT LINE NUMBER - 2");
      end if;

      if not End_Of_Line (File1) then
         Failed ("LINE TERMINATOR NOT WRITTEN WHEN FILE " & "IS CLOSED");
      end if;

      if not End_Of_Page (File1) then
         Failed ("PAGE TERMINATOR NOT WRITTEN WHEN FILE " & "IS CLOSED");
      end if;

      if not End_Of_File (File1) then
         Failed ("FILE TERMINATOR NOT WRITTEN WHEN FILE " & "IS CLOSED");
      end if;

      begin
         Delete (File1);
      exception
         when Use_Error =>
            null;
      end;

      Create (File2, Out_File, Legal_File_Name (2));
      Put (File2, 'A');
      New_Line (File2);
      Put (File2, 'B');
      New_Page (File2);
      Put (File2, 'C');
      New_Line (File2);

      Close (File2);

      Open (File2, In_File, Legal_File_Name (2));

      Get (File2, Item);

      Get (File2, Item);
      if Item /= 'B' then
         Failed ("INCORRECT VALUE READ - 2");
      end if;

      if Line (File2) /= 2 then
         Failed ("INCORRECT LINE NUMBER - 3");
      end if;

      Get (File2, Item);

      if Line (File2) /= 1 then
         Failed ("INCORRECT LINE NUMBER - 4");
      end if;

      if Page (File2) /= 2 then
         Failed ("INCORRECT PAGE NUMBER - 1");
      end if;

      if not End_Of_Page (File2) then
         Failed ("PAGE TERMINATOR NOT WRITTEN WHEN FILE " & "IS CLOSED - 2");
      end if;

      if not End_Of_File (File2) then
         Failed ("FILE TERMINATOR NOT WRITTEN WHEN FILE " & "IS CLOSED - 2");
      end if;

      begin
         Delete (File2);
      exception
         when Use_Error =>
            null;
      end;

      Create (File3, Out_File, Legal_File_Name (3));
      Put (File3, 'A');
      New_Page (File3);
      Put (File3, 'B');
      New_Page (File3);
      New_Line (File3);
      Put (File3, 'C');
      New_Page (File3);

      Close (File3);

      Open (File3, In_File, Legal_File_Name (3));

      Get (File3, Item);

      Get (File3, Item);
      if Item /= 'B' then
         Failed ("INCORRECT VALUE READ - 3");
      end if;

      Get (File3, Item);

      if Line (File3) /= 2 then
         Failed ("INCORRECT LINE NUMBER - 5");
      end if;

      if Page (File3) /= 3 then
         Failed ("INCORRECT PAGE NUMBER - 2");
      end if;

      if not End_Of_File (File3) then
         Failed ("FILE TERMINATOR NOT WRITTEN WHEN FILE " & "IS CLOSED - 3");
      end if;

      begin
         Delete (File3);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3106a;
