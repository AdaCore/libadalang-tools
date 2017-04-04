-- CE3413B.ADA

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
--     CHECK THAT PAGE RAISES LAYOUT_ERROR WHEN THE VALUE OF THE
--     PAGE NUMBER EXCEEDS COUNT'LAST.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- HISTORY:
--     JLH 07/27/88  CREATED ORIGINAL TEST.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3413b is

   File : File_Type;
   Incomplete, Inapplicable : exception;
   Item : String (1 .. 3) := "ABC";
   Lst  : Natural;

begin

   Test
     ("CE3413B",
      "CHECK THAT PAGE RAISES LAYOUT_ERROR WHEN THE " &
      "VALUE OF THE PAGE NUMBER EXCEEDS COUNT'LAST");

   begin

      if Count'Last > 150_000 then
         raise Inapplicable;
      end if;

      begin
         Create (File, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT " & "CREATE");
            raise Incomplete;
      end;

      for I in 1 .. Count'Last - 1 loop
         New_Page (File);
      end loop;

      Put (File, Item);

      New_Page (File);
      Put (File, "DEF");

      begin
         if Page (File) <= Positive_Count (Count'Last) then
            Failed ("PAGE NUMBER INCORRECT AFTER PAGE SET - 1");
         end if;
         Failed ("LAYOUT_ERROR NOT RAISED FOR PAGE - 1");
      exception
         when Layout_Error =>
            null;
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR RAISED FOR PAGE - 1");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED FOR PAGE - 1");
      end;

      Close (File);

      begin
         Open (File, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      for I in 1 .. Count'Last - 1 loop
         Skip_Page (File);
      end loop;

      if Page (File) /= Count'Last then
         Failed ("INCORRECT PAGE NUMBER");
      end if;

      Get_Line (File, Item, Lst);
      if Item /= "ABC" then
         Failed ("INCORRECT VALUE READ");
      end if;

      Skip_Page (File);

      begin
         if Page (File) <= Positive_Count (Count'Last) then
            Failed ("PAGE NUMBER INCORRECT AFTER PAGE SET - 2");
         end if;
         Failed ("LAYOUT_ERROR NOT RAISED FOR PAGE - 2");
      exception
         when Layout_Error =>
            null;
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR RAISED FOR PAGE - 2");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED FOR PAGE - 2");
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
      when Inapplicable =>
         Not_Applicable
           ("THE VALUE OF COUNT'LAST IS GREATER " &
            "THAN 150000.  THE CHECKING OF THIS " &
            "OBJECTIVE IS IMPRACTICAL");

   end;

   Result;

end Ce3413b;
