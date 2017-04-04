-- CE2401K.ADA

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
--     CHECK THAT DATA CAN BE OVERWRITTEN IN THE DIRECT FILE AND
--     THE CORRECT VALUES CAN LATER BE READ.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF INOUT_FILE MODE AND OPENING OF OUT_FILE MODE FOR
--     DIRECT FILES.

-- HISTORY:
--     DWC 08/12/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Direct_Io;

procedure Ce2401k is
   End_Subtest : exception;
begin

   Test
     ("CE2401K",
      "CHECK THAT DATA CAN BE OVERWRITTEN IN " &
      "THE DIRECT FILE AND THE CORRECT VALUES " &
      "CAN LATER BE READ.");

   declare
      package Dir_Io is new Direct_Io (Integer);
      use Dir_Io;
      File : File_Type;
   begin
      begin
         Create (File, Inout_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable ("CREATE WITH INOUT_FILE MODE " & "NOT SUPPORTED");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED ERROR RAISED ON " & "CREATE");
            raise End_Subtest;
      end;

      declare
         Out_Item1 : Integer        := 10;
         Out_Item2 : Integer        := 21;
         In_Item   : Integer;
         One       : Positive_Count := 1;
         Two       : Positive_Count := 2;
      begin
         begin
            Write (File, Out_Item1, One);
            Write (File, Out_Item2, Two);
            Write (File, Out_Item2, One);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE " & "IN INOUT_FILE MODE");
               raise End_Subtest;
         end;

         begin
            Read (File, In_Item, One);
            if Out_Item2 /= In_Item then
               Failed ("INCORRECT INTEGER VALUE READ - 1");
               raise End_Subtest;
            end if;
         end;

         begin
            Read (File, In_Item, Two);
            if Out_Item2 /= In_Item then
               Failed ("INCORRECT INTEGER VALUE READ - 2");
               raise End_Subtest;
            end if;
         end;

         Close (File);

         begin
            Open (File, Out_File, Legal_File_Name);
         exception
            when Use_Error =>
               raise End_Subtest;
         end;

         begin
            Write (File, Out_Item1, One);
            Write (File, Out_Item2, Two);
            Write (File, Out_Item1, Two);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE " & "IN OUT_FILE MODE");
               raise End_Subtest;
         end;

         begin
            Reset (File, In_File);
         exception
            when Use_Error =>
               raise End_Subtest;
         end;

         begin
            Read (File, In_Item, One);
            if Out_Item1 /= In_Item then
               Failed ("INCORRECT INTEGER VALUE READ - 3");
               raise End_Subtest;
            end if;
         exception
            when Use_Error =>
               Failed ("READ IN IN_FILE MODE - 1");
         end;

         begin
            Read (File, In_Item, Two);
            if Out_Item1 /= In_Item then
               Failed ("INCORRECT INTEGER VALUE READ - 4");
               raise End_Subtest;
            end if;
         exception
            when Use_Error =>
               Failed ("READ IN IN_FILE MODE - 2");
         end;
      end;

      begin
         Delete (File);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   Result;

end Ce2401k;
