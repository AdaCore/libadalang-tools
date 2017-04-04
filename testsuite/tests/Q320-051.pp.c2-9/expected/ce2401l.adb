-- CE2401L.ADA

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
--     CHECK THAT REWRITING AN ELEMENT DOES NOT CHANGE THE SIZE OF
--     THE FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH INOUT_FILE MODE FOR DIRECT FILES.

-- HISTORY:
--     DWC 08/12/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Direct_Io;

procedure Ce2401l is
   End_Subtest : exception;
begin

   Test
     ("CE2401L",
      "CHECK THAT REWRITING AN ELEMENT DOES NOT " &
      "CHANGE THE SIZE OF THE FILE");

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
         Out_Item1     : Integer        := 10;
         Out_Item2     : Integer        := 21;
         Out_Item4     : Integer        := 43;
         In_Item       : Integer;
         One           : Positive_Count := 1;
         Two           : Positive_Count := 2;
         Four          : Positive_Count := 4;
         Old_File_Size : Positive_Count;
      begin
         begin
            Write (File, Out_Item1, One);
            Write (File, Out_Item4, Four);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE " & "IN INOUT_FILE MODE");
               raise End_Subtest;
         end;

         Old_File_Size := Size (File);

         Write (File, Out_Item1, One);
         Write (File, Out_Item4, Four);

         if Old_File_Size /= Size (File) then
            Failed ("FILE SIZE CHANGED DURING REWRITE - 1");
            raise End_Subtest;
         end if;

         Write (File, Out_Item1, One);
         Write (File, Out_Item2, Two);
         Write (File, Out_Item4, Four);

         Old_File_Size := Size (File);

         Write (File, Out_Item1, Four);

         if Old_File_Size /= Size (File) then
            Failed ("FILE SIZE CHANGED DURING REWRITE - 2");
            raise End_Subtest;
         end if;
      exception
         when End_Subtest =>
            null;
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

end Ce2401l;
