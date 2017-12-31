-- C910002.A
--
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
--
-- OBJECTIVE:
--      Check that the contents of a task object include the values
--      of its discriminants.
--      Check that selected_component notation can be used to
--      denote a discriminant of a task.
--
-- TEST DESCRIPTION:
--      This test declares a task type that contains discriminants.
--      Objects of the task type are created with different values.
--      The task type has nested tasks that are used to check that
--      the discriminate values are the expected values.
--      Note that the names of the discriminants in the body of task
--      type DTT denote the current instance of the unit.
--
--
-- CHANGE HISTORY:
--      12 OCT 95   SAIC    Initial release for 2.1
--       8 MAY 96   SAIC    Incorporated Reviewer comments.
--
--!

with Report;
procedure C910002 is
   Verbose : constant Boolean := False;
begin
   Report.Test
     ("C910002",
      "Check that selected_component notation can be" &
      " used to access task discriminants");
   declare

      task type Dtt (Ia, Ib : Integer; Ca, Cb : Character) is
         entry Check_Values (First_Int : Integer; First_Char : Character);
      end Dtt;

      task body Dtt is
         Int1  : Integer;
         Char1 : Character;

         -- simple nested task to check the character values
         task Check_Chars is
            entry Start_Check;
         end Check_Chars;
         task body Check_Chars is
         begin
            accept Start_Check;
            if Dtt.Ca /= Char1 or Dtt.Cb /= Character'Succ (Char1) then
               Report.Failed
                 ("character check failed.  Expected: '" & Char1 &
                  Character'Succ (Char1) & "' but found '" & Dtt.Ca & Dtt.Cb &
                  "'");
            elsif Verbose then
               Report.Comment ("char check for " & Char1);
            end if;
         exception
            when others =>
               Report.Failed ("exception in Check_Chars");
         end Check_Chars;

         -- use a discriminated task to check the integer values
         task type Check_Ints (First : Integer);
         task body Check_Ints is
         begin
            if Dtt.Ia /= Check_Ints.First or Ib /= First + 1 then
               Report.Failed
                 ("integer check failed.  Expected:" &
                  Integer'Image (Check_Ints.First) &
                  Integer'Image (First + 1) & " but found" &
                  Integer'Image (Dtt.Ia) & Integer'Image (Ib));
            elsif Verbose then
               Report.Comment ("int check for" & Integer'Image (First));
            end if;
         exception
            when others =>
               Report.Failed ("exception in Check_Ints");
         end Check_Ints;
      begin
         accept Check_Values (First_Int : Integer; First_Char : Character) do
            Int1  := First_Int;
            Char1 := First_Char;
         end Check_Values;

         -- kick off the character check
         Check_Chars.Start_Check;

         -- do the integer check
         declare
            Int_Checker : Check_Ints (Int1);
         begin
            null;  -- let task do its thing
         end;

         -- do one test here too
         if Dtt.Ia /= Int1 then
            Report.Failed
              ("DTT check failed.  Expected:" & Integer'Image (Int1) &
               " but found:" & Integer'Image (Dtt.Ia));
         elsif Verbose then
            Report.Comment ("DTT check for" & Integer'Image (Int1));
         end if;
      exception
         when others =>
            Report.Failed ("exception in DTT");
      end Dtt;

      T1a : Dtt (1, 2, 'a', 'b');
      T9c : Dtt (9, 10, 'C', 'D');
   begin   -- test encapsulation
      T1a.Check_Values (1, 'a');
      T9c.Check_Values (9, 'C');
   end;

   Report.Result;
end C910002;
