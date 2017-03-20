-- C95085B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED UNDER APPROPRIATE CIRCUMSTANCES
-- WITH RESPECT TO PARAMETERS OF RECORD TYPES IN ENTRY CALLS.  SUBTESTS
-- INVOLVE ACTUAL RECORD PARAMETERS WHOSE CONSTRAINT VALUES ARE NOT
-- EQUAL TO THE CONSTRAINTS ON THEIR CORRESPONDING FORMAL PARAMETERS:
--        (A) IN PARAMETER, STATIC AGGREGATE.
--        (B) IN PARAMETER, DYNAMIC AGGREGATE.
--        (C) IN PARAMETER, VARIABLE.
--        (D) IN OUT PARAMETER, EXCEPTION RAISED ON CALL.
--        (E) OUT PARAMETER, EXCEPTION RAISED ON CALL.

-- JWC 10/25/85

with Report; use Report;
procedure C95085b is

   subtype Int is Integer range 0 .. 10;

   type Rec (N : Int := 0) is record
      A : String (1 .. N);
   end record;

   subtype Srec is Rec (N => 3);

begin

   Test
     ("C95085B",
      "CHECK RAISING OF CONSTRAINT_ERROR FOR " & "PARAMETERS OF RECORD TYPES");

   declare

      task Tsk1 is
         entry E (R : in Srec);
      end Tsk1;

      task body Tsk1 is
      begin
         loop
            begin
               select
                  accept E (R : in Srec) do
                     Failed ("EXCEPTION NOT RAISED ON " & "CALL TO TSK1");
                  end E;
               or
                  terminate;
               end select;
            exception
               when others =>
                  Failed ("EXCEPTION RAISED IN TSK1");
            end;
         end loop;
      end Tsk1;

   begin

      begin -- (A)
         Tsk1.E ((2, "AA"));
         Failed ("EXCEPTION NOT RAISED IN SUBTEST (A)");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED IN SUBTEST (A)");
      end; -- (A)

      begin -- (B)
         Tsk1.E ((Ident_Int (2), "AA"));
         Failed ("EXCEPTION NOT RAISED IN SUBTEST (B)");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED IN SUBTEST (B)");
      end; -- (B)

      declare -- (C)
         R : Rec := (Ident_Int (2), "AA");
      begin -- (C)
         Tsk1.E (R);
         Failed ("EXCEPTION NOT RAISED IN SUBTEST (C)");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED IN SUBTEST (C)");
      end; -- (C)

   end;

   declare -- (D)

      R : Rec := (Ident_Int (2), "AA");

      task Tsk2 is
         entry E (R : in out Srec);
      end Tsk2;

      task body Tsk2 is
      begin
         select
            accept E (R : in out Srec) do
               Failed ("EXCEPTION NOT RAISED ON CALL TO " & "TSK2");
            end E;
         or
            terminate;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TSK2");
      end Tsk2;

   begin -- (D)
      Tsk2.E (R);
      Failed ("EXCEPTION NOT RAISED IN SUBTEST (D)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN SUBTEST (D)");
   end; -- (D)

   declare -- (E)

      R : Rec;

      task Tsk3 is
         entry E (R : out Srec);
      end Tsk3;

      task body Tsk3 is
      begin
         select
            accept E (R : out Srec) do
               Failed ("EXCEPTION NOT RAISED ON CALL TO " & "TSK3");
            end E;
         or
            terminate;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TSK3");
      end Tsk3;

   begin -- (E)
      Tsk3.E (R);
      Failed ("EXCEPTION NOT RAISED IN SUBTEST (E)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN SUBTEST (E)");
   end; -- (E)

   Result;

end C95085b;
