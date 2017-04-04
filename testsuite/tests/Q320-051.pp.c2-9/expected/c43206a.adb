-- C43206A.ADA

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
-- CHECK THAT THE BOUNDS OF A NULL ARRAY AGGREGATE ARE DETERMINED BY THE BOUNDS
-- SPECIFIED BY THE CHOICES. IN PARTICULAR, CHECK THAT:

--   A) THE UPPER BOUND IS NOT REQUIRED TO BE THE PREDECESSOR OF
--      THE LOWER BOUND.

--   B) NEITHER THE UPPER NOR THE LOWER BOUND NEED BELONG TO THE
--      INDEX SUBTYPE FOR NULL RANGES.

--   C) IF ONE CHOICE OF A MULTIDIMENSIONAL AGGREGATE IS NON-NULL
--      BUT THE AGGREGATE IS A NULL ARRAY, CONSTRAINT_ERROR IS
--      RAISED WHEN THE NON-NULL CHOICES DO NOT BELONG TO THE
--      INDEX SUBTYPE.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X

-- EG  02/02/84
-- JBG 12/6/84
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

with Report;

procedure C43206a is

   use Report;

begin

   Test
     ("C43206A",
      "CHECK THAT THE BOUNDS OF A NULL ARRAY ARE " &
      "DETERMINED BY THE BOUNDS SPECIFIED BY THE " &
      "CHOICES");

   declare

      subtype St1 is Integer range 10 .. 15;
      subtype St2 is Integer range 1 .. 5;

      type T1 is array (St1 range <>) of Integer;
      type T2 is array (St2 range <>, St1 range <>) of Integer;

   begin

      Case_A : begin

         Case_A1 : declare

            procedure Proc1 (A : T1) is
            begin
               if A'First /= 12 or A'Last /= 10 then
                  Failed ("CASE A1 : INCORRECT BOUNDS");
               end if;
            end Proc1;

         begin

            Proc1 ((12 .. 10 => -2));

         exception

            when others =>
               Failed ("CASE A1 : EXCEPTION RAISED");

         end Case_A1;

         Case_A2 : declare

            procedure Proc1 (A : String) is
            begin
               if A'First /= 5 or A'Last /= 2 then
                  Failed ("CASE A2 : INCORRECT BOUNDS");
               end if;
            end Proc1;

         begin

            Proc1 ((5 .. 2 => 'E'));

         exception

            when others =>
               Failed ("CASE A2 : EXCEPTION RAISED");

         end Case_A2;

      end Case_A;

      Case_B : begin

         Case_B1 : declare

            procedure Proc1 (A : T1; L, U : Integer) is
            begin
               if A'First /= L or A'Last /= U then
                  Failed ("CASE B1 : INCORRECT BOUNDS");
               end if;
            end Proc1;

         begin

            begin

               Proc1 ((5 .. Integer'First => -2), 5, Integer'First);

            exception

               when Constraint_Error =>
                  Failed
                    ("CASE B1A : CONSTRAINT_ERROR " & "RAISED FOR NULL RANGE");
               when others =>
                  Failed ("CASE B1A : EXCEPTION RAISED");

            end;

            begin

               Proc1 ((Ident_Int (6) .. 3 => -2), 6, 3);

            exception

               when others =>
                  Failed ("CASE B1B : EXCEPTION RAISED");

            end;

         end Case_B1;

         Case_B2 : declare

            procedure Proc1 (A : String) is
            begin
               if A'First /= 1 or A'Last /= Integer'First then
                  Failed ("CASE B2 : INCORRECT BOUNDS");
               end if;
            end Proc1;

         begin

            Proc1 ((1 .. Integer'First => ' '));

         exception

            when others =>
               Failed ("CASE B2 : EXCEPTION RAISED");

         end Case_B2;

      end Case_B;

      Case_C : begin

         Case_C1 : declare

            procedure Proc1 (A : T2) is
            begin
               if A'First (1) /= 5 or
                 A'Last (1) /= 3 or
                 A'First (2) /= Integer'Last - 1 or
                 A'Last (2) /= Integer'Last
               then
                  Failed ("CASE C1 : INCORRECT BOUNDS");
               end if;
            end Proc1;

         begin

            Proc1
              ((5 .. 3 =>
                  (Ident_Int (Integer'Last - 1) .. Ident_Int (Integer'Last) =>
                     -2)));
            Failed ("CASE C1 : CONSTRAINT_ERROR NOT RAISED");

         exception

            when Constraint_Error =>
               null;

            when others =>
               Failed ("CASE C1 : EXCEPTION RAISED");

         end Case_C1;

         Case_C2 : declare

            procedure Proc1 (A : T2) is
            begin
               if A'First (1) /= Integer'First or
                 A'Last (1) /= Integer'First + 1 or
                 A'First (2) /= 14 or
                 A'Last (2) /= 11
               then
                  Failed ("CASE C2 : INCORRECT BOUNDS");
               end if;
            end Proc1;

         begin

            Proc1
              ((Ident_Int (Integer'First) .. Ident_Int (Integer'First + 1) =>
                  (14 .. Ident_Int (11) => -2)));
            Failed ("CASE C2 : CONSTRAINT_ERROR NOT RAISED");

         exception

            when Constraint_Error =>
               null;

            when others =>
               Failed ("CASE C2 : EXCEPTION RAISED");

         end Case_C2;

      end Case_C;

   end;

   Result;

end C43206a;
