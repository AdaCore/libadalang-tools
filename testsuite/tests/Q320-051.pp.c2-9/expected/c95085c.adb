-- C95085C.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED UNDER THE
-- APPROPRIATE CIRCUMSTANCES FOR ARRAY PARAMETERS IN ENTRY CALLS,
-- NAMELY WHEN THE ACTUAL BOUNDS DON'T MATCH THE FORMAL BOUNDS
-- (BEFORE THE CALL FOR ALL MODES).
-- SUBTESTS ARE:
--      (A) IN MODE, ONE DIMENSION, STATIC AGGREGATE.
--      (B) IN MODE, TWO DIMENSIONS, DYNAMIC AGGREGATE.
--      (C) IN MODE, TWO DIMENSIONS, DYNAMIC VARIABLE.
--      (D) IN OUT MODE, THREE DIMENSIONS, STATIC VARIABLE.
--      (E) OUT MODE, ONE DIMENSION, DYNAMIC VARIABLE.
--      (F) IN OUT MODE, NULL STRING AGGREGATE.
--      (G) IN OUT MODE, TWO DIMENSIONS, NULL AGGREGATE (OK CASE).
--          IN OUT MODE, TWO DIMENSIONS, NULL AGGREGATE.

-- JWC 10/28/85
-- PWN 11/30/94 REMOVED TEST ILLEGAL IN ADA 9X.

with Report; use Report;
procedure C95085c is

begin
   Test
     ("C95085C",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "ACTUAL ARRAY BOUNDS DON'T MATCH FORMAL BOUNDS");

   --------------------------------------------------

   declare -- (A)
      subtype St is String (1 .. 3);

      task Tsk is
         entry E (A : St);
      end Tsk;

      task body Tsk is
      begin
         select
            accept E (A : St) do
               Failed ("EXCEPTION NOT RAISED ON CALL - (A)");
            end E;
         or
            terminate;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (A)");
      end Tsk;

   begin -- (A)

      Tsk.E ("AB");
      Failed ("EXCEPTION NOT RAISED BEFORE CALL - (A)");

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (A)");
   end; -- (A)

   --------------------------------------------------

   declare -- (B)

      subtype S is Integer range 1 .. 3;
      type T is array (S, S) of Integer;

      task Tsk is
         entry E (A : T);
      end Tsk;

      task body Tsk is
      begin
         select
            accept E (A : T) do
               Failed ("EXCEPTION NOT RAISED ON CALL - (B)");
            end E;
         or
            terminate;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (B)");
      end Tsk;

   begin -- (B)

      Tsk.E ((1 .. 3 => (1 .. Ident_Int (2) => 0)));
      Failed ("EXCEPTION NOT RAISED BEFORE CALL - (B)");

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (B)");
   end; -- (B)

   --------------------------------------------------

   declare -- (C)

      subtype S is Integer range 1 .. 5;
      type T is array (S range <>, S range <>) of Integer;
      subtype St is T (1 .. 3, 1 .. 3);
      V : T (1 .. Ident_Int (2), 1 .. 3) :=
        (1 .. Ident_Int (2) => (1 .. 3 => 0));

      task Tsk is
         entry E (A : St);
      end Tsk;

      task body Tsk is
      begin
         select
            accept E (A : St) do
               Failed ("EXCEPTION NOT RAISED ON CALL - (C)");
            end E;
         or
            terminate;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (C)");
      end Tsk;

   begin -- (C)

      Tsk.E (V);
      Failed ("EXCEPTION NOT RAISED BEFORE CALL - (C)");

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (C)");
   end; -- (C)

   --------------------------------------------------

   declare -- (D)

      subtype S is Integer range 1 .. 5;
      type T is array (S range <>, S range <>, S range <>) of Integer;
      subtype St is T (1 .. 3, 1 .. 3, 1 .. 3);
      V : T (1 .. 3, 1 .. 2, 1 .. 3) := (1 .. 3 => (1 .. 2 => (1 .. 3 => 0)));

      task Tsk is
         entry E (A : in out St);
      end Tsk;

      task body Tsk is
      begin
         select
            accept E (A : in out St) do
               Failed ("EXCEPTION NOT RAISED ON CALL - (D)");
            end E;
         or
            terminate;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (D)");
      end Tsk;

   begin -- (D)

      Tsk.E (V);
      Failed ("EXCEPTION NOT RAISED BEFORE CALL - (D)");

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (D)");
   end; -- (D)

   --------------------------------------------------

   declare -- (G)

      subtype S is Integer range 1 .. 5;
      type T is array (S range <>, S range <>) of Character;
      subtype St is T (2 .. 1, 2 .. 1);
      V : T (2 .. 1, 2 .. 1) := (2 .. 1 => (2 .. 1 => ' '));

      task Tsk is
         entry E (A : in out St);
      end Tsk;

      task body Tsk is
      begin
         select
            accept E (A : in out St) do
               Comment ("OK CASE CALLED CORRECTLY");
            end E;
         or
            terminate;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (G)");
      end Tsk;

   begin -- (G)

      Tsk.E (V);

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED ON OK CASE - (G)");
      when others =>
         Failed ("OTHER EXCEPTION RAISED ON OK CASE - (G)");
   end; -- (G)

   --------------------------------------------------

   Result;
end C95085c;
