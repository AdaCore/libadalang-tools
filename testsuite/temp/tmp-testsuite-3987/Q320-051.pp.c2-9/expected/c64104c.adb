-- C64104C.ADA

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
--   APPROPRIATE CIRCUMSTANCES FOR ARRAY PARAMETERS, NAMELY
--   WHEN THE ACTUAL BOUNDS DON'T MATCH THE FORMAL BOUNDS
--   (BEFORE THE CALL FOR ALL MODES).
--   SUBTESTS ARE:
--      (A) IN MODE, ONE DIMENSION, STATIC AGGREGATE.
--      (B) IN MODE, TWO DIMENSIONS, DYNAMIC AGGREGATE.
--      (C) IN MODE, TWO DIMENSIONS, DYNAMIC VARIABLE.
--      (D) IN OUT MODE, THREE DIMENSIONS, STATIC VARIABLE.
--      (E) OUT MODE, ONE DIMENSION, DYNAMIC VARIABLE.
--      (F) IN OUT MODE, NULL STRING AGGREGATE.
--      (G) IN OUT MODE, TWO DIMENSIONS, NULL AGGREGATE (OK CASE).
--          IN OUT MODE, TWO DIMENSIONS, NULL AGGREGATE.

-- JRK 3/17/81
-- SPS 10/26/82
-- CPP 8/6/84
-- PWN 11/30/94 REMOVED TEST ILLEGAL IN ADA 9X.

with Report;
procedure C64104c is

   use Report;

begin
   Test
     ("C64104C",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "ACTUAL ARRAY BOUNDS DON'T MATCH FORMAL BOUNDS");

   --------------------------------------------------

   declare -- (A)
      subtype St is String (1 .. 3);

      procedure P (A : St) is
      begin
         Failed ("EXCEPTION NOT RAISED ON CALL - (A)");
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (A)");
      end P;

   begin -- (A)

      P ("AB");
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

      procedure P (A : T) is
      begin
         Failed ("EXCEPTION NOT RAISED ON CALL - (B)");
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (B)");
      end P;

   begin -- (B)

      P ((1 .. 3 => (1 .. Ident_Int (2) => 0)));
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

      procedure P (A : St) is
      begin
         Failed ("EXCEPTION NOT RAISED ON CALL - (C)");
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (C)");
      end P;

   begin -- (C)

      P (V);
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

      procedure P (A : in out St) is
      begin
         Failed ("EXCEPTION NOT RAISED ON CALLL - (D)");
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (D)");
      end P;

   begin -- (D)

      P (V);
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

      procedure P (A : in out St) is
      begin
         Comment ("OK CASE CALLED CORRECTLY");
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (G)");
      end P;

   begin -- (G)

      P (V);

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED ON OK CASE - (G)");
      when others =>
         Failed ("OTHER EXCEPTION RAISED ON OK CASE - (G)");
   end; -- (G)

   --------------------------------------------------

   --------------------------------------------------

   Result;
end C64104c;
