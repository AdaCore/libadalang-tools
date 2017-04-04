-- C95086E.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED BEFORE OR AFTER THE ENTRY CALL FOR
-- IN OUT ARRAY PARAMETERS, WHERE THE ACTUAL PARAMETER HAS THE FORM OF A TYPE
-- CONVERSION. THE FOLLOWING CASES ARE TESTED:
--   (A) OK CASE.
--   (B) FORMAL CONSTRAINED, BOTH FORMAL AND ACTUAL HAVE SAME NUMBER
--       COMPONENTS PER DIMENSION, BUT ACTUAL INDEX BOUNDS LIE OUTSIDE
--       FORMAL INDEX SUBTYPE.
--   (C) FORMAL CONSTRAINED, FORMAL AND ACTUAL HAVE DIFFERENT NUMBER
--       COMPONENTS PER DIMENSION, BOTH FORMAL AND ACTUAL ARE NULL
--       ARRAYS.
--   (D) FORMAL CONSTRAINED, ACTUAL NULL, WITH INDEX BOUNDS OUTSIDE
--       FORMAL INDEX SUBTYPE.
--   (E) FORMAL UNCONSTRAINED, ACTUAL NULL, WITH INDEX BOUNDS OUTSIDE
--       FORMAL INDEX SUBTYPE FOR NULL DIMENSIONS ONLY.

-- RJW 2/3/86
-- TMB 11/15/95 ELIMINATED INCOMPATIBILITY WITH ADA95 TMB 11/19/96 FIXED
-- SLIDING PROBLEM IN SECTION D

with Report; use Report;
procedure C95086e is

begin
   Test
     ("C95086E",
      "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
      "BEFORE OR AFTER THE ENTRY CALL FOR IN OUT ARRAY " &
      "PARAMETERS, WITH THE ACTUAL HAVING THE FORM OF A TYPE " &
      "CONVERSION");

   ---------------------------------------------

   declare -- (A)

      subtype Index is Integer range 1 .. 5;
      type Array_Type is array (Index range <>, Index range <>) of Boolean;
      subtype Formal is Array_Type (1 .. 3, 1 .. 3);
      subtype Actual is Array_Type (1 .. 3, 1 .. 3);
      Ar     : Actual  := (1 .. 3 => (1 .. 3 => True));
      Called : Boolean := False;

      task T is
         entry E (X : in out Formal);
      end T;

      task body T is
      begin
         accept E (X : in out Formal) do
            Called := True;
         end E;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (A)");
      end T;

   begin -- (A)

      T.E (Formal (Ar));

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL - (A)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (A)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (A)");
   end; -- (A)

   ---------------------------------------------

   declare -- (B)

      subtype Index is Integer range 1 .. 3;
      type Formal is array (Index, Index) of Boolean;
      type Actual is array (3 .. 5, 3 .. 5) of Boolean;
      Ar     : Actual  := (3 .. 5 => (3 .. 5 => False));
      Called : Boolean := False;

      task T is
         entry E (X : in out Formal);
      end T;

      task body T is
      begin
         accept E (X : in out Formal) do
            Called   := True;
            X (3, 3) := True;
         end E;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (B)");
      end T;

   begin -- (B)

      T.E (Formal (Ar));
      if Ar (5, 5) /= True then
         Failed ("INCORRECT RETURNED VALUE - (B)");
      end if;

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL - (B)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (B)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (B)");
   end; -- (B)

   ---------------------------------------------

   declare -- (C)

      subtype Index is Integer range 1 .. 5;
      type Array_Type is array (Index range <>, Index range <>) of Character;
      subtype Formal is Array_Type (2 .. 0, 1 .. 3);
      Ar     : Array_Type (2 .. 1, 1 .. 3) := (2 .. 1 => (1 .. 3 => ' '));
      Called : Boolean                     := False;

      task T is
         entry E (X : in out Formal);
      end T;

      task body T is
      begin
         accept E (X : in out Formal) do
            if X'Last /= 0 and X'Last (2) /= 3 then
               Failed ("WRONG BOUNDS PASSED - (C)");
            end if;
            Called := True;
            X      := (2 .. 0 => (1 .. 3 => 'A'));
         end E;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (C)");
      end T;

   begin -- (C)

      T.E (Formal (Ar));
      if Ar'Last /= 1 and Ar'Last (2) /= 3 then
         Failed ("BOUNDS CHANGED - (C)");
      end if;

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL - (C)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (C)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (C)");
   end; -- (C)

   ---------------------------------------------

   declare -- (D)

      subtype Index is Integer range 1 .. 3;
      type Formal is
        array (Index range 1 .. 3, Index range 3 .. 1) of Character;
      type Actual is array (3 .. 5, 5 .. 3) of Character;
      Ar     : Actual  := (3 .. 5 => (5 .. 3 => ' '));
      Called : Boolean := False;

      task T is
         entry E (X : in out Formal);
      end T;

      task body T is
      begin
         accept E (X : in out Formal) do
            if X'Last /= 3 and X'Last (2) /= 1 then
               Failed ("WRONG BOUNDS PASSED - (D)");
            end if;
            Called := True;
            X      := (1 .. 3 => (3 .. 1 => 'A'));
         end E;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (D)");
      end T;

   begin -- (D)

      T.E (Formal (Ar));
      if Ar'Last /= 5 and Ar'Last (2) /= 3 then
         Failed ("BOUNDS CHANGED - (D)");
      end if;

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL - (D)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (D)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (D)");
   end; -- (D)

   ---------------------------------------------

   declare -- (E)

      subtype Index is Integer range 1 .. 3;
      type Formal is array (Index range <>, Index range <>) of Character;
      type Actual is
        array (Positive range 5 .. 2, Positive range 1 .. 3) of Character;
      Ar     : Actual  := (5 .. 2 => (1 .. 3 => ' '));
      Called : Boolean := False;

      task T is
         entry E (X : in out Formal);
      end T;

      task body T is
      begin
         accept E (X : in out Formal) do
            if X'Last /= 2 and X'Last (2) /= 3 then
               Failed ("WRONG BOUNDS PASSED - (E)");
            end if;
            Called := True;
            X      := (3 .. 1 => (1 .. 3 => ' '));
         end E;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (E)");
      end T;

   begin -- (E)

      T.E (Formal (Ar));
      if Ar'Last /= 2 and Ar'Last (2) /= 3 then
         Failed ("BOUNDS CHANGED - (E)");
      end if;

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL - (E)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (E)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (E)");
   end; -- (E)

   ---------------------------------------------

   Result;
end C95086e;
