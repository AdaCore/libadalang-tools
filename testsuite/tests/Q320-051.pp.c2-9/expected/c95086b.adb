-- C95086B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED FOR ACCESS PARAMETERS
--   BEFORE AN ENTRY CALL, WHEN AN IN OR IN OUT ACTUAL ACCESS
--   PARAMETER HAS VALUE NULL, BUT WITH CONSTRAINTS DIFFERENT
--   FROM THE FORMAL PARAMETER.
--
--   SUBTESTS ARE:
--       (A) IN MODE, STATIC ONE DIMENSIONAL BOUNDS.
--       (B) IN OUT MODE, DYNAMIC RECORD DISCRIMINANTS.
--       (C) CASE (A), BUT ACTUAL PARAMETER IS A TYPE CONVERSION.
--       (D) CASE (B), BUT ACTUAL PARAMETER IS A TYPE CONVERSION.

-- RJW 1/27/86

with Report; use Report;
procedure C95086b is

begin
   Test
     ("C95086B",
      "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
      "BEFORE AN ENTRY CALL, WHEN AN IN OR IN OUT ACTUAL " &
      "ACCESS PARAMETER HAS VALUE NULL, BUT WITH CONSTRAINTS " &
      "DIFFERENT FROM THE FORMAL PARAMETER");

   --------------------------------------------------

   declare -- (A)

      type E is (E1, E2, E3, E4);
      type T is array (E range <>) of Integer;

      type A is access T;
      subtype Sa is A (E2 .. E4);
      V : A (E1 .. E2) := null;

      task T1 is
         entry P (X : Sa);
      end T1;

      task body T1 is
      begin
         accept P (X : Sa);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (A)");
      end T1;

   begin -- (A)

      T1.P (V);

   exception
      when others =>
         Failed ("EXCEPTION RAISED - (A)");
   end; -- (A)

   --------------------------------------------------

   declare -- (B)

      type Arr is array (Character range <>) of Integer;

      type T (B : Boolean := False; C : Character := 'A') is record
         I : Integer;
         case B is
            when False =>
               J : Integer;
            when True =>
               A : Arr ('A' .. C);
         end case;
      end record;

      type A is access T;
      subtype Sa is A (True, 'C');
      V : A (Ident_Bool (False), Ident_Char ('B')) := null;

      task T1 is
         entry P (X : in out Sa);
      end T1;

      task body T1 is
      begin
         accept P (X : in out Sa) do
            null;
         end P;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (B)");
      end T1;

   begin -- (B)

      T1.P (V);

   exception
      when others =>
         Failed ("EXCEPTION RAISED - (B)");
   end; -- (B)

   --------------------------------------------------

   declare -- (C)

      type E is (E1, E2, E3, E4);
      type T is array (E range <>) of Integer;

      type A is access T;
      subtype Sa is A (E2 .. E4);
      V : A (E1 .. E2) := null;

      task T1 is
         entry P (X : Sa);
      end T1;

      task body T1 is
      begin
         accept P (X : Sa) do
            null;
         end P;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (C)");
      end T1;

   begin -- (C)

      T1.P (Sa (V));

   exception
      when others =>
         Failed ("EXCEPTION RAISED - (C)");
   end; -- (C)

   --------------------------------------------------

   declare -- (D)

      type Arr is array (Character range <>) of Integer;

      type T (B : Boolean := False; C : Character := 'A') is record
         I : Integer;
         case B is
            when False =>
               J : Integer;
            when True =>
               A : Arr ('A' .. C);
         end case;
      end record;

      type A is access T;
      subtype Sa is A (True, 'C');
      V : A (Ident_Bool (False), Ident_Char ('B')) := null;

      task T1 is
         entry P (X : in out Sa);
      end T1;

      task body T1 is
      begin
         accept P (X : in out Sa);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (D)");
      end T1;

   begin -- (D)

      T1.P (Sa (V));

   exception
      when others =>
         Failed ("EXCEPTION RAISED - (D)");
   end; -- (D)

   --------------------------------------------------

   Result;
end C95086b;
