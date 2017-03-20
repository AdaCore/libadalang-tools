-- C64105B.ADA

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
--   IN THE FOLLOWING CIRCUMSTANCES:
--       (1) BEFORE THE CALL, WHEN AN IN OR IN OUT ACTUAL ACCESS
--           PARAMETER HAS VALUE NULL, BUT WITH CONSTRAINTS DIFFERENT
--           FROM THE FORMAL PARAMETER.
--       (2)
--       (3)
--   SUBTESTS ARE:
--       (A) CASE 1, IN MODE, STATIC ONE DIMENSIONAL BOUNDS.
--       (B) CASE 1, IN OUT MODE, DYNAMIC RECORD DISCRIMINANTS.
--       (C) CASE (A), BUT ACTUAL PARAMETER IS A TYPE CONVERSION.
--       (D) CASE (B), BUT ACTUAL PARAMETER IS A TYPE CONVERSION.

-- JRK 3/20/81
-- SPS 10/26/82
-- CPP 8/6/84

with Report;
procedure C64105b is

   use Report;

begin
   Test
     ("C64105B",
      "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
      "BEFORE THE CALL, WHEN AN IN OR IN OUT ACTUAL ACCESS " &
      "PARAMETER HAS VALUE NULL, BUT WITH CONSTRAINTS DIFFERENT " &
      "FROM THE FORMAL PARAMETER");

   --------------------------------------------------

   declare -- (A)

      type E is (E1, E2, E3, E4);
      type T is array (E range <>) of Integer;

      type A is access T;
      subtype Sa is A (E2 .. E4);
      V : A (E1 .. E2) := null;

      procedure P (X : Sa) is
      begin
         null;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (A)");
      end P;

   begin -- (A)

      P (V);

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

      procedure P (X : in out Sa) is
      begin
         null;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (B)");
      end P;

   begin -- (B)

      P (V);

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

      procedure P (X : Sa) is
      begin
         null;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (C)");
      end P;

   begin -- (C)

      P (Sa (V));

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

      procedure P (X : in out Sa) is
      begin
         null;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (D)");
      end P;

   begin -- (D)

      P (Sa (V));

   exception
      when others =>
         Failed ("EXCEPTION RAISED - (D)");
   end; -- (D)

   --------------------------------------------------

   Result;
end C64105b;
