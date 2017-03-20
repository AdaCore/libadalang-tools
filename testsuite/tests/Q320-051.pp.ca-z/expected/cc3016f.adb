-- CC3016F.ADA

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
--   CHECK THAT AN INSTANTIATED PACKAGE HAS THE PROPERTIES REQUIRED
--   OF A PACKAGE.

--   CHECK THAT IF THE PARENT TYPE IN A DERIVED TYPE DEFINITION IS
--   A GENERIC FORMAL TYPE, THE OPERATIONS DECLARED FOR THE DERIVED
--   TYPE IN THE TEMPLATE ARE DETERMINED BY THE DECLARATION OF THE
--   FORMAL TYPE.  THE OPERATIONS DECLARED FOR DERIVED TYPE IN THE
--   INSTANCE ARE DETERMINED BY THE ACTUAL TYPE DENOTED BY THE FORMAL
--   PARAMETER.  SEE AI-00398.

-- HISTORY:
--   DAS  8 OCT 90   INITIAL VERSION.
--   JRL 02/19/93  ADDED USE CLAUSES FOR INSTANCES TO ENSURE DIRECT
--                 OPERATOR VISIBILITY. CHANGED NT4'LAST TO P4.NT4'LAST
--                 IN ASSIGNMENT STATEMENT FOR P4.X IN EXAMPLE_4.
--                 CORRECTED ABE ERRORS IN EXAMPLE_2 AND EXAMPLE_3.
--                 CHANGED R3."+" FROM MULTIPLICATION TO SUBTRACTION TO
--                 AVOID CONSTRAINT_ERROR.

with Report;

procedure Cc3016f is
begin
   Report.Test
     ("CC3016F",
      "CHECK THAT IF THE PARENT TYPE IN A " &
      "DERIVED TYPE DEFINITION IS A GENERIC " &
      "FORMAL TYPE, THE OPERATIONS DECLARED " &
      "FOR THE DERIVED TYPE IN THE TEMPLATE " &
      "ARE DETERMINED BY THE DECLARATION OF " &
      "THE FORMAL TYPE, AND THAT THE " &
      "OPERATIONS DECLARED FOR THE DERIVED " &
      "TYPE IN THE INSTANCE ARE DETERMINED BY " &
      "THE ACTUAL TYPE DENOTED BY THE FORMAL " &
      "PARAMETER (AI-00398)");
   Example_2 : declare
      generic
         type Priv is private;
      package Gp2 is
         type Nt2 is new Priv;
      end Gp2;

      package R2 is
         type T2 is range 1 .. 10;
         function F return T2;
      end R2;

      package P2 is new Gp2 (Priv => R2.T2);
      use P2;

      Xx1 : P2.Nt2;
      Xx2 : P2.Nt2;
      Xx3 : P2.Nt2;

      package body R2 is
         function F return T2 is
         begin
            return T2'Last;
         end F;
      end R2;
   begin
      Xx1 := 5;                   -- IMPLICIT CONVERSION FROM
      -- UNIVERSAL INTEGER TO P2.NT2
      -- IN P2.
      Xx2 := Xx1 + Xx1;           -- PREDEFINED "+" DECLARED FOR
      -- P2.NT2.
      Xx3 := P2.F;                -- FUNCTION F DERIVED WITH THE
      -- INSTANCE.

   end Example_2;

   Example_3 : declare
      generic
         type T3 is range <>;
      package Gp3 is
         type Nt3 is new T3;
         X : Nt3 := 5;
         Y : Nt3 := X + 3;      -- USES PREDEFINED "+" EVEN IN
         -- INSTANCES
      end Gp3;

      package R3 is
         type S is range 1 .. 10;
         function "+" (Left : in S; Right : in S) return S;
      end R3;

      package P3 is new Gp3 (T3 => R3.S);
      use P3;

      Z : P3.Nt3;

      package body R3 is
         function "+" (Left : in S; Right : in S) return S is
         begin  -- IMPLEMENT AS SUBTRACTION, NOT ADDITION
            return Left - Right;
         end "+";
      end R3;
   begin
      Z := P3.X + 3;     -- USES REDEFINED "+"

      if (P3.Y /= P3.Nt3'(8)) then
         Report.Failed ("PREDEFINED ""+"" NOT USED TO COMPUTE " & "P3.Y");
      end if;

      if (Z /= P3.Nt3'(2)) then
         Report.Failed ("REDEFINED ""+"" NOT USED TO COMPUTE Z");
      end if;
   end Example_3;

   Example_4 : declare
      generic
         type T4 is limited private;
      package Gp4 is
         type Nt4 is new T4;
         X : Nt4;
      end Gp4;

      package P4 is new Gp4 (Boolean);
      use P4;

   begin
      P4.X := P4.Nt4'Last;
      if (P4.X or (not P4.X)) then
         Report.Comment ("P4.X CORRECTLY HAS A BOOLEAN TYPE");
      end if;
   end Example_4;

   Example_5 : declare
      generic
         type T5 (D : Positive) is private;
      package Gp5 is
         type Nt5 is new T5;
         X : Nt5 (D => 5);
         Y : Positive := X.D;  -- REFERS TO DISCRIMINANT OF NT5
      end Gp5;

      type Rec (A : Positive) is record
         D : Positive := 7;
      end record;
      package P5 is new Gp5 (T5 => Rec);
      -- P5.Y INITIALIZED WITH VALUE USING COMPONENT SELECTION
      -- OPERATION FOR THE DISCRIMINANT, I.E. FOR PARENT TYPE
      -- T5 WHICH DENOTES REC.

      W1 : Positive := P5.X.D;     -- VALUE IS 7
      W2 : Positive := P5.X.A;     -- VALUE IS 5
      W3 : Positive := P5.Y;       -- VALUE IS 5;
   begin
      if ((W1 /= 7) or (W2 /= 5) or (W3 /= 5)) then
         Report.Failed ("INCORRECT COMPONENT SELECTION");
      end if;
   end Example_5;

   Report.Result;

end Cc3016f;
