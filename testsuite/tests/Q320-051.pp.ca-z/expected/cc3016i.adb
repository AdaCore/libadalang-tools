-- CC3016I.ADA

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

--   CHECK THAT IF THE DESIGNATED TYPE OF AN ACCESS TYPE IS A GENERIC
--   FORMAL TYPE, OR IS A TYPE DERIVED DIRECTLY OR INDIRECTLY FROM A
--   GENERIC FORMAL TYPE, THE OPERATIONS DECLARED FOR THE ACCESS TYPE
--   IN THE TEMPLATE ARE DETERMINED BY THE DECLARATION OF THE FORMAL
--   TYPE.  THE OPERATIONS DECLARED FOR ACCESS TYPE IN THE INSTANCE
--   ARE DETERMINED BY THE ACTUAL TYPE DENOTED BY THE FORMAL PARAMETER.
--   SEE AI-00398.

-- HISTORY:
--   DAS  8 OCT 90   INITIAL VERSION.

with Report; use Report;

procedure Cc3016i is
begin
   Test
     ("CC3016I",
      "CHECK THAT AN INSTANTIATED PACKAGE HAS THE " &
      "PROPERTIES REQUIRED OF A PACKAGE.");

   Example_5a : declare
      generic
         type T5a (D : Positive) is private;
      package Gp5a is
         type Nt5a is new T5a;
         X : Nt5a (D => 5);
         Y : Positive := X.D;  -- REFERS TO DISCRIMINANT OF NT5A
      end Gp5a;

      type Rec (A : Positive) is record
         D : Positive := 7;
      end record;
      package P5a is new Gp5a (T5a => Rec);
      -- P5A.Y INITIALIZED WITH VALUE USING COMPONENT SELECTION OPERATION FOR
      -- THE DISCRIMINANT, I.E. FOR PARENT TYPE T5A WHICH DENOTES REC.

      W1 : Positive := P5a.X.D;    -- VALUE IS 7
      W2 : Positive := P5a.X.A;    -- VALUE IS 5
      W3 : Positive := P5a.Y;      -- VALUE IS 5;
   begin
      if ((W1 /= 7) or (W2 /= 5) or (W3 /= 5)) then
         Failed ("INCORRECT COMPONENT SELECTION - ACCESS");
      end if;
   end Example_5a;

   Result;

end Cc3016i;
