-- C37102B.ADA

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
-- CHECK THAT, FOR A RECORD TYPE, THE IDENTIFIER FOR A DISCRIMINANT
-- CAN BE USED AS A SELECTED COMPONENT IN AN INDEX OR DISCRIMINANT
-- CONSTRAINT, AS THE NAME OF A DISCRIMINANT IN A DISCRIMINANT
-- SPECIFICATION, AND AS THE PARAMETER NAME IN A FUNCTION CALL IN A
-- DISCRIMINANT OR INDEX CONSTRAINT.

-- R.WILLIAMS 8/25/86

with Report; use Report;
procedure C37102b is

begin
   Test
     ("C37102B",
      "CHECK THAT, FOR A RECORD TYPE, THE " &
      "IDENTIFIER FOR A DISCRIMINANT CAN BE USED " &
      "AS A SELECTED COMPONENT IN AN INDEX OR " &
      "DISCRIMINANT CONSTRAINT, AS THE NAME OF A " &
      "DISCRIMINANT IN A DISCRIMINANT " &
      "SPECIFICATION, AND AS THE PARAMETER NAME " &
      "IN A FUNCTION CALL IN A DISCRIMINANT OR " &
      "INDEX CONSTRAINT");

   declare

      function F (D : Integer) return Integer is
      begin
         return Ident_Int (D);
      end F;

      package P is

         type D is new Integer;

         type Rec1 is record
            D : Integer := Ident_Int (1);
         end record;

         G : Rec1;

         type Rec2 (D : Integer := 3) is record
            null;
         end record;

         H : Rec2 (Ident_Int (5));

         type Arr is array (Integer range <>) of Integer;

         type Q (D : Integer := 0) is record
            J : Rec2 (D => H.D);
            K : Arr (G.D .. F (D => 5));
            L : Rec2 (F (D => 4));
         end record;

      end P;

      use P;

   begin
      declare
         R : Q;

      begin
         if R.J.D /= 5 then
            Failed ("INCORRECT VALUE FOR R.J");
         end if;

         if R.K'First /= 1 then
            Failed ("INCORRECT VALUE FOR R.K'FIRST");
         end if;

         if R.K'Last /= 5 then
            Failed ("INCORRECT VALUE FOR R.K'LAST");
         end if;

         if R.L.D /= 4 then
            Failed ("INCORRECT VALUE FOR R.L");
         end if;
      end;

   end;

   Result;
end C37102b;
