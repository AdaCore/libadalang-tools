-- C67005A.ADA

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
-- CHECK IF A RENAMING DECLARATION DECLARES AN EQUALITY OPERATOR, THE TYPES OF
-- THE PARAMETERS NEED NOT BE LIMITED TYPES.

-- JBG 9/28/83

with Report; use Report;
procedure C67005a is
begin
   Test
     ("C67005A",
      "CHECK THAT AN EQUALITY OPERATOR DECLARED BY " &
      "A RENAMING DECLARATION NEED NOT HAVE " &
      "PARAMETERS OF A LIMITED TYPE");
   declare
      generic
         type Lp is limited private;
         with function Equal (L, R : Lp) return Boolean;
      package Equality_Operator is
         function "=" (L, R : Lp) return Boolean;
      end Equality_Operator;

      package body Equality_Operator is
         function "=" (L, R : Lp) return Boolean is
         begin
            return Equal (L, R);
         end "=";
      end Equality_Operator;

      package Polar_Coordinates is
         type Polar_Coord is record
            R     : Integer;
            Theta : Integer;
         end record;
         function Equal (L, R : Polar_Coord) return Boolean;
         package Polar_Equal is new Equality_Operator (Polar_Coord, Equal);
         function "="
           (L, R : Polar_Coord) return Boolean renames
           Polar_Equal."=";
      end Polar_Coordinates;

      package body Polar_Coordinates is
         function Equal (L, R : Polar_Coord) return Boolean is
         begin
            return (L.Theta mod 360) = (R.Theta mod 360) and L.R = R.R;
         end Equal;
      end Polar_Coordinates;

      use Polar_Coordinates;

      package Variables is
         P270 : Polar_Coord := (R => 3, Theta => 270);
         P360 : Polar_Coord := (R => 3, Theta => Ident_Int (360));
      end Variables;

      use Variables;

   begin

      if P270 /= (3, -90) then
         Failed ("INCORRECT INEQUALITY OPERATOR");
      end if;

      if P360 = (3, 0) then
         null;
      else
         Failed ("INCORRECT EQUALITY OPERATOR");
      end if;

      Result;

   end;
end C67005a;
