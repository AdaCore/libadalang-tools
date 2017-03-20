-- C37312A.ADA

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
--     CHECK THAT A DISCRIMINANT CAN HAVE A GENERIC FORMAL DISCRETE
--     TYPE WHEN IT DOES NOT GOVERN A VARIANT PART AND THAT AN
--     OBJECT OF A GENERIC FORMAL TYPE CAN CONSTRAIN A COMPONENT
--     IN A VARIANT PART.

-- HISTORY:
--     AH  08/22/86  CREATED ORIGINAL TEST.
--     JET 08/13/87  REVISED FROM CLASS 'A' TO CLASS 'C' TEST.

with Report; use Report;

procedure C37312a is

begin
   Test ("C37312A", "DISCRIMINANT TYPE IS GENERIC FORMAL TYPE");

   declare
      type T is range 1 .. 5;

      generic
         type G1 is range <>;
      package P is
         type G2 (D1 : G1) is record
            R1 : G1;
            R2 : Boolean;
         end record;

         type Str is array (G1 range <>) of Integer;
         type G3 (D : G1; E : Integer) is record
            case E is
               when 1 =>
                  S1 : Str (G1'First .. D);
               when others =>
                  S2 : Integer;
            end case;
         end record;

      end P;

      package Pkg is new P (G1 => T);
      use Pkg;

      A2 : G2 (1)    := (1, 5, False);
      A3 : G3 (5, 1) := (5, 1, (1, 2, 3, 4, 5));

   begin
      A2.R2     := Ident_Bool (True);
      A3.S1 (1) := Ident_Int (6);

      if A2 /= (1, 5, True) then
         Failed ("INVALID CONTENTS OF RECORD A2");
      end if;
      if A3 /= (5, 1, (6, 2, 3, 4, 5)) then
         Failed ("INVALID CONTENTS OF RECORD A3");
      end if;
   end;

   Result;

end C37312a;
