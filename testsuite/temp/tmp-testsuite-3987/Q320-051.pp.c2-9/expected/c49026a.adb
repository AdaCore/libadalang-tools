-- C49026A.ADA

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
-- CHECK THAT A QUALIFIED EXPRESSION CAN APPEAR IN A STATIC EXPRESSION.

-- L.BROWN  10/07/86

with Report; use Report;

procedure C49026a is

   type Enum is (Red, Green, Blue, Yellow);
   type Int1 is range 1 .. 50;
   type Flt1 is digits 3 range 1.0 .. 5.0;
   type Fix1 is delta 0.125 range 0.0 .. 10.0;
   type Int2 is range 1 .. Int1'(25);
   type Flt2 is digits 3 range 1.0 .. Flt1'(2.0);
   type Fix2 is delta 0.125 range 0.0 .. Fix1'(5.0);
   type Flt3 is digits Int1'(3);
   type Fix3 is delta Fix1'(0.125) range 0.0 .. 5.0;
   Obj1    : Integer := 2;
   Cas_Obj : Enum    := Green;

begin

   Test
     ("C49026A",
      "QUALIFIED EXPRESSIONS CAN APPEAR IN STATIC " & "EXPRESSIONS");

   case Cas_Obj is
      when Enum'(Green) =>
         Obj1 := 3;
      when others =>
         Failed ("INCORRECT VALUE FOR QUALIFIED EXPRESSION 1");
   end case;

   Result;

end C49026a;
