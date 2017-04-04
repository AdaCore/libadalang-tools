-- C45343A.ADA

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
-- CHECK THAT CATENATION OF NULL OPERANDS YIELDS THE CORRECT RESULT, WITH THE
-- CORRECT BOUNDS.

-- BHS 6/29/84

with Report;
procedure C45343a is

   use Report;

   type Arr is array (Integer range <>) of Integer;
   subtype Arr_8 is Arr (1 .. 8);
   A1, A2 : Arr_8;

   procedure Cat (A : Arr; I1, I2 : Integer; Num : Character) is
   begin
      if A'First /= I1 or A'Last /= I2 then
         Failed ("INCORRECT CATENATION - " & Num);
      end if;
   end Cat;

begin

   Test ("C45343A", "CATENATION OF NULL OPERANDS");

   A1 := (1, 2, 3, 4, 5, 6, 7, 8);
   A2 := A1 (1 .. 0) & A1 (6 .. 5) & A1 (1 .. 8);
   if A2 /= (1, 2, 3, 4, 5, 6, 7, 8) then
      Failed ("INCORRECT CATENATION RESULT - 1");
   end if;

   A1 := (1, 2, 3, 4, 5, 6, 7, 8);
   A2 := A1 (2 .. 8) & A1 (1 .. 0) & 9;
   if A2 /= (2, 3, 4, 5, 6, 7, 8, 9) then
      Failed ("INCORRECT CATENATION RESULT - 2");
   end if;

   Cat (A1 (1 .. 0) & A1 (Ident_Int (2) .. 0), 2, 0, '3');
   Cat (A1 (Ident_Int (1) .. 0) & A2 (2 .. 0), 2, 0, '4');

   Cat (A1 (1 .. 0) & A1 (6 .. 5) & A1 (2 .. 8), 2, 8, '5');
   Cat (A1 (2 .. 8) & A1 (1 .. 0), 2, 8, '6');

   Cat (A2 (1 .. 0) & A2 (6 .. 5) & A2 (Ident_Int (2) .. 8), 2, 8, '7');
   Cat (A2 (Ident_Int (2) .. 8) & A2 (1 .. 0), 2, 8, '8');

   Result;

end C45343a;
