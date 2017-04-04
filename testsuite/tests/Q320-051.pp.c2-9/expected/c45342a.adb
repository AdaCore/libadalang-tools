-- C45342A.ADA

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
-- CHECK THAT CATENATION OF TWO OR MORE NON-NULL OPERANDS YIELDS THE CORRECT
-- RESULT, WITH THE CORRECT BOUNDS, WHETHER BOUNDS ARE STATIC OR DYNAMIC.

-- BHS 6/27/84

with Report;
procedure C45342a is

   use Report;

   subtype S is Integer range 1 .. 100;
   type Arr is array (S range <>) of Integer;

   A, B : Arr (2 .. 9);

   function F (Ar_Var1, Ar_Var2, Ar_Var3 : Arr) return Arr is
   begin
      return Ar_Var1 & Ar_Var2 & Ar_Var3;
   end F;

   procedure Cat (A : Arr; I1, I2 : Integer; Num : Character) is
   begin
      if A'First /= I1 or A'Last /= I2 then
         Failed ("INCORRECT CATENATION BOUNDS - " & Num);
      end if;
   end Cat;

begin

   Test
     ("C45342A",
      "CHECK THAT CATENATION OF NON-NULL OPERANDS " &
      "YIELDS CORRECT RESULT WITH CORRECT BOUNDS");

   begin
      A := (1, 2, 3, 4, 5, 6, 7, 8);
      B := A (2 .. 4) & A (2 .. 5) & A (2 .. 2);
      if B /= (1, 2, 3, 1, 2, 3, 4, 1) then
         Failed ("INCORRECT CATENATION RESULT - 1");
      end if;

      A := (8, 7, 6, 5, 4, 3, 2, 1);
      if F (A (2 .. 3), A (2 .. 4), A (2 .. 4)) /=
        (8, 7, 8, 7, 6, 8, 7, 6)
      then
         Failed ("INCORRECT CATENATION RESULT - 2");
      end if;

      Cat (A (3 .. 5) & A (2 .. 3), 3, 7, '3');
   end;

   declare
      Dyn2 : Integer := Ident_Int (2);
      Dyn3 : Integer := Ident_Int (3);
      Dyn4 : Integer := Ident_Int (4);
      Dyn6 : Integer := Ident_Int (6);

   begin
      A := (1, 2, 3, 4, 5, 6, 7, 8);
      B := A (Dyn2 .. Dyn3) & A (Dyn2 .. Dyn4) & A (Dyn2 .. Dyn4);
      if B /= (1, 2, 1, 2, 3, 1, 2, 3) then
         Failed ("INCORRECT CATENATION RESULT - 4");
      end if;

      A := (8, 7, 6, 5, 4, 3, 2, 1);
      if F (A (Dyn2 .. Dyn6), A (Dyn2 .. Dyn3), A (Dyn2 .. Dyn2)) /=
        (8, 7, 6, 5, 4, 8, 7, 8)
      then
         Failed ("INCORRECT CATENATION RESULT - 5");
      end if;

      Cat (A (Dyn3 .. 5) & A (2 .. 3), 3, 7, '6');
   end;

   Result;

end C45342a;
