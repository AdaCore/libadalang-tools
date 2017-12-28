-- C35902D.ADA

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
-- CHECK THAT THE BINARY POINT IN THE MANTISSA OF A FIXED POINT NUMBER CAN LIE
-- OUTSIDE THE MANTISSA (EITHER TO THE LEFT OR TO THE RIGHT).

-- WRG 7/18/86

with Report; use Report;
with System; use System;
procedure C35902d is

begin

   Test
     ("C35902D",
      "CHECK THAT THE BINARY POINT IN THE MANTISSA " &
      "OF A FIXED POINT NUMBER CAN LIE OUTSIDE THE " &
      "MANTISSA (EITHER TO THE LEFT OR TO THE RIGHT)");

   Comment ("VALUE OF SYSTEM.MAX_MANTISSA IS" & Positive'Image (Max_Mantissa));

   A :
   declare

      Rs : constant := 2.0;

      type One_To_The_Right is
        delta Rs range -(2.0**(Max_Mantissa + 1)) .. 2.0**(Max_Mantissa + 1);
      -- THE BINARY POINT IS ONE PLACE TO THE RIGHT OF THE LARGEST POSSIBLE
      -- MANTISSA.

      R1, R2 : One_To_The_Right;

   begin

      R1 := Rs;
      for I in Positive range 1 .. Max_Mantissa - 1 loop
         R1 := R1 * Ident_Int (2);
      end loop;
      R2 := R1 - Rs;
      R2 := R2 + R1;
      -- AT THIS POINT, R2 SHOULD EQUAL ONE_TO_THE_RIGHT'LARGE.
      R2 := -R2;
      R2 := R2 + (R1 - Rs);
      for I in Positive range 1 .. Max_Mantissa - 1 loop
         R2 := R2 / Ident_Int (2);
      end loop;
      if R2 /= -Rs then
         Failed ("IDENTITY-PRESERVING OPERATIONS ARE FLAKY - A");
      end if;

   exception

      when others =>
         Failed ("EXCEPTION RAISED - A");

   end A;

   B :
   declare

      Ls : constant := 2.0**(-(Max_Mantissa + 1));

      type One_To_The_Left is delta Ls range -(2.0**(-1)) .. 2.0**(-1);
      -- THE BINARY POINT IS ONE PLACE TO THE LEFT OF THE LARGEST POSSIBLE
      -- MANTISSA.

      L1, L2 : One_To_The_Left;

   begin

      L1 := Ls;
      for I in Positive range 1 .. Max_Mantissa - 1 loop
         L1 := L1 * Ident_Int (2);
      end loop;
      L2 := L1 - Ls;
      L2 := L2 + L1;
      -- AT THIS POINT, L2 SHOULD EQUAL ONE_TO_THE_LEFT'LARGE.
      L2 := -L2;
      L2 := L2 + (L1 - Ls);
      for I in Positive range 1 .. Max_Mantissa - 1 loop
         L2 := L2 / Ident_Int (2);
      end loop;
      if L2 /= -Ls then
         Failed ("IDENTITY-PRESERVING OPERATIONS ARE FLAKY - B");
      end if;

   exception

      when others =>
         Failed ("EXCEPTION RAISED - B");

   end B;

   Result;

end C35902d;
