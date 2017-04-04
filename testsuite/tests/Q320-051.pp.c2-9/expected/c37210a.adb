-- C37210A.ADA

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
-- CHECK THAT THE EXPRESSION IN A DISCRIMINANT ASSOCIATION WITH MORE THAN ONE
-- NAME IS EVALUATED ONCE FOR EACH NAME.

-- R.WILLIAMS 8/28/86

with Report; use Report;
procedure C37210a is

   Bump : Integer := Ident_Int (0);

   function F return Integer is
   begin
      Bump := Bump + 1;
      return Bump;
   end F;

   function Check (Str : String) return Integer is
   begin
      if Bump /= 2 then
         Failed ("INCORRECT DISCRIMINANT VALUES FOR " & Str);
      end if;
      Bump := Ident_Int (0);
      return 5;
   end Check;

begin
   Test
     ("C37210A",
      "CHECK THAT THE EXPRESSION IN A " &
      "DISCRIMINANT ASSOCIATION WITH MORE THAN " &
      "ONE NAME IS EVALUATED ONCE FOR EACH NAME");

   declare
      type Rec (D1, D2 : Integer) is record
         null;
      end record;

      R : Rec (D1 | D2 => F);

      I1 : Integer := Check ("R");

      type Acc is access Rec;

      Ac : Acc (D1 | D2 => F);

      I2 : Integer := Check ("AC");

      package Pkg is
         type Priv (D1, D2 : Integer) is private;
         type Pacc is access Priv;

         type Lim (D1, D2 : Integer) is limited private;
         type Lacc is access Lim;

      private
         type Priv (D1, D2 : Integer) is record
            null;
         end record;

         type Lim (D1, D2 : Integer) is record
            null;
         end record;
      end Pkg;

      use Pkg;

   begin

      declare
         P : Priv (D1 | D2 => F);

         I1 : Integer := Check ("P");

         Pa : Pacc (D1 | D2 => F);

         I2 : Integer := Check ("PA");

         L : Lim (D1 | D2 => F);

         I3 : Integer := Check ("L");

         La : Lacc (D1 | D2 => F);

         I : Integer;
      begin
         I := Check ("LA");
      end;
   end;

   Result;
end C37210a;
