-- C43105A.ADA

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
--     IN A RECORD AGGREGATE, (X => E, Y => E), WHERE E IS AN OVERLOADED
--     ENUMERATION LITERAL, OVERLOADING RESOLUTION OCCURS SEPARATELY FOR
--     THE DIFFERENT OCCURRENCES OF E.

-- HISTORY:
--     DHH 08/10/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43105a is

begin
   Test
     ("C43105A",
      "IN A RECORD AGGREGATE, (X => E, Y => E), WHERE " &
      "E IS AN OVERLOADED ENUMERATION LITERAL, " &
      "OVERLOADING RESOLUTION OCCURS SEPARATELY FOR " &
      "THE DIFFERENT OCCURRENCES OF E");

   declare
      type Color is (Red, Yellow, Green);
      type Palette is (Green, Yellow, Red);

      type Rec is record
         X : Color;
         Y : Palette;
      end record;

      type Recd is record
         X : Palette;
         Y : Color;
      end record;

      Rec1 : Rec;
      Rec2 : Recd;

      function Ident_C (C : Color) return Color is
      begin
         if Equal (3, 3) then
            return C;
         else
            return Green;
         end if;
      end Ident_C;

      function Ident_P (P : Palette) return Palette is
      begin
         if Equal (3, 3) then
            return P;
         else
            return Red;
         end if;
      end Ident_P;

   begin
      Rec1 := (X => Yellow, Y => Yellow);
      Rec2 := (X => Yellow, Y => Yellow);

      if Rec1.X /= Ident_C (Rec2.Y) then
         Failed ("COLOR RESOLUTION FAILED");
      end if;

      if Rec1.Y /= Ident_P (Rec2.X) then
         Failed ("PALETTE RESOLUTION FAILED");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED");
   end;

   Result;
end C43105a;
