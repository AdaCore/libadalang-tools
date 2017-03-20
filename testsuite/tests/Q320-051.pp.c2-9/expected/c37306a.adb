-- C37306A.ADA

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
-- CHECK THAT IN A VARIANT PART OF A RECORD THE CHOICES WITHIN AND
-- BETWEEN ALTERNATIVES CAN APPEAR IN NON-MONOTONIC ORDER.

-- ASL 7/13/81
-- JWC 6/28/85   RENAMED TO -AB

with Report;
procedure C37306a is

   use Report;

begin
   Test ("C37306A", "NON-MONOTONIC ORDER OF CHOICES IN VARIANT PARTS");

   declare
      type Color is (White, Red, Orange, Yellow, Green, Aqua, Blue, Black);

      type Rec (Disc : Color := Blue) is record
         case Disc is
            when Orange =>
               null;
            when Green | White | Black =>
               null;
            when Yellow =>
               null;
            when Blue | Red =>
               null;
            when others =>
               null;
         end case;
      end record;

      R : Rec;
   begin
      R := (Disc => White);

      if Equal (3, 4) then
         R := (Disc => Red);
      end if;

      if R.Disc /= White then
         Failed ("ASSIGNMENT FAILED");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED");
   end;

   Result;
end C37306a;
