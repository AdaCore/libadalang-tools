-- C37305A.ADA

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
-- CHECK THAT CHOICES DENOTING A NULL RANGE OF VALUES ARE PERMITTED,
-- AND THAT FOR CHOICES CONSISTING OF A SUBTYPE NAME FOLLOWED BY A
-- RANGE CONSTRAINT WHERE THE LOWER BOUND IS GREATER THAN THE UPPER
-- BOUND, THE BOUNDS NEED NOT BE IN THE RANGE OF THE SUBTYPE VALUES.

-- CHECK THAT AN OTHERS ALTERNATIVE CAN BE PROVIDED EVEN IF ALL VALUES
-- OF THE CASE EXPRESSION HAVE BEEN COVERED BY PRECEDING ALTERNATIVES.

-- ASL 7/14/81
-- JWC 6/28/85   RENAMED TO -AB

with Report;
procedure C37305a is

   use Report;

begin
   Test
     ("C37305A",
      "NULL RANGES ALLOWED IN CHOICES FOR VARIANT " &
      "PARTS.  OTHERS ALTERNATIVE ALLOWED AFTER ALL VALUES " &
      "PREVIOUSLY COVERED");

   declare
      subtype St is Integer range 1 .. 10;

      type Rec (Disc : St := 1) is record
         case Disc is
            when 0 .. -1 =>
               null;
            when 1 .. -3 =>
               null;
            when 6 .. 5 =>
               Comp : Integer;
            when 11 .. 10 =>
               null;
            when 15 .. 12 =>
               null;
            when 11 .. 0 =>
               null;
            when 1 .. 10 =>
               null;
            when others =>
               null;
         end case;
      end record;

      R : Rec;
   begin
      R := (Disc => 4);

      if Equal (3, 4) then
         R := (Disc => 7);
      end if;

      if R.Disc /= 4 then
         Failed ("ASSIGNMENT FAILED");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED");
   end;

   Result;

end C37305a;
