-- C54A22A.ADA

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
-- CHECK ALL FORMS OF CHOICE IN CASE CHOICES.

-- DAT 1/29/81
-- SPS 1/21/83

with Report;
procedure C54a22a is

   use Report;

   type T is range 1 .. 10;
   C5 : constant T := 5;
   subtype S1 is T range 1 .. 5;
   subtype S2 is T range C5 + 1 .. 7;
   subtype Sn is T range C5 + 4 .. C5 - 4 + 7;  -- NULL RANGE.
   subtype S10 is T range C5 + 5 .. T'Last;

begin
   Test ("C54A22A", "CHECK ALL FORMS OF CASE CHOICES");

   case T'(C5 + 3) is
      when Sn                       -- 9..8
      | S1 range 1 .. 0             -- 1..0
      | S2 range C5 + 2 .. C5 + 1   -- 7..6
      | 3 .. 2                      -- 3..2
      =>
         Failed ("WRONG CASE 1");

      when S1 range 4 .. C5        -- 4..5
      |
        S1 range C5 - 4 .. C5 / 2   -- 1..2
        |
        3 .. 1 + C5 mod 3           -- 3..3
        |
        Sn                          -- 9..8
        |
        S1 range 5 .. C5 - 1        -- 5..4
        |
        6 .. 7                      -- 6..7
        |
        S10                         -- 10..10
        |
        9                           -- 9
                          |
        S10 range 10 .. 9 =>        -- 10..9
         Failed ("WRONG CASE 2");

      when C5 + C5 - 2 .. 8         -- 8
      =>
         null;
   end case;

   Result;
end C54a22a;
