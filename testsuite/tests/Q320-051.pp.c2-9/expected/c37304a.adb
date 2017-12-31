-- C37304A.ADA

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
-- CHECK THAT ALL FORMS OF CHOICE ARE PERMITTED IN A VARIANT_PART, AND, IN
-- PARTICULAR, THAT FORMS LIKE ST RANGE L..R, AND ST ARE PERMITTED.

-- ASL 7/31/81
--  RM 8/26/82
-- SPS 1/21/83

with Report;
procedure C37304a is

   use Report;

begin

   Test ("C37304A", "ALL FORMS OF CHOICE ALLOWED IN A VARIANT_PART");

   declare

      type T is range 1 .. 10;
      C5 : constant T := 5;
      subtype S1 is T range 1 .. 5;
      subtype S2 is T range C5 + 1 .. 7;
      subtype Sn is T range C5 + 4 .. C5 - 4 + 7;  -- NULL RANGE.
      subtype S10 is T range C5 + 5 .. T'Last;

      type Vrec (Disc : T := 8) is record
         case Disc is
            when Sn                       -- 9..8
            | S1 range 1 .. 0             -- 1..0
            | S2 range C5 + 2 .. C5 + 1   -- 7..6
            | 3 .. 2                      -- 3..2
            =>
               null;

            when S1 range 4 .. C5        -- 4..5
            | S1 range C5 - 4 .. C5 / 2   -- 1..2
            |
              3 .. 1 + C5 mod 3           -- 3..3
              | Sn                          -- 9..8
              | S1 range 5 .. C5 - 1        -- 5..4
              | 6 .. 7                      -- 6..7
              | S10                         -- 10..10
              |
              9                           -- 9
              | S10 range 10 .. 9           -- 10..9
              =>
               null;

            when C5 + C5 - 2 .. 8         -- 8
            =>
               null;

         end case;
      end record;

      V : Vrec;

   begin

      if Equal (3, 3) then
         V := (Disc => 5);
      end if;
      if V.Disc /= 5 then
         Failed ("ASSIGNMENT FAILED");
      end if;

   end;

   Result;

end C37304a;
