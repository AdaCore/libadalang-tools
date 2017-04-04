-- C87B03A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- THE EXPRESSION IN A NUMBER DECLARATION MUST BE EITHER OF THE TYPE
-- UNIVERSAL_INTEGER OR UNIVERSAL_REAL.

-- TRH  16 JUNE 82

with Report; use Report;

procedure C87b03a is

begin
   Test ("C87B03A", "OVERLOADED EXPRESSIONS IN NUMBER DECLARATIONS");

   declare
      function "+" (X, Y : Integer) return Integer renames Standard."-";

      function "+" (X, Y : Float) return Float renames Standard."-";

      I1 : constant         := 1 + 1;
      I2 : constant Integer := 1 + 1;

      R1 : constant       := 1.0 + 1.0;
      R2 : constant Float := 1.0 + 1.0;

   begin
      if I1 /= 2 or I2 /= 0 or R1 /= 2.0 or R2 /= 0.0 then
         Failed
           ("OVERLOADED EXPRESSIONS IN NUMBER DECLARATIONS" &
            " RESOLVED INCORRECTLY");
      end if;
   end;

   Result;
end C87b03a;
