-- C87B10A.ADA

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
-- IN A RANGE CONSTRAINT OF A FIXED POINT OR FLOATING POINT TYPE DEFINITION,
-- BOTH BOUNDS MUST BE OF SOME REAL TYPE, ALTHOUGH THE TWO BOUNDS DO NOT HAVE
-- TO BE OF THE SAME TYPE.

-- TRH 7/28/82
-- DSJ 6/10/83
-- JBG 9/19/84

with Report; use Report;

procedure C87b10a is

   subtype Dur is Duration;

   function "+" (X : Float) return Integer is
   begin
      Failed
        ("RANGE CONSTRAINT FOR REAL TYPE DEFINITIONS " &
         "MUST HAVE REAL BOUNDS");
      return -10;
   end "+";

   function "+" (X, Y : Float) return Integer is
   begin
      Failed
        ("RANGE CONSTRAINT FOR REAL TYPE DEFINITIONS " &
         "MUST HAVE REAL BOUNDS");
      return -10;
   end "+";

begin
   Test
     ("C87B10A",
      "RANGE BOUNDS IN REAL TYPE DEFINITIONS MUST BE" &
      " OF SOME (NOT NECESSARILY THE SAME) REAL TYPE");

   declare
      type R1 is digits 2 range 0.0 .. 1.0 + Float'(1.0);
      type R2 is delta 0.1 range Float'(1.0) + 1.0 .. Dur'(2.0);
      type R3 is digits 2 range +1.0 .. "+" (Float'(2.0), 2.0);
      type R4 is delta 0.1 range 0.0 + Float'(0.0) .. +1.0;

   begin
      if 2.0 not in R1 or -1.0 in R2 or -1.0 in R3 or -0.9 in R4 then
         Failed
           ("RANGE BOUNDS IN REAL TYPE DEFINITIONS DO NOT " &
            "HAVE TO BE OF THE SAME REAL TYPE");
      end if;
   end;

   Result;
end C87b10a;
