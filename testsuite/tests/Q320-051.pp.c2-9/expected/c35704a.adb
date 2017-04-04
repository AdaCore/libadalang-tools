-- C35704A.ADA

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
-- CHECK THAT FIXED POINT VALUES CAN BE USED IN FLOATING POINT RANGE CONSTRAINT
-- IN TYPE DEFINITION.

-- BAW 9/5/80
-- JCR 4/7/82

with Report;
procedure C35704a is

   use Report;

begin
   Test
     ("C35704A",
      "CHECK THAT L AND R CAN BE FIXED POINT" &
      " IN A FLOATING POINT TYPE DEFINITION");

   declare

      type F is delta 0.5 range -5.0 .. 5.0;

      F1 : constant F := -4.0;
      F2 : constant F := 4.0;

      type G1 is digits 5 range F1 .. F2;
   begin

      if (abs (G1'First) - 4.0) /= 0.0 or (abs (G1'Last) - 4.0) /= 0.0 then
         Failed ("ERROR IN USING FIXED-POINT IN RANGE " & "CONSTRAINT");
      end if;

   end;
   Result;

end C35704a;
