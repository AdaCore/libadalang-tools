-- C35704D.ADA

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
-- CHECK THAT A COMBINATION OF FIXED AND FLOAT CAN BE USED IN A FLOATING POINT
-- RANGE CONSTRAINT IN A TYPE DEFINITION.

-- JCR 4/7/82

with Report;
procedure C35704d is

   use Report;

begin
   Test
     ("C35704D",
      "MIXED FIXED AND FLOAT IN FLOATING " &
      "POINT RANGE CONSTRAINT IN A TYPE DEFINITION");

   declare

      type F is digits 5;
      type R is delta 0.5 range -5.0 .. 5.0;

      T1 : constant F := -4.0;
      T2 : constant F := 4.0;

      R1 : constant R := -4.0;
      R2 : constant R := 4.0;

      type G1 is digits 5 range T1 .. R2;
      type G2 is digits 5 range R1 .. T2;

   begin

      if (abs (G1'First) - 4.0) /= 0.0 or
        (abs (G1'Last) - 4.0) /= 0.0 or
        (abs (G2'First) - 4.0) /= 0.0 or
        (abs (G2'Last) - 4.0) /= 0.0

      then
         Failed ("MIXED FIXED AND FLOAT IN FLOAT RANGE " & "CONSTRAINT");

      end if;

   end;

   Result;

end C35704d;
