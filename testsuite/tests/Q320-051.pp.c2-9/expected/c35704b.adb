-- C35704B.ADA

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
-- CHECK THAT DIFFERENT FLOATING POINT TYPES FROM SAME PARENT CAN BE USED IN A
-- FLOATING POINT RANGE CONSTRAINT IN A TYPE DEFINITION.

-- JCR 4/7/82

with Report;
procedure C35704b is

   use Report;

begin
   Test
     ("C35704B",
      "DIFFERENT FLOATING POINT TYPES " &
      "FROM THE SAME PARENT IN FLOATING POINT" &
      "TYPE DEFINITION'S RANGE CONSTRAINT");

   declare
      type F is digits 5 range -5.0 .. 5.0;

      type F1 is new F;

      type G1 is digits 5 range F1'First .. F'Last;
      type G2 is digits 5 range F'First .. F1'Last;

   begin

      if G1'First /= G1 (G2'First) or G1'Last /= G1 (G2'Last) or
        G2'First /= G2 (F'First) or G2'Last /= G2 (F'Last) then
         Failed ("USING DIFF FLOATING POINT TYPES " & "FROM SAME PARENT");

      end if;

   end;

   Result;

end C35704b;
