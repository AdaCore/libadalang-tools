-- C95082G.ADA

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
-- CHECK THAT FOR CALLS TO ENTRIES HAVING AT LEAST ONE DEFAULT PARAMETER, THE
-- CORRECT ASSOCIATION IS MADE BETWEEN ACTUAL AND FORMAL PARAMETERS.

-- JWC 7/17/85

with Report; use Report;
procedure C95082g is

   Y1, Y2, Y3 : Integer := 0;

   task T is
      entry E
        (I1         :     Integer; I2 : Integer := 2; I3 : Integer := 3;
         O1, O2, O3 : out Integer);
   end T;

   task body T is
   begin
      loop
         select
            accept E
              (I1         :     Integer; I2 : Integer := 2; I3 : Integer := 3;
               O1, O2, O3 : out Integer)
            do
               O1 := I1;
               O2 := I2;
               O3 := I3;
            end E;
         or
            terminate;
         end select;
      end loop;
   end T;

begin

   Test
     ("C95082G",
      "CHECK ASSOCIATIONS BETWEEN ACTUAL AND FORMAL " &
      "PARAMETERS (HAVING DEFAULT VALUES)");

   T.E (I1 => 11, I2 => 12, I3 => 13, O1 => Y1, O2 => Y2, O3 => Y3);
   if (Y1 /= 11) or (Y2 /= 12) or (Y3 /= 13) then
      Failed ("INCORRECT PARAMETER ASSOCIATION - 1");
   end if;

   T.E (I1 => 21, O1 => Y1, O2 => Y2, O3 => Y3);
   if (Y1 /= 21) or (Y2 /= 2) or (Y3 /= 3) then
      Failed ("INCORRECT PARAMETER ASSOCIATION - 2");
   end if;

   T.E (O1 => Y1, O3 => Y3, I1 => 31, I3 => 33, O2 => Y2);
   if (Y1 /= 31) or (Y2 /= 2) or (Y3 /= 33) then
      Failed ("INCORRECT PARAMETER ASSOCIATION - 3");
   end if;

   T.E (41, 42, O1 => Y1, O2 => Y2, O3 => Y3);
   if (Y1 /= 41) or (Y2 /= 42) or (Y3 /= 3) then
      Failed ("INCORRECT PARAMETER ASSOCIATION - 4");
   end if;

   T.E (51, O3 => Y3, O1 => Y1, O2 => Y2, I3 => 53);
   if (Y1 /= 51) or (Y2 /= 2) or (Y3 /= 53) then
      Failed ("INCORRECT PARAMETER ASSOCIATION - 5");
   end if;

   Result;

end C95082g;
