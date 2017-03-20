-- CD1009L.ADA

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
-- OBJECTIVE:
--     CHECK THAT A 'SMALL' CLAUSE MAY BE GIVEN IN THE VISIBLE OR
--     PRIVATE PART OF A PACKAGE FOR A FIXED POINT TYPE DECLARED
--     IN THE VISIBLE PART OF THE SAME PACKAGE.

-- HISTORY:
--     VCL 10/08/87  CREATED ORIGINAL TEST.
--     BCB 03/20/89  CHANGED EXTENSION FROM '.ADA' TO '.DEP', CHANGED
--                    COMMENT FROM FLOATING POINT TO FIXED POINT.

with Report; use Report;
procedure Cd1009l is
begin
   Test
     ("CD1009L",
      "A 'SMALL' CLAUSE MAY BE GIVEN IN THE VISIBLE " &
      "OR PRIVATE PART OF A PACKAGE FOR A " &
      "FIXED POINT TYPE DECLARED IN THE VISIBLE " &
      "PART OF THE SAME PACKAGE");
   declare
      package Pack is
         type Specified is delta 2.0**(-2) range 0.0 .. 1.0;

         Specified_Small : constant := Specified'Small;

         type Check_Type_1 is delta 2.0**(-1) range 0.0 .. 1.0;
         for Check_Type_1'Small use Specified_Small;

         type Check_Type_2 is delta 2.0**(-1) range 0.0 .. 1.0;
      private
         for Check_Type_2'Small use Specified_Small;
      end Pack;

      use Pack;
   begin
      if Check_Type_1'Small /= Specified_Small then
         Failed ("INCORRECT RESULTS FOR CHECK_TYPE_1'SMALL");
      end if;

      if Check_Type_2'Small /= Specified_Small then
         Failed ("INCORRECT RESULTS FOR CHECK_TYPE_2'SMALL");
      end if;
   end;

   Result;
end Cd1009l;
