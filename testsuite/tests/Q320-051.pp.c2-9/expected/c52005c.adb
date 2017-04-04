-- C52005C.ADA

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
-- CHECK THAT THE CONSTRAINT_ERROR EXCEPTION IS RAISED
--    WHEN A STATIC EXPRESSION VALUE IS OUTSIDE THE STATIC RANGE
--    OF FIXED POINT ASSIGNMENTS.

-- DCB 2/6/80
-- JRK 7/21/80
-- SPS 3/21/83

with Report;
procedure C52005c is

   use Report;

begin
   Test
     ("C52005C",
      "CHECK THAT CONSTRAINT_ERROR EXCEPTION IS RAISED" &
      " ON STATIC OUT OF RANGE FIXED POINT ASSIGNMENTS");

-----------------------

   declare
      type Real is delta 0.01 range 0.00 .. 9.99;
      Fx1 : Real range 0.00 .. 7.00 := 4.50;

   begin
      Fx1 := 7.01;

      Failed ("EXCEPTION NOT RAISED FOR OUT OF RANGE FIXED ASSNMT");

   exception
      when Constraint_Error =>
         if Fx1 /= 4.50 then
            Failed ("VALUE ALTERED BEFORE FIXED PT RANGE EXCEPTION");
         end if;

   end;

-------------------------

   declare
      type Real is delta 0.01 range 0.00 .. 9.99;
      Fx2 : Real range 0.00 .. 7.00 := 4.50;

   begin
      Fx2 := 7.00;

   exception
      when Constraint_Error =>
         Failed ("EXCEPTION RAISED ON LEGAL FIXED PT ASSNMT");

   end;

-------------------------

   Result;
end C52005c;
