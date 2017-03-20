-- C87B05A.ADA

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
-- IN AN INTEGER TYPE DEFINITION WITH A RANGE CONSTRAINT, THE BOUNDS
-- OF THE RANGE MUST BE OF SOME INTEGER TYPE.

-- TRH  1 JULY 82

with Report; use Report;

procedure C87b05a is

   Err : Boolean := False;
   type Whole is new Integer range 0 .. Integer'Last;
   type Age is new Integer range 0 .. 120;

   function "+" (X : Whole) return Float is
   begin
      Err := True;
      return 2.0;
   end "+";

   function "-" (X : Age) return Boolean is
   begin
      Err := True;
      return False;
   end "-";

begin
   Test
     ("C87B05A",
      "OVERLOADED EXPRESSIONS IN RANGE BOUNDS " &
      " OF INTEGER TYPE DEFINITIONS");

   declare
      type Adult is range 18 .. "+" (Whole'(120));
      type Minor is range "-" (Age'(0)) .. "+" (Whole'(17));
      type Neg10 is range "-" (Age'(10)) .. "-" (Age'(1));

   begin
      if Err then
         Failed
           ("RESOLUTION INCORRECT - INTEGER TYPE " &
            "DEFINITIONS MUST HAVE INTEGER TYPE " &
            "RANGE BOUNDS");
      end if;
   end;

   Result;
end C87b05a;
