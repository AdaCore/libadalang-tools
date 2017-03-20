-- C87B42A.ADA

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
-- A CONDITIONAL EXPRESSION MUST BE OF A BOOLEAN TYPE.

-- TRH  27 JULY 82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C87b42a is

   type Bit is new Boolean;
   type Boolean is (False, True);
   type Lit is (False, True);
   type Flag is (Pass, Fail);

   generic
      type T is private;
      Arg : in T;
      Stat : Flag;
   function F1 return T;

   function F1 return T is
   begin
      if Stat = Fail then
         Failed ("CONDITIONAL EXPRESSION MUST BE OF A BOOLEAN" & " TYPE");
      end if;
      return Arg;
   end F1;

   function F is new F1 (Boolean, False, Fail);
   function F is new F1 (Bit, False, Pass);
   function F is new F1 (Lit, False, Fail);
   function F is new F1 (Integer, -11, Fail);
   function F is new F1 (Float, +0.0, Fail);

begin
   Test ("C87B42A", "OVERLOADED CONDITIONAL EXPRESSIONS");

   while (F or not F) loop
      if (F or else not F) then
         null;
      end if;
      exit when (F and not F);
      exit when (F or not F);
      exit when (F);
      exit when (not F);
   end loop;

   Result;
end C87b42a;
