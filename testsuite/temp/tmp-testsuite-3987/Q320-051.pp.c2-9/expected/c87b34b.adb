-- C87B34B.ADA

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

-- THE "IN" (OR MEMBERSHIP) OPERATOR OF THE FORM:  X IN L .. R
-- REQUIRES THE OPERANDS X, L AND R TO BE OF THE SAME SCALAR TYPE.

-- TRH  19 JULY 82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C87b34b is

   type Flag is (Pass, Fail);

   generic
      type T is private;
      Arg : in T;
      Stat : in Flag;
   function F1 return T;

   function F1 return T is
   begin
      if Stat = Fail then
         Failed ("RESOLUTION INCORRECT FOR 'IN' MEMBERSHIP TEST");
      end if;
      return Arg;
   end F1;

   function X is new F1 (Float, 2.0, Pass);
   function L is new F1 (Float, -1.0, Pass);
   function R is new F1 (Float, 1.0, Pass);
   function X is new F1 (Integer, 5, Fail);
   function L is new F1 (Integer, 1, Fail);
   function L is new F1 (Character, 'A', Fail);
   function R is new F1 (Character, 'E', Fail);
   function X is new F1 (Boolean, True, Fail);
   function R is new F1 (Boolean, True, Fail);

begin
   Test ("C87B34B", "OVERLOADED MEMBERSHIP OPERANDS");

   if X in L .. R then
      Failed ("RESOLUTION INCORRECT FOR MEMBERSHIP OPERATOR");
   end if;

   Result;
end C87b34b;
