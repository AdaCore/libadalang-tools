-- C84008A.ADA

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
--     CHECK THAT THE NAMES MADE VISIBLE BY A USE CLAUSE IN THE VISIBLE
--     PART OF A PACKAGE ARE VISIBLE IN THE PRIVATE PART AND BODY OF
--     THE PACKAGE.

-- HISTORY:
--     JET 03/10/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C84008a is

   package Pack1 is
      type A is range 0 .. 100;
      type B is range -100 .. 0;
   end Pack1;

   package Pack2 is
      use Pack1;
      type C is private;
      procedure Proc (X : out A; Y : out B);
   private
      type C is new A range 0 .. 9;
   end Pack2;

   Var1 : Pack1.A;
   Var2 : Pack1.B;

   package body Pack2 is
      procedure Proc (X : out A; Y : out B) is
         subtype D is B range -9 .. 0;
      begin
         if Equal (3, 3) then
            X := A'(2);
            Y := D'(-2);
         else
            X := A'(0);
            Y := D'(0);
         end if;
      end Proc;
   end Pack2;

begin
   Test
     ("C84008A",
      "CHECK THAT THE NAMES MADE VISIBLE BY A USE " &
      "CLAUSE IN THE VISIBLE PART OF A PACKAGE ARE " &
      "VISIBLE IN THE PRIVATE PART AND BODY OF " &
      "THE PACKAGE");

   Pack2.Proc (Var1, Var2);

   if Pack1."/=" (Var1, 2) then
      Failed ("INCORRECT RETURN VALUE FOR VAR1");
   end if;

   if Pack1."/=" (Var2, Pack1."-" (2)) then
      Failed ("INCORRECT RETURN VALUE FOR VAR2");
   end if;

   Result;
end C84008a;
