-- C49022A.ADA

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
-- CHECK THAT NAMED NUMBER DECLARATIONS (INTEGER) MAY USE EXPRESSIONS WITH
-- INTEGERS.

-- BAW 29 SEPT 80
-- TBN 10/28/85 RENAMED FROM C4A001A.ADA. ADDED RELATIONAL
--                  OPERATORS AND USE OF NAMED NUMBERS.

with Report;
procedure C49022a is

   use Report;

   Add1 : constant := 1 + 1;
   Add2 : constant := 1 + (-1);
   Add3 : constant := (-1) + 1;
   Add4 : constant := (-1) + (-1);
   Sub1 : constant := 1 - 1;
   Sub2 : constant := 1 - (-1);
   Sub3 : constant := (-1) - 1;
   Sub4 : constant := (-1) - (-1);
   Mul1 : constant := 1 * 1;
   Mul2 : constant := 1 * (-1);
   Mul3 : constant := (-1) * 1;
   Mul4 : constant := (-1) * (-1);
   Div1 : constant := 1 / 1;
   Div2 : constant := 1 / (-1);
   Div3 : constant := (-1) / 1;
   Div4 : constant := (-1) / (-1);
   Rem1 : constant := 14 rem 5;
   Rem2 : constant := 14 rem (-5);
   Rem3 : constant := (-14) rem 5;
   Rem4 : constant := (-14) rem (-5);
   Mod1 : constant := 4 mod 3;
   Mod2 : constant := 4 mod (-3);
   Mod3 : constant := (-4) mod 3;
   Mod4 : constant := (-4) mod (-3);
   Exp1 : constant := 1**1;
   Exp2 : constant := (-1)**1;
   Abs1 : constant := abs (-10);
   Abs2 : constant := abs (+10);
   Tot1 : constant := Add1 + Sub1 - Mul1 + Div1 - Rem3 + Mod2 - Exp1;
   Les1 : constant := Boolean'Pos (1 < 2);
   Les2 : constant := Boolean'Pos (1 < (-2));
   Les3 : constant := Boolean'Pos ((-1) < (-2));
   Les4 : constant := Boolean'Pos (Add1 < Sub1);
   Gre1 : constant := Boolean'Pos (2 > 1);
   Gre2 : constant := Boolean'Pos ((-1) > 2);
   Gre3 : constant := Boolean'Pos ((-1) > (-2));
   Gre4 : constant := Boolean'Pos (Add1 > Sub1);
   Leq1 : constant := Boolean'Pos (1 <= 1);
   Leq2 : constant := Boolean'Pos ((-1) <= 1);
   Leq3 : constant := Boolean'Pos ((-1) <= (-2));
   Leq4 : constant := Boolean'Pos (Add2 <= Sub3);
   Geq1 : constant := Boolean'Pos (2 >= 1);
   Geq2 : constant := Boolean'Pos ((-2) >= 1);
   Geq3 : constant := Boolean'Pos ((-2) >= (-1));
   Geq4 : constant := Boolean'Pos (Add2 >= Sub3);
   Equ1 : constant := Boolean'Pos (2 = 2);
   Equ2 : constant := Boolean'Pos ((-2) = 2);
   Equ3 : constant := Boolean'Pos ((-2) = (-2));
   Equ4 : constant := Boolean'Pos (Add2 = Sub3);
   Neq1 : constant := Boolean'Pos (2 /= 2);
   Neq2 : constant := Boolean'Pos ((-2) /= 1);
   Neq3 : constant := Boolean'Pos ((-2) /= (-2));
   Neq4 : constant := Boolean'Pos (Add2 /= Sub3);

begin
   Test
     ("C49022A",
      "CHECK THAT NAMED NUMBER DECLARATIONS (INTEGER) " &
      "MAY USE EXPRESSIONS WITH INTEGERS");

   if Add1 /= 2 or Add2 /= 0 or Add3 /= 0 or Add4 /= -2 then
      Failed ("ERROR IN THE ADDING OPERATOR +");
   end if;

   if Sub1 /= 0 or Sub2 /= 2 or Sub3 /= -2 or Sub4 /= 0 then
      Failed ("ERROR IN THE ADDING OPERATOR -");
   end if;

   if Mul1 /= 1 or Mul2 /= -1 or Mul3 /= -1 or Mul4 /= 1 then
      Failed ("ERROR IN THE MULTIPLYING OPERATOR *");
   end if;

   if Div1 /= 1 or Div2 /= -1 or Div3 /= -1 or Div4 /= 1 then
      Failed ("ERROR IN THE MULTIPLYING OPERATOR /");
   end if;

   if Rem1 /= 4 or Rem2 /= 4 or Rem3 /= -4 or Rem4 /= -4 then
      Failed ("ERROR IN THE MULTIPLYING OPERATOR REM");
   end if;

   if Mod1 /= 1 or Mod2 /= -2 or Mod3 /= 2 or Mod4 /= -1 then
      Failed ("ERROR IN THE MULTIPLYING OPERATOR MOD");
   end if;

   if Exp1 /= 1 or Exp2 /= -1 then
      Failed ("ERROR IN THE EXPONENTIATING OPERATOR");
   end if;

   if Abs1 /= 10 or Abs2 /= 10 then
      Failed ("ERROR IN THE ABS OPERATOR");
   end if;

   if Tot1 /= 3 then
      Failed ("ERROR IN USING NAMED NUMBERS WITH OPERATORS");
   end if;

   if Les1 /= 1 or Les2 /= 0 or Les3 /= 0 or Les4 /= 0 then
      Failed ("ERROR IN THE LESS THAN OPERATOR");
   end if;

   if Gre1 /= 1 or Gre2 /= 0 or Gre3 /= 1 or Gre4 /= 1 then
      Failed ("ERROR IN THE GREATER THAN OPERATOR");
   end if;

   if Leq1 /= 1 or Leq2 /= 1 or Leq3 /= 0 or Leq4 /= 0 then
      Failed ("ERROR IN THE LESS THAN EQUAL OPERATOR");
   end if;

   if Geq1 /= 1 or Geq2 /= 0 or Geq3 /= 0 or Geq4 /= 1 then
      Failed ("ERROR IN THE GREATER THAN EQUAL OPERATOR");
   end if;

   if Equ1 /= 1 or Equ2 /= 0 or Equ3 /= 1 or Equ4 /= 0 then
      Failed ("ERROR IN THE EQUAL OPERATOR");
   end if;

   if Neq1 /= 0 or Neq2 /= 1 or Neq3 /= 0 or Neq4 /= 1 then
      Failed ("ERROR IN THE NOT EQUAL OPERATOR");
   end if;

   Result;

end C49022a;
