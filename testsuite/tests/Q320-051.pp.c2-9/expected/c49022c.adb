-- C49022C.ADA

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
-- CHECK THAT NAMED NUMBER DECLARATIONS (REAL) MAY USE EXPRESSIONS WITH REALS.

-- BAW 29 SEPT 80
-- TBN 10/24/85 RENAMED FROM C4A011A.ADA. ADDED RELATIONAL
--                  OPERATORS AND NAMED NUMBERS.

with Report;
procedure C49022c is

   use Report;

   Add1 : constant := 2.5 + 1.5;
   Add2 : constant := 2.5 + (-1.5);
   Add3 : constant := (-2.5) + 1.5;
   Add4 : constant := (-2.5) + (-1.5);
   Sub1 : constant := 2.5 - 1.5;
   Sub2 : constant := 2.5 - (-1.5);
   Sub3 : constant := (-2.5) - 1.5;
   Sub4 : constant := (-2.5) - (-1.5);
   Mul1 : constant := 2.5 * 1.5;
   Mul2 : constant := 2.5 * (-1.5);
   Mul3 : constant := (-2.5) * 1.5;
   Mul4 : constant := (-2.5) * (-1.5);
   Mlr1 : constant := 2 * 1.5;
   Mlr2 : constant := (-2) * 1.5;
   Mlr3 : constant := 2 * (-1.5);
   Mlr4 : constant := (-2) * (-1.5);
   Mll1 : constant := 1.5 * 2;
   Mll2 : constant := 1.5 * (-2);
   Mll3 : constant := (-1.5) * 2;
   Mll4 : constant := (-1.5) * (-2);
   Div1 : constant := 3.75 / 2.5;
   Div2 : constant := 3.75 / (-2.5);
   Div3 : constant := (-3.75) / 2.5;
   Div4 : constant := (-3.75) / (-2.5);
   Dvi1 : constant := 3.0 / 2;
   Dvi2 : constant := (-3.0) / 2;
   Dvi3 : constant := 3.0 / (-2);
   Dvi4 : constant := (-3.0) / (-2);
   Exp1 : constant := 2.0**1;
   Exp2 : constant := 2.0**(-1);
   Exp3 : constant := (-2.0)**1;
   Exp4 : constant := (-2.0)**(-1);
   Abs1 : constant := abs (-3.75);
   Abs2 : constant := abs (+3.75);
   Tot1 : constant := Add1 + Sub4 - Mul1 + Div1 - Exp2 + Abs1;
   Les1 : constant := Boolean'Pos (1.5 < 2.0);
   Les2 : constant := Boolean'Pos (1.5 < (-2.0));
   Les3 : constant := Boolean'Pos ((-1.5) < (-2.0));
   Les4 : constant := Boolean'Pos (Add2 < Sub1);
   Gre1 : constant := Boolean'Pos (2.0 > 1.5);
   Gre2 : constant := Boolean'Pos ((-2.0) > 1.5);
   Gre3 : constant := Boolean'Pos ((-2.0) > (-1.5));
   Gre4 : constant := Boolean'Pos (Add1 > Sub1);
   Leq1 : constant := Boolean'Pos (1.5 <= 2.0);
   Leq2 : constant := Boolean'Pos (1.5 <= (-2.0));
   Leq3 : constant := Boolean'Pos ((-1.5) <= (-2.0));
   Leq4 : constant := Boolean'Pos (Add2 <= Sub1);
   Geq1 : constant := Boolean'Pos (2.0 >= 1.5);
   Geq2 : constant := Boolean'Pos ((-2.0) >= 1.5);
   Geq3 : constant := Boolean'Pos ((-2.0) >= (-1.5));
   Geq4 : constant := Boolean'Pos (Add1 >= Sub2);
   Equ1 : constant := Boolean'Pos (1.5 = 2.0);
   Equ2 : constant := Boolean'Pos ((-1.5) = 2.0);
   Equ3 : constant := Boolean'Pos ((-1.5) = (-1.5));
   Equ4 : constant := Boolean'Pos (Add1 = Sub2);
   Neq1 : constant := Boolean'Pos (1.5 /= 1.5);
   Neq2 : constant := Boolean'Pos ((-1.5) /= 1.5);
   Neq3 : constant := Boolean'Pos ((-1.5) /= (-2.0));
   Neq4 : constant := Boolean'Pos (Add1 /= Sub2);

begin
   Test
     ("C49022C",
      "CHECK THAT NAMED NUMBER DECLARATIONS (REAL) " &
      "MAY USE EXPRESSIONS WITH REALS.");

   if Add1 /= 4.0 or Add2 /= 1.0 or Add3 /= -1.0 or Add4 /= -4.0 then
      Failed ("ERROR IN THE ADDING OPERATOR +");
   end if;

   if Sub1 /= 1.0 or Sub2 /= 4.0 or Sub3 /= -4.0 or Sub4 /= -1.0 then
      Failed ("ERROR IN THE ADDING OPERATOR -");
   end if;

   if Mul1 /= 3.75 or Mul2 /= -3.75 or Mul3 /= -3.75 or Mul4 /= 3.75 then
      Failed ("ERROR IN THE MULTIPLYING OPERATOR *");
   end if;

   if Mlr1 /= 3.0 or Mlr2 /= -3.0 or Mlr3 /= -3.0 or Mlr4 /= 3.0 then
      Failed ("ERROR IN THE MULTIPLYING OPERATOR *");
   end if;

   if Mll1 /= 3.0 or Mll2 /= -3.0 or Mll3 /= -3.0 or Mll4 /= 3.0 then
      Failed ("ERROR IN THE MULTIPLYING OPERATOR *");
   end if;

   if Div1 /= 1.5 or Div2 /= -1.5 or Div3 /= -1.5 or Div4 /= 1.5 then
      Failed ("ERROR IN THE MULTIPLYING OPERATOR /");
   end if;

   if Dvi1 /= 1.5 or Dvi2 /= -1.5 or Dvi3 /= -1.5 or Dvi4 /= 1.5 then
      Failed ("ERROR IN THE MULTIPLYING OPERATOR /");
   end if;

   if Exp1 /= 2.0 or Exp2 /= 0.5 or Exp3 /= -2.0 or Exp4 /= -0.5 then
      Failed ("ERROR IN THE EXPONENTIATING OPERATOR");
   end if;

   if Abs1 /= 3.75 or Abs2 /= 3.75 then
      Failed ("ERROR IN THE ABS OPERATOR");
   end if;

   if Tot1 /= 4.00 then
      Failed ("ERROR IN USE OF NAMED NUMBERS WITH OPERATORS");
   end if;

   if Les1 /= 1 or Les2 /= 0 or Les3 /= 0 or Les4 /= 0 then
      Failed ("ERROR IN THE LESS THAN OPERATOR");
   end if;

   if Gre1 /= 1 or Gre2 /= 0 or Gre3 /= 0 or Gre4 /= 1 then
      Failed ("ERROR IN THE GREATER THAN OPERATOR");
   end if;

   if Leq1 /= 1 or Leq2 /= 0 or Leq3 /= 0 or Leq4 /= 1 then
      Failed ("ERROR IN THE LESS THAN EQUAL OPERATOR");
   end if;

   if Geq1 /= 1 or Geq2 /= 0 or Geq3 /= 0 or Geq4 /= 1 then
      Failed ("ERROR IN THE GREATER THAN EQUAL OPERATOR");
   end if;

   if Equ1 /= 0 or Equ2 /= 0 or Equ3 /= 1 or Equ4 /= 1 then
      Failed ("ERROR IN THE EQUAL OPERATOR");
   end if;

   if Neq1 /= 0 or Neq2 /= 1 or Neq3 /= 1 or Neq4 /= 0 then
      Failed ("ERROR IN THE NOT EQUAL OPERATOR");
   end if;

   Result;

end C49022c;
