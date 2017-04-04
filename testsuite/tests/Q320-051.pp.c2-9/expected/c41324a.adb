-- C41324A.ADA

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
-- CHECK THAT IMPLICITLY DECLARED RELATIONAL OPERATORS AND ARITHMETIC OPERATORS
-- (+, -, *, /, ABS) MAY BE SELECTED FROM OUTSIDE THE PACKAGE USING AN EXPANDED
-- NAME, FOR A FIXED POINT TYPE.

-- TBN  7/16/86

with Report; use Report;
procedure C41324a is

   package P is
      type Fixed is delta 0.125 range -1.0E1 .. 1.0E1;
      Obj_Fix_1 : Fixed := -5.5;
      Obj_Fix_2 : Fixed := 1.5;
      Obj_Fix_3 : Fixed := 10.0;
   end P;

   Fix_Var   : P.Fixed;
   Fix_Var_1 : P.Fixed := P."-" (P.Fixed'(5.5));
   Fix_Var_2 : P.Fixed := P.Fixed'(1.5);
   Fix_Var_3 : P.Fixed := P.Fixed'(1.0E1);

begin
   Test
     ("C41324A",
      "CHECK THAT IMPLICITLY DECLARED RELATIONAL " &
      "OPERATORS AND ARITHMETIC OPERATORS (+, -, *, " &
      "/, ABS) MAY BE SELECTED FROM OUTSIDE THE " &
      "PACKAGE USING AN EXPANDED NAME, FOR A FIXED " &
      "POINT TYPE");

   if P."=" (Fix_Var_1, P."-" (P.Fixed'(6.0))) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
   end if;

   if P."/=" (Fix_Var_1, P.Obj_Fix_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
   end if;

   if P."<" (Fix_Var_2, P.Obj_Fix_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
   end if;

   if P.">" (Fix_Var_2, P.Obj_Fix_3) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
   end if;

   if P."<=" (Fix_Var_3, P.Fixed'(9.9)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
   end if;

   if P."<=" (Fix_Var_3, P.Fixed'(10.0)) then
      null;
   else
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
   end if;

   if P.">=" (P.Obj_Fix_2, Fix_Var_3) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
   end if;

   if P.">=" (P.Obj_Fix_2, Fix_Var_2) then
      null;
   else
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
   end if;

   Fix_Var := P."+" (Fix_Var_1, P.Obj_Fix_2);
   if P."/=" (Fix_Var, P."-" (P.Fixed'(4.0))) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 9");
   end if;

   Fix_Var := P."-" (Fix_Var_2, P.Obj_Fix_1);
   if P."/=" (Fix_Var, P.Fixed'(7.0)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 10");
   end if;

   Fix_Var := P."*" (Fix_Var_2, 2);
   if P."/=" (Fix_Var, P.Fixed'(3.0)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 11");
   end if;

   Fix_Var := P."*" (3, Fix_Var_2);
   if P."/=" (Fix_Var, P.Fixed'(4.5)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 12");
   end if;

   Fix_Var := P."/" (Fix_Var_3, 2);
   if P."/=" (Fix_Var, P.Fixed'(5.0)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 13");
   end if;

   Fix_Var := P."ABS" (Fix_Var_1);
   if P."/=" (Fix_Var, P.Fixed'(5.5)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 14");
   end if;

   Result;
end C41324a;
