-- C41323A.ADA

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
-- (+, -, *, /, **, ABS) MAY BE SELECTED FROM OUTSIDE THE PACKAGE USING AN
-- EXPANDED NAME, FOR A FLOATING POINT TYPE.

-- TBN  7/16/86

with Report; use Report;
procedure C41323a is

   package P is
      type Float is digits 5 range -1.0E1 .. 1.0E1;
      Obj_Flo_1 : Float := -5.5;
      Obj_Flo_2 : Float := 1.5;
      Obj_Flo_3 : Float := 10.0;
   end P;

   Flo_Var   : P.Float;
   Flo_Var_1 : P.Float := P."-" (P.Float'(5.5));
   Flo_Var_2 : P.Float := P.Float'(1.5);
   Flo_Var_3 : P.Float := P.Float'(1.0E1);

begin
   Test
     ("C41323A",
      "CHECK THAT IMPLICITLY DECLARED RELATIONAL " &
      "OPERATORS AND ARITHMETIC OPERATORS (+, -, *, " &
      "/, **, ABS) MAY BE SELECTED FROM OUTSIDE THE " &
      "PACKAGE USING AN EXPANDED NAME, FOR A " &
      "FLOATING POINT TYPE");

   if P."=" (Flo_Var_1, P."-" (P.Float'(5.55))) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
   end if;

   if P."/=" (Flo_Var_1, P.Obj_Flo_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
   end if;

   if P."<" (Flo_Var_2, P.Obj_Flo_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
   end if;

   if P.">" (Flo_Var_2, P.Obj_Flo_3) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
   end if;

   if P."<=" (Flo_Var_3, P.Float'(9.9)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
   end if;

   if P."<=" (Flo_Var_3, P.Float'(10.0)) then
      null;
   else
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
   end if;

   if P.">=" (P.Obj_Flo_2, Flo_Var_3) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
   end if;

   if P.">=" (P.Obj_Flo_3, Flo_Var_3) then
      null;
   else
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
   end if;

   Flo_Var := P."+" (Flo_Var_1, P.Obj_Flo_2);
   if P."/=" (Flo_Var, P."-" (P.Float'(4.0))) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 9");
   end if;

   Flo_Var := P."+" (Flo_Var_1);
   if P."/=" (Flo_Var, P.Obj_Flo_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 10");
   end if;

   Flo_Var := P."-" (Flo_Var_2, P.Obj_Flo_1);
   if P."/=" (Flo_Var, P.Float'(7.0)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 11");
   end if;

   Flo_Var := P."*" (Flo_Var_2, P.Float'(2.0));
   if P."/=" (Flo_Var, P.Float'(3.0)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 12");
   end if;

   Flo_Var := P."/" (Flo_Var_3, P.Float'(2.0));
   if P."/=" (Flo_Var, P.Float'(5.0)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 13");
   end if;

   Flo_Var := P."**" (P.Float'(2.0), 3);
   if P."/=" (Flo_Var, P.Float'(8.0)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 14");
   end if;

   Flo_Var := P."ABS" (Flo_Var_1);
   if P."/=" (Flo_Var, P.Float'(5.5)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 15");
   end if;

   Result;
end C41323a;
