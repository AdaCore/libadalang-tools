-- C41322A.ADA

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
-- (+, -, *, /, **, ABS, MOD, REM) MAY BE SELECTED FROM OUTSIDE THE PACKAGE
-- USING AN EXPANDED NAME, FOR AN INTEGER TYPE.

-- TBN  7/16/86

with Report; use Report;
procedure C41322a is

   package P is
      type Int is range -10 .. 10;
      Obj_Int_1 : Int := -10;
      Obj_Int_2 : Int := 1;
      Obj_Int_3 : Int := 10;
   end P;

   Int_Var   : P.Int;
   Int_Var_1 : P.Int := P."-" (P.Int'(10));
   Int_Var_2 : P.Int := P.Int'(1);
   Int_Var_3 : P.Int := P.Int'(10);

begin
   Test
     ("C41322A",
      "CHECK THAT IMPLICITLY DECLARED RELATIONAL " &
      "OPERATORS AND ARITHMETIC OPERATORS (+, -, *, " &
      "/, **, ABS, MOD, REM) MAY BE SELECTED FROM " &
      "OUTSIDE THE PACKAGE USING AN EXPANDED NAME, " &
      "FOR AN INTEGER TYPE");

   if P."=" (Int_Var_1, P.Int'(2)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
   end if;

   if P."/=" (Int_Var_1, P.Obj_Int_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
   end if;

   if P."<" (Int_Var_2, 0) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
   end if;

   if P.">" (Int_Var_2, P.Obj_Int_3) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
   end if;

   if P."<=" (Int_Var_3, P.Int'(9)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
   end if;

   for J in P.Int'(4) .. P.Int'(4) loop
      if P.">=" (J, Int_Var_3) then
         Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
      end if;
   end loop;

   Int_Var := P."+" (Int_Var_1, P.Int'(2));
   if P."/=" (Int_Var, P."-" (P.Int'(8))) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
   end if;

   Int_Var := P."+" (P.Int'(2));
   if P."/=" (Int_Var, P.Int'(2)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
   end if;

   Int_Var := P."-" (Int_Var_2, P.Int'(0));
   if P."/=" (Int_Var, P.Obj_Int_2) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 9");
   end if;

   Int_Var := P."*" (Int_Var_2, P.Int'(5));
   if P."/=" (Int_Var, P.Int'(5)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 10");
   end if;

   Int_Var := P."/" (Int_Var_3, P.Int'(2));
   if P."/=" (Int_Var, P.Int'(5)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 11");
   end if;

   Int_Var := P."**" (P.Int'(2), 3);
   if P."/=" (Int_Var, P.Int'(8)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 12");
   end if;

   Int_Var := P."ABS" (Int_Var_1);
   if P."/=" (Int_Var, P.Obj_Int_3) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 13");
   end if;

   Int_Var := P."MOD" (Int_Var_1, P.Int'(3));
   if P."/=" (Int_Var, P.Int'(2)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 14");
   end if;

   Int_Var := P."REM" (Int_Var_1, P.Int'(3));
   if P."/=" (Int_Var, P."-" (Int_Var_2)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 15");
   end if;

   Result;
end C41322a;
