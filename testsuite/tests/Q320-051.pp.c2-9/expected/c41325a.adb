-- C41325A.ADA

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
-- CHECK THAT THE FOLLOWING IMPLICITLY DECLARED ENTITIES CAN BE SELECTED FROM
-- OUTSIDE THE PACKAGE USING AN EXPANDED NAME, FOR AN ARRAY TYPE.
--     CASE 1: CHECK EQUALITY AND INEQUALITY WHEN COMPONENT TYPE IS
--               NON-LIMITED, FOR MULTIDIMENSIONAL ARRAYS.
--     CASE 2: FOR ONE DIMENSIONAL ARRAYS:
--               A) CHECK CATENATION, EQUALITY, AND INEQUALITY WHEN
--                    COMPONENT TYPE IS NON-LIMITED.
--               B) CHECK RELATIONAL OPERATORS WHEN COMPONENT TYPE IS
--                    DISCRETE.
--               C) CHECK THE "NOT" OPERATOR AND THE LOGICAL OPERATORS
--                    WHEN COMPONENT TYPE IS BOOLEAN.

-- TBN  7/17/86

with Report; use Report;
procedure C41325a is

   package P is
      type Catarray is array (Integer range <>) of Integer;

      type Array_1 is array (1 .. 10) of Integer;
      type Array_2 is array (1 .. 4, 1 .. 4) of Integer;
      type Array_3 is array (1 .. 2, 1 .. 3, 1 .. 4) of Integer;
      type Array_4 is array (1 .. 10) of Boolean;
      type Array_5 is array (1 .. 4, 1 .. 4) of Boolean;
      type Array_6 is array (1 .. 2, 1 .. 3, 1 .. 4) of Boolean;

      Obj_Ara_1 : Array_1 := (1 .. 10 => Ident_Int (0));
      Obj_Ara_2 : Array_2 := (1 .. 4 => (1 .. 4 => Ident_Int (0)));
      Obj_Ara_3 : Array_3 := (1 .. 2 => (1 .. 3 => (1 .. 4 => Ident_Int (0))));
      Obj_Ara_4 : Array_4 := (1 .. 10 => Ident_Bool (False));
      Obj_Ara_5 : Array_5 := (1 .. 4 => (1 .. 4 => Ident_Bool (False)));
      Obj_Ara_6 : Array_6 :=
        (1 .. 2 => (1 .. 3 => (1 .. 4 => Ident_Bool (False))));
      Obj_Ara_7  : Catarray (1 .. 10) := (1 .. 10 => Ident_Int (0));
      Obj_Ara_20 : Catarray (1 .. 20) :=
        (1 .. 10 => 1, 11 .. 20 => Ident_Int (0));
   end P;

   Var_Ara_1 : P.Array_1 := (1 .. 10 => Ident_Int (1));
   Var_Ara_2 : P.Array_2 := (1 .. 4 => (1 .. 4 => Ident_Int (1)));
   Var_Ara_3 : P.Array_3 := (1 .. 2 => (1 .. 3 => (1 .. 4 => Ident_Int (1))));
   Var_Ara_4 : P.Array_4 := (1 .. 10 => Ident_Bool (True));
   Var_Ara_5 : P.Array_5 := (1 .. 4 => (1 .. 4 => Ident_Bool (True)));
   Var_Ara_6 : P.Array_6 :=
     (1 .. 2 => (1 .. 3 => (1 .. 4 => Ident_Bool (True))));
   Var_Ara_7  : P.Catarray (1 .. 10) := (1 .. 10 => Ident_Int (1));
   Var_Ara_8  : P.Array_4            := (1 .. 10 => Ident_Bool (True));
   Var_Ara_20 : P.Catarray (1 .. 20) := (1 .. 20 => Ident_Int (0));

begin
   Test
     ("C41325A",
      "CHECK THAT IMPLICITLY DECLARED ENTITIES CAN " &
      "BE SELECTED FROM OUTSIDE THE PACKAGE USING AN " &
      "EXPANDED NAME, FOR AN ARRAY TYPE");

   -- CASE 1: MULTIDIMENSIONAL ARRAYS.

   if P."=" (Var_Ara_2, P.Obj_Ara_2) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
   end if;

   if P."=" (Var_Ara_5, P.Obj_Ara_5) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
   end if;

   if P."/=" (Var_Ara_2, P.Array_2'(1 .. 4 => (1 .. 4 => 1))) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
   end if;

   if P."/=" (Var_Ara_5, P.Array_5'(1 .. 4 => (1 .. 4 => True))) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
   end if;

   if P."=" (Var_Ara_3, P.Obj_Ara_3) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
   end if;

   if P."/="
       (Var_Ara_6,
        P.Array_6'(1 .. 2 => (1 .. 3 => (1 .. 4 => True))))
   then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
   end if;

   -- CASE 2: ONE DIMENSIONAL ARRAYS.

   if P."=" (Var_Ara_1, P.Obj_Ara_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
   end if;

   if P."/=" (Var_Ara_1, P.Array_1'(1 .. 10 => 1)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
   end if;

   Var_Ara_20 := P."&" (Var_Ara_7, P.Obj_Ara_7);
   if P."/=" (Var_Ara_20, P.Obj_Ara_20) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 9");
   end if;

   if P."<" (Var_Ara_1, P.Obj_Ara_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 10");
   end if;

   if P.">" (P.Obj_Ara_1, Var_Ara_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 11");
   end if;

   if P."<=" (Var_Ara_1, P.Obj_Ara_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 12");
   end if;

   if P."<=" (Var_Ara_1, P.Array_1'(1 .. 10 => 1)) then
      null;
   else
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 13");
   end if;

   if P.">=" (Var_Ara_1, P.Array_1'(1 .. 10 => 2)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 14");
   end if;

   if P.">=" (Var_Ara_1, P.Array_1'(1 .. 10 => 1)) then
      null;
   else
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 15");
   end if;

   Var_Ara_8 := P."NOT" (Var_Ara_4);
   if P."/=" (Var_Ara_8, P.Obj_Ara_4) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 16");
   end if;

   Var_Ara_8 := P."OR" (Var_Ara_4, P.Obj_Ara_4);
   if P."=" (Var_Ara_8, P.Obj_Ara_4) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 17");
   end if;

   Var_Ara_8 := P."AND" (Var_Ara_4, P.Obj_Ara_4);
   if P."/=" (Var_Ara_8, P.Obj_Ara_4) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 18");
   end if;

   Var_Ara_8 := P."XOR" (Var_Ara_4, P.Obj_Ara_4);
   if P."=" (Var_Ara_8, P.Obj_Ara_4) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 19");
   end if;

   Result;
end C41325a;
