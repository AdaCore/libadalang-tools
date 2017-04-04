-- C41327A.ADA

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
-- CHECK THAT IMPLICITLY DECLARED EQUALITY AND INEQUALITY OPERATORS MAY BE
-- SELECTED FROM OUTSIDE A PACKAGE USING AN EXPANDED NAME, FOR A PRIVATE TYPE.

-- TBN  7/18/86

with Report; use Report;
procedure C41327a is

   package P is
      type Key is private;
      type Char is private;
      function Init_Key (X : Natural) return Key;
      function Init_Char (X : Character) return Char;
   private
      type Key is new Natural;
      type Char is new Character;
   end P;

   Var_Key_1  : P.Key;
   Var_Key_2  : P.Key;
   Var_Char_1 : P.Char;
   Var_Char_2 : P.Char;

   package body P is

      function Init_Key (X : Natural) return Key is
      begin
         return (Key (X));
      end Init_Key;

      function Init_Char (X : Character) return Char is
      begin
         return (Char (X));
      end Init_Char;

   begin
      null;
   end P;

begin
   Test
     ("C41327A",
      "CHECK THAT IMPLICITLY DECLARED EQUALITY AND " &
      "INEQUALITY OPERATORS MAY BE SELECTED FROM " &
      "OUTSIDE A PACKAGE USING AN EXPANDED NAME, " &
      "FOR A PRIVATE TYPE");

   Var_Key_1  := P.Init_Key (1);
   Var_Key_2  := P.Init_Key (2);
   Var_Char_1 := P.Init_Char ('A');
   Var_Char_2 := P.Init_Char ('A');
   if P."=" (Var_Key_1, Var_Key_2) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
   end if;

   if P."/=" (Var_Char_1, Var_Char_2) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
   end if;

   Result;
end C41327a;
