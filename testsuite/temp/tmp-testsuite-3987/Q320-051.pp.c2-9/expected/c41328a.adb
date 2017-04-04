-- C41328A.ADA

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
-- CHECK THAT IMPLICITLY DECLARED DERIVED SUBPROGRAMS CAN BE SELECTED
-- FROM OUTSIDE A PACKAGE USING AN EXPANDED NAME, FOR A DERIVED TYPE.

-- TBN  7/21/86

with Report; use Report;
procedure C41328a is

   package P is
      package Q is
         type Pair is array (1 .. 2) of Integer;
         function Init (Int : Integer) return Pair;
         procedure Swap (Two : in out Pair);
      end Q;
      type Couple is new Q.Pair;
   end P;

   Var_1 : P.Couple;
   Var_2 : P.Couple;

   package body P is

      package body Q is

         function Init (Int : Integer) return Pair is
            A : Pair;
         begin
            A (1) := Int;
            A (2) := Int + 1;
            return (A);
         end Init;

         procedure Swap (Two : in out Pair) is
            Temp : Integer;
         begin
            Temp    := Two (1);
            Two (1) := Two (2);
            Two (2) := Temp;
         end Swap;

      begin
         null;
      end Q;

   begin
      null;
   end P;

begin
   Test
     ("C41328A",
      "CHECK THAT IMPLICITLY DECLARED DERIVED " &
      "SUBPROGRAMS CAN BE SELECTED FROM OUTSIDE A " &
      "PACKAGE USING AN EXPANDED NAME, FOR A DERIVED " &
      "TYPE");

   Var_1 := P.Init (Ident_Int (1));
   if P."/=" (Var_1, P.Couple'(1 => 1, 2 => 2)) then
      Failed ("INCORRECT RESULTS FROM DERIVED SUBPROGRAM - 1");
   end if;

   Var_2 := P.Init (Ident_Int (2));
   if P."=" (Var_2, P.Couple'(1 => 1, 2 => 2)) then
      Failed ("INCORRECT RESULTS FROM DERIVED SUBPROGRAM - 2");
   end if;

   P.Swap (Var_1);
   if P."=" (Var_1, P.Couple'(1 => 1, 2 => 2)) then
      Failed ("INCORRECT RESULTS FROM DERIVED SUBPROGRAM - 3");
   end if;

   P.Swap (Var_2);
   if P."/=" (Var_2, P.Couple'(1 => 3, 2 => 2)) then
      Failed ("INCORRECT RESULTS FROM DERIVED SUBPROGRAM - 4");
   end if;

   Result;
end C41328a;
