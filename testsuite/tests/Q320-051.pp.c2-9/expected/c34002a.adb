-- C34002A.ADA

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
-- CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED (IMPLICITLY) FOR
-- DERIVED INTEGER TYPES.

-- JRK 8/21/86

with System; use System;
with Report; use Report;

procedure C34002a is

   type Parent is range -100 .. 100;

   subtype Subparent is
     Parent range Parent'Val (Ident_Int (-50)) .. Parent'Val (Ident_Int (50));

   type T is
     new Subparent range
         Parent'Val (Ident_Int (-30)) ..
           Parent'Val (Ident_Int (30));

   type Fixed is delta 0.1 range -1_000.0 .. 1_000.0;

   X : T        := -30;
   W : Parent   := -100;
   N : constant := 1;
   M : constant := 100;
   B : Boolean  := False;
   F : Float    := 0.0;
   G : Fixed    := 0.0;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   function Ident (X : T) return T is
   begin
      if Equal (T'Pos (X), T'Pos (X)) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return T'First;
   end Ident;

begin
   Test
     ("C34002A",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "INTEGER TYPES");

   X := Ident (30);
   if X /= 30 then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= 30 then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= 30 then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := -30;
   end if;
   if T (W) /= -30 then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if Parent (X) /= 30 or Parent (T'Val (-100)) /= -100 then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

   if T (Ident_Int (-30)) /= -30 then
      Failed ("INCORRECT CONVERSION FROM INTEGER");
   end if;

   if Integer (X) /= 30 or Integer (T'Val (-100)) /= -100 then
      Failed ("INCORRECT CONVERSION TO INTEGER");
   end if;

   if Equal (3, 3) then
      F := -30.0;
   end if;
   if T (F) /= -30 then
      Failed ("INCORRECT CONVERSION FROM FLOAT");
   end if;

   if Float (X) /= 30.0 or Float (T'Val (-100)) /= -100.0 then
      Failed ("INCORRECT CONVERSION TO FLOAT");
   end if;

   if Equal (3, 3) then
      G := -30.0;
   end if;
   if T (G) /= -30 then
      Failed ("INCORRECT CONVERSION FROM FIXED");
   end if;

   if Fixed (X) /= 30.0 or Fixed (T'Val (-100)) /= -100.0 then
      Failed ("INCORRECT CONVERSION TO FIXED");
   end if;

   if Ident (N) /= 1 or X = M then
      Failed ("INCORRECT IMPLICIT CONVERSION");
   end if;

   if Ident (30) /= 30 or X = 100 then
      Failed ("INCORRECT INTEGER LITERAL");
   end if;

   if X = Ident (0) or X = 100 then
      Failed ("INCORRECT =");
   end if;

   if X /= Ident (30) or not (X /= 100) then
      Failed ("INCORRECT /=");
   end if;

   if X < Ident (30) or 100 < X then
      Failed ("INCORRECT <");
   end if;

   if X > Ident (30) or X > 100 then
      Failed ("INCORRECT >");
   end if;

   if X <= Ident (0) or 100 <= X then
      Failed ("INCORRECT <=");
   end if;

   if Ident (0) >= X or X >= 100 then
      Failed ("INCORRECT >=");
   end if;

   if not (X in T) or 100 in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (100 not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   if +X /= 30 or +T'Val (-100) /= -100 then
      Failed ("INCORRECT UNARY +");
   end if;

   if -X /= 0 - 30 or -T'Val (-100) /= 100 then
      Failed ("INCORRECT UNARY -");
   end if;

   if abs X /= 30 or abs T'Val (-100) /= 100 then
      Failed ("INCORRECT ABS");
   end if;

   if X + Ident (-1) /= 29 or X + 70 /= 100 then
      Failed ("INCORRECT BINARY +");
   end if;

   if X - Ident (30) /= 0 or X - 100 /= -70 then
      Failed ("INCORRECT BINARY -");
   end if;

   if X * Ident (-1) /= -30 or Ident (2) * 50 /= 100 then
      Failed ("INCORRECT *");
   end if;

   if X / Ident (3) /= 10 or 90 / X /= 3 then
      Failed ("INCORRECT /");
   end if;

   if X mod Ident (7) /= 2 or 100 mod X /= 10 then
      Failed ("INCORRECT MOD");
   end if;

   if X rem Ident (7) /= 2 or 100 rem X /= 10 then
      Failed ("INCORRECT REM");
   end if;

   if X**Ident_Int (1) /= 30 or T'Val (100)**Ident_Int (1) /= 100 then
      Failed ("INCORRECT **");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT 'ADDRESS");
   end if;

   if T'Base'Size < 8 then
      Failed ("INCORRECT 'BASE'SIZE");
   end if;

   if T'First /= -30 or
     T'Pos (T'Base'First) /= Parent'Pos (Parent'Base'First)
   then
      Failed ("INCORRECT 'FIRST");
   end if;

   if T'Image (X) /= " 30" or T'Image (-100) /= "-100" then
      Failed ("INCORRECT 'IMAGE");
   end if;

   if T'Last /= 30 or T'Pos (T'Base'Last) /= Parent'Pos (Parent'Base'Last) then
      Failed ("INCORRECT 'LAST");
   end if;

   if T'Pos (X) /= 30 or T'Pos (-100) /= -100 then
      Failed ("INCORRECT 'POS");
   end if;

   if T'Pred (X) /= 29 or T'Pred (100) /= 99 then
      Failed ("INCORRECT 'PRED");
   end if;

   if T'Size < 6 then
      Failed ("INCORRECT TYPE'SIZE");
   end if;

   if X'Size < 6 then
      Failed ("INCORRECT OBJECT'SIZE");
   end if;

   if T'Succ (Ident (29)) /= X or T'Succ (99) /= 100 then
      Failed ("INCORRECT 'SUCC");
   end if;

   if T'Val (Ident_Int (30)) /= X or T'Val (100) /= 100 then
      Failed ("INCORRECT 'VAL");
   end if;

   if T'Value (Ident_Str ("30")) /= X or T'Value ("100") /= 100 then
      Failed ("INCORRECT 'VALUE");
   end if;

   if T'Width /= 3 or T'Base'Width < 4 then
      Failed ("INCORRECT 'WIDTH");
   end if;

   Result;
end C34002a;
