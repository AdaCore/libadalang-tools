-- C34003A.ADA

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
-- DERIVED FLOATING POINT TYPES.

-- JRK 9/4/86
-- GJD 11/14/95 REMOVED USES OF OBSOLETE ADA 83 ATTRIBUTES.

with System; use System;
with Report; use Report;

procedure C34003a is

   type Parent is digits 5;

   subtype Subparent is
     Parent range Parent (Ident_Int (-50)) .. Parent (Ident_Int (50));

   type T is
     new Subparent digits 4 range Parent (Ident_Int (-30)) ..
         Parent (Ident_Int (30));

   type Fixed is delta 0.1 range -1_000.0 .. 1_000.0;

   X : T        := -30.0;
   W : Parent   := -100.0;
   R : constant := 1.0;
   M : constant := 100.0;
   B : Boolean  := False;
   F : Float    := 0.0;
   G : Fixed    := 0.0;

   Z : constant T := 0.0;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   function Ident (X : T) return T is
   begin
      if Equal (3, 3) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return T'First;
   end Ident;

begin
   Test
     ("C34003A",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " & "FLOATING POINT TYPES");

   X := Ident (30.0);
   if X /= 30.0 then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= 30.0 then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= 30.0 then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := -30.0;
   end if;
   if T (W) /= -30.0 then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if Parent (X) /= 30.0 or Parent (Z - 100.0) /= -100.0 then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

   if T (Ident_Int (-30)) /= -30.0 then
      Failed ("INCORRECT CONVERSION FROM INTEGER");
   end if;

   if Integer (X) /= 30 or Integer (Z - 100.0) /= -100 then
      Failed ("INCORRECT CONVERSION TO INTEGER");
   end if;

   if Equal (3, 3) then
      F := -30.0;
   end if;
   if T (F) /= -30.0 then
      Failed ("INCORRECT CONVERSION FROM FLOAT");
   end if;

   if Float (X) /= 30.0 or Float (Z - 100.0) /= -100.0 then
      Failed ("INCORRECT CONVERSION TO FLOAT");
   end if;

   if Equal (3, 3) then
      G := -30.0;
   end if;
   if T (G) /= -30.0 then
      Failed ("INCORRECT CONVERSION FROM FIXED");
   end if;

   if Fixed (X) /= 30.0 or Fixed (Z - 100.0) /= -100.0 then
      Failed ("INCORRECT CONVERSION TO FIXED");
   end if;

   if Ident (R) /= 1.0 or X = M then
      Failed ("INCORRECT IMPLICIT CONVERSION");
   end if;

   if Ident (30.0) /= 30.0 or X = 100.0 then
      Failed ("INCORRECT REAL LITERAL");
   end if;

   if X = Ident (0.0) or X = 100.0 then
      Failed ("INCORRECT =");
   end if;

   if X /= Ident (30.0) or not (X /= 100.0) then
      Failed ("INCORRECT /=");
   end if;

   if X < Ident (30.0) or 100.0 < X then
      Failed ("INCORRECT <");
   end if;

   if X > Ident (30.0) or X > 100.0 then
      Failed ("INCORRECT >");
   end if;

   if X <= Ident (0.0) or 100.0 <= X then
      Failed ("INCORRECT <=");
   end if;

   if Ident (0.0) >= X or X >= 100.0 then
      Failed ("INCORRECT >=");
   end if;

   if not (X in T) or 100.0 in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (100.0 not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   if +X /= 30.0 or +(Z - 100.0) /= -100.0 then
      Failed ("INCORRECT UNARY +");
   end if;

   if -X /= 0.0 - 30.0 or -(Z - 100.0) /= 100.0 then
      Failed ("INCORRECT UNARY -");
   end if;

   if abs X /= 30.0 or abs (Z - 100.0) /= 100.0 then
      Failed ("INCORRECT ABS");
   end if;

   if X + Ident (-1.0) /= 29.0 or X + 70.0 /= 100.0 then
      Failed ("INCORRECT BINARY +");
   end if;

   if X - Ident (30.0) /= 0.0 or X - 100.0 /= -70.0 then
      Failed ("INCORRECT BINARY -");
   end if;

   if X * Ident (-1.0) /= -30.0 or Ident (2.0) * 50.0 /= 100.0 then
      Failed ("INCORRECT *");
   end if;

   if X / Ident (3.0) /= 10.0 or 90.0 / X /= 3.0 then
      Failed ("INCORRECT /");
   end if;

   if X**Ident_Int (1) /= 30.0 or (Z + 100.0)**Ident_Int (1) /= 100.0 then
      Failed ("INCORRECT **");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT 'ADDRESS");
   end if;

   if T'Base'Size < 27 then
      Failed ("INCORRECT 'BASE'SIZE");
   end if;

   if T'Digits /= 4 or T'Base'Digits < 5 then
      Failed ("INCORRECT 'DIGITS");
   end if;

   if T'First /= -30.0 then
      Failed ("INCORRECT 'FIRST");
   end if;

   if T'Last /= 30.0 then
      Failed ("INCORRECT 'LAST");
   end if;

   if T'Machine_Emax < 1 or T'Base'Machine_Emax /= T'Machine_Emax then
      Failed ("INCORRECT 'MACHINE_EMAX");
   end if;

   if T'Machine_Emin > -1 or T'Base'Machine_Emin /= T'Machine_Emin then
      Failed ("INCORRECT 'MACHINE_EMIN");
   end if;

   if T'Machine_Mantissa < 1 or T'Base'Machine_Mantissa /= T'Machine_Mantissa
   then
      Failed ("INCORRECT 'MACHINE_MANTISSA");
   end if;

   if T'Machine_Overflows /= T'Base'Machine_Overflows then
      Failed ("INCORRECT 'MACHINE_OVERFLOWS");
   end if;

   if T'Machine_Radix < 2 or T'Base'Machine_Radix /= T'Machine_Radix then
      Failed ("INCORRECT 'MACHINE_RADIX");
   end if;

   if T'Machine_Rounds /= T'Base'Machine_Rounds then
      Failed ("INCORRECT 'MACHINE_ROUNDS");
   end if;

   if T'Size < 23 then
      Failed ("INCORRECT TYPE'SIZE");
   end if;

   if X'Size < 23 then
      Failed ("INCORRECT OBJECT'SIZE");
   end if;

   Result;
end C34003a;
