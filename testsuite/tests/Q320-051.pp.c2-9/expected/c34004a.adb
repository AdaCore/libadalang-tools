-- C34004A.ADA

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
-- OBJECTIVE:
--      CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
--      (IMPLICITLY) FOR DERIVED FIXED POINT TYPES.

-- HISTORY:
--      JRK 09/08/86  CREATED ORIGINAL TEST.
--      JET 08/06/87  FIXED BUGS IN DELTAS AND RANGE ERROR.
--      JET 09/22/88  CHANGED USAGE OF X'SIZE.
--      RDH 04/16/90  ADDED TEST FOR REAL VARIABLE VALUES.
--      THS 09/25/90  REMOVED ALL REFERENCES TO B, MODIFIED CHECK OF
--                    '=', INITIALIZED Z NON-STATICALLY, MOVED BINARY
--                    CHECKS.
--      DTN 11/30/95  REMOVED NON ADA95 ATTRIBUTES.
--      KAS 03/04/96  REMOVED COMPARISON OF T'SMALL TO T'BASE'SMALL

with System; use System;
with Report; use Report;

procedure C34004a is

   type Parent is delta 2.0**(-7) range -100.0 .. 100.0;

   subtype Subparent is
     Parent range Ident_Int (1) * (-50.0) .. Ident_Int (1) * (50.0);

   type T is
     new Subparent delta 2.0**(-4) range
       Ident_Int (1) * (-30.0) ..
         Ident_Int (1) * (30.0);

   type Fixed is delta 2.0**(-4) range -1_000.0 .. 1_000.0;

   X : T        := -30.0;
   I : Integer  := X'Size;  --CHECK FOR THE AVAILABILITY OF 'SIZE.
   W : Parent   := -100.0;
   R : constant := 1.0;
   M : constant := 100.0;
   F : Float    := 0.0;
   G : Fixed    := 0.0;

   procedure A (X : Address) is
   begin
      null;
   end A;

   function Ident (X : T) return T is
   begin
      if Equal (3, 3) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return T'First;
   end Ident;

begin

   declare
      Z : constant T := Ident (0.0);
   begin
      Test
        ("C34004A",
         "CHECK THAT THE REQUIRED PREDEFINED " &
         "OPERATIONS ARE DECLARED (IMPLICITLY) " &
         "FOR DERIVED FIXED POINT TYPES");

      X := Ident (30.0);
      if X /= 30.0 then
         Failed ("INCORRECT :=");
      end if;

      if X + Ident (-1.0) /= 29.0 or X + 70.0 /= 100.0 then
         Failed ("INCORRECT BINARY +");
      end if;

      if X - Ident (30.0) /= 0.0 or X - 100.0 /= -70.0 then
         Failed ("INCORRECT BINARY -");
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

      if not (X = Ident (30.0)) then
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

      if T (X * Ident (-1.0)) /= -30.0 or
        T (Ident (2.0) * (Z + 15.0)) /= 30.0
      then
         Failed ("INCORRECT * (FIXED, FIXED)");
      end if;

      if X * Ident_Int (-1) /= -30.0 or (Z + 50.0) * 2 /= 100.0 then
         Failed ("INCORRECT * (FIXED, INTEGER)");
      end if;

      if Ident_Int (-1) * X /= -30.0 or 2 * (Z + 50.0) /= 100.0 then
         Failed ("INCORRECT * (INTEGER, FIXED)");
      end if;

      if T (X / Ident (3.0)) /= 10.0 or T ((Z + 90.0) / X) /= 3.0 then
         Failed ("INCORRECT / (FIXED, FIXED)");
      end if;

      if X / Ident_Int (3) /= 10.0 or (Z + 90.0) / 30 /= 3.0 then
         Failed ("INCORRECT / (FIXED, INTEGER)");
      end if;

      A (X'Address);

      if T'Aft /= 2 or T'Base'Aft < 3 then
         Failed ("INCORRECT 'AFT");
      end if;

      if T'Base'Size < 15 then
         Failed ("INCORRECT 'BASE'SIZE");
      end if;

      if T'Delta /= 2.0**(-4) or T'Base'Delta > 2.0**(-7) then
         Failed ("INCORRECT 'DELTA");
      end if;

      if T'Fore /= 3 or T'Base'Fore < 4 then
         Failed ("INCORRECT 'FORE");
      end if;

      if T'Machine_Overflows /= T'Base'Machine_Overflows then
         Failed ("INCORRECT 'MACHINE_OVERFLOWS");
      end if;

      if T'Machine_Rounds /= T'Base'Machine_Rounds then
         Failed ("INCORRECT 'MACHINE_ROUNDS");
      end if;

      if T'Size < 10 then
         Failed ("INCORRECT TYPE'SIZE");
      end if;

      if T'Small > 2.0**(-4) or T'Base'Small > 2.0**(-7) then
         Failed ("INCORRECT 'SMALL");
      end if;
   end;

   Result;
end C34004a;
