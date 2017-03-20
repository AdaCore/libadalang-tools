-- C34006A.ADA

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
--     CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
--     (IMPLICITLY) FOR DERIVED RECORD TYPES WITHOUT DISCRIMINANTS
--     AND WITH NON-LIMITED COMPONENT TYPES.

-- HISTORY:
--     JRK 09/22/86  CREATED ORIGINAL TEST.
--     BCB 09/26/88  REMOVED COMPARISONS INVOLVING SIZE.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.

with System; use System;
with Report; use Report;

procedure C34006a is

   subtype Component is Integer;

   type Parent is record
      C : Component;
      B : Boolean := True;
   end record;

   type T is new Parent;

   X : T         := (2, False);
   K : Integer   := X'Size;
   W : Parent    := (2, False);
   C : Component := 1;
   B : Boolean   := False;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   function Ident (X : T) return T is
   begin
      if Equal (X.C, X.C) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return (-1, False);
   end Ident;

begin
   Test
     ("C34006A",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "RECORD TYPES WITHOUT DISCRIMINANTS AND WITH " &
      "NON-LIMITED COMPONENT TYPES");

   X := Ident ((1, True));
   if X /= (1, True) then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= (1, True) then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= (1, True) then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := (1, True);
   end if;
   if T (W) /= (1, True) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if Parent (X) /= (1, True) then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

   if Ident ((1, True)) /= (1, True) then
      Failed ("INCORRECT AGGREGATE");
   end if;

   if X.C /= 1 or X.B /= True then
      Failed ("INCORRECT SELECTION (VALUE)");
   end if;

   X.C := Ident_Int (3);
   X.B := Ident_Bool (False);
   if X /= (3, False) then
      Failed ("INCORRECT SELECTION (ASSIGNMENT)");
   end if;

   X := Ident ((1, True));
   if X = Ident ((1, False)) then
      Failed ("INCORRECT =");
   end if;

   if X /= Ident ((1, True)) then
      Failed ("INCORRECT /=");
   end if;

   if not (X in T) then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT 'ADDRESS");
   end if;

   if X.C'First_Bit < 0 then
      Failed ("INCORRECT 'FIRST_BIT");
   end if;

   if X.C'Last_Bit < 0 or X.C'Last_Bit - X.C'First_Bit + 1 /= X.C'Size then
      Failed ("INCORRECT 'LAST_BIT");
   end if;

   if X.C'Position < 0 then
      Failed ("INCORRECT 'POSITION");
   end if;

   Result;
end C34006a;
