-- C34001A.ADA

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
-- DERIVED ENUMERATION TYPES, EXCLUDING BOOLEAN TYPES.

-- JRK 8/20/86

with System; use System;
with Report; use Report;

procedure C34001a is

   type Parent is (E1, E2, E3, 'A', E4, E5, E6);

   subtype Subparent is
     Parent range
       Parent'Val (Ident_Int (Parent'Pos (E2))) ..
         Parent'Val (Ident_Int (Parent'Pos (E5)));

   type T is
     new Subparent range
       Parent'Val (Ident_Int (Parent'Pos (E3))) ..
         Parent'Val (Ident_Int (Parent'Pos (E4)));

   X : T       := E3;
   W : Parent  := E1;
   B : Boolean := False;

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
     ("C34001A",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "ENUMERATION TYPES, EXCLUDING BOOLEAN TYPES");

   X := Ident (E4);
   if X /= E4 then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= E4 then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= E4 then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := E3;
   end if;
   if T (W) /= E3 then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if Parent (X) /= E4 or Parent (T'Val (0)) /= E1 then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

   if Ident ('A') /= 'A' then
      Failed ("INCORRECT 'A'");
   end if;

   if Ident (E3) /= E3 or Ident (E4) = E1 then
      Failed ("INCORRECT ENUMERATION LITERAL");
   end if;

   if X = Ident ('A') or X = E1 then
      Failed ("INCORRECT =");
   end if;

   if X /= Ident (E4) or not (X /= E1) then
      Failed ("INCORRECT /=");
   end if;

   if X < Ident (E4) or X < E1 then
      Failed ("INCORRECT <");
   end if;

   if X > Ident (E4) or X > E6 then
      Failed ("INCORRECT >");
   end if;

   if X <= Ident ('A') or X <= E1 then
      Failed ("INCORRECT <=");
   end if;

   if Ident ('A') >= X or X >= E6 then
      Failed ("INCORRECT >=");
   end if;

   if not (X in T) or E1 in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (E1 not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT 'ADDRESS");
   end if;

   if T'Base'Size < 3 then
      Failed ("INCORRECT 'BASE'SIZE");
   end if;

   if T'First /= E3 or T'Base'First /= E1 then
      Failed ("INCORRECT 'FIRST");
   end if;

   if T'Image (X) /= "E4" or T'Image (E1) /= "E1" then
      Failed ("INCORRECT 'IMAGE");
   end if;

   if T'Last /= E4 or T'Base'Last /= E6 then
      Failed ("INCORRECT 'LAST");
   end if;

   if T'Pos (X) /= 4 or T'Pos (E1) /= 0 then
      Failed ("INCORRECT 'POS");
   end if;

   if T'Pred (X) /= 'A' or T'Pred (E2) /= E1 then
      Failed ("INCORRECT 'PRED");
   end if;

   if T'Size < 2 then
      Failed ("INCORRECT TYPE'SIZE");
   end if;

   if X'Size < 2 then
      Failed ("INCORRECT OBJECT'SIZE");
   end if;

   if T'Succ (Ident ('A')) /= X or T'Succ (E1) /= E2 then
      Failed ("INCORRECT 'SUCC");
   end if;

   if T'Val (Ident_Int (4)) /= X or T'Val (0) /= E1 then
      Failed ("INCORRECT 'VAL");
   end if;

   if T'Value (Ident_Str ("E4")) /= X or T'Value ("E1") /= E1 then
      Failed ("INCORRECT 'VALUE");
   end if;

   if T'Width /= 3 or T'Base'Width /= 3 then
      Failed ("INCORRECT 'WIDTH");
   end if;

   Result;
end C34001a;
