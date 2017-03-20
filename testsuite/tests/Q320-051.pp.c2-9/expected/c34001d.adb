-- C34001D.ADA

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
-- CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
-- (IMPLICITLY) FOR DERIVED BOOLEAN TYPES.

-- JRK 8/20/86

with System; use System;
with Report; use Report;

procedure C34001d is

   subtype Parent is Boolean;

   subtype Subparent is
     Parent range
       Parent'Val (Ident_Int (Parent'Pos (False))) ..
         Parent'Val (Ident_Int (Parent'Pos (True)));

   type T is
     new Subparent range
         Parent'Val (Ident_Int (Parent'Pos (True))) ..
           Parent'Val (Ident_Int (Parent'Pos (True)));

   X : T       := True;
   W : Parent  := False;
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
     ("C34001D",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "BOOLEAN TYPES");

   X := Ident (True);
   if X /= True then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= True then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= True then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := True;
   end if;
   if T (W) /= True then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if Parent (X) /= True or Parent (T'Val (0)) /= False then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

   if Ident (True) /= True or Ident (True) = False then
      Failed ("INCORRECT ENUMERATION LITERAL");
   end if;

   if not X /= False or not False /= X then
      Failed ("INCORRECT ""NOT""");
   end if;

   if (X and Ident (True)) /= True or (X and False) /= False then
      Failed ("INCORRECT ""AND""");
   end if;

   if (X or Ident (True)) /= True or (False or X) /= True then
      Failed ("INCORRECT ""OR""");
   end if;

   if (X xor Ident (True)) /= False or (X xor False) /= True then
      Failed ("INCORRECT ""XOR""");
   end if;

   if (X and then Ident (True)) /= True or (X and then False) /= False then
      Failed ("INCORRECT ""AND THEN""");
   end if;

   if (X or else Ident (True)) /= True or (False or else X) /= True then
      Failed ("INCORRECT ""OR ELSE""");
   end if;

   if not (X = Ident (True)) or X = False then
      Failed ("INCORRECT =");
   end if;

   if X /= Ident (True) or not (X /= False) then
      Failed ("INCORRECT /=");
   end if;

   if X < Ident (True) or X < False then
      Failed ("INCORRECT <");
   end if;

   if X > Ident (True) or False > X then
      Failed ("INCORRECT >");
   end if;

   if not (X <= Ident (True)) or X <= False then
      Failed ("INCORRECT <=");
   end if;

   if not (X >= Ident (True)) or False >= X then
      Failed ("INCORRECT >=");
   end if;

   if not (X in T) or False in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (False not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT 'ADDRESS");
   end if;

   if T'Base'Size < 1 then
      Failed ("INCORRECT 'BASE'SIZE");
   end if;

   if T'First /= True or T'Base'First /= False then
      Failed ("INCORRECT 'FIRST");
   end if;

   if T'Image (X) /= "TRUE" or T'Image (False) /= "FALSE" then
      Failed ("INCORRECT 'IMAGE");
   end if;

   if T'Last /= True or T'Base'Last /= True then
      Failed ("INCORRECT 'LAST");
   end if;

   if T'Pos (X) /= 1 or T'Pos (False) /= 0 then
      Failed ("INCORRECT 'POS");
   end if;

   if T'Pred (X) /= False then
      Failed ("INCORRECT 'PRED");
   end if;

   if T'Size < 1 then
      Failed ("INCORRECT TYPE'SIZE");
   end if;

   if X'Size < 1 then
      Failed ("INCORRECT OBJECT'SIZE");
   end if;

   if T'Succ (T'Val (Ident_Int (0))) /= X then
      Failed ("INCORRECT 'SUCC");
   end if;

   if T'Val (Ident_Int (1)) /= X or T'Val (0) /= False then
      Failed ("INCORRECT 'VAL");
   end if;

   if T'Value (Ident_Str ("TRUE")) /= X or T'Value ("FALSE") /= False then
      Failed ("INCORRECT 'VALUE");
   end if;

   if T'Width /= 4 or T'Base'Width /= 5 then
      Failed ("INCORRECT 'WIDTH");
   end if;

   Result;
end C34001d;
