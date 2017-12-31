-- C72002A.ADA

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
--     CHECK THAT THE DECLARATIVE ITEMS IN A PACKAGE SPECIFICATION ARE
--     ELABORATED IN THE ORDER DECLARED.

-- HISTORY:
--     DHH 03/09/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C72002a is

   A : Integer := 0;
   type Order_Array is array (1 .. 14) of Integer;
   Object_Array : Order_Array;
   type Real is digits 4;
   type Enum is (Red, Yellow, Blue);

   type Arr is array (1 .. 2) of Boolean;
   D : Arr := (True, True);
   E : Arr := (False, False);

   type Rec is record
      I : Integer;
   end record;
   B : Rec := (I => Ident_Int (1));
   C : Rec := (I => Ident_Int (2));

   function Given_Order (X : Integer) return Integer is
      Y : Integer;
   begin
      Y := X + 1;
      return Y;
   end Given_Order;

   function Bool (X : Integer) return Boolean is
   begin
      if X = Ident_Int (1) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return True;
      elsif X = Ident_Int (8) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return False;
      end if;
   end Bool;

   function Int (X : Integer) return Integer is
   begin
      if X = Ident_Int (2) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return Ident_Int (1);
      elsif X = Ident_Int (9) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return Ident_Int (2);
      end if;
   end Int;

   function Float (X : Integer) return Real is
   begin
      if X = Ident_Int (3) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return 1.0;
      elsif X = Ident_Int (10) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return 2.0;
      end if;
   end Float;

   function Char (X : Integer) return Character is
   begin
      if X = Ident_Int (4) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return 'A';
      elsif X = Ident_Int (11) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return 'Z';
      end if;
   end Char;

   function Enumr (X : Integer) return Enum is
   begin
      if X = Ident_Int (5) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return Red;
      elsif X = Ident_Int (12) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return Yellow;
      end if;
   end Enumr;

   function Arry (X : Integer) return Arr is
   begin
      if X = Ident_Int (6) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return D;
      elsif X = Ident_Int (13) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return E;
      end if;
   end Arry;

   function Recor (X : Integer) return Rec is
   begin
      if X = Ident_Int (7) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return B;
      elsif X = Ident_Int (14) then
         A                := Given_Order (A);
         Object_Array (X) := A;
         return C;
      end if;
   end Recor;

   package Pack is
      A : Boolean   := Bool (1);
      B : Integer   := Int (2);
      C : Real      := Float (3);
      D : Character := Char (4);
      E : Enum      := Enumr (5);
      F : Arr       := Arry (6);
      G : Rec       := Recor (7);
      H : Boolean   := Bool (8);
      I : Integer   := Int (9);
      J : Real      := Float (10);
      K : Character := Char (11);
      L : Enum      := Enumr (12);
      M : Arr       := Arry (13);
      N : Rec       := Recor (14);
   end Pack;

begin
   Test
     ("C72002A",
      "CHECK THAT THE DECLARATIVE ITEMS IN A PACKAGE " &
      "SPECIFICATION ARE ELABORATED IN THE ORDER " & "DECLARED");

   if Object_Array (1) /= Ident_Int (1) then
      Failed ("BOOLEAN 1 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (2) /= Ident_Int (2) then
      Failed ("INTEGER 1 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (3) /= Ident_Int (3) then
      Failed ("REAL 1 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (4) /= Ident_Int (4) then
      Failed ("CHARACTER 1 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (5) /= Ident_Int (5) then
      Failed ("ENUMERATION 1 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (6) /= Ident_Int (6) then
      Failed ("ARRAY 1 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (7) /= Ident_Int (7) then
      Failed ("RECORD 1 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (8) /= Ident_Int (8) then
      Failed ("BOOLEAN 2 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (9) /= Ident_Int (9) then
      Failed ("INTEGER 2 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (10) /= Ident_Int (10) then
      Failed ("REAL 2 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (11) /= Ident_Int (11) then
      Failed ("CHARACTER 2 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (12) /= Ident_Int (12) then
      Failed ("ENUMERATION 2 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (13) /= Ident_Int (13) then
      Failed ("ARRAY 2 ELABORATED OUT OF ORDER");
   end if;

   if Object_Array (14) /= Ident_Int (14) then
      Failed ("RECORD 2 ELABORATED OUT OF ORDER");
   end if;

   Result;
end C72002a;
