-- C34005P.ADA

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
--     (IMPLICITLY) FOR DERIVED ONE-DIMENSIONAL ARRAY TYPES WHOSE
--     COMPONENT TYPE IS A LIMITED TYPE.

-- HISTORY:
--     JRK 08/17/87  CREATED ORIGINAL TEST.
--     VCL 07/01/88  MODIFIED THE STATEMENTS INVOLVING THE 'SIZE
--                   ATTRIBUTE TO REMOVE ANY ASSUMPTIONS ABOUT THE
--                   SIZES.  ADDED EXCEPTION HANDLERS TO CATCH INCORRECT
--                   TYPE CONVERSIONS TO DERIVED SUBTYPES.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.
--     RLB 10/03/02  REMOVED ILLEGAL (BY AI-246) TYPE CONVERSIONS AND
--                   SUPPORTING CODE.
--     RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURNS.

with System; use System;
with Report; use Report;

procedure C34005p is

   package Pkg_L is

      type Lp is limited private;

      function Create (X : Integer) return Lp;

      function Value (X : Lp) return Integer;

      function Equal (X, Y : Lp) return Boolean;

      procedure Assign (X : out Lp; Y : Lp);

      C1 : constant Lp;
      C2 : constant Lp;
      C3 : constant Lp;
      C4 : constant Lp;
      C5 : constant Lp;
      C6 : constant Lp;

   private

      type Lp is new Integer;

      C1 : constant Lp := 1;
      C2 : constant Lp := 2;
      C3 : constant Lp := 3;
      C4 : constant Lp := 4;
      C5 : constant Lp := 5;
      C6 : constant Lp := 6;

   end Pkg_L;

   use Pkg_L;

   subtype Component is Lp;

   package Pkg_P is

      First : constant := 0;
      Last  : constant := 100;

      subtype Index is Integer range First .. Last;

      type Parent is array (Index range <>) of Component;

      function Create
        (F, L  : Index;
         C     : Component;
         Dummy : Parent   -- TO RESOLVE OVERLOADING.
         ) return Parent;

      function Equal (X, Y : Parent) return Boolean;

      function Aggr (X, Y : Component) return Parent;

      function Aggr (X, Y, Z : Component) return Parent;

   end Pkg_P;

   use Pkg_P;

   type T is new Parent (Ident_Int (5) .. Ident_Int (7));

   X : T;
   W : Parent (5 .. 7);
   C : Component;
   B : Boolean  := False;
   N : constant := 1;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   function V return T is
   begin
      return Result : T do
         for I in Result'Range loop
            Assign (Result (I), C);
         end loop;
      end return;
   end V;

   package body Pkg_L is

      function Create (X : Integer) return Lp is
      begin
         return Lp (Ident_Int (X));
      end Create;

      function Value (X : Lp) return Integer is
      begin
         return Integer (X);
      end Value;

      function Equal (X, Y : Lp) return Boolean is
      begin
         return X = Y;
      end Equal;

      procedure Assign (X : out Lp; Y : Lp) is
      begin
         X := Y;
      end Assign;

   end Pkg_L;

   package body Pkg_P is

      function Create
        (F, L  : Index;
         C     : Component;
         Dummy : Parent) return Parent
      is
         B : Component;
      begin
         return A : Parent (F .. L) do
            Assign (B, C);
            for I in F .. L loop
               Assign (A (I), B);
               Assign (B, Create (Value (B) + 1));
            end loop;
         end return;
      end Create;

      function Equal (X, Y : Parent) return Boolean is
      begin
         if X'Length /= Y'Length then
            return False;
         else
            for I in X'Range loop
               if not Equal (X (I), Y (I - X'First + Y'First)) then
                  return False;
               end if;
            end loop;
         end if;
         return True;
      end Equal;

      function Aggr (X, Y : Component) return Parent is
      begin
         return Result : Parent (Index'First .. Index'First + 1) do
            Assign (Result (Index'First), X);
            Assign (Result (Index'First + 1), Y);
         end return;
      end Aggr;

      function Aggr (X, Y, Z : Component) return Parent is
      begin
         return Result : Parent (Index'First .. Index'First + 2) do
            Assign (Result (Index'First), X);
            Assign (Result (Index'First + 1), Y);
            Assign (Result (Index'First + 2), Z);
         end return;
      end Aggr;

   end Pkg_P;

begin
   Test
     ("C34005P",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A LIMITED TYPE");

   Assign (X (Ident_Int (5)), Create (1));
   Assign (X (Ident_Int (6)), Create (2));
   Assign (X (Ident_Int (7)), Create (3));

   Assign (W (5), Create (1));
   Assign (W (6), Create (2));
   Assign (W (7), Create (3));

   Assign (C, Create (2));

   if not Equal (T'(X), Aggr (C1, C2, C3)) then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if not Equal (T (X), Aggr (C1, C2, C3)) then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if not Equal (T (W), Aggr (C1, C2, C3)) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if not Equal (Parent (X), Aggr (C1, C2, C3)) then
      Failed ("INCORRECT CONVERSION TO PARENT - 1");
   end if;

   begin
      if not Equal (Parent (Create (2, 3, C4, X)), Aggr (C4, C5)) then
         Failed ("INCORRECT CONVERSION TO PARENT - 2");
      end if;
   exception
      when others =>
         Failed
           ("EXCEPTION RAISED WHILE CHECKING BASE TYPE " &
            "VALUES OUTSIDE OF THE SUBTYPE T - 1");
   end;

   if not Equal (X (Ident_Int (5)), C1) then
      Failed ("INCORRECT INDEX (VALUE)");
   end if;

   begin
      if not Equal (X (Ident_Int (6) .. Ident_Int (7)), Aggr (C2, C3)) or
        not Equal (Create (1, 4, C4, X) (1 .. 3), Aggr (C4, C5, C6))
      then
         Failed ("INCORRECT SLICE (VALUE)");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED WHILE CHECKING SLICES");
   end;

   if not (X in T) or Aggr (C1, C2) in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (Aggr (C1, C2) not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT 'ADDRESS");
   end if;

   if T'First /= 5 then
      Failed ("INCORRECT TYPE'FIRST");
   end if;

   if X'First /= 5 then
      Failed ("INCORRECT OBJECT'FIRST");
   end if;

   if V'First /= 5 then
      Failed ("INCORRECT VALUE'FIRST");
   end if;

   if T'First (N) /= 5 then
      Failed ("INCORRECT TYPE'FIRST (N)");
   end if;

   if X'First (N) /= 5 then
      Failed ("INCORRECT OBJECT'FIRST (N)");
   end if;

   if V'First (N) /= 5 then
      Failed ("INCORRECT VALUE'FIRST (N)");
   end if;

   if T'Last /= 7 then
      Failed ("INCORRECT TYPE'LAST");
   end if;

   if X'Last /= 7 then
      Failed ("INCORRECT OBJECT'LAST");
   end if;

   if V'Last /= 7 then
      Failed ("INCORRECT VALUE'LAST");
   end if;

   if T'Last (N) /= 7 then
      Failed ("INCORRECT TYPE'LAST (N)");
   end if;

   if X'Last (N) /= 7 then
      Failed ("INCORRECT OBJECT'LAST (N)");
   end if;

   if V'Last (N) /= 7 then
      Failed ("INCORRECT VALUE'LAST (N)");
   end if;

   if T'Length /= 3 then
      Failed ("INCORRECT TYPE'LENGTH");
   end if;

   if X'Length /= 3 then
      Failed ("INCORRECT OBJECT'LENGTH");
   end if;

   if V'Length /= 3 then
      Failed ("INCORRECT VALUE'LENGTH");
   end if;

   if T'Length (N) /= 3 then
      Failed ("INCORRECT TYPE'LENGTH (N)");
   end if;

   if X'Length (N) /= 3 then
      Failed ("INCORRECT OBJECT'LENGTH (N)");
   end if;

   if V'Length (N) /= 3 then
      Failed ("INCORRECT VALUE'LENGTH (N)");
   end if;

   declare
      Y : Parent (T'Range);
   begin
      if Y'First /= 5 or Y'Last /= 7 then
         Failed ("INCORRECT TYPE'RANGE");
      end if;
   end;

   declare
      Y : Parent (X'Range);
   begin
      if Y'First /= 5 or Y'Last /= 7 then
         Failed ("INCORRECT OBJECT'RANGE");
      end if;
   end;

   declare
      Y : Parent (V'Range);
   begin
      if Y'First /= 5 or Y'Last /= 7 then
         Failed ("INCORRECT VALUE'RANGE");
      end if;
   end;

   declare
      Y : Parent (T'Range (N));
   begin
      if Y'First /= 5 or Y'Last /= 7 then
         Failed ("INCORRECT TYPE'RANGE (N)");
      end if;
   end;

   declare
      Y : Parent (X'Range (N));
   begin
      if Y'First /= 5 or Y'Last /= 7 then
         Failed ("INCORRECT OBJECT'RANGE (N)");
      end if;
   end;

   declare
      Y : Parent (V'Range (N));
   begin
      if Y'First /= 5 or Y'Last /= 7 then
         Failed ("INCORRECT VALUE'RANGE (N)");
      end if;
   end;

   if X'Size < T'Size then
      Comment ("X'SIZE < T'SIZE");
   elsif X'Size = T'Size then
      Comment ("X'SIZE = T'SIZE");
   else
      Comment ("X'SIZE > T'SIZE");
   end if;

   Result;
end C34005p;
