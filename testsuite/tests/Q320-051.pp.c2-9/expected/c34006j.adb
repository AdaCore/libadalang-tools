-- C34006J.ADA

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
--     (IMPLICITLY) FOR DERIVED RECORD TYPES WITH DISCRIMINANTS AND WITH
--     A LIMITED COMPONENT TYPE.

-- HISTORY:
--     JRK 08/25/87  CREATED ORIGINAL TEST.
--     VCL 06/28/88  MODIFIED THE STATEMENTS INVOLVING THE 'SIZE
--                   ATTRIBUTE TO REMOVE ANY ASSUMPTIONS ABOUT THE
--                   SIZES.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.
--     RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURNS.

with System; use System;
with Report; use Report;

procedure C34006j is

   package Pkg_L is

      type Lp is limited private;

      function Create (X : Integer) return Lp;

      function Equal (X, Y : Lp) return Boolean;

      procedure Assign (X : out Lp; Y : Lp);

      C4 : constant Lp;
      C5 : constant Lp;

   private

      type Lp is new Integer;

      C4 : constant Lp := 4;
      C5 : constant Lp := 5;

   end Pkg_L;

   use Pkg_L;

   subtype Component is Lp;

   package Pkg_P is

      Max_Len : constant := 10;

      subtype Length is Natural range 0 .. Max_Len;

      type Parent (B : Boolean := True; L : Length := 3) is record
         I : Integer := 2;
         case B is
            when True =>
               S : String (1 .. L) := (1 .. L => 'A');
               C : Component;
            when False =>
               F : Float := 5.0;
         end case;
      end record;

      function Create
        (B : Boolean; L : Length; I : Integer; S : String; C : Component;
         F : Float; X : Parent  -- TO RESOLVE OVERLOADING.
         ) return Parent;

      function Equal (X, Y : Parent) return Boolean;

      function Aggr
        (B : Boolean; L : Length; I : Integer; S : String; C : Component)
         return Parent;

      function Aggr
        (B : Boolean; L : Length; I : Integer; F : Float) return Parent;

   end Pkg_P;

   use Pkg_P;

   type T is new Parent (Ident_Bool (True), Ident_Int (3));

   X : T;
   W : Parent;
   B : Boolean := False;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   package body Pkg_L is

      function Create (X : Integer) return Lp is
      begin
         return Lp (Ident_Int (X));
      end Create;

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
        (B : Boolean; L : Length; I : Integer; S : String; C : Component;
         F : Float; X : Parent) return Parent
      is
      begin
         return A : Parent (B, L) do
            A.I := I;
            case B is
               when True =>
                  A.S := S;
                  Assign (A.C, C);
               when False =>
                  A.F := F;
            end case;
         end return;
      end Create;

      function Equal (X, Y : Parent) return Boolean is
      begin
         if X.B /= Y.B or X.L /= Y.L or X.I /= Y.I then
            return False;
         end if;
         case X.B is
            when True =>
               return X.S = Y.S and Equal (X.C, Y.C);
            when False =>
               return X.F = Y.F;
         end case;
      end Equal;

      function Aggr
        (B : Boolean; L : Length; I : Integer; S : String; C : Component)
         return Parent
      is
      begin
         return Result : Parent (B, L) do
            Result.I := I;
            Result.S := S;
            Assign (Result.C, C);
         end return;
      end Aggr;

      function Aggr
        (B : Boolean; L : Length; I : Integer; F : Float) return Parent
      is
      begin
         return Result : Parent (B, L) do
            Result.I := I;
            Result.F := F;
         end return;
      end Aggr;

   end Pkg_P;

begin
   Test
     ("C34006J",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "RECORD TYPES WITH DISCRIMINANTS AND WITH A " &
      "LIMITED COMPONENT TYPE");

   X.I := Ident_Int (1);
   X.S := Ident_Str ("ABC");
   Assign (X.C, Create (4));

   W.I := Ident_Int (1);
   W.S := Ident_Str ("ABC");
   Assign (W.C, Create (4));

   if not Equal (T'(X), Aggr (True, 3, 1, "ABC", C4)) then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if not Equal (T (X), Aggr (True, 3, 1, "ABC", C4)) then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if not Equal (T (W), Aggr (True, 3, 1, "ABC", C4)) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if not Equal (Parent (X), Aggr (True, 3, 1, "ABC", C4)) or
     not Equal
       (Parent (Create (False, 2, 3, "XX", C5, 6.0, X)),
        Aggr (False, 2, 3, 6.0))
   then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

   if X.B /= True or X.L /= 3 or
     Create (False, 2, 3, "XX", C5, 6.0, X).B /= False or
     Create (False, 2, 3, "XX", C5, 6.0, X).L /= 2 then
      Failed ("INCORRECT SELECTION (DISCRIMINANT)");
   end if;

   if X.I /= 1 or X.S /= "ABC" or not Equal (X.C, C4) or
     Create (False, 2, 3, "XX", C5, 6.0, X).I /= 3 or
     Create (False, 2, 3, "XX", C5, 6.0, X).F /= 6.0 then
      Failed ("INCORRECT SELECTION (VALUE)");
   end if;

   X.I := Ident_Int (7);
   X.S := Ident_Str ("XYZ");
   if not Equal (X, Aggr (True, 3, 7, "XYZ", C4)) then
      Failed ("INCORRECT SELECTION (ASSIGNMENT)");
   end if;

   X.I := Ident_Int (1);
   X.S := Ident_Str ("ABC");
   if not (X in T) or Aggr (False, 2, 3, 6.0) in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (Aggr (False, 2, 3, 6.0) not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT 'ADDRESS");
   end if;

   if not X'Constrained then
      Failed ("INCORRECT 'CONSTRAINED");
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

   if X'Size < T'Size then
      Comment ("X'SIZE < T'SIZE");
   elsif X'Size = T'Size then
      Comment ("X'SIZE = T'SIZE");
   else
      Comment ("X'SIZE > T'SIZE");
   end if;

   Result;
exception
   when others =>
      Failed
        ("UNEXPECTED EXCEPTION RAISED WHILE CHECKING BASIC " & "OPERATIONS");
      Result;
end C34006j;
