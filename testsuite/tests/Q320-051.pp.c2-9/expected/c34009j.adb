-- C34009J.ADA

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
--     (IMPLICITLY) FOR DERIVED LIMITED PRIVATE TYPES WITH
--     DISCRIMINANTS.

-- HISTORY:
--     JRK 09/01/87  CREATED ORIGINAL TEST.
--     WMC 03/13/92  REVISED TYPE'SIZE CHECKS.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System; use System;
with Report; use Report;

procedure C34009j is

   package Pkg is

      Max_Len : constant := 10;

      subtype Length is Natural range 0 .. Max_Len;

      type Parent (B : Boolean := True; L : Length := 3) is limited private;

      function Create
        (B : Boolean; L : Length; I : Integer; S : String; J : Integer;
         F : Float; X : Parent  -- TO RESOLVE OVERLOADING.
         ) return Parent;

      function Con
        (B : Boolean; L : Length; I : Integer; S : String; J : Integer)
         return Parent;

      function Con
        (B : Boolean; L : Length; I : Integer; F : Float) return Parent;

      function Equal (X, Y : Parent) return Boolean;

      procedure Assign (X : out Parent; Y : Parent);

   private

      type Parent (B : Boolean := True; L : Length := 3) is record
         I : Integer := 2;
         case B is
            when True =>
               S : String (1 .. L) := (1 .. L => 'A');
               J : Integer         := 2;
            when False =>
               F : Float := 5.0;
         end case;
      end record;

   end Pkg;

   use Pkg;

   type T is new Parent (Ident_Bool (True), Ident_Int (3));

   X : T;
   W : Parent;
   B : Boolean := False;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   package body Pkg is

      function Create
        (B : Boolean; L : Length; I : Integer; S : String; J : Integer;
         F : Float; X : Parent) return Parent
      is
      begin
         case B is
            when True =>
               return (True, L, I, S, J);
            when False =>
               return (False, L, I, F);
         end case;
      end Create;

      function Con
        (B : Boolean; L : Length; I : Integer; S : String; J : Integer)
         return Parent
      is
      begin
         return (True, L, I, S, J);
      end Con;

      function Con
        (B : Boolean; L : Length; I : Integer; F : Float) return Parent
      is
      begin
         return (False, L, I, F);
      end Con;

      function Equal (X, Y : Parent) return Boolean is
      begin
         return X = Y;
      end Equal;

      procedure Assign (X : out Parent; Y : Parent) is
      begin
         X := Y;
      end Assign;

   end Pkg;

begin
   Test
     ("C34009J",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "LIMITED PRIVATE TYPES WITH DISCRIMINANTS");

   if Equal (3, 3) then
      Assign (X, Con (True, 3, 1, "ABC", 4));
   end if;
   if not Equal (T'(X), Con (True, 3, 1, "ABC", 4)) then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if not Equal (T (X), Con (True, 3, 1, "ABC", 4)) then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      Assign (W, Con (True, 3, 1, "ABC", 4));
   end if;
   if not Equal (T (W), Con (True, 3, 1, "ABC", 4)) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if not Equal (Parent (X), Con (True, 3, 1, "ABC", 4)) or
     not Equal
       (Parent (Create (False, 2, 3, "XX", 5, 6.0, X)), Con (False, 2, 3, 6.0))
   then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

   if X.B /= True or X.L /= 3 or
     Create (False, 2, 3, "XX", 5, 6.0, X).B /= False or
     Create (False, 2, 3, "XX", 5, 6.0, X).L /= 2 then
      Failed ("INCORRECT SELECTION (DISCRIMINANT)");
   end if;

   if not (X in T) or Con (False, 2, 3, 6.0) in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (Con (False, 2, 3, 6.0) not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT 'ADDRESS");
   end if;

   if not X'Constrained then
      Failed ("INCORRECT OBJECT'CONSTRAINED");
   end if;

   if T'Size <= 0 then
      Failed ("INCORRECT TYPE'SIZE");
   end if;

   if X'Size < T'Size or X.B'Size < Boolean'Size or X.L'Size < Length'Size then
      Failed ("INCORRECT OBJECT'SIZE");
   end if;

   Result;
end C34009j;
