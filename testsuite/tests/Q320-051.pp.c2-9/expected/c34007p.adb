-- C34007P.ADA

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
--     (IMPLICITLY) FOR DERIVED ACCESS TYPES WHOSE DESIGNATED TYPE IS A
--     RECORD TYPE WITH DISCRIMINANTS.

-- HISTORY:
--     JRK 09/29/86  CREATED ORIGINAL TEST.
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  REVISED TEST SO
--                   T'STORAGE_SIZE IS NOT REQUIRED TO BE > 1.
--     BCB 09/26/88  REMOVED COMPARISON INVOLVING OBJECT SIZE.
--     BCB 03/07/90  PUT CHECK FOR 'STORAGE_SIZE IN EXCEPTION HANDLER.
--     THS 09/18/90  REMOVED DECLARATION OF B, MADE THE BODY OF
--                   PROCEDURE A NULL, AND DELETED ALL REFERENCES TO B.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System; use System;
with Report; use Report;

procedure C34007p is

   subtype Component is Integer;

   subtype Length is Natural range 0 .. 10;

   type Designated (B : Boolean := True; L : Length := 1) is record
      I : Integer;
      case B is
         when True =>
            S : String (1 .. L);
            C : Component;
         when False =>
            F : Float := 5.0;
      end case;
   end record;

   subtype Subdesignated is Designated (Ident_Bool (True), Ident_Int (3));

   package Pkg is

      type Parent is access Designated;

      function Create
        (B : Boolean; L : Length; I : Integer; S : String; C : Component;
         F : Float; X : Parent  -- TO RESOLVE OVERLOADING.
         ) return Parent;

   end Pkg;

   use Pkg;

   type T is new Parent (Ident_Bool (True), Ident_Int (3));

   X : T         := new Designated'(True, 3, 2, "AAA", 2);
   K : Integer   := X'Size;
   Y : T         := new Designated'(True, 3, 1, "ABC", 4);
   W : Parent    := new Designated'(True, 3, 2, "AAA", 2);
   C : Component := 1;

   procedure A (X : Address) is
   begin
      null;
   end A;

   package body Pkg is

      function Create
        (B : Boolean; L : Length; I : Integer; S : String; C : Component;
         F : Float; X : Parent) return Parent
      is
      begin
         case B is
            when True =>
               return new Designated'(True, L, I, S, C);
            when False =>
               return new Designated'(False, L, I, F);
         end case;
      end Create;

   end Pkg;

   function Ident (X : T) return T is
   begin
      if X = null or else Equal (X.I, X.I) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return new Designated'(True, 3, -1, "---", -1);
   end Ident;

begin
   Test
     ("C34007P",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
      "RECORD TYPE WITH DISCRIMINANTS");

   if Y = null or else Y.all /= (True, 3, 1, "ABC", 4) then
      Failed ("INCORRECT INITIALIZATION");
   end if;

   X := Ident (Y);
   if X /= Y then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= Y then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= Y then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := new Designated'(True, 3, 1, "ABC", 4);
   end if;
   X := T (W);
   if X = null or else X = Y or else X.all /= (True, 3, 1, "ABC", 4) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   X := Ident (Y);
   W := Parent (X);
   if W = null or else W.all /= (True, 3, 1, "ABC", 4) or else T (W) /= Y then
      Failed ("INCORRECT CONVERSION TO PARENT - 1");
   end if;

   W := Parent (Create (False, 2, 3, "XX", 5, 6.0, X));
   if W = null or else W.all /= (False, 2, 3, 6.0) then
      Failed ("INCORRECT CONVERSION TO PARENT - 2");
   end if;

   if Ident (null) /= null or X = null then
      Failed ("INCORRECT NULL");
   end if;

   X := Ident (new Designated'(True, 3, 1, "ABC", 4));
   if (X = null or else X = Y or else X.all /= (True, 3, 1, "ABC", 4)) or
     X = new Designated'(False, 3, 1, 4.0) then
      Failed ("INCORRECT ALLOCATOR");
   end if;

   X := Ident (Y);
   if X.B /= True or X.L /= 3 or
     Create (False, 2, 3, "XX", 5, 6.0, X).B /= False or
     Create (False, 2, 3, "XX", 5, 6.0, X).L /= 2 then
      Failed ("INCORRECT SELECTION (DISCRIMINANT)");
   end if;

   if X.I /= 1 or X.S /= "ABC" or X.C /= 4 or
     Create (False, 2, 3, "XX", 5, 6.0, X).I /= 3 or
     Create (False, 2, 3, "XX", 5, 6.0, X).F /= 6.0 then
      Failed ("INCORRECT SELECTION (VALUE)");
   end if;

   X.I := Ident_Int (7);
   X.S := Ident_Str ("XYZ");
   X.C := Ident_Int (9);
   if X /= Y or Y.all /= (True, 3, 7, "XYZ", 9) then
      Failed ("INCORRECT SELECTION (ASSIGNMENT)");
   end if;

   Y.all := (True, 3, 1, "ABC", 4);
   X     := Ident (Y);
   begin
      Create (False, 2, 3, "XX", 5, 6.0, X).I := 10;
      Create (False, 2, 3, "XX", 5, 6.0, X).F := 10.0;
   exception
      when others =>
         Failed ("EXCEPTION FOR SELECTION (ASSIGNMENT)");
   end;

   if X.all /= (True, 3, 1, "ABC", 4) or
     Create (False, 2, 3, "XX", 5, 6.0, X).all /= (False, 2, 3, 6.0) then
      Failed ("INCORRECT .ALL (VALUE)");
   end if;

   X.all := (True, 3, 10, "ZZZ", 15);
   if X /= Y or Y.all /= (True, 3, 10, "ZZZ", 15) then
      Failed ("INCORRECT .ALL (ASSIGNMENT)");
   end if;

   Y.all := (True, 3, 1, "ABC", 4);
   begin
      Create (False, 2, 3, "XX", 5, 6.0, X).all := (False, 2, 10, 15.0);
   exception
      when others =>
         Failed ("EXCEPTION FOR .ALL (ASSIGNMENT)");
   end;

   X := Ident (null);
   begin
      if X.all = (False, 0, 0, 0.0) then
         Failed ("NO EXCEPTION FOR NULL.ALL - 1");
      else
         Failed ("NO EXCEPTION FOR NULL.ALL - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION FOR NULL.ALL");
   end;

   X := Ident (Y);
   if X = null or X = new Subdesignated or not (X = Y) or
     X = Create (False, 2, 3, "XX", 5, 6.0, X) then
      Failed ("INCORRECT =");
   end if;

   if X /= Y or not (X /= null) or
     not (X /= Create (False, 2, 3, "XX", 5, 6.0, X)) then
      Failed ("INCORRECT /=");
   end if;

   if not (X in T) or Create (False, 2, 3, "XX", 5, 6.0, X) in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (Create (False, 2, 3, "XX", 5, 6.0, X) not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   A (X'Address);

   if T'Size < 1 then
      Failed ("INCORRECT TYPE'SIZE");
   end if;

   begin
      if T'Storage_Size /= Parent'Storage_Size then
         Failed
           ("COLLECTION SIZE OF DERIVED TYPE IS NOT " &
            "EQUAL TO COLLECTION SIZE OF PARENT TYPE");
      end if;
   exception
      when Program_Error =>
         Comment
           ("PROGRAM_ERROR RAISED FOR " & "UNDEFINED STORAGE_SIZE (AI-00608)");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED");
   end;

   Result;
end C34007p;