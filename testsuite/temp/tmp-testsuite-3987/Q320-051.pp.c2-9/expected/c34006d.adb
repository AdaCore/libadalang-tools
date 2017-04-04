-- C34006D.ADA

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
--     NON-LIMITED COMPONENT TYPES.

-- HISTORY:
--     JRK 09/22/86  CREATED ORIGINAL TEST.
--     BCB 11/13/87  CHANGED TEST SO AN OBJECT'S SIZE MAY BE LESS THAN
--                   THAT OF ITS TYPE.
--     RJW 08/21/89  MODIFIED CHECKS FOR SIZE.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System; use System;
with Report; use Report;

procedure C34006d is

   subtype Component is Integer;

   package Pkg is

      Max_Len : constant := 10;

      subtype Length is Natural range 0 .. Max_Len;

      type Parent (B : Boolean := True; L : Length := 1) is record
         I : Integer;
         case B is
            when True =>
               S : String (1 .. L);
               C : Component;
            when False =>
               F : Float := 5.0;
         end case;
      end record;

      function Create
        (B : Boolean;
         L : Length;
         I : Integer;
         S : String;
         C : Component;
         F : Float;
         X : Parent  -- TO RESOLVE OVERLOADING.
         ) return Parent;

   end Pkg;

   use Pkg;

   type T is new Parent (Ident_Bool (True), Ident_Int (3));

   X : T         := (True, 3, 2, "AAA", 2);
   W : Parent    := (True, 3, 2, "AAA", 2);
   C : Component := 1;
   B : Boolean   := False;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   package body Pkg is

      function Create
        (B : Boolean;
         L : Length;
         I : Integer;
         S : String;
         C : Component;
         F : Float;
         X : Parent) return Parent
      is
      begin
         case B is
            when True =>
               return (True, L, I, S, C);
            when False =>
               return (False, L, I, F);
         end case;
      end Create;

   end Pkg;

   function Ident (X : T) return T is
   begin
      if Equal (X.I, X.I) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return (True, 3, -1, "---", -1);
   end Ident;

begin
   Test
     ("C34006D",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "RECORD TYPES WITH DISCRIMINANTS AND WITH " &
      "NON-LIMITED COMPONENT TYPES");

   X := Ident ((True, 3, 1, "ABC", 4));
   if X /= (True, 3, 1, "ABC", 4) then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= (True, 3, 1, "ABC", 4) then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= (True, 3, 1, "ABC", 4) then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := (True, 3, 1, "ABC", 4);
   end if;
   if T (W) /= (True, 3, 1, "ABC", 4) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   begin
      if Parent (X) /= (True, 3, 1, "ABC", 4) or
        Parent (Create (False, 2, 3, "XX", 5, 6.0, X)) /= (False, 2, 3, 6.0)
      then
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 1");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 1");
   end;

   if Ident ((True, 3, 1, "ABC", 4)) /= (True, 3, 1, "ABC", 4) or
     X = (False, 3, 1, 4.0)
   then
      Failed ("INCORRECT AGGREGATE");
   end if;

   begin
      if X.B /= True or
        X.L /= 3 or
        Create (False, 2, 3, "XX", 5, 6.0, X).B /= False or
        Create (False, 2, 3, "XX", 5, 6.0, X).L /= 2
      then
         Failed ("INCORRECT SELECTION (DISCRIMINANT)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 2");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 2");
   end;

   begin
      if X.I /= 1 or
        X.S /= "ABC" or
        X.C /= 4 or
        Create (False, 2, 3, "XX", 5, 6.0, X).I /= 3 or
        Create (False, 2, 3, "XX", 5, 6.0, X).F /= 6.0
      then
         Failed ("INCORRECT SELECTION (VALUE)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 3");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 3");
   end;

   X.I := Ident_Int (7);
   X.S := Ident_Str ("XYZ");
   X.C := Ident_Int (9);
   if X /= (True, 3, 7, "XYZ", 9) then
      Failed ("INCORRECT SELECTION (ASSIGNMENT)");
   end if;

   X := Ident ((True, 3, 1, "ABC", 4));
   if X = Ident ((True, 3, 1, "ABC", 5)) or X = (False, 2, 3, 6.0) then
      Failed ("INCORRECT =");
   end if;

   if X /= Ident ((True, 3, 1, "ABC", 4)) or not (X /= (False, 2, 3, 6.0)) then
      Failed ("INCORRECT /=");
   end if;

   if not (X in T) or (False, 2, 3, 6.0) in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not ((False, 2, 3, 6.0) not in T) then
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

   Result;
end C34006d;
