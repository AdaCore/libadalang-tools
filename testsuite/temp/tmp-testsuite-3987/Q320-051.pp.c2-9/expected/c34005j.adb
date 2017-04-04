-- C34005J.ADA

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
--     (IMPLICITLY) FOR DERIVED ONE-DIMENSIONAL ARRAY TYPES
--     WHOSE COMPONENT TYPE IS A BOOLEAN TYPE.

-- HISTORY:
--     JRK 9/16/86  CREATED ORIGINAL TEST.
--     RJW 8/21/89  MODIFIED CHECKS FOR TYPE AND OBJECT SIZES.
--     PWN 11/30/94 REMOVED 'BASE USE ILLEGAL IN ADA 9X.

with System; use System;
with Report; use Report;

procedure C34005j is

   subtype Component is Boolean;

   package Pkg is

      First : constant := 0;
      Last  : constant := 100;

      subtype Index is Integer range First .. Last;

      type Parent is array (Index range <>) of Component;

      function Create
        (F, L  : Index;
         C     : Component;
         Dummy : Parent   -- TO RESOLVE OVERLOADING.
         ) return Parent;

   end Pkg;

   use Pkg;

   type T is new Parent (Ident_Int (5) .. Ident_Int (7));

   type Arrt is array (Integer range <>) of Component;
   subtype Arr is Arrt (2 .. 4);

   X : T               := (others => True);
   W : Parent (5 .. 7) := (others => True);
   C : Component       := False;
   B : Boolean         := False;
   U : Arr             := (others => C);
   N : constant        := 1;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   function V return T is
   begin
      return (others => C);
   end V;

   package body Pkg is

      function Create
        (F, L  : Index;
         C     : Component;
         Dummy : Parent) return Parent
      is
         A : Parent (F .. L);
         B : Component := C;
      begin
         for I in F .. L loop
            A (I) := B;
            B     := not B;
         end loop;
         return A;
      end Create;

   end Pkg;

   function Ident (X : T) return T is
   begin
      if Equal (X'Length, X'Length) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return (others => False);
   end Ident;

begin
   Test
     ("C34005J",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A BOOLEAN TYPE");

   X := Ident ((True, False, True));
   if X /= (True, False, True) then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= (True, False, True) then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= (True, False, True) then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := (True, False, True);
   end if;
   if T (W) /= (True, False, True) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   begin
      if Parent (X) /= (True, False, True) or
        Parent (Create (2, 3, False, X)) /= (False, True)
      then
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 1");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 1");
   end;

   if Equal (3, 3) then
      U := (True, False, True);
   end if;
   if T (U) /= (True, False, True) then
      Failed ("INCORRECT CONVERSION FROM ARRAY");
   end if;

   begin
      if Arr (X) /= (True, False, True) or
        Arrt (Create (1, 2, True, X)) /= (True, False)
      then
         Failed ("INCORRECT CONVERSION TO ARRAY");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 2");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 2");
   end;

   if Ident ((True, False, True)) /= (True, False, True) or
     X = (True, False)
   then
      Failed ("INCORRECT AGGREGATE");
   end if;

   begin
      if X (Ident_Int (5)) /= True or Create (2, 3, False, X) (3) /= True then
         Failed ("INCORRECT INDEX (VALUE)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 3");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 3");
   end;

   X (Ident_Int (7)) := False;
   if X /= (True, False, False) then
      Failed ("INCORRECT INDEX (ASSIGNMENT)");
   end if;

   begin
      X := Ident ((True, False, True));
      if X (Ident_Int (6) .. Ident_Int (7)) /= (False, True) or
        Create (1, 4, False, X) (1 .. 3) /= (False, True, False)
      then
         Failed ("INCORRECT SLICE (VALUE)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 4");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 4");
   end;

   X (Ident_Int (5) .. Ident_Int (6)) := (False, True);
   if X /= (False, True, True) then
      Failed ("INCORRECT SLICE (ASSIGNMENT)");
   end if;

   begin
      X := Ident ((True, False, True));
      if not X /= (False, True, False) or
        not Create (2, 3, False, X) /= (True, False)
      then
         Failed ("INCORRECT ""NOT""");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 5");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 5");
   end;

   begin
      if (X and Ident ((True, True, False))) /= (True, False, False) or
        (Create (1, 4, False, X) and (False, False, True, True)) /=
          (False, False, False, True)
      then
         Failed ("INCORRECT ""AND""");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 6");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 6");
   end;

   begin
      if (X or Ident ((True, False, False))) /= (True, False, True) or
        (Create (1, 4, False, X) or (False, False, True, True)) /=
          (False, True, True, True)
      then
         Failed ("INCORRECT ""OR""");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 7");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 7");
   end;

   begin
      if (X xor Ident ((True, True, False))) /= (False, True, True) or
        (Create (1, 4, False, X) xor (False, False, True, True)) /=
          (False, True, True, False)
      then
         Failed ("INCORRECT ""XOR""");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 8");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 8");
   end;

   if X = Ident ((True, False, False)) or X = (True, False) then
      Failed ("INCORRECT =");
   end if;

   if X /= Ident ((True, False, True)) or not (X /= (False, True)) then
      Failed ("INCORRECT /=");
   end if;

   if X < Ident ((True, False, True)) or X < (True, False) then
      Failed ("INCORRECT <");
   end if;

   if X > Ident ((True, False, True)) or X > (True, True) then
      Failed ("INCORRECT >");
   end if;

   if X <= Ident ((True, False, False)) or X <= (True, False, False, True) then
      Failed ("INCORRECT <=");
   end if;

   if X >= Ident ((True, True, False)) or X >= (True, False, True, False) then
      Failed ("INCORRECT >=");
   end if;

   if not (X in T) or (True, False) in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not ((True, False) not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   begin
      if X & (False, True, False) /= (True, False, True, False, True, False) or
        Create (2, 3, False, X) & (False, True) /= (False, True, False, True)
      then
         Failed ("INCORRECT & (ARRAY, ARRAY)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 9");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 9");
   end;

   begin
      if X & False /= (True, False, True, False) or
        Create (2, 3, False, X) & False /= (False, True, False)
      then
         Failed ("INCORRECT & (ARRAY, COMPONENT)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 10");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 10");
   end;

   begin
      if False & X /= (False, True, False, True) or
        False & Create (2, 3, True, X) /= (False, True, False)
      then
         Failed ("INCORRECT & (COMPONENT, ARRAY)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 11");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 11");
   end;

   if Equal (3, 3) then
      C := False;
   end if;

   begin
      if C & True /= Create (2, 3, False, X) then
         Failed ("INCORRECT & (COMPONENT, COMPONENT)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 12");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 12");
   end;

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

   Result;
end C34005j;
