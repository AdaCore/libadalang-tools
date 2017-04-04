-- C34005G.ADA

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
--    CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
--    (IMPLICITLY) FOR DERIVED ONE-DIMENSIONAL ARRAY TYPES
--    WHOSE COMPONENT TYPE IS A CHARACTER TYPE.

-- HISTORY:
--    JRK 9/15/86  CREATED ORIGINAL TEST.
--    RJW 8/21/89  MODIFIED CHECKS FOR OBJECT AND TYPE SIZES.
--    PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System; use System;
with Report; use Report;

procedure C34005g is

   type Component is new Character;

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

   X : T               := (others => 'B');
   W : Parent (5 .. 7) := (others => 'B');
   C : Component       := 'A';
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
            B     := Component'Succ (B);
         end loop;
         return A;
      end Create;

   end Pkg;

   function Ident (X : T) return T is
   begin
      if Equal (X'Length, X'Length) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return (others => '-');
   end Ident;

begin
   Test
     ("C34005G",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A CHARACTER TYPE");

   X := Ident ("ABC");
   if X /= "ABC" then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= "ABC" then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= "ABC" then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := "ABC";
   end if;
   if T (W) /= "ABC" then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   begin
      if Parent (X) /= "ABC" or Parent (Create (2, 3, 'D', X)) /= "DE" then
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 1");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 1");
   end;

   if Equal (3, 3) then
      U := "ABC";
   end if;
   if T (U) /= "ABC" then
      Failed ("INCORRECT CONVERSION FROM ARRAY");
   end if;

   begin
      if Arr (X) /= "ABC" or Arrt (Create (1, 2, 'C', X)) /= "CD" then
         Failed ("INCORRECT CONVERSION TO ARRAY");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 2");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 2");
   end;

   if Ident ("ABC") /= ('A', 'B', 'C') or X = "AB" then
      Failed ("INCORRECT STRING LITERAL");
   end if;

   if Ident (('A', 'B', 'C')) /= "ABC" or X = ('A', 'B') then
      Failed ("INCORRECT AGGREGATE");
   end if;

   begin
      if X (Ident_Int (5)) /= 'A' or Create (2, 3, 'D', X) (3) /= 'E' then
         Failed ("INCORRECT INDEX (VALUE)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 3");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 3");
   end;

   X (Ident_Int (7)) := 'D';
   if X /= "ABD" then
      Failed ("INCORRECT INDEX (ASSIGNMENT)");
   end if;

   begin
      X := Ident ("ABC");
      if X (Ident_Int (6) .. Ident_Int (7)) /= "BC" or
        Create (1, 4, 'D', X) (1 .. 3) /= "DEF"
      then
         Failed ("INCORRECT SLICE (VALUE)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 4");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 4");
   end;

   X (Ident_Int (5) .. Ident_Int (6)) := "DE";
   if X /= "DEC" then
      Failed ("INCORRECT SLICE (ASSIGNMENT)");
   end if;

   X := Ident ("ABC");
   if X = Ident ("ABD") or X = "AB" then
      Failed ("INCORRECT =");
   end if;

   if X /= Ident ("ABC") or not (X /= "BC") then
      Failed ("INCORRECT /=");
   end if;

   if X < Ident ("ABC") or X < "AB" then
      Failed ("INCORRECT <");
   end if;

   if X > Ident ("ABC") or X > "AC" then
      Failed ("INCORRECT >");
   end if;

   if X <= Ident ("ABB") or X <= "ABBD" then
      Failed ("INCORRECT <=");
   end if;

   if X >= Ident ("ABD") or X >= "ABCA" then
      Failed ("INCORRECT >=");
   end if;

   if not (X in T) or "AB" in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not ("AB" not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   begin
      if X & "DEF" /= "ABCDEF" or Create (2, 3, 'B', X) & "DE" /= "BCDE" then
         Failed ("INCORRECT & (ARRAY, ARRAY)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 5");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 5");
   end;

   begin
      if X & 'D' /= "ABCD" or Create (2, 3, 'B', X) & 'D' /= "BCD" then
         Failed ("INCORRECT & (ARRAY, COMPONENT)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 6");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 6");
   end;

   begin
      if 'D' & X /= "DABC" or 'B' & Create (2, 3, 'C', X) /= "BCD" then
         Failed ("INCORRECT & (COMPONENT, ARRAY)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 7");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 7");
   end;

   if Equal (3, 3) then
      C := 'B';
   end if;

   begin
      if C & 'C' /= Create (2, 3, 'B', X) then
         Failed ("INCORRECT & (COMPONENT, COMPONENT)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 8");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 8");
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
end C34005g;
