-- C34005M.ADA

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
--     (IMPLICITLY) FOR DERIVED MULTI-DIMENSIONAL ARRAY TYPES WHOSE
--     COMPONENT TYPE IS A NON-LIMITED TYPE.

-- HISTORY:
--     JRK 9/17/86  CREATED ORIGINAL TEST.
--     PWN 11/30/94 REMOVED 'BASE USE ILLEGAL IN ADA 9X.

with System; use System;
with Report; use Report;

procedure C34005m is

   subtype Component is Integer;

   package Pkg is

      First : constant := 0;
      Last  : constant := 10;

      subtype Index is Integer range First .. Last;

      type Parent is array (Index range <>, Index range <>) of Component;

      function Create
        (F1, L1 : Index; F2, L2 : Index; C : Component;
         Dummy  : Parent   -- TO RESOLVE OVERLOADING.
        )
         return Parent;

   end Pkg;

   use Pkg;

   type T is
     new Parent (Ident_Int (4) .. Ident_Int (5),
        Ident_Int (6) .. Ident_Int (8));

   type Arrt is array (Integer range <>, Integer range <>) of Component;

   subtype Arr is Arrt (8 .. 9, 2 .. 4);

   X : T                       := (others => (others => 2));
   W : Parent (4 .. 5, 6 .. 8) := (others => (others => 2));
   C : Component               := 1;
   B : Boolean                 := False;
   U : Arr                     := (others => (others => C));
   N : constant                := 2;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   function V return T is
   begin
      return (others => (others => C));
   end V;

   package body Pkg is

      function Create
        (F1, L1 : Index; F2, L2 : Index; C : Component; Dummy : Parent)
         return Parent
      is
         A : Parent (F1 .. L1, F2 .. L2);
         B : Component := C;
      begin
         for I in F1 .. L1 loop
            for J in F2 .. L2 loop
               A (I, J) := B;
               B        := B + 1;
            end loop;
         end loop;
         return A;
      end Create;

   end Pkg;

   function Ident (X : T) return T is
   begin
      if Equal (X'Length, X'Length) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return (others => (others => -1));
   end Ident;

begin
   Test
     ("C34005M",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A NON-LIMITED TYPE");

   X := Ident (((1, 2, 3), (4, 5, 6)));
   if X /= ((1, 2, 3), (4, 5, 6)) then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= ((1, 2, 3), (4, 5, 6)) then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= ((1, 2, 3), (4, 5, 6)) then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := ((1, 2, 3), (4, 5, 6));
   end if;
   if T (W) /= ((1, 2, 3), (4, 5, 6)) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   begin
      if Parent (X) /= ((1, 2, 3), (4, 5, 6)) or
        Parent (Create (6, 9, 2, 3, 4, X)) /=
          ((4, 5), (6, 7), (8, 9), (10, 11))
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
      U := ((1, 2, 3), (4, 5, 6));
   end if;
   if T (U) /= ((1, 2, 3), (4, 5, 6)) then
      Failed ("INCORRECT CONVERSION FROM ARRAY");
   end if;

   begin
      if Arr (X) /= ((1, 2, 3), (4, 5, 6)) or
        Arrt (Create (7, 9, 2, 5, 3, X)) /=
          ((3, 4, 5, 6), (7, 8, 9, 10), (11, 12, 13, 14))
      then
         Failed ("INCORRECT CONVERSION TO ARRAY");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 2");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 2");
   end;

   if Ident (((1, 2, 3), (4, 5, 6))) /= ((1, 2, 3), (4, 5, 6)) or
     X = ((1, 2), (3, 4), (5, 6)) then
      Failed ("INCORRECT AGGREGATE");
   end if;

   begin
      if X (Ident_Int (4), Ident_Int (6)) /= 1 or
        Create (6, 9, 2, 3, 4, X) (9, 3) /= 11 then
         Failed ("INCORRECT INDEX (VALUE)");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 3");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 3");
   end;

   X (Ident_Int (5), Ident_Int (8)) := 7;
   if X /= ((1, 2, 3), (4, 5, 7)) then
      Failed ("INCORRECT INDEX (ASSIGNMENT)");
   end if;

   X := Ident (((1, 2, 3), (4, 5, 6)));
   if X = Ident (((1, 2, 3), (4, 5, 7))) or X = ((1, 2), (4, 5)) then
      Failed ("INCORRECT =");
   end if;

   if X /= Ident (((1, 2, 3), (4, 5, 6))) or
     not (X /= ((1, 2, 3), (4, 5, 6), (7, 8, 9))) then
      Failed ("INCORRECT /=");
   end if;

   if not (X in T) or ((1, 2), (3, 4)) in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (((1, 2, 3), (4, 5, 6), (7, 8, 9)) not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT 'ADDRESS");
   end if;

   if T'First /= 4 then
      Failed ("INCORRECT TYPE'FIRST");
   end if;

   if X'First /= 4 then
      Failed ("INCORRECT OBJECT'FIRST");
   end if;

   if V'First /= 4 then
      Failed ("INCORRECT VALUE'FIRST");
   end if;

   if T'First (N) /= 6 then
      Failed ("INCORRECT TYPE'FIRST (N)");
   end if;

   if X'First (N) /= 6 then
      Failed ("INCORRECT OBJECT'FIRST (N)");
   end if;

   if V'First (N) /= 6 then
      Failed ("INCORRECT VALUE'FIRST (N)");
   end if;

   if T'Last /= 5 then
      Failed ("INCORRECT TYPE'LAST");
   end if;

   if X'Last /= 5 then
      Failed ("INCORRECT OBJECT'LAST");
   end if;

   if V'Last /= 5 then
      Failed ("INCORRECT VALUE'LAST");
   end if;

   if T'Last (N) /= 8 then
      Failed ("INCORRECT TYPE'LAST (N)");
   end if;

   if X'Last (N) /= 8 then
      Failed ("INCORRECT OBJECT'LAST (N)");
   end if;

   if V'Last (N) /= 8 then
      Failed ("INCORRECT VALUE'LAST (N)");
   end if;

   if T'Length /= 2 then
      Failed ("INCORRECT TYPE'LENGTH");
   end if;

   if X'Length /= 2 then
      Failed ("INCORRECT OBJECT'LENGTH");
   end if;

   if V'Length /= 2 then
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
      Y : Parent (T'Range, 1 .. 3);
   begin
      if Y'First /= 4 or Y'Last /= 5 then
         Failed ("INCORRECT TYPE'RANGE");
      end if;
   end;

   declare
      Y : Parent (X'Range, 1 .. 3);
   begin
      if Y'First /= 4 or Y'Last /= 5 then
         Failed ("INCORRECT OBJECT'RANGE");
      end if;
   end;

   declare
      Y : Parent (V'Range, 1 .. 3);
   begin
      if Y'First /= 4 or Y'Last /= 5 then
         Failed ("INCORRECT VALUE'RANGE");
      end if;
   end;

   declare
      Y : Parent (1 .. 2, T'Range (N));
   begin
      if Y'First (N) /= 6 or Y'Last (N) /= 8 then
         Failed ("INCORRECT TYPE'RANGE (N)");
      end if;
   end;

   declare
      Y : Parent (1 .. 2, X'Range (N));
   begin
      if Y'First (N) /= 6 or Y'Last (N) /= 8 then
         Failed ("INCORRECT OBJECT'RANGE (N)");
      end if;
   end;

   declare
      Y : Parent (1 .. 2, V'Range (N));
   begin
      if Y'First (N) /= 6 or Y'Last (N) /= 8 then
         Failed ("INCORRECT VALUE'RANGE (N)");
      end if;
   end;

   if T'Size < T'Length * T'Length (N) * Component'Size then
      Failed ("INCORRECT TYPE'SIZE");
   end if;

   if X'Size < X'Length * X'Length (N) * Component'Size then
      Failed ("INCORRECT OBJECT'SIZE");
   end if;

   Result;
end C34005m;
