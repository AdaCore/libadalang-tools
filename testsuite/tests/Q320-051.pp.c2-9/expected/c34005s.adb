-- C34005S.ADA

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
--     COMPONENT TYPE IS A LIMITED TYPE.  THIS TEST IS PART 1 OF 2
--     TESTS WHICH COVER THE OBJECTIVE.  THE SECOND PART IS IN TEST
--     C34005V.

-- HISTORY:
--     JRK 08/20/87  CREATED ORIGINAL TEST.
--     BCB 04/12/90  SPLIT ORIGINAL TEST INTO C34005S.ADA AND
--                   C34005V.ADA
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.
--     RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURNS.

with System; use System;
with Report; use Report;

procedure C34005s is

   package Pkg_L is

      type Lp is limited private;

      function Create (X : Integer) return Lp;

      function Value (X : Lp) return Integer;

      function Equal (X, Y : Lp) return Boolean;

      procedure Assign (X : out Lp; Y : Lp);

      C1  : constant Lp;
      C2  : constant Lp;
      C3  : constant Lp;
      C4  : constant Lp;
      C5  : constant Lp;
      C6  : constant Lp;
      C7  : constant Lp;
      C8  : constant Lp;
      C9  : constant Lp;
      C10 : constant Lp;
      C11 : constant Lp;
      C12 : constant Lp;
      C13 : constant Lp;
      C14 : constant Lp;

   private

      type Lp is new Integer;

      C1  : constant Lp := 1;
      C2  : constant Lp := 2;
      C3  : constant Lp := 3;
      C4  : constant Lp := 4;
      C5  : constant Lp := 5;
      C6  : constant Lp := 6;
      C7  : constant Lp := 7;
      C8  : constant Lp := 8;
      C9  : constant Lp := 9;
      C10 : constant Lp := 10;
      C11 : constant Lp := 11;
      C12 : constant Lp := 12;
      C13 : constant Lp := 13;
      C14 : constant Lp := 14;

   end Pkg_L;

   use Pkg_L;

   subtype Component is Lp;

   package Pkg_P is

      First : constant := 0;
      Last  : constant := 10;

      subtype Index is Integer range First .. Last;

      type Parent is array (Index range <>, Index range <>) of Component;

      function Create (F1, L1 : Index; F2, L2 : Index; C : Component;
         Dummy                : Parent   -- TO RESOLVE OVERLOADING.
         ) return Parent;

      function Equal (X, Y : Parent) return Boolean;

   end Pkg_P;

   use Pkg_P;

   type T is
     new Parent (Ident_Int (4) .. Ident_Int (5),
        Ident_Int (6) .. Ident_Int (8));

   type Arrt is array (Integer range <>, Integer range <>) of Component;

   subtype Arr is Arrt (8 .. 9, 2 .. 4);

   X : T;
   W : Parent (4 .. 5, 6 .. 8);
   C : Component;
   B : Boolean  := False;
   U : Arr;
   N : constant := 2;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   function V return T is
   begin
      return Result : T do
         for I in Result'Range loop
            for J in Result'Range (2) loop
               Assign (Result (I, J), C);
            end loop;
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

      function Create (F1, L1 : Index; F2, L2 : Index; C : Component;
         Dummy                : Parent) return Parent
      is
         B : Component;
      begin
         return A : Parent (F1 .. L1, F2 .. L2) do
            Assign (B, C);
            for I in F1 .. L1 loop
               for J in F2 .. L2 loop
                  Assign (A (I, J), B);
                  Assign (B, Create (Value (B) + 1));
               end loop;
            end loop;
         end return;
      end Create;

      function Equal (X, Y : Parent) return Boolean is
      begin
         if X'Length /= Y'Length or X'Length (2) /= Y'Length (2) then
            return False;
         else
            for I in X'Range loop
               for J in X'Range (2) loop
                  if not Equal
                      (X (I, J),
                       Y (I - X'First + Y'First,
                          J - X'First (2) + Y'First (2)))
                  then
                     return False;
                  end if;
               end loop;
            end loop;
         end if;
         return True;
      end Equal;

   end Pkg_P;

   function Equal (X, Y : Arrt) return Boolean is
   begin
      if X'Length /= Y'Length or X'Length (2) /= Y'Length (2) then
         return False;
      else
         for I in X'Range loop
            for J in X'Range (2) loop
               if not Equal
                   (X (I, J),
                    Y (I - X'First + Y'First, J - X'First (2) + Y'First (2)))
               then
                  return False;
               end if;
            end loop;
         end loop;
      end if;
      return True;
   end Equal;

begin
   Test
     ("C34005S",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A LIMITED TYPE.    THIS TEST IS PART " &
      "1 OF 2 TESTS WHICH COVER THE OBJECTIVE.  THE " &
      "SECOND PART IS IN TEST C34005V");

   Assign (X (Ident_Int (4), Ident_Int (6)), Create (1));
   Assign (X (Ident_Int (4), Ident_Int (7)), Create (2));
   Assign (X (Ident_Int (4), Ident_Int (8)), Create (3));
   Assign (X (Ident_Int (5), Ident_Int (6)), Create (4));
   Assign (X (Ident_Int (5), Ident_Int (7)), Create (5));
   Assign (X (Ident_Int (5), Ident_Int (8)), Create (6));

   Assign (W (4, 6), Create (1));
   Assign (W (4, 7), Create (2));
   Assign (W (4, 8), Create (3));
   Assign (W (5, 6), Create (4));
   Assign (W (5, 7), Create (5));
   Assign (W (5, 8), Create (6));

   Assign (C, Create (2));

   Assign (U (8, 2), Create (1));
   Assign (U (8, 3), Create (2));
   Assign (U (8, 4), Create (3));
   Assign (U (9, 2), Create (4));
   Assign (U (9, 3), Create (5));
   Assign (U (9, 4), Create (6));

   if not Equal (X (Ident_Int (4), Ident_Int (6)), C1) or
     not Equal (Create (6, 9, 2, 3, C4, X) (9, 3), C11) then
      Failed ("INCORRECT INDEX (VALUE)");
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
end C34005s;
