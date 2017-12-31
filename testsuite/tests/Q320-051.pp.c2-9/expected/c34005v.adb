-- C34005V.ADA

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
--     COMPONENT TYPE IS A LIMITED TYPE.  THIS TEST IS PART 2 OF 2
--     TESTS WHICH COVER THE OBJECTIVE.  THE FIRST PART IS IN TEST
--     C34005S.

-- HISTORY:
--     BCB 04/12/90  CREATED ORIGINAL TEST FROM SPLIT OF C34005S.ADA.
--     RLB 10/03/02  REMOVED ILLEGAL (BY AI-246) TYPE CONVERSIONS AND
--                   SUPPORTING CODE.
--     RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURNS.
--     RLB 08/17/07  FIXED SPELLING OF "RETURN".

with System; use System;
with Report; use Report;

procedure C34005v is

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

      function Aggr (A, B, C, D : Component) return Parent;

      function Aggr (A, B, C, D, E, F : Component) return Parent;

      function Aggr (A, B, C, D, E, F, G, H : Component) return Parent;

      function Aggr (A, B, C, D, E, F, G, H, I : Component) return Parent;

   end Pkg_P;

   use Pkg_P;

   type T is
     new Parent (Ident_Int (4) .. Ident_Int (5),
        Ident_Int (6) .. Ident_Int (8));

   X : T;
   W : Parent (4 .. 5, 6 .. 8);
   C : Component;
   B : Boolean  := False;
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

      function Aggr (A, B, C, D : Component) return Parent is
      begin
         return
           X : Parent (Index'First .. Index'First + 1,
              Index'First .. Index'First + 1)
         do
            Assign (X (Index'First, Index'First), A);
            Assign (X (Index'First, Index'First + 1), B);
            Assign (X (Index'First + 1, Index'First), C);
            Assign (X (Index'First + 1, Index'First + 1), D);
         end return;
      end Aggr;

      function Aggr (A, B, C, D, E, F : Component) return Parent is
      begin
         return
           X : Parent (Index'First .. Index'First + 1,
              Index'First .. Index'First + 2)
         do
            Assign (X (Index'First, Index'First), A);
            Assign (X (Index'First, Index'First + 1), B);
            Assign (X (Index'First, Index'First + 2), C);
            Assign (X (Index'First + 1, Index'First), D);
            Assign (X (Index'First + 1, Index'First + 1), E);
            Assign (X (Index'First + 1, Index'First + 2), F);
         end return;
      end Aggr;

      function Aggr (A, B, C, D, E, F, G, H : Component) return Parent is
      begin
         return
           X : Parent (Index'First .. Index'First + 3,
              Index'First .. Index'First + 1)
         do
            Assign (X (Index'First, Index'First), A);
            Assign (X (Index'First, Index'First + 1), B);
            Assign (X (Index'First + 1, Index'First), C);
            Assign (X (Index'First + 1, Index'First + 1), D);
            Assign (X (Index'First + 2, Index'First), E);
            Assign (X (Index'First + 2, Index'First + 1), F);
            Assign (X (Index'First + 3, Index'First), G);
            Assign (X (Index'First + 3, Index'First + 1), H);
         end return;
      end Aggr;

      function Aggr (A, B, C, D, E, F, G, H, I : Component) return Parent is
      begin
         return
           X : Parent (Index'First .. Index'First + 2,
              Index'First .. Index'First + 2)
         do
            Assign (X (Index'First, Index'First), A);
            Assign (X (Index'First, Index'First + 1), B);
            Assign (X (Index'First, Index'First + 2), C);
            Assign (X (Index'First + 1, Index'First), D);
            Assign (X (Index'First + 1, Index'First + 1), E);
            Assign (X (Index'First + 1, Index'First + 2), F);
            Assign (X (Index'First + 2, Index'First), G);
            Assign (X (Index'First + 2, Index'First + 1), H);
            Assign (X (Index'First + 2, Index'First + 2), I);
         end return;
      end Aggr;

   end Pkg_P;

begin
   Test
     ("C34005V",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A LIMITED TYPE.  THIS TEST IS PART 2 " &
      "OF 2 TESTS WHICH COVER THE OBJECTIVE.  THE " &
      "FIRST PART IS IN TEST C34005S");

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

   if not Equal (T'(X), Aggr (C1, C2, C3, C4, C5, C6)) then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if not Equal (T (X), Aggr (C1, C2, C3, C4, C5, C6)) then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if not Equal (T (W), Aggr (C1, C2, C3, C4, C5, C6)) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   begin
      if not Equal (Parent (X), Aggr (C1, C2, C3, C4, C5, C6)) or
        not Equal
          (Parent (Create (6, 9, 2, 3, C4, X)),
           Aggr (C4, C5, C6, C7, C8, C9, C10, C11))
      then
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR WHEN PREPARING TO CONVERT " & "TO PARENT");
      when others =>
         Failed ("EXCEPTION WHEN PREPARING TO CONVERT " & "TO PARENT");
   end;

   if not (X in T) or Aggr (C1, C2, C3, C4) in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (Aggr (C1, C2, C3, C4, C5, C6, C7, C8, C9) not in T)
   then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   Result;
end C34005v;
