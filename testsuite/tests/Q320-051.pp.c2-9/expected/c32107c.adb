-- C32107C.ADA

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
-- FOR OBJECTS OF A GENERIC FORMAL TYPE WHOSE ACTUAL PARAMETER IS A TYPE WITH
-- DEFAULT VALUES, CHECK THAT OBJECT DECLARATIONS ARE ELABORATED IN THE ORDER
-- OF THEIR OCCURRENCE, I.E., THAT EXPRESSIONS ASSOCIATED WITH ONE DECLARATION
-- (INCLUDING DEFAULT EXPRESSIONS) ARE EVALUATED BEFORE ANY EXPRESSION
-- BELONGING TO THE NEXT DECLARATION.

-- R.WILLIAMS 9/24/86

with Report; use Report;
procedure C32107c is

   Bump : Integer := 0;

   G1, H1 : Integer;

   function F return Integer is
   begin
      Bump := Bump + 1;
      return Bump;
   end F;

   function G return Integer is
   begin
      Bump := Bump + 1;
      G1   := Bump;
      return Bump;
   end G;

   function H return Integer is
   begin
      Bump := Bump + 1;
      H1   := Bump;
      return Bump;
   end H;

begin
   Test
     ("C32107C",
      "FOR OBJECTS OF A GENERIC FORMAL TYPE WHOSE " &
      "ACTUAL PARAMETER IS A TYPE WITH DEFAULT " &
      "VALUES, CHECK THAT OBJECT DECLARATIONS ARE " &
      "ELABORATED IN THE ORDER OF THEIR " &
      "OCCURRENCE, I.E., THAT EXPRESSIONS " &
      "ASSOCIATED WITH ONE DECLARATION (INCLUDING " &
      "DEFAULT EXPRESSIONS) ARE EVALUATED BEFORE " &
      "ANY EXPRESSION BELONGING TO THE NEXT " & "DECLARATION");

   declare -- (A).
      type Rec (D : Integer := F) is record
         A : Integer := F;
      end record;

      function Get_A (R : Rec) return Integer is
      begin
         return R.A;
      end Get_A;

      generic
         type T is (<>);
         type Priv (D : T) is private;
         with function Get_A (P : Priv) return Integer is <>;
      procedure P;

      procedure P is
         P1          : Priv (T'Val (F));
         P2          : Priv (T'Val (F * 100));
         Order_Check : Integer;

      begin
         Order_Check :=
           T'Pos (P1.D) + T'Pos (P2.D) + (Get_A (P1) * 10) +
           (Get_A (P2) * 1_000);
         if Order_Check /= 4_321 then
            Failed
              ("OBJECTS NOT ELABORATED IN PROPER " &
               "ORDER VALUE OF ORDER_CHECK SHOULD BE " &
               "4321 -- ACTUAL VALUE IS " & Integer'Image (Order_Check) &
               " - (A)");
         end if;
      end P;

      procedure Proc is new P (Integer, Rec);

   begin
      Proc;
   end; -- (A).

   Bump := 0;

   declare -- (B).
      type Rec (D1 : Integer := F; D2 : Integer := F) is record
         A : Integer := F;
      end record;

      function Get_A (R : Rec) return Integer is
      begin
         return R.A;
      end Get_A;

      generic
         type T is (<>);
         type Priv (D1 : T; D2 : T) is private;
         with function Get_A (P : Priv) return Integer is <>;
      procedure P;

      procedure P is
         P1          : Priv (T'Val (F * 1_000), T'Val (F * 10_000));
         P2          : Priv (T'Val (F), T'Val (F * 10));
         Order_Check : Integer;

      begin
         Order_Check :=
           T'Pos (P1.D1) + T'Pos (P1.D2) + T'Pos (P2.D1) + T'Pos (P2.D2) +
           (Get_A (P1) * 100);
         if (Get_A (P2) = 6) and
           (Order_Check = 12_345 or Order_Check = 21_345 or
            Order_Check = 21_354 or Order_Check = 12_354)
         then
            Comment
              ("ORDER_CHECK HAS VALUE " & Integer'Image (Order_Check) &
               " - (B)");
         else
            Failed
              ("OBJECTS NOT ELABORATED IN PROPER " &
               "ORDER VALUE OF ORDER_CHECK SHOULD BE " &
               "6 12345, 6 21345, 6 21354, OR " &
               "6 12354 -- ACTUAL VALUE IS " & Integer'Image (Get_A (P2)) &
               Integer'Image (Order_Check) & " - (B)");
         end if;

      end P;

      procedure Proc is new P (Integer, Rec);

   begin
      Proc;
   end; -- (B).

   Result;
end C32107c;
