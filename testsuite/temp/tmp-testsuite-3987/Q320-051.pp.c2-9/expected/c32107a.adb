-- C32107A.ADA

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
-- CHECK THAT OBJECT DECLARATIONS ARE ELABORATED IN THE ORDER OF THEIR
-- OCCURRENCE, I.E., THAT EXPRESSIONS ASSOCIATED WITH ONE DECLARATION
-- (INCLUDING DEFAULT EXPRESSIONS, IF APPROPRIATE) ARE EVALUATED BEFORE
-- ANY EXPRESSION BELONGING TO THE NEXT DECLARATION. ALSO, CHECK THAT
-- EXPRESSIONS IN THE SUBTYPE INDICATION OR THE CONSTRAINED ARRAY
-- DEFINITION ARE EVALUATED BEFORE ANY INITIALIZATION EXPRESSIONS ARE
-- EVALUATED.

-- R.WILLIAMS 9/24/86

with Report; use Report;
procedure C32107a is

   Bump : Integer := 0;

   Order_Check : Integer;

   G1, H1, I1 : Integer;

   First_Call : Boolean := True;

   type Arr1 is array (Positive range <>) of Integer;

   type Arr1_Name is access Arr1;

   type Arr2 is array (Positive range <>, Positive range <>) of Integer;

   type Rec (D : Integer) is record
      Comp : Integer;
   end record;

   type Rec_Name is access Rec;

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

   function I return Integer is
   begin
      if First_Call then
         Bump       := Bump + 1;
         I1         := Bump;
         First_Call := False;
      end if;
      return I1;
   end I;

begin
   Test
     ("C32107A",
      "CHECK THAT OBJECT DECLARATIONS ARE " &
      "ELABORATED IN THE ORDER OF THEIR " &
      "OCCURRENCE, I.E., THAT EXPRESSIONS " &
      "ASSOCIATED WITH ONE DECLARATION (INCLUDING " &
      "DEFAULT EXPRESSIONS, IF APPROPRIATE) ARE " &
      "EVALUATED BEFORE ANY EXPRESSION BELONGING " &
      "TO THE NEXT DECLARATION.  ALSO, CHECK THAT " &
      "EXPRESSIONS IN THE SUBTYPE INDICATION OR " &
      "THE CONSTRAINED ARRAY DEFINITION ARE " &
      "EVALUATED BEFORE ANY INITIALIZATION " &
      "EXPRESSIONS ARE EVALUATED");

   declare -- (A).
      I1 : Integer                                  := 10_000 * F;
      A1 : constant array (1 .. H) of Rec (G * 100) :=
        (1 .. H1 => (G1 * 100, I * 10));
      I2 : constant Integer := F * 1_000;
   begin
      Order_Check := I1 + I2 + A1'Last + A1 (1).D + A1 (1).Comp;
      if Order_Check = 15_243 or Order_Check = 15_342 then
         Comment
           ("ORDER_CHECK HAS VALUE " & Integer'Image (Order_Check) & " - (A)");
      else
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 15343 OR " &
            "15242 -- ACTUAL VALUE IS " &
            Integer'Image (Order_Check) &
            " - (A)");
      end if;
   end; -- (A).

   Bump := 0;

   declare -- (B).
      A : Arr2 (1 .. F, 1 .. F * 10);
      R : Rec (G * 100) := (G1 * 100, F * 1_000);
      I : Integer range 1 .. H;
      S : Rec (F * 10);
   begin
      Order_Check := A'Last (1) + A'Last (2) + R.D + R.Comp;
      if (H1 + S.D = 65) and (Order_Check = 4_321 or Order_Check = 4_312) then
         Comment
           ("ORDER_CHECK HAS VALUE 65 " &
            Integer'Image (Order_Check) &
            " - (B)");
      else
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 65 4321 OR " &
            "65 4312 -- ACTUAL VALUE IS " &
            Integer'Image (H1 + S.D) &
            Integer'Image (Order_Check) &
            " - (B)");
      end if;
   end; -- (B).

   Bump := 0;

   declare -- (C).
      I1 : constant Integer range 1 .. G * 10 := F;
      A1 : array (1 .. F * 100) of Integer range 1 .. H * 1_000;
   begin
      Order_Check := I1 + (G1 * 10) + A1'Last + (H1 * 1_000);
      if Order_Check = 4_312 or Order_Check = 3_412 then
         Comment
           ("ORDER_CHECK HAS VALUE " & Integer'Image (Order_Check) & " - (C)");
      else
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 4312 OR " &
            "3412 -- ACTUAL VALUE IS " &
            Integer'Image (Order_Check) &
            " - (C)");
      end if;
   end; -- (C).

   Bump       := 0;
   First_Call := True;

   declare -- (D).
      A1 : array (1 .. G) of Rec (H * 10_000) :=
        (1 .. G1 => (H1 * 10_000, I * 100));
      R1 : constant Rec := (F * 1_000, F * 10);

   begin
      Order_Check := A1'Last + A1 (1).D + A1 (1).Comp + R1.D + R1.Comp;
      if Order_Check = 25_341 or
        Order_Check = 24_351 or
        Order_Check = 15_342 or
        Order_Check = 14_352
      then
         Comment
           ("ORDER_CHECK HAS VALUE " & Integer'Image (Order_Check) & " - (D)");
      else
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 25341, " &
            "24351, 15342 OR 14352  -- ACTUAL VALUE IS " &
            Integer'Image (Order_Check) &
            " - (D)");
      end if;
   end; -- (D).

   Bump := 0;

   declare -- (E).
      A1 : constant Arr1_Name := new Arr1'(1 .. F => F * 10);
      R1 : Rec_Name (H * 100) := new Rec'(H1 * 100, F * 1_000);

   begin
      Order_Check := A1.all'Last + A1.all (1) + R1.D + R1.Comp;
      if Order_Check /= 4_321 then
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 4321 " &
            "-- ACTUAL VALUE IS " &
            Integer'Image (Order_Check) &
            " - (E)");
      end if;
   end; -- (E).

   Bump       := 0;
   First_Call := True;

   declare -- (F).
      A1 : constant array (1 .. G) of Integer range 1 .. H * 100 :=
        (1 .. G1 => I * 10);
      A2 : Arr1 (1 .. F * 1_000);
   begin
      Order_Check := A1'Last + (H1 * 100) + A1 (1) + A2'Last;
      if Order_Check = 4_231 or Order_Check = 4_132 then
         Comment
           ("ORDER_CHECK HAS VALUE " & Integer'Image (Order_Check) & " - (F)");
      else
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 4231 OR " &
            "4132 -- ACTUAL VALUE IS " &
            Integer'Image (Order_Check) &
            " - (F)");
      end if;
   end; -- (F).

   Bump := 0;

   declare -- (G).
      A1 : Arr1_Name (1 .. G)         := new Arr1 (1 .. G1);
      R1 : constant Rec_Name (H * 10) := new Rec'(H1 * 10, F * 100);
   begin
      Order_Check := A1.all'Last + R1.D + R1.Comp;
      if Order_Check /= 321 then
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 321 OR " &
            "-- ACTUAL VALUE IS " &
            Integer'Image (Order_Check) &
            " - (G)");
      end if;
   end; -- (G).

   Bump := 0;

   declare -- (H).
      type Rec (D : Integer := F) is record
         Comp : Integer := F * 10;
      end record;

      R1 : Rec;
      R2 : Rec (G * 100) := (G1 * 100, F * 1_000);
   begin
      Order_Check := R1.D + R1.Comp + R2.D + R2.Comp;
      if Order_Check = 4_321 or
        Order_Check = 4_312 or
        Order_Check = 3_421 or
        Order_Check = 3_412
      then
         Comment
           ("ORDER_CHECK HAS VALUE " & Integer'Image (Order_Check) & " - (H)");
      else
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 4321, " &
            "4312, 3421, OR 3412 -- ACTUAL VALUE IS " &
            Integer'Image (Order_Check) &
            " - (H)");
      end if;
   end; -- (H).

   Bump := 0;

   declare -- (I).
      type Rec2 (D1, D2 : Integer) is record
         Comp : Integer;
      end record;

      R1 : Rec2 (G * 1_000, H * 10_000) := (G1 * 1_000, H1 * 10_000, F * 100);
      R2 : Rec2 (F, F * 10);
   begin
      Order_Check := R1.D1 + R1.D2 + R1.Comp + R2.D1 + R2.D2;
      if Order_Check = 21_354 or
        Order_Check = 21_345 or
        Order_Check = 12_345 or
        Order_Check = 12_354
      then
         Comment
           ("ORDER_CHECK HAS VALUE " & Integer'Image (Order_Check) & " - (I)");
      else
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 21354, " &
            "21345, 12354, OR 12345 -- ACTUAL VALUE IS " &
            Integer'Image (Order_Check) &
            " - (I)");
      end if;

   end; -- (I).

   Bump := 0;

   declare -- (J).
      package P is
         type Priv (D : Integer) is private;

         P1 : constant Priv;
         P2 : constant Priv;

         function Get_A (P : Priv) return Integer;
      private
         type Priv (D : Integer) is record
            Comp : Integer;
         end record;
         P1 : constant Priv := (F, F * 10);
         P2 : constant Priv := (F * 100, F * 1_000);
      end P;

      package body P is
         function Get_A (P : Priv) return Integer is
         begin
            return P.Comp;
         end Get_A;
      end P;

      use P;
   begin
      Order_Check := P1.D + Get_A (P1) + P2.D + Get_A (P2);
      if Order_Check = 4_321 or
        Order_Check = 4_312 or
        Order_Check = 3_412 or
        Order_Check = 3_421
      then
         Comment
           ("ORDER_CHECK HAS VALUE " & Integer'Image (Order_Check) & " - (J)");
      else
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 4321, " &
            "4312, 3421, OR 3412 -- ACTUAL VALUE IS " &
            Integer'Image (Order_Check) &
            " - (J)");
      end if;
   end; -- (J).

   Bump := 0;

   declare -- (K).
      package P is
         type Priv (D1, D2 : Integer) is private;

      private
         type Priv (D1, D2 : Integer) is record
            null;
         end record;
      end P;

      use P;

      P1 : Priv (F, F * 10);
      P2 : Priv (F * 100, F * 1_000);

   begin
      Order_Check := P1.D1 + P1.D2 + P2.D1 + P2.D2;
      if Order_Check = 4_321 or
        Order_Check = 4_312 or
        Order_Check = 3_412 or
        Order_Check = 3_421
      then
         Comment
           ("ORDER_CHECK HAS VALUE " & Integer'Image (Order_Check) & " - (K)");
      else
         Failed
           ("OBJECTS NOT ELABORATED IN PROPER ORDER " &
            "VALUE OF ORDER_CHECK SHOULD BE 4321, 4312, " &
            "3421, OR 3412 -- ACTUAL VALUE IS " &
            Integer'Image (Order_Check) &
            " - (K)");
      end if;

   end; -- (K).

   Result;
end C32107a;
