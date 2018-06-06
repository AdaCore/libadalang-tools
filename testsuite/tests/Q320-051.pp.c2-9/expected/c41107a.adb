-- C41107A.ADA

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
-- CHECK THAT FOR AN ARRAY HAVING BOTH POSITIVE AND NEGATIVE
--   INDEX VALUES, THE PROPER COMPONENT IS SELECTED - A.
-- CHECK THAT FOR AN ARRAY INDEXED WITH AN ENUMERATION TYPE,
--   APPROPRIATE COMPONENTS CAN BE SELECTED - B.
-- CHECK THAT SUBSCRIPT EXPRESSIONS CAN BE OF COMPLEXITY GREATER
--   THAN VARIABLE + - CONSTANT - C.
-- CHECK THAT MULTIPLY DIMENSIONED ARRAYS ARE PROPERLY INDEXED - D.

-- WKB 7/29/81
-- JBG 8/21/83

with Report; use Report;
procedure C41107a is

   type T1 is array (Integer range -2 .. 2) of Integer;
   A : T1 := (1, 2, 3, 4, 5);

   type Color is (Red, Orange, Yellow, Green, Blue);
   type T2 is array (Color range Red .. Blue) of Integer;
   B : T2 := (5, 4, 3, 2, 1);

   C : String (1 .. 7) := "ABCDEFG";

   type T4 is array (1 .. 4, 1 .. 3) of Integer;
   D : T4 :=
     (1 => (1, 2, 3), 2 => (4, 5, 6), 3 => (7, 8, 9), 4 => (0, -1, -2));

   V1 : Integer := Ident_Int (1);
   V2 : Integer := Ident_Int (2);
   V3 : Integer := Ident_Int (3);

   procedure P1
     (X : in Integer; Y : in out Integer; Z : out Integer; W : String)
   is
   begin
      if X /= 1 then
         Failed ("WRONG VALUE FOR IN PARAMETER - " & W);
      end if;
      if Y /= 4 then
         Failed ("WRONG VALUE FOR IN OUT PARAMETER - " & W);
      end if;
      Y := 11;
      Z := 12;
   end P1;

   procedure P2 (X : in Character; Y : in out Character; Z : out Character) is
   begin
      if X /= 'D' then
         Failed ("WRONG VALUE FOR IN PARAMETER - C");
      end if;
      if Y /= 'F' then
         Failed ("WRONG VALUE FOR IN OUT PARAMETER - C");
      end if;
      Y := 'Y';
      Z := 'Z';
   end P2;

begin
   Test
     ("C41107A",
      "CHECK THAT THE PROPER COMPONENT IS SELECTED " &
      "FOR ARRAYS WITH POS AND NEG INDICES, " &
      "ENUMERATION INDICES, COMPLEX SUBSCRIPT " &
      "EXPRESSIONS, AND MULTIPLE DIMENSIONS");

   if A (Ident_Int (1)) /= 4 then
      Failed ("WRONG VALUE FOR EXPRESSION - A");
   end if;
   A (Ident_Int (-2)) := 10;
   if A /= (10, 2, 3, 4, 5) then
      Failed ("WRONG TARGET FOR ASSIGNMENT - A");
   end if;
   A := (2, 1, 0, 3, 4);
   P1 (A (-1), A (2), A (-2), "A");
   if A /= (12, 1, 0, 3, 11) then
      Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - A");
   end if;

   if B (Green) /= 2 then
      Failed ("WRONG VALUE FOR EXPRESSION - B");
   end if;
   B (Yellow) := 10;
   if B /= (5, 4, 10, 2, 1) then
      Failed ("WRONG TARGET FOR ASSIGNMENT - B");
   end if;
   B := (1, 4, 2, 3, 5);
   P1 (B (Red), B (Orange), B (Blue), "B");
   if B /= (1, 11, 2, 3, 12) then
      Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - B");
   end if;

   if C (3 .. 6) (3**2 / 3 * (2 - 1) - 6 / 3 + 2) /= 'C' then
      Failed ("WRONG VALUE FOR EXPRESSION - C");
   end if;
   C (3 .. 6) (V3**2 / V1 * (V3 - V2) + Ident_Int (4) - V3 * V2 - V1) := 'W';
   if C /= "ABCDEWG" then
      Failed ("WRONG TARGET FOR ASSIGNMENT - C");
   end if;
   C := "ABCDEFG";
   P2 (C (3 .. 6) (V3 + V1), C (3 .. 6) (V3 * V2),
      C (3 .. 6) ((V1 + V2) * V1));
   if C /= "ABZDEYG" then
      Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - C");
   end if;

   if D (Ident_Int (1), Ident_Int (3)) /= 3 then
      Failed ("WRONG VALUE FOR EXPRESSION - D");
   end if;
   D (Ident_Int (4), Ident_Int (2)) := 10;
   if D /= ((1, 2, 3), (4, 5, 6), (7, 8, 9), (0, 10, -2)) then
      Failed ("WRONG TARGET FOR ASSIGNMENT - D");
   end if;
   D := (1 => (0, 2, 3), 2 => (4, 5, 6), 3 => (7, 8, 9), 4 => (1, -1, -2));
   P1 (D (4, 1), D (2, 1), D (3, 2), "D");
   if D /= ((0, 2, 3), (11, 5, 6), (7, 12, 9), (1, -1, -2)) then
      Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - D");
   end if;

   Result;
end C41107a;
