-- C87B32A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE FOLLOWING RULES:

-- FOR ATTRIBUTES OF THE FORM: T'SUCC (X), T'PRED (X), T'POS (X),
-- AND T'IMAGE (X) , THE OPERAND X MUST BE OF TYPE T.
--
-- FOR THE ATTRIBUTE OF THE FORM T'VAL (X), THE OPERAND X MUST BE
-- OF AN INTEGER TYPE.
--
-- FOR THE ATTRIBUTE OF THE FORM T'VALUE (X), THE OPERAND X MUST
-- BE OF THE PREDEFINED TYPE STRING.

-- TRH  13 SEPT 82
-- JRK  12 JAN  84

with Report; use Report;

procedure C87b32a is

   type Color is (Brown, Red, White);
   type School is (Harvard, Brown, Yale);
   type Cook is (Simmer, Saute, Brown, Boil);
   type Sugar is (Dextrose, Cane, Glucose, Brown);
   type Whole is new Integer range 0 .. Integer'Last;
   type Lit_Char is
     ('+', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
   type Lit_String is array (Positive range <>) of Lit_Char;

   function "+" (X, Y : Whole) return Whole renames "*";

   function F1 return String is
   begin
      return "+10";
   end F1;

   function F1 return Lit_String is
   begin
      Failed ("THE VALUE ATTRIBUTE TAKES A PREDEFINED STRING " & "OPERAND");
      return "+3";
   end F1;

   function F1 return Character is
   begin
      Failed ("THE VALUE ATTRIBUTE TAKES A STRING OPERAND");
      return '2';
   end F1;

   function F2 (X : Integer) return Float is
   begin
      Failed ("THE VAL ATTRIBUTE TAKES AN INTEGER TYPE OPERAND");
      return 0.0;
   end F2;

   function F2 (X : Integer := 1) return Integer is
   begin
      return X;
   end F2;

begin
   Test
     ("C87B32A",
      "OVERLOADED OPERANDS FOR THE ATTRIBUTES " &
      "T'PRED, T'SUCC, T'POS, T'VAL, T'IMAGE AND T'VALUE");

   if Color'Pos (Brown) /= 0 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 1");
   end if;

   if School'Pos (Brown) /= 1 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 2");
   end if;

   if Cook'Pos (Brown) /= 2 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 3");
   end if;

   if Sugar'Pos (Brown) /= 3 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 4");
   end if;

   if School'Pred (Brown) /= Harvard then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 5");
   end if;

   if Cook'Pred (Brown) /= Saute then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 6");
   end if;

   if Sugar'Pred (Brown) /= Glucose then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 7");
   end if;

   if Color'Succ (Brown) /= Red then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 8");
   end if;

   if School'Succ (Brown) /= Yale then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 9");
   end if;

   if Cook'Succ (Brown) /= Boil then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 10");
   end if;

   if Color'Val (F2 (0)) /= Brown then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 11");
   end if;

   if School'Val (F2) /= Brown then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 12");
   end if;

   if Cook'Val (F2 (2)) /= Brown then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 13");
   end if;

   if Sugar'Val (F2) /= Cane then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 14");
   end if;

   if Whole'Pos (1 + 1) /= 1 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 15");
   end if;

   if Whole'Val (1 + 1) /= 2 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 16");
   end if;

   if Whole'Succ (1 + 1) /= 2 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 17");
   end if;

   if Whole'Pred (1 + 1) /= 0 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 18");
   end if;

   if Whole'Value ("+1") + 1 /= 1 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 19");
   end if;

   if Whole'Image (1 + 1) /= " 1" then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 20");
   end if;

   if Whole'Value (F1) + 1 /= 10 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 21");
   end if;

   if Whole'Val (1) + 1 /= 1 then
      Failed
        ("RESOLUTION INCORRECT FOR OPERANDS OF THE ATTRIBUTES" &
         " PRED, SUCC, VAL, POS, IMAGE AND VALUE - 22");
   end if;

   Result;
end C87b32a;
