-- C67003F.ADA

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
-- CHECK THAT THE PREDEFINED OPERATORS FOR THE PREDEFINED TYPES CAN BE
--   REDEFINED.
-- CHECK THAT THE REDEFINED OPERATOR IS INVOKED WHEN INFIX OR PREFIX
--   NOTATION IS USED.

-- HISTORY:
--   WMC 03/21/92   TEST CREATED FROM CONSOLIDATION OF C67003[A-E].ADA

with Report;

procedure C67003f is

   use Report;

begin

   Test
     ("C67003F",
      "CHECK THAT REDEFINITION OF " & "OPERATORS FOR PREDEFINED TYPES WORKS");

   declare     -- INTEGER OPERATORS.

      -- INTEGER INFIX OPERATORS.

      function "*" (X, Y : Integer) return Integer is
      begin
         if X /= Y then
            return 1;
         else
            return 0;
         end if;
      end "*";

      function "+" (X, Y : Integer) return Integer is
      begin
         if X /= Y then
            return 2;
         else
            return 0;
         end if;
      end "+";

      function "REM" (X, Y : Integer) return Integer is
      begin
         if X /= Y then
            return 3;
         else
            return 0;
         end if;
      end "REM";

      -- INTEGER PREFIX OPERATORS.

      function "+" (X : Integer) return Integer is
      begin
         if X /= 0 then
            return 4;
         else
            return 0;
         end if;
      end "+";

      function "ABS" (X : Integer) return Integer is
      begin
         if X /= 0 then
            return 5;
         else
            return 0;
         end if;
      end "ABS";

      -- INTEGER RELATIONAL OPERATOR.

      function "<" (X, Y : Integer) return Boolean is
      begin
         return X = Y;
      end "<";

   begin

      if Ident_Int (3) * Ident_Int (5) /= 1 then
         Failed ("REDEFINITION OF INTEGER ""*"" IS DEFECTIVE");
      end if;

      if Ident_Int (1) + Ident_Int (30) /= 2 then
         Failed ("REDEFINITION OF INTEGER ""+"" IS DEFECTIVE");
      end if;

      if Ident_Int (7) rem Ident_Int (8) /= 3 then
         Failed ("REDEFINITION OF ""REM"" IS DEFECTIVE");
      end if;

      if +(Ident_Int (10)) /= 4 then
         Failed ("REDEFINITION OF INTEGER UNARY ""+"" IS DEFECTIVE");
      end if;

      if abs (Ident_Int (2)) /= 5 then
         Failed ("REDEFINITION OF INTEGER ""ABS"" IS DEFECTIVE");
      end if;

      if Ident_Int (7) < Ident_Int (8) then
         Failed ("REDEFINITION OF INTEGER ""<"" IS DEFECTIVE");
      end if;

   end;

   declare   -- FLOAT OPERATORS.

      -- NOTE THAT ALL LITERAL VALUES USED SHOULD BE
      --   REPRESENTABLE EXACTLY.

      function Ident_Float (X : Float) return Float is
         I : Integer := Integer (X);
      begin
         if Equal (I, I) then          -- ALWAYS EQUAL.
            return X;
         end if;
         return 0.0;
      end Ident_Float;

      -- FLOAT INFIX OPERATORS.

      function "-" (X, Y : Float) return Float is
      begin
         if X /= Y then
            return 1.0;
         else
            return 0.0;
         end if;
      end "-";

      function "/" (X, Y : Float) return Float is
      begin
         if X /= Y then
            return 2.0;
         else
            return 0.0;
         end if;
      end "/";

      function "**" (X : Float; Y : Integer) return Float is
      begin
         if Integer (X) /= Y then
            return 3.0;
         else
            return 0.0;
         end if;
      end "**";

      -- FLOAT PREFIX OPERATOR.

      function "-" (X : Float) return Float is
      begin
         if X /= 0.0 then
            return 4.0;
         else
            return 0.0;
         end if;
      end "-";

      -- FLOAT RELATIONAL OPERATOR.

      function "<=" (X, Y : Float) return Boolean is
      begin
         return X = Y;
      end "<=";

   begin

      if Ident_Float (50.0) - Ident_Float (100.0) /= 1.0 then
         Failed ("REDEFINITION OF FLOAT ""-"" IS DEFECTIVE");
      end if;

      if Ident_Float (5.0) / Ident_Float (1.0) /= 2.0 then
         Failed ("REDEFINITION OF FLOAT ""/"" IS DEFECTIVE");
      end if;

      if Ident_Float (3.0)**Ident_Int (2) /= 3.0 then
         Failed ("REDEFINITION OF FLOAT ""**"" IS DEFECTIVE");
      end if;

      if -(Ident_Float (5.0)) /= 4.0 then
         Failed ("REDEFINITION OF FLOAT UNARY ""-"" IS DEFECTIVE");
      end if;

      if Ident_Float (1.0) <= Ident_Float (5.0) then
         Failed ("REDEFINITION OF FLOAT ""<="" IS DEFECTIVE");
      end if;

   end;

   declare     -- BOOLEAN OPERATORS.

      -- BOOLEAN LOGICAL OPERATORS.

      function "AND" (X, Y : Boolean) return Boolean is
      begin
         if X and then Y then
            return False;
         else
            return True;
         end if;
      end "AND";

      function "XOR" (X, Y : Boolean) return Boolean is
      begin
         return X = Y;
      end "XOR";

      -- BOOLEAN RELATIONAL OPERATOR.

      function ">" (X, Y : Boolean) return Boolean is
      begin
         return X = Y;
      end ">";

   begin

      if Ident_Bool (True) and Ident_Bool (True) then
         Failed ("REDEFINITION OF ""AND"" IS DEFECTIVE");
      end if;

      if Ident_Bool (True) xor Ident_Bool (False) then
         Failed ("REDEFINITION OF ""XOR"" IS DEFECTIVE");
      end if;

      if Ident_Bool (True) > Ident_Bool (False) then
         Failed ("REDEFINITION OF BOOLEAN "">"" IS DEFECTIVE");
      end if;

   end;

   declare     -- STRING OPERATORS.

      S1 : String (1 .. 2) := "A" & Ident_Char ('B');
      S2 : String (1 .. 2) := "C" & Ident_Char ('D');

      function "&" (X, Y : String) return String is
         Z : String (1 .. X'Length + Y'Length);
      begin
         Z (1 .. Y'Length)          := Y;
         Z (Y'Length + 1 .. Z'Last) := X;
         return Z;
      end "&";

      function "&" (X : Character; Y : String) return String is
         Z : String (1 .. Y'Length + 1);
      begin
         Z (1 .. Y'Length) := Y;
         Z (Z'Last)        := X;
         return Z;
      end "&";

      -- STRING RELATIONAL OPERATOR.

      function ">=" (X, Y : String) return Boolean is
      begin
         return X = Y;
      end ">=";

   begin

      if S1 & S2 /= "CDAB" then
         Failed ("BAD REDEFINITION OF ""&"" (S,S)");
      end if;

      if Ident_Char ('C') & S1 /= "ABC" then
         Failed ("BAD REDEFINITION OF ""&"" (C,S)");
      end if;

      if S2 >= S1 then
         Failed ("BAD REDEFINITION OF STRING "">=""");
      end if;

   end;

   declare      -- CHARACTER OPERATORS.

      -- CHARACTER RELATIONAL OPERATORS.

      function ">" (X, Y : Character) return Boolean is
      begin
         return X = Y;
      end ">";

      function "<=" (X, Y : Character) return Boolean is
      begin
         return X = Y;
      end "<=";

   begin

      if Ident_Char ('C') > Ident_Char ('B') then
         Failed ("REDEFINITION OF CHARACTER "">"" IS DEFECTIVE");
      end if;

      if Ident_Char ('A') <= Ident_Char ('E') then
         Failed ("REDEFINITION OF CHARACTER ""<="" IS DEFECTIVE");
      end if;

   end;

   Result;

end C67003f;
