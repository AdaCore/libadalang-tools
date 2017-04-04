-- C67002C.ADA

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
-- CHECK THAT ALL OPERATOR SYMBOLS CAN BE USED IN (OVERLOADED) FUNCTION
-- SPECIFICATIONS WITH THE REQUIRED NUMBER OF PARAMETERS. THIS TEST CHECKS
-- FORMAL SUBPROGRAM PARAMETERS.
--   SUBTESTS ARE:
--        (A) THROUGH (P): "=", "AND", "OR", "XOR", "<", "<=",
--             ">", ">=", "&", "*", "/", "MOD", "REM", "**", "+", "-",
--             RESPECTIVELY.  ALL OF THESE HAVE TWO PARAMETERS.
--        (Q), (R), (S), AND (T): "+", "-", "NOT", "ABS", RESPECTIVELY,
--             WITH ONE PARAMETER.

-- CPP 6/26/84

with Report; use Report;
procedure C67002c is

   function Two_Params (I1, I2 : Integer) return Character is
   begin
      if I1 > I2 then
         return 'G';
      else
         return 'L';
      end if;
   end Two_Params;

   function One_Param (I1 : Integer) return Character is
   begin
      if I1 < Ident_Int (0) then
         return 'N';
      else
         return 'P';
      end if;
   end One_Param;

begin
   Test
     ("C67002C",
      "USE OF OPERATOR SYMBOLS IN " & "(OVERLOADED) FUNCTION SPECIFICATIONS");

   -------------------------------------------------

   declare -- (A)

      package Equ is
         type Lp is limited private;
         function "=" (Lpa, Lpb : Lp) return Boolean;
      private
         type Lp is new Integer;
      end Equ;
      use Equ;

      Lp1, Lp2 : Lp;

      package body Equ is
         function "=" (Lpa, Lpb : Lp) return Boolean is
         begin
            return Lpa > Lpb;
         end "=";
      begin
         Lp1 := Lp (Ident_Int (7));
         Lp2 := Lp (Ident_Int (8));
      end Equ;

      generic
         with function "=" (Lpa, Lpb : Lp) return Boolean;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Lp1 = Lp2) or not (Lp2 = Lp1) or (Lp1 = Lp1) or (Lp2 /= Lp1) then
            Failed ("OVERLOADING OF ""="" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Equal is new Pkg ("=" => Equ."=");

   begin -- (A)
      null;
   end; -- (A)

   -------------------------------------------------

   declare -- (B)

      generic
         with function "AND" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) and 1) /= 'G' or (5 and 10) /= 'L' then
            Failed ("OVERLOADING OF ""AND"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("AND" => Two_Params);

   begin -- (B)
      null;
   end; -- (B)

   -------------------------------------------------

   declare -- (C)

      generic
         with function "OR" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) or 1) /= 'G' or (5 or 10) /= 'L' then
            Failed ("OVERLOADING OF ""OR"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("OR" => Two_Params);

   begin -- (C)
      null;
   end; -- (C)

   -------------------------------------------------

   declare -- (D)

      generic
         with function "XOR" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) xor 1) /= 'G' or (5 xor 10) /= 'L' then
            Failed ("OVERLOADING OF ""XOR"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("XOR" => Two_Params);

   begin -- (D)
      null;
   end; -- (D)

   -------------------------------------------------

   declare -- (E)

      generic
         with function "<" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) < 1) /= 'G' or (5 < 10) /= 'L' then
            Failed ("OVERLOADING OF ""<"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("<" => Two_Params);

   begin -- (E)
      null;
   end; -- (E)

   -------------------------------------------------

   declare -- (F)

      generic
         with function "<=" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) <= 1) /= 'G' or (5 <= 10) /= 'L' then
            Failed ("OVERLOADING OF ""<="" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("<=" => Two_Params);

   begin -- (F)
      null;
   end; -- (F)

   -------------------------------------------------

   declare -- (G)

      generic
         with function ">" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) > 1) /= 'G' or (5 > 10) /= 'L' then
            Failed ("OVERLOADING OF "">"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg (">" => Two_Params);

   begin -- (G)
      null;
   end; -- (G)

   -------------------------------------------------

   declare -- (H)

      generic
         with function ">=" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) >= 1) /= 'G' or (5 >= 10) /= 'L' then
            Failed ("OVERLOADING OF "">="" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg (">=" => Two_Params);

   begin -- (H)
      null;
   end; -- (H)

   -------------------------------------------------

   declare -- (I)

      generic
         with function "&" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) & 1) /= 'G' or (5 & 10) /= 'L' then
            Failed ("OVERLOADING OF ""&"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("&" => Two_Params);

   begin -- (I)
      null;
   end; -- (I)

   -------------------------------------------------

   declare -- (J)

      generic
         with function "*" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) * 1) /= 'G' or (5 * 10) /= 'L' then
            Failed ("OVERLOADING OF ""*"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("*" => Two_Params);

   begin -- (J)
      null;
   end; -- (J)

   -------------------------------------------------

   declare -- (K)

      generic
         with function "/" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) / 1) /= 'G' or (5 / 10) /= 'L' then
            Failed ("OVERLOADING OF ""/"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("/" => Two_Params);

   begin -- (K)
      null;
   end; -- (K)

   -------------------------------------------------

   declare -- (L)

      generic
         with function "MOD" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) mod 1) /= 'G' or (5 mod 10) /= 'L' then
            Failed ("OVERLOADING OF ""MOD"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("MOD" => Two_Params);

   begin -- (L)
      null;
   end; -- (L)

   -------------------------------------------------

   declare -- (M)

      generic
         with function "REM" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) rem 1) /= 'G' or (5 rem 10) /= 'L' then
            Failed ("OVERLOADING OF ""REM"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("REM" => Two_Params);

   begin -- (M)
      null;
   end; -- (M)

   -------------------------------------------------

   declare -- (N)

      generic
         with function "**" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10)**1) /= 'G' or (5**10) /= 'L' then
            Failed ("OVERLOADING OF ""**"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("**" => Two_Params);

   begin -- (N)
      null;
   end; -- (N)

   -------------------------------------------------

   declare -- (O)

      generic
         with function "+" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) + 1) /= 'G' or (5 + 10) /= 'L' then
            Failed ("OVERLOADING OF ""+"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("+" => Two_Params);

   begin -- (O)
      null;
   end; -- (O)

   -------------------------------------------------

   declare -- (P)

      generic
         with function "-" (I1, I2 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (Ident_Int (10) - 1) /= 'G' or (5 - 10) /= 'L' then
            Failed ("OVERLOADING OF ""-"" OPERATOR DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("-" => Two_Params);

   begin -- (P)
      null;
   end; -- (P)

   -------------------------------------------------

   declare -- (Q)

      generic
         with function "+" (I1 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (+Ident_Int (25) /= 'P') or (+(0 - 25) /= 'N') then
            Failed
              ("OVERLOADING OF ""+"" " & "OPERATOR (ONE OPERAND) DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("+" => One_Param);

   begin -- (Q)
      null;
   end; -- (Q)

   -------------------------------------------------

   declare -- (R)

      generic
         with function "-" (I1 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (-Ident_Int (25) /= 'P') or (-(0 - 25) /= 'N') then
            Failed
              ("OVERLOADING OF ""-"" " & "OPERATOR (ONE OPERAND) DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("-" => One_Param);

   begin -- (R)
      null;
   end; -- (R)

   -------------------------------------------------

   declare -- (S)

      generic
         with function "NOT" (I1 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (not Ident_Int (25) /= 'P') or (not (0 - 25) /= 'N') then
            Failed
              ("OVERLOADING OF ""NOT"" " & "OPERATOR (ONE OPERAND) DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("NOT" => One_Param);

   begin -- (S)
      null;
   end; -- (S)

   -------------------------------------------------

   declare -- (T)

      generic
         with function "ABS" (I1 : Integer) return Character;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if (abs Ident_Int (25) /= 'P') or (abs (0 - 25) /= 'N') then
            Failed
              ("OVERLOADING OF ""ABS"" " & "OPERATOR (ONE OPERAND) DEFECTIVE");
         end if;
      end Pkg;

      package Pack is new Pkg ("ABS" => One_Param);

   begin -- (T)
      null;
   end; -- (T)

   -------------------------------------------------

   Result;
end C67002c;
