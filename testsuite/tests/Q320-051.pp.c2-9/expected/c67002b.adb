-- C67002B.ADA

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
-- CHECK THAT OPERATOR SYMBOLS CAN BE USED IN (OVERLOADED)
--   FUNCTION SPECIFICATIONS WITH THE REQUIRED NUMBER OF PARAMETERS.
--   THIS TEST CHECKS THE CASE OF CERTAIN OPERATOR SYMBOLS.
--   SUBTESTS ARE:
--        (A) THROUGH (E): "AND", "OR", "XOR", "MOD", "REM"
--            RESPECTIVELY.  ALL OF THESE HAVE TWO PARAMETERS.
--        (F) AND (G): "NOT" AND "ABS", RESPECTIVELY,
--             WITH ONE PARAMETER.

-- CPP 6/26/84

with Report;
procedure C67002b is

   use Report;

begin
   Test
     ("C67002B",
      "USE OF OPERATOR SYMBOLS IN " & "(OVERLOADED) FUNCTION SPECIFICATIONS");

   -------------------------------------------------

   declare -- (A)
      function "And" (I1, I2 : Integer) return Character is
      begin
         if I1 > I2 then
            return 'G';
         else
            return 'L';
         end if;
      end "And";

   begin -- (A)
      if (Ident_Int (10) and 1) /= 'G' or (5 and 10) /= 'L' then
         Failed ("OVERLOADING OF ""And"" OPERATOR DEFECTIVE");
      end if;
   end; -- (A)

   -------------------------------------------------

   declare -- (B)
      function "or" (I1, I2 : Integer) return Character is
      begin
         if I1 > I2 then
            return 'G';
         else
            return 'L';
         end if;
      end "or";

   begin -- (B)
      if (Ident_Int (10) or 1) /= 'G' or (5 or 10) /= 'L' then
         Failed ("OVERLOADING OF ""or"" OPERATOR DEFECTIVE");
      end if;
   end; -- (B)

   -------------------------------------------------

   declare -- (C)
      function "xOR" (I1, I2 : Integer) return Character is
      begin
         if I1 > I2 then
            return 'G';
         else
            return 'L';
         end if;
      end "xOR";

   begin -- (C)
      if (Ident_Int (10) xor 1) /= 'G' or (5 xor 10) /= 'L' then
         Failed ("OVERLOADING OF ""xOR"" OPERATOR DEFECTIVE");
      end if;
   end; -- (C)

   -------------------------------------------------

   declare -- (D)
      function "mOd" (I1, I2 : Integer) return Character is
      begin
         if I1 > I2 then
            return 'G';
         else
            return 'L';
         end if;
      end "mOd";

   begin -- (D)
      if (Ident_Int (10) mod 1) /= 'G' or (5 mod 10) /= 'L' then
         Failed ("OVERLOADING OF ""mOd"" OPERATOR DEFECTIVE");
      end if;
   end; -- (D)

   -------------------------------------------------

   declare -- (E)
      function "REM" (I1, I2 : Integer) return Character is
      begin
         if I1 > I2 then
            return 'G';
         else
            return 'L';
         end if;
      end "REM";

   begin -- (E)
      if (Ident_Int (10) rem 1) /= 'G' or (5 rem 10) /= 'L' then
         Failed ("OVERLOADING OF ""REM"" OPERATOR DEFECTIVE");
      end if;
   end; -- (E)

   -------------------------------------------------

   declare -- (F)
      function "NOT" (I1 : Integer) return Character is
      begin
         if I1 < Ident_Int (0) then
            return 'N';
         else
            return 'P';
         end if;
      end "NOT";

   begin -- (F)
      if (not Ident_Int (25) /= 'P') or (not (0 - 25) /= 'N') then
         Failed
           ("OVERLOADING OF ""NOT"" " & "OPERATOR (ONE OPERAND) DEFECTIVE");
      end if;
   end; -- (F)

   -------------------------------------------------

   declare -- (G)
      function "ABS" (I1 : Integer) return Character is
      begin
         if I1 < Ident_Int (0) then
            return 'N';
         else
            return 'P';
         end if;
      end "ABS";

   begin -- (G)
      if (abs Ident_Int (25) /= 'P') or (abs (0 - 25) /= 'N') then
         Failed
           ("OVERLOADING OF ""ABS"" " & "OPERATOR (ONE OPERAND) DEFECTIVE");
      end if;
   end; -- (T)

   -------------------------------------------------

   Result;
end C67002b;
