-- C87B40A.ADA

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
--
-- THE SAME OPERATIONS ARE PREDEFINED FOR THE TYPE UNIVERSAL_INTEGER
-- AS FOR ANY INTEGER TYPE. THE SAME OPERATIONS ARE PREDEFINED FOR THE
-- TYPE UNIVERSAL_REAL AS FOR ANY FLOATING POINT TYPE. IN ADDITION
-- THESE OPERATIONS INCLUDE THE FOLLOWING MULTIPLICATION AND DIVISION
-- OPERATORS:
--
--   "*" (UNIVERSAL_REAL, UNIVERSAL_INTEGER) RETURN UNIVERSAL_REAL
--   "*" (UNIVERSAL_INTEGER, UNIVERSAL_REAL) RETURN UNIVERSAL_REAL
--   "*" (UNIVERSAL_REAL,    UNIVERSAL_REAL) RETURN UNIVERSAL_REAL
--   "*" (UNIVERSAL_INTEGER, UNIVERSAL_INTEGER) RETURN UNIVERSAL_INTEGER
--   "/" (UNIVERSAL_REAL, UNIVERSAL_INTEGER) RETURN UNIVERSAL_REAL
--  "**" (UNIVERSAL_INTEGER, INTEGER) RETURN UNIVERSAL_INTEGER
--  "**" (UNIVERSAL_REAL, INTEGER) RETURN UNIVERSAL_REAL
-- "MOD" (UNIVERSAL_INTEGER, UNIVERSAL_INTEGER) RETURN UNIVERSAL_INTEGER
-- "DIV" (UNIVERSAL_INTEGER, UNIVERSAL_INTEGER) RETURN UNIVERSAL_INTEGER
-- "ABS" (UNIVERSAL_INTEGER) RETURN UNIVERSAL INTEGER
-- "ABS" (UNIVERSAL_REAL) RETURN UNIVERSAL_REAL

-- TRH  15 SEPT 82

with Report; use Report;

procedure C87b40a is

   Err : Boolean                    := False;
   B   : array (1 .. 12) of Boolean := (1 .. 12 => True);

   function "-" (X : Integer) return Integer renames Standard."+";

   function "+" (X : Integer) return Integer is
   begin
      Err := True;
      return X;
   end "+";

   function "+" (X : Float) return Float is
   begin
      Err := True;
      return X;
   end "+";

begin
   Test ("C87B40A", "OVERLOADING RESOLUTION OF UNIVERSAL " & "EXPRESSIONS");

   B (1) := 1.0 * (+1) in 0.0 .. 0.0;       -- 1.0 * 1
   B (2) := (+1) * 1.0 in 0.0 .. 0.0;       -- 1 * 1.0
   B (3) := 1.0 / (+1) in 0.0 .. 0.0;       -- 1.0 / 1
   B (4) := (+1) + (+1) <= (+1) - (+1); -- 1+1< 1 - 1
   B (5) := (+1) * (+1) > (+1) / (+1); -- 1*1 > 1/1
   B (6) := (+1) mod (+1) /= (+1) rem (+1); -- 1 MOD 1 /= 1 REM 1

   begin
      B (7) := (+2)**(-2) < "-" (-1);     -- 2**2 < 1
   exception
      when Constraint_Error =>
         Failed ("INCORRECT RESOLUTION FOR INTEGER EXPONENT - 7");
   end;

   B (8)  := (+1) rem (+1) > "ABS" (+1);     -- 1 REM 1 > ABS 1
   B (9)  := (+1.0) + (+1.0) <= (+1.0) - (+1.0); -- 2.0 <= 0.0
   B (10) := (+1.0) * (+1.0) > (+1.0) / (+1.0); -- 1.0 > 1.0
   B (11) := (+2.0)**(-1) < "-" (-1.0);       -- 2.0 < 1.0
   B (12) := (+2.0)**(-1) <= "ABS" (+1.0);       -- 2.0 <= 1.0

   for I in B'Range loop
      if B (I) /= False then
         Failed
           ("RESOLUTION OR OPERATIONS INCORRECT FOR " &
            "UNIVERSAL EXPRESSIONS - " &
            Integer'Image (I));
      end if;
   end loop;

   if Err then
      Failed ("RESOLUTION INCORRECT FOR UNIVERSAL EXPRESSIONS");
   end if;

   Result;
end C87b40a;
