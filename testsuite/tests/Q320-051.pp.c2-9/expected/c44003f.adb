-- C44003F.ADA

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
--     CHECK FOR CORRECT PRECEDENCE OF PRE-DEFINED AND OVERLOADED
--     OPERATIONS ON ENUMERATION TYPES OTHER THAN BOOLEAN OR CHARACTER
--     AND ONE-DIMENSIONAL ARRAYS WITH COMPONENTS OF SUCH TYPES.

-- HISTORY:
--     RJW 10/13/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C44003f is

   type Enum is (Zero, One, Two, Three, Four, Five);

begin
   Test
     ("C44003F",
      "CHECK FOR CORRECT PRECEDENCE OF PRE-DEFINED " &
      "AND OVERLOADED OPERATIONS ON ENUMERATION " &
      "TYPES OTHER THAN BOOLEAN OR CHARACTER AND " &
      "ONE-DIMENSIONAL ARRAYS WITH COMPONENTS OF " & "SUCH TYPES");

----- ENUMERATION TYPE:

   declare
      E1 : Enum := One;
      E2 : Enum := Two;
      E5 : Enum := Five;

      function "AND" (Left, Right : Enum) return Enum is
      begin
         return Zero;
      end "AND";

      function "<" (Left, Right : Enum) return Enum is
      begin
         return Three;
      end "<";

      function "-" (Left, Right : Enum) return Enum is
      begin
         return Enum'Val (Enum'Pos (Left) - Enum'Pos (Right));
      end "-";

      function "+" (Right : Enum) return Enum is
      begin
         return Right;
      end "+";

      function "*" (Left, Right : Enum) return Enum is
      begin
         return Enum'Val (Enum'Pos (Left) * Enum'Pos (Right));
      end "*";

      function "**" (Left, Right : Enum) return Enum is
      begin
         return Enum'Val (Enum'Pos (Left)**Enum'Pos (Right));
      end "**";

   begin
      if not (+E1 < E2) or not (E2 >= +E2) or not (E5 = +Five) then
         Failed ("INCORRECT RESULT - 1");
      end if;

      if (E5**E1 and E2) /= (E5 - E1 * E5**E1) then
         Failed ("INCORRECT RESULT - 2");
      end if;

   end;

----- ARRAYS:

   declare
      type Arr is array (Integer range <>) of Enum;

      subtype Sarr is Arr (1 .. 3);

      E1 : Sarr := (others => One);
      E2 : Sarr := (others => Two);
      E5 : Sarr := (others => Five);

      function "XOR" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => Zero);
      end "XOR";

      function "<=" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => Three);
      end "<=";

      function "+" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => Zero);
      end "+";

      function "MOD" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => Three);
      end "MOD";

      function "**" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => Four);
      end "**";
   begin
      if (E5**E1 <= E2 + E5 mod E1 xor E1) /= (1 .. 3 => Zero) then
         Failed ("INCORRECT RESULT - 3");
      end if;

      if (E5**E1 & E2) /= (Four, Four, Four, Two, Two, Two) or
        (E1 mod E2 <= E5) /= (1 .. 3 => Three) then
         Failed ("INCORRECT RESULT - 4");
      end if;
   end;

   Result;

end C44003f;
