-- C44003D.ADA

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
--     CHECK FOR CORRECT PRECEDENCE OF PREDEFINED AND OVERLOADED
--     OPERATIONS ON PREDEFINED TYPE FLOAT, USER-DEFINED TYPES, AND
--     ONE-DIMENSIONAL ARRAYS WITH COMPONENTS OF TYPE FLOAT.

-- HISTORY:
--     RJW 10/13/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C44003d is

begin
   Test
     ("C44003D",
      "CHECK FOR CORRECT PRECEDENCE OF PREDEFINED " &
      "AND OVERLOADED OPERATIONS ON PREDEFINED TYPE " &
      "FLOAT, USER-DEFINED TYPES, AND ONE-DIMEN" &
      "SIONAL ARRAYS WITH COMPONENTS OF TYPE FLOAT");

----- PREDEFINED FLOAT:

   declare
      F1 : Float := 1.0;
      F2 : Float := 2.0;
      F5 : Float := 5.0;

      function "OR" (Left, Right : Float) return Float is
      begin
         return 4.5;
      end "OR";

      function "<" (Left, Right : Float) return Float is
      begin
         return 5.5;
      end "<";

      function "-" (Left, Right : Float) return Float is
      begin
         return 6.5;
      end "-";

      function "+" (Right : Float) return Float is
      begin
         return 7.5;
      end "+";

      function "*" (Left, Right : Float) return Float is
      begin
         return 8.5;
      end "*";

      function "NOT" (Right : Float) return Float is
      begin
         return 9.5;
      end "NOT";

   begin
      if not
        (-abs F1 + F2 / F1 + F5**2 = 26.0 and F1 > 0.0 and -F2 * F2**3 = -8.5)
      then
         Failed ("INCORRECT RESULT - 1");
      end if;

      if (F1 or not F2 < F1 - F5 * F5**3) /= 4.5 then
         Failed ("INCORRECT RESULT - 2");
      end if;
   end;

----- USER-DEFINED TYPE:

   declare
      type Usr is digits 5;

      F1 : Usr := 1.0;
      F2 : Usr := 2.0;
      F5 : Usr := 5.0;

      function "AND" (Left, Right : Usr) return Usr is
      begin
         return 4.5;
      end "AND";

      function ">=" (Left, Right : Usr) return Usr is
      begin
         return 5.5;
      end ">=";

      function "+" (Left, Right : Usr) return Usr is
      begin
         return 6.5;
      end "+";

      function "-" (Right : Usr) return Usr is
      begin
         return 7.5;
      end "-";

      function "/" (Left, Right : Usr) return Usr is
      begin
         return 8.5;
      end "/";

      function "**" (Left, Right : Usr) return Usr is
      begin
         return 9.5;
      end "**";
   begin
      if +F5 - F2 * F1**2 /= 3.0 or abs F1 <= 0.0 or -F2 * F2**3.0 /= 7.5 then
         Failed ("INCORRECT RESULT - 3");
      end if;

      if (F1 and F2 >= F1 + F5 / F5**3) /= 4.5 then
         Failed ("INCORRECT RESULT - 4");
      end if;
   end;

----- ARRAYS:

   declare
      type Arr is array (Integer range <>) of Float;

      subtype Sarr is Arr (1 .. 3);

      F1 : Sarr := (others => 1.0);
      F2 : Sarr := (others => 2.0);
      F5 : Sarr := (others => 5.0);

      function "XOR" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => 4.5);
      end "XOR";

      function "<=" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => 5.5);
      end "<=";

      function "&" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => 6.5);
      end "&";

      function "MOD" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => 8.5);
      end "MOD";

      function "ABS" (Right : Arr) return Arr is
      begin
         return (1 .. 3 => 9.5);
      end "ABS";
   begin
      if (abs F1 <= F2 & F5 mod F1 xor F1) /= (1 .. 3 => 4.5) then
         Failed ("INCORRECT RESULT - 5");
      end if;

      if (abs F1 & F2) /= (1 .. 3 => 6.5) or
        (F1 mod F2 <= F5) /= (1 .. 3 => 5.5) then
         Failed ("INCORRECT RESULT - 6");
      end if;
   end;

   Result;
end C44003d;
