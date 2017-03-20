-- C44003G.ADA

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
--     OPERATIONS ON BOOLEAN TYPES AND ONE-DIMENSIONAL ARRAYS WITH
--     COMPONENTS OF TYPE BOOLEAN.

-- HISTORY:
--     RJW 10/13/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C44003g is

begin
   Test
     ("C44003G",
      "CHECK FOR CORRECT PRECEDENCE OF PRE-DEFINED " &
      "AND OVERLOADED OPERATIONS ON BOOLEAN TYPES " &
      "AND ONE-DIMENSIONAL ARRAYS WITH COMPONENTS OF " &
      "TYPE BOOLEAN");

----- PREDEFINED BOOLEAN:

   declare
      T : Boolean := True;
      F : Boolean := False;

      function "AND" (Left, Right : Boolean) return Boolean is
      begin
         return False;
      end "AND";

      function "<" (Left, Right : Boolean) return Boolean is
      begin
         return True;
      end "<";

      function "-" (Left, Right : Boolean) return Boolean is
      begin
         return True;
      end "-";

      function "+" (Right : Boolean) return Boolean is
      begin
         return not Right;
      end "+";

      function "*" (Left, Right : Boolean) return Boolean is
      begin
         return False;
      end "*";

      function "**" (Left, Right : Boolean) return Boolean is
      begin
         return True;
      end "**";

   begin
      if not (+T = F) or
        T /= +F or
        (True and False**True) or
        not (+T < F) or
        not (T - F * T) or
        (not T - F xor +F - F)
      then
         Failed ("INCORRECT RESULT - 1");
      end if;

   end;

----- ARRAYS:

   declare
      type Arr is array (Integer range <>) of Boolean;

      subtype Sarr is Arr (1 .. 3);

      T : Sarr := (others => True);
      F : Sarr := (others => False);

      function "XOR" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => False);
      end "XOR";

      function "<=" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => True);
      end "<=";

      function "+" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => False);
      end "+";

      function "MOD" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => True);
      end "MOD";

      function "**" (Left, Right : Arr) return Arr is
      begin
         return (1 .. 3 => False);
      end "**";
   begin
      if (F**T <= F + T mod T xor T) /= (1 .. 3 => False) then
         Failed ("INCORRECT RESULT - 2");
      end if;

      if F**T & T /= not T & T or (T mod F <= T) /= (1 .. 3 => True) then
         Failed ("INCORRECT RESULT - 3");
      end if;
   end;

   Result;
end C44003g;
