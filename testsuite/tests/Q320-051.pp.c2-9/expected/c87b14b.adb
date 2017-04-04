-- C87B14B.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- IN SUBTYPE INDICATIONS WITH INDEX CONSTRAINTS, THE LOWER AND UPPER BOUNDS
-- MUST BE OF THE INDEX BASE TYPE.
--
-- TEST (B): INDEX CONSTRAINTS WITH OVERLOADED OPERATOR SYMBOLS.

-- TRH  30 JUNE 82

with Report; use Report;

procedure C87b14b is

   subtype Char is Character;
   subtype Var is Char range 'X' .. 'Z';
   subtype Note is Char range 'A' .. 'G';
   type List is array (Char range <>) of Char;
   type Grid is array (Char range <>, Char range <>) of Char;

   function "*" (X, Y : Integer) return Var is
   begin
      return 'X';
   end "*";

   function "*" (X, Y : Integer) return Boolean is
   begin
      Failed
        ("RESOLUTION INCORRECT - INDEX CONSTRAINTS " &
         " IN SUBTYPE INDICATIONS");
      return True;
   end "*";

   function "+" (X, Y : Integer) return Note is
   begin
      return 'A';
   end "+";

   function "+" (X, Y : Integer) return Float is
   begin
      Failed
        ("RESOLUTION INCORRECT - INDEX CONSTRAINTS " &
         " IN SUBTYPE INDICATIONS");
      return 2.0;
   end "+";

begin
   Test
     ("C87B14B",
      "OVERLOADED OPERATOR SYMBOLS IN INDEX " &
      "CONSTRAINTS OF SUBTYPE INDICATIONS");

   declare

      subtype List1 is List ('W' .. "*" (0, 0));
      subtype List2 is List ("+" (0, 0) .. 'C');
      subtype List3 is List ("+" (0, 0) .. "*" (0, 0));
      subtype List4 is List ("*" (0, 0) .. "*" (0, 0));

      subtype Grid1 is Grid ('V' .. "*" (0, 0), "*" (0, 0) .. 'Y');
      subtype Grid2 is Grid ("*" (0, 0) .. 'W', 'H' .. "+" (0, 0));
      subtype Grid3 is
        Grid ("*" (0, 0) .. "*" (0, 0), "+" (0, 0) .. "+" (0, 0));
      subtype Grid4 is Grid ("+" (0, 0) .. "*" (0, 0), 'L' .. 'N');

   begin
      null;
   end;

   Result;
end C87b14b;
