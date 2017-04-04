-- C87B14C.ADA

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
-- IN SUBTYPE INDICATIONS WITH INDEX CONSTRAINTS, THE LOWER AND UPPER
-- BOUNDS MUST BE OF THE INDEX BASE TYPE.
--
-- TEST (C): INDEX CONSTRAINTS WITH OVERLOADED INFIX OPERATORS.

-- TRH  30 JUNE 82

with Report; use Report;

procedure C87b14c is

   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type List is array (Day range <>) of Boolean;
   type Grid is array (Day range <>, Day range <>) of Boolean;
   subtype Weekend is Day range Sat .. Sun;
   subtype Weekday is Day range Mon .. Fri;

   function "*" (X, Y : Integer) return Weekday is
   begin
      return Mon;
   end "*";

   function "*" (X, Y : Integer) return Boolean is
   begin
      Failed
        ("RESOLUTION INCORRECT - INDEX CONSTRAINTS " &
         " IN SUBTYPE INDICATIONS");
      return True;
   end "*";

   function "+" (X, Y : Integer) return Weekend is
   begin
      return Sat;
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
     ("C87B14C",
      "OVERLOADED EXPRESSIONS IN INDEX CONSTRAINTS " &
      "OF SUBTYPE INDICATIONS");

   declare
      subtype List1 is List (Wed .. (0 + 0));
      subtype List2 is List (0 * 0 .. Tue);
      subtype List3 is List ((0 + 0) .. (0 + 0));
      subtype List4 is List ((0 * 0) .. (0 + 0));

      subtype Grid1 is Grid (Mon .. (0 * 0), (0 * 0) .. Tue);
      subtype Grid2 is Grid ((0 * 0) .. Wed, Fri .. (0 + 0));
      subtype Grid3 is Grid ((0 * 0) .. (0 * 0), (0 + 0) .. (0 + 0));
      subtype Grid4 is Grid ((0 * 0) .. (0 + 0), Tue .. Thu);

   begin
      null;
   end;

   Result;
end C87b14c;
