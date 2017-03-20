-- C87B14A.ADA

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
-- TEST (A): INDEX CONSTRAINTS WITH OVERLOADED FUNCTIONS.

-- TRH  30 JUNE 82

with Report; use Report;

procedure C87b14a is

   subtype Whole is Integer range 0 .. Integer'Last;
   subtype Base10 is Integer range 0 .. 9;
   type List is array (Integer range <>) of Boolean;
   type Grid is array (Integer range <>, Integer range <>) of Boolean;

   function F1 return Whole is
   begin
      return 1;
   end F1;

   function F1 return Boolean is
   begin
      Failed
        ("RESOLUTION INCORRECT - INDEX CONSTRAINTS " &
         " IN SUBTYPE INDICATIONS");
      return True;
   end F1;

   function F2 return Base10 is
   begin
      return 2;
   end F2;

   function F2 return Float is
   begin
      Failed
        ("RESOLUTION INCORRECT - INDEX CONSTRAINTS " &
         " IN SUBTYPE INDICATIONS");
      return 2.0;
   end F2;

begin
   Test
     ("C87B14A",
      "OVERLOADED EXPRESSIONS IN INDEX CONSTRAINTS " &
      "OF SUBTYPE INDICATIONS");

   declare
      subtype List1 is List (1 .. F1);
      subtype List2 is List (F1 .. 1);
      subtype List3 is List (F2 .. F2);
      subtype List4 is List (F1 .. F2);

      subtype Grid1 is Grid (1 .. F1, F1 .. 1);
      subtype Grid2 is Grid (F1 .. 2, 2 .. F2);
      subtype Grid3 is Grid (F1 .. F1, F2 .. F2);
      subtype Grid4 is Grid (F1 .. F2, 1 .. 2);

   begin
      null;
   end;

   Result;
end C87b14a;
