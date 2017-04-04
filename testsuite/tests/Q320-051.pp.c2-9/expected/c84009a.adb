-- C84009A.ADA

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
--     CHECK THAT A USE CLAUSE MAKES AN IMPLICITLY OR EXPLICITLY
--     DECLARED OPERATOR DIRECTLY VISIBLE IF NO HOMOGRAPH OF THE
--     OPERATOR IS ALREADY DIRECTLY VISIBLE.

-- HISTORY:
--     JET 03/10/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C84009a is

   type Int is new Integer range -100 .. 100;

   package Pack is
      function "+" (Left : Integer; Right : Int) return Integer;
      function "-" (Left, Right : Int) return Int;
      function "-" (Right : Int) return Integer;
      function "+" (Right : Int) return Integer;
   end Pack;

   function "+" (Right : Int) return Integer is
   begin
      return Integer'(1) + Integer (Right);
   end "+";

   package body Pack is
      function "+" (Left : Integer; Right : Int) return Integer is
      begin
         return Left + Integer (Right);
      end "+";

      function "-" (Left, Right : Int) return Int is
      begin
         Failed ("BINARY ""-"" ALREADY VISIBLE FOR TYPE INT");
         return Left + (-Right);
      end "-";

      function "-" (Right : Int) return Integer is
      begin
         return Integer'(0) - Integer (Right);
      end "-";

      function "+" (Right : Int) return Integer is
      begin
         Failed ("UNARY ""+"" ALREADY VISIBLE FOR TYPE INT");
         return Integer'(0) + Integer (Right);
      end "+";
   end Pack;

   use Pack;

begin
   Test
     ("C84009A",
      "CHECK THAT A USE CLAUSE MAKES AN IMPLICITLY " &
      "OR EXPLICITLY DECLARED OPERATOR DIRECTLY " &
      "VISIBLE IF NO HOMOGRAPH OF THE OPERATOR IS " &
      "ALREADY DIRECTLY VISIBLE");

   if Integer'(10) + Int'(10) /= Ident_Int (20) then
      Failed ("INCORRECT RESULT FROM BINARY ""+""");
   end if;

   if Int'(5) - Int'(3) /= Int'(2) then
      Failed ("INCORRECT RESULT FROM BINARY ""-""");
   end if;

   if -Int'(20) /= Ident_Int (-Integer'(20)) then
      Failed ("INCORRECT RESULT FROM UNARY ""-""");
   end if;

   if +Int'(20) /= Ident_Int (+Integer'(21)) then
      Failed ("INCORRECT RESULT FROM UNARY ""+""");
   end if;

   Result;
end C84009a;
