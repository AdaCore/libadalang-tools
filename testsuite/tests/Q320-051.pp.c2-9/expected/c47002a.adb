-- C47002A.ADA

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
-- CHECK THAT VALUES BELONGING TO EACH CLASS OF TYPE CAN BE WRITTEN AS THE
-- OPERANDS OF QUALIFIED EXPRESSIONS. THIS TEST IS FOR DISCRETE TYPES.

-- RJW 7/23/86

with Report; use Report;
procedure C47002a is

begin

   Test
     ("C47002A",
      "CHECK THAT VALUES HAVING DISCRETE TYPES " &
      "CAN BE WRITTEN AS THE OPERANDS OF " &
      "QUALIFIED EXPRESSIONS");

   declare  -- ENUMERATION TYPES.

      type Week is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      type Weekend is (Sat, Sun);

      type Char is ('B', 'A');

      type Mybool is (True, False);

      type Nbool is new Boolean;

   begin
      if Weekend'(Sat) >= Sun then
         Failed ("INCORRECT RESULTS FOR TYPE WEEKEND");
      end if;

      if Char'('B') >= 'A' then
         Failed ("INCORRECT RESULTS FOR TYPE CHAR");
      end if;

      if Mybool'(True) >= False then
         Failed ("INCORRECT RESULTS FOR TYPE MYBOOL");
      end if;

      if Nbool'(True) <= False then
         Failed ("INCORRECT RESULTS FOR TYPE NBOOL");
      end if;
   end;

   declare -- INTEGER TYPES.

      type Results is (Int1, Int2, Int3);

      type Newint is new Integer;

      type Int is range -10 .. 10;

      function F (I : Newint) return Results is
      begin
         return Int1;
      end F;

      function F (I : Int) return Results is
      begin
         return Int2;
      end F;

      function F (I : Integer) return Results is
      begin
         return Int3;
      end F;

   begin
      if F (Newint'(5)) /= Int1 then
         Failed ("INCORRECT RESULTS FOR TYPE NEWINT");
      end if;

      if F (Int'(5)) /= Int2 then
         Failed ("INCORRECT RESULTS FOR TYPE INT");
      end if;

      if F (Integer'(5)) /= Int3 then
         Failed ("INCORRECT RESULTS FOR TYPE INTEGER");
      end if;
   end;

   Result;
end C47002a;
