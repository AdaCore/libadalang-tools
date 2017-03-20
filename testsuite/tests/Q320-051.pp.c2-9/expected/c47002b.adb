-- C47002B.ADA

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
-- CHECK THAT VALUES BELONGING TO EACH CLASS OF TYPE CAN BE WRITTEN AS
-- THE OPERANDS OF QUALIFIED EXPRESSIONS.
-- THIS TEST IS FOR REAL TYPES.

-- RJW 7/23/86

with Report; use Report;
procedure C47002b is

begin

   Test
     ("C47002B",
      "CHECK THAT VALUES HAVING REAL TYPES " &
      "CAN BE WRITTEN AS THE OPERANDS OF " &
      "QUALIFIED EXPRESSIONS");

   declare -- FLOATING POINT TYPES.

      type Results is (Fl1, Fl2, Fl3);

      type Flt is digits 3 range -5.0 .. 5.0;

      type Nflt is new Float;

      function F (Fl : Flt) return Results is
      begin
         return Fl1;
      end F;

      function F (Fl : Nflt) return Results is
      begin
         return Fl2;
      end F;

      function F (Fl : Float) return Results is
      begin
         return Fl3;
      end F;

   begin
      if F (Flt'(0.0)) /= Fl1 then
         Failed ("INCORRECT RESULTS FOR TYPE FLT");
      end if;

      if F (Nflt'(0.0)) /= Fl2 then
         Failed ("INCORRECT RESULTS FOR TYPE NFLT");
      end if;

      if F (Float'(0.0)) /= Fl3 then
         Failed ("INCORRECT RESULTS FOR TYPE FLOAT");
      end if;
   end;

   declare -- FIXED POINT TYPES.

      type Results is (Fi1, Fi2, Fi3);

      type Fixed is delta 0.5 range -5.0 .. 5.0;

      type Nfix is new Fixed;

      function F (Fi : Fixed) return Results is
      begin
         return Fi1;
      end F;

      function F (Fi : Nfix) return Results is
      begin
         return Fi2;
      end F;

      function F (Fi : Duration) return Results is
      begin
         return Fi3;
      end F;

   begin
      if F (Fixed'(0.0)) /= Fi1 then
         Failed ("INCORRECT RESULTS FOR TYPE FIXED");
      end if;

      if F (Nfix'(0.0)) /= Fi2 then
         Failed ("INCORRECT RESULTS FOR TYPE NFIX");
      end if;

      if F (Duration'(0.0)) /= Fi3 then
         Failed ("INCORRECT RESULTS FOR TYPE DURATION");
      end if;
   end;

   Result;
end C47002b;
