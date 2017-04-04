-- C58004D.ADA

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
-- CHECK THAT A RETURN STATEMENT TERMINATES EXECUTION
--    OF THE INNERMOST ENCLOSING SUBPROGRAM.

-- CHECKS GENERIC SUBPROGRAMS.

-- SPS 3/7/83
-- JRK 1/31/84

with Report;
procedure C58004d is

   use Report;

   I1, I2 : Integer;

   generic
   procedure Addm (Ia1 : in out Integer; Ia2 : in Integer);

   procedure Addm (Ia1 : in out Integer; Ia2 : in Integer) is

      generic
      procedure Mult (Im1 : in out Integer; Im2 : in Integer);

      procedure Mult (Im1 : in out Integer; Im2 : in Integer) is
      begin
         Im1 := Im1 * Im2;

         if Im1 > 0 then
            return;
         end if;

         Im1 := 0;
      end Mult;

      procedure Mlt is new Mult;

   begin
      Mlt (Ia1, Ia2);
      Ia1 := Ia1 + Ia2;

      if Ia1 > 0 then
         return;
      end if;

      Ia1 := 0;
   end Addm;

   procedure Adm is new Addm;

begin
   Test
     ("C58004D",
      "CHECK THAT RETURN TERMINATES EXECUTION OF ONLY" &
      " THE INNERMOST ENCLOSING GENERIC SUBPROGRAM");

   I1 := 2;
   I2 := 3;
   Adm (I1, I2);     -- SAME AS I1 := (I1 * I2) + I2

   if I1 = 0 then
      Failed ("RETURN DOES NOT TERMINATE SUBPROGRAM");
   elsif I1 = 6 then
      Failed ("RETURN TERMINATES ALL SUBPROGRAMS NOT JUST INNERMOST");
   elsif I1 /= 9 then
      Failed ("RETURN STATEMENT NOT WORKING CORRECTLY");
   end if;

   Result;
end C58004d;
