-- C59002A.ADA

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
-- CHECK THAT JUMPS OUT OF AN EXCEPTION HANDLER CONTAINED IN A BLOCK
--    TO A STATEMENT IN AN ENCLOSING UNIT ARE ALLOWED AND ARE PERFORMED
--    CORRECTLY.

-- RM 05/22/81
-- SPS 3/8/83

with Report;
procedure C59002a is

   use Report;

begin

   Test
     ("C59002A",
      "CHECK THAT JUMPS OUT OF EXCEPTION HANDLERS" & " ARE ALLOWED");

   declare

      Flow  : Integer              := 1;
      Expon : Integer range 0 .. 3 := 0;

   begin

      goto Start;

      Failed ("'GOTO' NOT OBEYED");

      <<Back_Label>>
      Flow  := Flow * 3**Expon;                    -- 1*5*9
      Expon := Expon + 1;
      goto Finish;

      <<Start>>
      Flow  := Flow * 7**Expon;                    -- 1
      Expon := Expon + 1;

      declare
      begin
         raise Constraint_Error;
         Failed ("EXCEPTION NOT RAISED  -  1");
      exception
         when Constraint_Error =>
            goto Forward_Label;
      end;

      Failed ("INNER 'GOTO' NOT OBEYED  -  1");

      <<Forward_Label>>
      Flow  := Flow * 5**Expon;                    -- 1*5
      Expon := Expon + 1;

      declare
      begin
         raise Constraint_Error;
         Failed ("EXCEPTION NOT RAISED  -  2");
      exception
         when Constraint_Error =>
            goto Back_Label;
      end;

      Failed ("INNER 'GOTO' NOT OBETED  -  2");

      <<Finish>>
      Flow := Flow * 2**Expon;                    -- 1*5*9*8

      if Flow /= 360 then
         Failed ("WRONG FLOW OF CONTROL");
      end if;

   end;

   Result;

end C59002a;
