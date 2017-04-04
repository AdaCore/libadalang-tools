-- C45303A.ADA

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
-- CHECK THAT ADDITION AND SUBTRACTION YIELD RESULTS BELONGING TO THE BASE
-- TYPE.

-- JBG 2/24/84
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST. JRL 10/13/96 Fixed static
-- expressions which contained values outside
--              the base range.

with Report; use Report;
procedure C45303a is

   type Int is range 1 .. 10;

   X, Y : Int := Int (Ident_Int (9));

begin

   Test ("C45303A", "CHECK SUBTYPE OF INTEGER ADDITION/SUBTRACTION");

   begin

      if X + Y - 10 /= Int (Ident_Int (8)) then
         Failed ("INCORRECT RESULT - ADDITION");
      end if;

   exception

      when Constraint_Error =>
         if Int'Pos (Int'Base'Last) >= 18 then
            Failed
              ("ADDITION DOES NOT YIELD RESULT " &
               "BELONGING TO THE BASE TYPE");
         else
            Comment ("BASE TYPE HAS RANGE LESS THAN 18 - ADD");
         end if;
   end;

   begin

      if 2 - X - Int (Ident_Int (1)) /= Int'Val (Ident_Int (-8)) then
         Failed ("INCORRECT RESULT - SUBTRACTION");
      end if;

   exception

      when Constraint_Error =>
         if Int'Pos (Int'Base'First) <= -8 then
            Failed
              ("SUBTRACTION DOES NOT YIELD RESULT " &
               "BELONGING TO THE BASE TYPE");
         else
            Comment ("BASE TYPE HAS RANGE GREATER THAN -8 - SUB");
         end if;
   end;

   Result;

end C45303a;
