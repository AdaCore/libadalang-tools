-- C35504B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED FOR I'SUCC, I'PRED,
-- I'POS, I'VAL, I'IMAGE, AND I'VALUE FOR INTEGER ARGUMENTS
-- OUTSIDE THE RANGE OF I.

-- DAT 3/30/81
-- SPS 01/13/83

with Report; use Report;

procedure C35504b is

   subtype I is Integer range 0 .. 0;

begin
   Test
     ("C35504B",
      "CONSTRAINT_ERROR IS NOT RAISED FOR" &
      " INTEGER SUBTYPE ATTRIBUTES 'SUCC, 'PRED, 'POS, 'VAL," &
      " 'IMAGE, AND 'VALUE WHOSE ARGUMENTS ARE OUTSIDE THE" &
      " SUBTYPE");

   begin
      if I'Succ (-1) /= I'Pred (1) then
         Failed ("WRONG ATTRIBUTE VALUE - 1");
      end if;

      if I'Succ (100) /= 101 then
         Failed ("WRONG ATTRIBUTE VALUE - 2");
      end if;

      if I'Pred (100) /= 99 then
         Failed ("WRONG ATTRIBUTE VALUE - 3");
      end if;

      if I'Pos (-100) /= -100 then
         Failed ("WRONG ATTRIBUTE VALUE - 4");
      end if;

      if I'Val (-100) /= -100 then
         Failed ("WRONG ATTRIBUTE VALUE - 5");
      end if;

      if I'Image (1_234) /= " 1234" then
         Failed ("WRONG ATTRIBUTE VALUE - 6");
      end if;

      if I'Value ("999") /= 999 then
         Failed ("WRONG ATTRIBUTE VALUE - 7");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED");
   end;

   Result;
end C35504b;
