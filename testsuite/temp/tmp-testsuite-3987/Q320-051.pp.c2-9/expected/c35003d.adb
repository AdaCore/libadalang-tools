-- C35003D.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A FLOATING-POINT
--     SUBTYPE INDICATION WHEN THE LOWER OR UPPER BOUND OF A NON-NULL
--     RANGE LIES OUTSIDE THE RANGE OF THE TYPE MARK.

-- HISTORY:
--     JET 07/11/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C35003d is

   subtype Flt1 is Float range -100.0 .. 100.0;

begin
   Test
     ("C35003D",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A " &
      "FLOATING-POINT SUBTYPE INDICATION WHEN THE " &
      "LOWER OR UPPER BOUND OF A NON-NULL RANGE LIES " &
      "OUTSIDE THE RANGE OF THE TYPE MARK");
   begin
      declare
         subtype F is Flt1 range 0.0 .. 101.0 + Flt1 (Ident_Int (0));
      begin
         Failed ("NO EXCEPTION RAISED (F1)");
         declare
            Z : F := 1.0;
         begin
            if not Equal (Integer (Z), Integer (Z)) then
               Comment ("DON'T OPTIMIZE Z");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN WRONG PLACE (F1)");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED (F1)");
   end;

   begin
      declare
         subtype F is Flt1 range -101.0 .. 0.0;
      begin
         Failed ("NO EXCEPTION RAISED (F2)");
         declare
            Z : F := -1.0;
         begin
            if not Equal (Integer (Z), Integer (Z)) then
               Comment ("DON'T OPTIMIZE Z");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN WRONG PLACE (F2)");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED (F2)");
   end;

   Result;

end C35003d;
