-- C45411D.ADA

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
--     CHECK THAT UNARY "+" AND "-" YIELD CORRECT RESULTS FOR
--     OPERANDS OF DERIVED INTEGER TYPES.

-- HISTORY:
--     JET 07/11/88  CREATED ORIGINAL TEST.
--     PWN 10/27/95  REMOVED OUT OF RANGE STATIC VALUE CHECKS.

with Report; use Report;

procedure C45411d is

   type Int is range -100 .. 100;

   type Dt1 is new Integer;
   type Dt2 is new Int;

   D1 : Dt1 := 1;
   D2 : Dt2 := 1;

   function Ident (A : Dt1) return Dt1 is
   begin
      return A * Dt1 (Ident_Int (1));
   end Ident;

   function Ident (A : Dt2) return Dt2 is
   begin
      return A * Dt2 (Ident_Int (1));
   end Ident;

begin
   Test
     ("C45411D",
      "CHECK THAT UNARY ""+"" AND ""-"" YIELD " &
      "CORRECT RESULTS FOR OPERANDS OF DERIVED " &
      "INTEGER TYPES");

   for I in Dt1'(1 - 2) .. Dt1'(1) loop
      if "-" (Right => D1) /= Ident (I) then
         Failed ("INCORRECT RESULT FOR ""-"" DT1 -" & Dt1'Image (I + 2));
      end if;

      if +D1 /= Ident (D1) then
         Failed ("INCORRECT RESULT FOR ""+"" DT1 -" & Dt1'Image (I + 2));
      end if;
      D1 := D1 - 1;
   end loop;

   if Dt1'Last + Dt1'First = 0 then
      if Ident (-Dt1'Last) /= Dt1'First then
         Failed ("-DT1'LAST IS NOT EQUAL TO DT1'FIRST");
      end if;
   else
      if Ident (-Dt1'Last) /= Dt1'First + 1 then
         Failed ("-DT1'LAST IS NOT EQUAL TO DT1'FIRST+1");
      end if;
   end if;

   for I in Dt2'(1 - 2) .. Dt2'(1) loop
      if -D2 /= Ident (I) then
         Failed ("INCORRECT RESULT FOR ""-"" DT2 -" & Dt2'Image (I + 2));
      end if;

      if "+" (Right => D2) /= Ident (D2) then
         Failed ("INCORRECT RESULT FOR ""+"" DT2 -" & Dt2'Image (I + 2));
      end if;
      D2 := D2 - 1;
   end loop;

   Result;

end C45411d;
