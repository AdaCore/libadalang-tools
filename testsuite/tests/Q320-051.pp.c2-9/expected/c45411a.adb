-- C45411A.ADA

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
--     PREDEFINED INTEGER OPERANDS.

-- HISTORY:
--     JET 01/25/88  CREATED ORIGINAL TEST.
--     PWN 10/27/95  REMOVED OUT OF RANGE STATIC VALUE CHECKS.

with Report; use Report;

procedure C45411a is

   type Dt is new Integer range -3 .. 3;
   I1 : Integer := 1;
   D1 : Dt      := 1;

begin
   Test
     ("C45411A",
      "CHECK THAT UNARY ""+"" AND ""-"" YIELD " &
      "CORRECT RESULTS FOR PREDEFINED INTEGER " & "OPERANDS");

   for I in (1 - 2) .. Integer (1) loop
      if "-" (Right => I1) /= Ident_Int (I) then
         Failed ("INCORRECT RESULT FOR ""-"" -" & Integer'Image (I + 2));
      end if;

      if +I1 /= Ident_Int (I1) then
         Failed ("INCORRECT RESULT FOR ""+"" -" & Integer'Image (I + 2));
      end if;
      I1 := I1 - 1;
   end loop;

   for I in (1 - 2) .. Integer (1) loop
      if -I /= Ident_Int (0) - I then
         Failed ("INCORRECT RESULT FOR ""-"" -" & Integer'Image (I + 5));
      end if;

      if "+" (Right => Ident_Int (I)) /= I then
         Failed ("INCORRECT RESULT FOR ""+"" -" & Integer'Image (I + 5));
      end if;
   end loop;

   if -1 /= Ident_Int (1) - 2 then
      Failed ("INCORRECT RESULT FOR ""-"" - 7");
   end if;

   if "-" (Right => 0) /= Ident_Int (0) then
      Failed ("INCORRECT RESULT FOR ""-"" - 8");
   end if;

   if "-" (Right => "-" (Right => 1)) /= Ident_Int (1) then
      Failed ("INCORRECT RESULT FOR ""-"" - 9");
   end if;

   if "+" (Right => 1) /= Ident_Int (2) - 1 then
      Failed ("INCORRECT RESULT FOR ""+"" - 7");
   end if;

   if +0 /= Ident_Int (0) then
      Failed ("INCORRECT RESULT FOR ""+"" - 8");
   end if;

   if +(-1) /= Ident_Int (1) - 2 then
      Failed ("INCORRECT RESULT FOR ""+"" - 9");
   end if;

   for I in (1 - 2) .. Integer (1) loop
      if "-" (Right => D1) /= Dt (Ident_Int (I)) then
         Failed ("INCORRECT RESULT FOR ""-"" -" & Integer'Image (I + 11));
      end if;

      if +D1 /= Dt (Ident_Int (Integer (D1))) then
         Failed ("INCORRECT RESULT FOR ""+"" -" & Integer'Image (I + 11));
      end if;
      D1 := D1 - 1;
   end loop;

   if Integer'Last + Integer'First = 0 then
      if Ident_Int (-Integer'Last) /= Integer'First then
         Failed ("-INTEGER'LAST IS NOT EQUAL TO INTEGER'FIRST");
      end if;
   else
      if Ident_Int (-Integer'Last) /= Integer'First + 1 then
         Failed ("-INTEGER'LAST IS NOT EQUAL TO INTEGER'FIRST+1");
      end if;
   end if;

   Result;

end C45411a;
