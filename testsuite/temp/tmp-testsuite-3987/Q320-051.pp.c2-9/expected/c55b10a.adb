-- C55B10A.ADA

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
--     CHECK THAT, IN 'FOR I IN L .. R LOOP', IF EITHER L OR R IS AN
--     OVERLOADED ENUMERATION LITERAL, THE OVERLOADING IS CORRECTLY
--     RESOLVED AND THE LOOP PARAMETER HAS THE APPROPRIATE TYPE.

-- HISTORY:
--     DHH 08/15/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C55b10a is

   type Enum is (Alph, Bet, Neither);

   Global : Enum := Neither;

   type Alpha is (A, B, C, D, E);
   type Beta is (G, F, E, D, C);

   procedure Var (Dec : Alpha) is
   begin
      if Equal (3, 3) then
         Global := Alph;
      end if;
   end Var;

   procedure Var (Dec : Beta) is
   begin
      if Equal (3, 3) then
         Global := Bet;
      end if;
   end Var;

begin
   Test
     ("C55B10A",
      "CHECK THAT, IN 'FOR I IN L .. R LOOP', IF " &
      "EITHER L OR R IS AN OVERLOADED ENUMERATION " &
      "LITERAL, THE OVERLOADING IS CORRECTLY RESOLVED " &
      "AND THE LOOP PARAMETER HAS THE APPROPRIATE TYPE");

   for I in A .. E loop
      Var (I);

      if Global /= Alph then
         Failed ("WRONG TYPE FOR ALPHA");
      end if;
   end loop;

   for I in G .. E loop
      Var (I);

      if Global /= Bet then
         Failed ("WRONG TYPE FOR BETA");
      end if;
   end loop;

   Result;
end C55b10a;
