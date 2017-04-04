-- C54A24A.ADA

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
-- CHECK THAT NULL SUBRANGE CHOICES MAY OCCUR IN CASE STATEMENT, WITH
-- OUT-OF-BOUNDS RANGE BOUNDS, AND WHERE VACUOUS CHOICES ARE NULL. CHECK
-- THAT AN UNNEEDED OTHERS CHOICE IS PERMITTED.

-- DAT 1/29/81
-- JBG 8/21/83

with Report;
procedure C54a24a is

   use Report;

   type T is range 1 .. 1_010;
   subtype St is T range 5 .. 7;

   V : St := 6;

begin
   Test
     ("C54A24A",
      "CHECK NULL CASE SUBRANGE CHOICES, WITH " & "OUTRAGEOUS BOUNDS");

   case V is
      when -1_000 .. -1_010 =>
         null;
      when T range -5 .. -6 =>
         null;
      when 12 .. 11 | St range 1_000 .. 99 =>
         null;
      when St range -99 .. -999 =>
         null;
      when St range 6 .. 6 =>
         V := V - 1;
      when T range St'Base'Last .. St'Base'First =>
         null;
      when 5 | 7 =>
         null;
      when St range T'Base'Last .. T'Base'First =>
         null;
      when T'Base'Last .. T'Base'First =>
         null;
      when others =>
         V := V + 1;
   end case;
   if V /= 5 then
      Failed ("IMPROPER CASE EXECUTION");
   end if;

   Result;
end C54a24a;
