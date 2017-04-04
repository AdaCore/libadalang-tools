-- C35801D.ADA

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
-- CHECK THAT THE ATTRIBUTES FIRST AND LAST RETURN VALUES HAVING THE
-- SAME BASE TYPE AS THE PREFIX WHEN THE PREFIX IS A GENERIC FORMAL
-- SUBTYPE WHOSE ACTUAL ARGUMENT IS A FLOATING POINT TYPE.

-- R.WILLIAMS 8/21/86

with Report; use Report;
procedure C35801d is
   type Real is digits 3 range -100.0 .. 100.0;

   type Nflt is new Float;

   generic
      type F is digits <>;
   procedure P (Str : String);

   procedure P (Str : String) is

      subtype Sf is F range -1.0 .. 1.0;
      F1 : Sf := 0.0;
      F2 : Sf := 0.0;

   begin
      if Equal (3, 3) then
         F1 := Sf'First;
         F2 := Sf'Last;
      end if;

      if F1 /= -1.0 or F2 /= 1.0 then
         Failed ("WRONG RESULTS FROM " & Str & "'FIRST OR " & Str & "'LAST");
      end if;
   end P;

   procedure Np1 is new P (Float);

   procedure Np2 is new P (Nflt);

   procedure Np3 is new P (Real);

begin
   Test
     ("C35801D",
      "CHECK THAT THE ATTRIBUTES FIRST AND " &
      "LAST RETURN VALUES HAVING THE SAME " &
      "BASE TYPE AS THE PREFIX WHEN THE " &
      "PREFIX IS A GENERIC FORMAL SUBTYPE " &
      "WHOSE ACTUAL ARGUMENT IS A FLOATING " &
      "POINT TYPE");

   Np1 ("FLOAT");
   Np2 ("NFLT");
   Np3 ("REAL");

   Result;
end C35801d;
