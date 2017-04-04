-- C87B08A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- FOR EACH REAL TYPE, THERE EXISTS AN IMPLICIT CONVERSION THAT CONVERTS A
-- UNIVERSAL REAL VALUE INTO THE CORRESPONDING VALUE OF THE REAL TYPE. THIS
-- TEST USES LITERALS AS UNIVERSAL REAL VALUES.

-- TRH  16 AUG 82
-- PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C87b08a is

   type Fixed is delta 0.1 range -2.0 .. 2.0;
   type Flt is digits 2 range -2.0 .. 2.0;
   type Flag is (Pass, Fail);

   generic
      type T is private;
      Stat : in Flag;
   procedure P1 (X : T);

   procedure P1 (X : T) is
   begin
      if Stat = Fail then
         Failed
           ("INCORRECT IMPLICIT CONVERSION FROM UNIVERSAL" &
            " REAL VALUES TO REAL TYPE VALUES");
      end if;
   end P1;

   procedure P is new P1 (Integer, Fail);
   procedure P is new P1 (Flt, Pass);
   procedure Q is new P1 (Fixed, Pass);
   procedure Q is new P1 (Boolean, Fail);
   procedure Q is new P1 (Character, Fail);

begin
   Test
     ("C87B08A",
      "IMPLICIT CONVERSION OF UNIVERSAL REAL " &
      "VALUES TO REAL VALUES EXISTS FOR ANY REAL TYPE");

   P (0.0);
   P (1.0 + 1.0);
   Q (1.0);
   Q (1.0 - 1.0);

   Result;
end C87b08a;
