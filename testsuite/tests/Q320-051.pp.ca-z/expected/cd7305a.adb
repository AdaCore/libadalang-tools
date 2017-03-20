-- CD7305A.ADA

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
--     CHECK, FOR DIGITS 5, THAT MACHINE_RADIX, MACHINE_MANTISSA,
--     MACHINE_EMAX, AND MACHINE_EMIN HAVE THE CORRECT VALUES.

-- HISTORY:
--     DHH 09/15/88  CREATED ORIGINAL TEST.
--     PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
procedure Cd7305a is

   type T is digits 5;

   B : Boolean := False;

begin
   Test
     ("CD7305A",
      "CHECK, FOR DIGITS 5, THAT MACHINE_RADIX, " &
      "MACHINE_MANTISSA, MACHINE_EMAX, AND " &
      "MACHINE_EMIN HAVE THE CORRECT VALUES");

   if T'Machine_Radix < 2 or T'Base'Machine_Radix /= T'Machine_Radix then
      Failed ("INCORRECT 'MACHINE_RADIX");
   end if;

   Result;
end Cd7305a;
