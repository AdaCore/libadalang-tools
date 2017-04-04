-- C35502O.ADA

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
-- CHECK THAT 'FIRST AND 'LAST GIVE CORRECT RESULTS FOR TYPES
-- AND SUBTYPES.

-- DAT 3/17/81
-- R. WILLIAMS 11/11/86      RENAMED FROM C35104A.ADA.

with Report; use Report;
procedure C35502o is

   type E is (E1, E2, E3, E4, E5);

   subtype S is E range E2 .. E4;

begin
   Test
     ("C35502O",
      "CHECK THAT 'FIRST AND 'LAST WORK FOR" &
      " ENUMERATION TYPES AND SUBTYPES");

   if E'First /= E1 or
     E'Last /= E5 or
     E'Base'First /= E1 or
     E'Base'Last /= E5 or
     S'Base'First /= E1 or
     S'Base'Last /= E5 or
     S'First /= E2 or
     S'Last /= E4 or
     Boolean'First /= False or
     Boolean'Last /= True
   then
      Failed ("'FIRST OR 'LAST GIVES WRONG RESULTS");
   end if;

   Result;
end C35502o;
