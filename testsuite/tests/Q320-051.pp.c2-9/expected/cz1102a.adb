-- CZ1102A.ADA
--
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
--
-- CHECK THAT THE DYNAMIC VALUE ROUTINES OF THE REPORT PACKAGE WORK
--   CORRECTLY.

-- JRK 8/7/81
-- JRK 10/27/82
-- RLB 03/20/00 - Added checks for Integer'First and Integer'Last.

with Report; use Report;

procedure Cz1102a is

begin

   Test
     ("CZ1102A",
      "CHECK THAT THE DYNAMIC VALUE ROUTINES OF " &
      "THE REPORT PACKAGE WORK CORRECTLY");

   if not Equal (0, 0) or
     Equal (0, 1) or
     not Equal (1, 1) or
     not Equal (3, 3) or
     not Equal (4, 4) or
     not Equal (-1, -1) or
     not Equal (Integer'First, Integer'First) or
     not Equal (Integer'Last, Integer'Last) or
     Equal (-1, 0)
   then
      Failed ("'EQUAL' NOT WORKING");
   end if;

   if Ident_Int (5) /= 5 then
      Failed ("'IDENT_INT' NOT WORKING");
   end if;

   if Ident_Char ('E') /= 'E' then
      Failed ("'IDENT_CHAR' NOT WORKING");
   end if;

   if Ident_Bool (True) /= True then
      Failed ("'IDENT_BOOL' NOT WORKING");
   end if;

   if Ident_Str ("") /= "" or
     Ident_Str ("K") /= "K" or
     Ident_Str ("PQRS") /= "PQRS"
   then
      Failed ("'IDENT_STR' NOT WORKING");
   end if;

   Result;

end Cz1102a;
