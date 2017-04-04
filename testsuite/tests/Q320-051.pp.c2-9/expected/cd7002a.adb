-- CD7002A.ADA

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
--     CHECK THAT A VARIABLE OF TYPE ADDRESS CAN BE DECLARED IN A UNIT
--     WHICH HAS A WITH CLAUSE NAMING SYSTEM.

-- HISTORY:
--     DHH 08/31/88  CREATED ORIGINAL TEST.

with System;
with Report; use Report;
procedure Cd7002a is

   I : Integer;

   Object : System.Address := I'Address;

   subtype My_Address is System.Address;

begin
   Test
     ("CD7002A",
      "CHECK THAT A VARIABLE OF TYPE ADDRESS CAN BE " &
      "DECLARED IN A UNIT WHICH HAS A WITH CLAUSE " &
      "NAMING SYSTEM");

   if not Ident_Bool (Object in My_Address) then
      Failed ("INCORRECT RESULT");
   end if;

   Result;
end Cd7002a;
