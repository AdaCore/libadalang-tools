-- C36302A.ADA

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
-- CHECK THAT A STRING VARIABLE MAY BE DECLARED WITH AN INDEX STARTING WITH AN
-- INTEGER GREATER THAN 1.

-- DAT 2/17/81
-- JWC 6/28/85 RENAMED TO -AB

with Report;
procedure C36302a is

   use Report;

   S5 : String (5 .. 10);
   Sx : String (Integer'Last - 5 .. Integer'Last);

begin
   Test ("C36302A", "STRING VARIABLE INDICES NEEDN'T START AT 1");

   if S5'First /= 5 or
     S5'Last /= 10 or
     S5'Length /= 6 or
     Sx'First /= Integer'Last - 5 or
     Sx'Last /= Integer'Last or
     Sx'Length /= 6
   then
      Failed ("WRONG STRING ATTRIBUTES");
   end if;

   Result;
end C36302a;
