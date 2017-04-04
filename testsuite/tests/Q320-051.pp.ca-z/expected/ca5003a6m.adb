-- CA5003A6M.ADA

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
-- CHECK THAT THE ELABORATION OF LIBRARY UNITS REQUIRED BY
--   A MAIN PROGRAM IS PERFORMED CONSISTENTLY WITH THE PARTIAL
--   ORDERING DEFINED BY THE COMPILATION ORDER RULES.

-- SEPARATE FILES ARE:
--   CA5003A0  A LIBRARY PACKAGE.
--   CA5003A1  A LIBRARY PACKAGE SPECIFICATION.
--   CA5003A2  A LIBRARY PACKAGE SPECIFICATION.
--   CA5003A3  A LIBRARY PACKAGE SPECIFICATION.
--   CA5003A4  A LIBRARY PACKAGE SPECIFICATION.
--   CA5003A5  A LIBRARY PACKAGE SPECIFICATION.
--   CA5003A6M THE MAIN PROCEDURE.

-- PACKAGE A5 MUST BE ELABORATED AFTER A2, A3, AND A4. PACKAGE A3 MUST BE
-- ELABORATED AFTER A2. PACKAGE A4 MUST BE ELABORATED AFTER A2.

-- WKB 7/22/81
-- JBG 10/6/83

with Report, Ca5003a0;
use Report, Ca5003a0;
with Ca5003a1, Ca5003a5;
procedure Ca5003a6m is

begin

   Test
     ("CA5003A",
      "CHECK THAT ELABORATION ORDER IS CONSISTENT " &
      "WITH PARTIAL ORDERING REQUIREMENTS");

   Comment ("ACTUAL ELABORATION ORDER WAS " & Order);

   if Order /= "12345" and
     Order /= "12435" and
     Order /= "21345" and
     Order /= "21435" and
     Order /= "23145" and
     Order /= "24135" and
     Order /= "23415" and
     Order /= "24315" and
     Order /= "23451" and
     Order /= "24351"
   then
      Failed ("ILLEGAL ELABORATION ORDER");
   end if;

   Result;
end Ca5003a6m;
