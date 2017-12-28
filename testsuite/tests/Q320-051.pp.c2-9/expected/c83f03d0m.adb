-- C83F03D0M.ADA

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
-- MAIN PROGRAM REQUIRING A SEPARATELY COMPILED PACKAGE BODY SUBUNIT
--    ( C83F03D1.ADA )

-- CHECK THAT IF A PACKAGE BODY IS NESTED INSIDE A SEPARATELY COMPILED
--    PACKAGE BODY
--    THE INNER PACKAGE BODY CAN CONTAIN A LABEL IDENTIFIER IDENTICAL
--    TO A LABEL IDENTIFIER IN THE OUTER PACKAGE BODY OR TO AN IDENTI-
--    FIER DECLARED IN THE OUTER PACKAGE BODY OR IN ITS SPECIFICATION
--    OR IN ITS ENVIRONMENT.

-- CASE 2: PACKAGE BODY IS A COMPILATION SUBUNIT

--    RM    08 SEPTEMBER 1980
--    JRK   14 NOVEMBER  1980

with Report;
procedure C83f03d0m is

   use Report;

   X1 : Integer := 17;

   type T1 is (A, B, C);

   Z : T1 := A;

   Flow_Index : Integer := 0;

   package C83f03d1 is

      Y3 : Integer := 100;

      type T3 is (D, E, F);

      package P is
         Aa : Boolean := False;
      end P;

   end C83f03d1;

   Y1 : Integer := 100;

   package body C83f03d1 is separate;

begin

   Test
     ("C83F03D",
      "CHECK THE RECOGNITION OF LABELS IN NESTED" &
      " PACKAGES SEPARATELY COMPILED AS SUBUNITS");

   <<Label_In_Main>>

   if Flow_Index /= 10 then
      Failed ("INCORRECT FLOW OF CONTROL");
   end if;

   Result;   --  POSS. ERROR DURING ELABORATION OF  P

end C83f03d0m;
