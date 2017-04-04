-- C59002C.ADA

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
-- CHECK THAT JUMPS OUT OF SELECT STATEMENTS (OTHER THAN
--    FROM INSIDE  ACCEPT  BODIES IN SELECT_ALTERNATIVES)
--    ARE POSSIBLE AND ARE CORRECTLY PERFORMED.

-- THIS TEST CONTAINS SHARED VARIABLES.

-- RM 08/15/82
-- SPS 12/13/82
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with Report;
with System; use System;
procedure C59002c is

   use Report;

   Flow_String : String (1 .. 2) := "XX";
   Index       : Integer         := 1;

begin

   Test ("C59002C", "CHECK THAT ONE CAN JUMP OUT OF SELECT STATE" & "MENTS");

   -------------------------------------------------------------------

   declare

      task T is

         entry E1;
         entry E2;
      end T;

      task body T is
      begin

         while E2'Count <= 0 loop
            delay 1.0;
         end loop;

         select
            accept E1 do
               Failed (" E1  ACCEPTED; NO ENTRY CALL (1)");
            end E1;
         or
            accept E2;
            goto L123;
            Failed ("'GOTO' NOT OBEYED (1)");
         or
            delay 10.0;
            Failed ("DELAY ALTERNATIVE SELECTED (1)");
         end select;

         Failed ("WRONG DESTINATION FOR 'GOTO' (1)");

         <<L123>>

         Flow_String (Index) := 'A';
         Index               := Index + 1;

      end T;

   begin

      T.E2;

   end;

   -------------------------------------------------------------------

   declare

      task T is
         entry E1;
         entry E2;
      end T;

      task body T is
      begin

         select
            accept E1 do
               Failed (" E1  ACCEPTED; NO ENTRY CALL (2)");
            end E1;
         or
            accept E2 do
               Failed (" E2  ACCEPTED; NO ENTRY CALL (2)");
            end E2;
         or
            delay 10.0;
            goto L321;
            Failed ("'GOTO' NOT OBEYED (2)");
         end select;

         Failed ("WRONG DESTINATION FOR 'GOTO' (2)");

         <<L321>>

         Flow_String (Index) := 'B';
         Index               := Index + 1;

      end T;

   begin

      null;

   end;

   -------------------------------------------------------------------

   if Flow_String /= "AB" then
      Failed ("WRONG FLOW OF CONTROL");
   end if;

   Result;

end C59002c;
