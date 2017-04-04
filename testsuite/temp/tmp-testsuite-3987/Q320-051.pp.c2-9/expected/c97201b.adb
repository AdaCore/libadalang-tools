-- C97201B.ADA

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
-- CHECK THAT A CONDITIONAL ENTRY CALL IS NOT ACCEPTED IF THERE IS
-- ANOTHER TASK QUEUED FOR THE ENTRY.

-- WRG 7/11/86
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C97201b is

begin

   Test
     ("C97201B",
      "CHECK THAT A CONDITIONAL ENTRY CALL IS NOT " &
      "ACCEPTED IF THERE IS ANOTHER TASK QUEUED " &
      "FOR THE ENTRY");

   declare

      task T is
         entry E;
         entry Synch;
         entry Done;
      end T;

      task body T is
      begin
         -- ENSURE THAT E HAS BEEN CALLED BEFORE PROCEEDING:
         while E'Count = 0 loop
            delay 1.0;
         end loop;

         accept Synch;

         select when Ident_Bool (False) =>
            accept E;
            Failed ("CLOSED ALTERNATIVE TAKEN");
         or
            accept Done do
               if E'Count /= 1 then
                  Failed
                    (Natural'Image (E'Count) &
                     " CALLS WERE QUEUED FOR ENTRY " &
                     "E OF TASK T");
               end if;
            end Done;
         or
            delay 1_000.0;
            Failed ("DELAY EXPIRED; E'COUNT =" & Natural'Image (E'Count));
         end select;

         while E'Count > 0 loop
            accept E;
         end loop;
      end T;

      task Agent;

      task body Agent is
      begin
         T.E;
      end Agent;

   begin

      T.Synch;

      delay 10.0;

      select
         T.E;
         Failed ("CONDITIONAL ENTRY CALL ACCEPTED");
      else
         Comment ("ELSE PART EXECUTED");
         T.Done;
      end select;

   end;

   Result;

end C97201b;
