-- C97112A.ADA

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
-- CHECK THAT A DELAY STATEMENT IS ALLOWED IN THE SEQUENCE OF STATEMENTS
-- OF A SELECT ALTERNATIVE OF A SELECTIVE WAIT CONTAINING A TERMINATE
-- ALTERNATIVE OR AN ELSE PART.

-- WRG 7/9/86

with Report;   use Report;
with Calendar; use Calendar;
procedure C97112a is

   Accept_Alternative_Taken : Boolean := False;

begin

   Test
     ("C97112A",
      "CHECK THAT A DELAY STATEMENT IS ALLOWED IN " &
      "THE SEQUENCE OF STATEMENTS OF A SELECT " &
      "ALTERNATIVE OF A SELECTIVE WAIT CONTAINING A " &
      "TERMINATE ALTERNATIVE OR AN ELSE PART");

   --------------------------------------------------

   A : declare

      task T is
         entry E;
      end T;

      task body T is
         Before, After : Time;
      begin
         select
            accept E;
            Accept_Alternative_Taken := True;
            Before                   := Clock;
            delay 10.0;
            After := Clock;
            if After - Before < 10.0 then
               Failed ("INSUFFICIENT DELAY (A)");
            end if;
         or
            terminate;
         end select;
      end T;

   begin

      T.E;

   end A;

   if not Accept_Alternative_Taken then
      Failed ("ACCEPT ALTERNATIVE NOT TAKEN");
   end if;

   --------------------------------------------------

   B : declare

      task T is
         entry E;
      end T;

      task body T is
         Before, After : Time;
      begin
         --ENSURE THAT E HAS BEEN CALLED BEFORE PROCEEDING:
         while E'Count = 0 loop
            delay 1.0;
         end loop;

         select
            accept E;
            Before := Clock;
            delay 10.0;
            After := Clock;
            if After - Before < 10.0 then
               Failed ("INSUFFICIENT DELAY (B-1)");
            end if;
         else
            Failed ("ELSE PART EXECUTED (B-1)");
         end select;

         select
            accept E;
            Failed ("ACCEPT STATEMENT EXECUTED (B-2)");
         else
            Before := Clock;
            delay 10.0;
            After := Clock;
            if After - Before < 10.0 then
               Failed ("INSUFFICIENT DELAY (B-2)");
            end if;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED");
      end T;

   begin

      T.E;

   end B;

   --------------------------------------------------

   Result;

end C97112a;
