-- C95040D.ADA

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
-- CHECK THAT TASKING_ERROR IS RAISED IN A CALLING
-- TASK WHEN THE TASK OWNING THE ENTRY TERMINATES BEFORE RENDEZVOUS
-- CAN OCCUR.

-- CHECK THAT RE-RAISING TASKING_ERROR, ONCE TRAPPED IN THE CALLER,
-- DOES NOT PROPAGATE OUTSIDE THE TASK BODY.

-- GOM 11/29/84
-- JWC 05/14/85
-- PWB 02/11/86  CORRECTED CALL TO TEST TO SHOW CORRECT TEST NAME.
-- RLB 12/15/99  REMOVED POTENTIALLY ERRONEOUS CALLS TO REPORT.COMMENT.

with Report; use Report;

procedure C95040d is

   procedure Driver is

      task Nest is
         entry Outer;
         entry Inner;
      end Nest;

      task Slave;

      task body Nest is
      begin
         --COMMENT("AT TOP OF 'NEST' TASK WAITING ON 'OUTER' " &
         --        "RENDEZVOUS");

         accept Outer do
            --COMMENT("IN 'OUTER' RENDEZVOUS OF 'NEST' TASK " &
            --        "ABOUT TO 'RETURN'");

            return;  -- CAUSES 'INNER' RENDEZVOUS TO BE SKIPPED.

            accept Inner do
               Failed
                 ("'INNER' RENDEZVOUS OF 'NEST' TASK " &
                  "SHOULD NEVER BE PERFORMED");
            end Inner;
         end Outer;

         --COMMENT("'OUTER' RENDEZVOUS COMPLETED IN 'NEST' TASK " &
         --        "AND NOW TERMINATING");
      end Nest;

      task body Slave is
      begin
         --COMMENT("AT TOP OF 'SLAVE' TASK. CALLING 'INNER' " &
         --        "RENDEZVOUS");

         Nest.Inner;

         Failed ("SHOULD HAVE RAISED 'TASKING_ERROR' IN 'SLAVE' " & "TASK");
      exception
         when Tasking_Error =>
            --COMMENT("'SLAVE' TASK CORRECTLY TRAPPING " &
            --        "'TASKING_ERROR' AND RE-RAISING IT (BUT " &
            --        "SHOULD NOT BE PROPAGATED)");
            raise;
      end Slave;

   begin  -- START OF DRIVER PROCEDURE.

      --COMMENT("AT TOP OF 'DRIVER'. CALLING 'OUTER' ENTRY OF " &
      --        "'NEST' TASK");

      Nest.Outer;

      --COMMENT("'OUTER' RENDEZVOUS COMPLETED. 'DRIVER' AWAITING " &
      --        "TERMINATION OF 'NEST' AND 'SLAVE' TASKS");

   exception
      when Tasking_Error =>
         Failed
           ("'TASKING_ERROR' CAUGHT IN 'DRIVER' WHEN IT " &
            "SHOULD HAVE BEEN CAUGHT IN 'SLAVE' TASK, OR " &
            "'TASKING_ERROR' WAS INCORRECTLY PROPAGATED BY " &
            "'SLAVE' TASK");
   end Driver;

begin  -- START OF MAIN PROGRAM.

   Test
     ("C95040D",
      "CHECK THAT 'TASKING_ERROR' IS RAISED IN A " &
      "CALLER TASK WHEN TASK OWNING THE ENTRY CANNOT " &
      "PERFORM RENDEZVOUS. ALSO CHECK THAT " &
      "'TASKING_ERROR', ONCE RAISED, IS NOT PROPAGATED " &
      "OUTSIDE THE TASK BODY");

   --COMMENT("MAIN PROGRAM CALLING 'DRIVER' PROCEDURE");

   Driver;

   --COMMENT("MAIN PROGRAM NOW TERMINATING");

   Result;
end C95040d;
