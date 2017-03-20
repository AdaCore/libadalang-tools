-- C960004.A
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
-- OBJECTIVE:
--      With the triggering statement being a delay and with the Asynchronous
--      Select statement being in a tasking situation complete the abortable
--      part before the delay expires.  Check that the delay is cancelled
--      and that the optional statements in the triggering part are not
--      executed.
--
-- TEST DESCRIPTION:
--      Simulate the creation of a carrier task to control the output of
--      a message via a line driver.  If the message sending process is
--      not complete (the completion of the rendezvous) within a
--      specified time the carrier task is designed to take corrective action.
--      Use an asynchronous select to control the timing; arrange that
--      the abortable part (the rendezvous) completes almost immediately.
--      Check that the optional statements are not executed and that the
--      test completes well before the time of the trigger delay request thus
--      showing that it has been cancelled.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with Ada.Calendar;

procedure C960004 is

   function "-"
     (Left, Right : Ada.Calendar.Time) return Duration renames
     Ada.Calendar."-";
   Tc_Start_Time   : Ada.Calendar.Time := Ada.Calendar.Clock;
   Tc_Elapsed_Time : Duration;

   -- Note: a properly executing test will complete immediately.
   Allowable_Ack_Time : Duration := 600.0;

begin

   Report.Test
     ("C960004",
      "ATC: When abortable part completes before " &
      "a triggering delay, check that the delay " &
      "is cancelled & optional statements " &
      "are not performed. Tasking situation");

   declare  -- To get the Report.Result after all has completed

      type Sequence_Number is range 1 .. 1_999_999;   -- Message Number
      subtype S_Length_Subtype is Integer range 1 .. 80;

      type Message_Type (Max_String : S_Length_Subtype := 1) is record
         Message_Number : Sequence_Number;
         Alpha          : String (1 .. Max_String);
      end record;

      -- TC:  Dummy message for the test
      Dummy_Alpha     : constant String := "This could be printed";
      Message_To_Send : Message_Type (Max_String => Dummy_Alpha'Length);

      -- This is the carrier task.  One of these is created for each
      -- message that requires ACK
      --
      task type Require_Ack_Task is
         entry Message_In (Message_To_Send : Message_Type);
      end Require_Ack_Task;
      type Acc_Require_Ack_Task is access Require_Ack_Task;

      --:::::::::::::::::::::::::::::::::
      -- There would also be another task type "No_ACK_Task" which would
      -- be the carrier task for those messages not requiring an ACK.
      -- This task would call Send_Message.ACK_Not_Required.  It is not
      -- shown in this test as it is not used.
      --:::::::::::::::::::::::::::::::::

      task Send_Message is
         entry Ack_Required (Message_To_Send : Message_Type);
         entry Ack_Not_Required (Message_To_Send : Message_Type);
      end Send_Message;

      -- This is the carrier task.  One of these is created for each
      -- message that requires ACK
      --
      task body Require_Ack_Task is
         Hold_Message : Message_Type;

         procedure Time_Out (Failed_Message_Number : Sequence_Number) is
         begin
            -- Take remedial action on the timed-out message
            null;    -- stub

            Report.Failed
              ("Optional statements in triggering part" & " were performed");
         end Time_Out;

      begin
         accept Message_In (Message_To_Send : Message_Type) do
            Hold_Message := Message_To_Send;    -- to release caller
         end Message_In;

         -- Now put the message out to the Send_Message task and
         -- wait (no more than Allowable_Ack_Time) for its completion
         --
         select
            delay Allowable_Ack_Time;
            -- ACK not received in specified time
            Time_Out (Hold_Message.Message_Number);
         then abort
            -- If the rendezvous is not completed in the above time, this
            -- call is cancelled
            --    Note: for this test this call will complete immediately
            --          and thus the trigger should be cancelled
            Send_Message.Ack_Required (Hold_Message);
         end select;

      exception
         when others =>
            Report.Failed ("Unexpected exception in Require_ACK_task");
      end Require_Ack_Task;

      -- This is the Line Driver task
      --
      task body Send_Message is
         Hold_Non_Ack_Message : Message_Type;
      begin
         loop
            select
               accept Ack_Required (Message_To_Send : Message_Type) do
                  -- Here send the message from within the rendezvous
                  -- waiting for full transmission to complete
                  null;  -- stub
                  -- Note: In this test this accept will complete immediately
               end Ack_Required;
            or
               accept Ack_Not_Required (Message_To_Send : Message_Type) do
                  Hold_Non_Ack_Message := Message_To_Send;
               end Ack_Not_Required;
               -- Here send the message from outside the rendezvous
               null;  -- stub
            or
               terminate;
            end select;
         end loop;
      exception
         when others =>
            Report.Failed ("Unexpected exception in Send_Message");
      end Send_Message;

   begin -- declare
      -- Build a dummy message
      Message_To_Send.Alpha          := Dummy_Alpha;
      Message_To_Send.Message_Number := 110_693;

      declare
         New_Require_Ack_Task : Acc_Require_Ack_Task := new Require_Ack_Task;
      begin
         -- Create a carrier task for this message and pass the latter in
         New_Require_Ack_Task.Message_In (Message_To_Send);
      end; -- declare

   end; -- declare

   --Once we are out of the above declarative region, all tasks have completed

   Tc_Elapsed_Time := Ada.Calendar.Clock - Tc_Start_Time;

   -- Check that the test has completed well before the time of the requested
   -- delay to ensure the delay was cancelled
   --
   if (Tc_Elapsed_Time > Allowable_Ack_Time / 2) then
      Report.Failed ("Triggering delay statement was not cancelled");
   end if;

   Report.Result;
end C960004;
