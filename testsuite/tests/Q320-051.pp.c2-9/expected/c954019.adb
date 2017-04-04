-- C954019.A
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
--      Check that when a requeue is to the same entry the items go to the
--      right queue and that they are placed back on the end of the queue.
--
-- TEST DESCRIPTION:
--      Simulate part of a message handling application where the messages are
--      composed of several segments.  The sequence of the segments within the
--      message is specified by Seg_Sequence_No.   The segments are handled by
--      different tasks  and finally forwarded to an output driver.  The
--      segments can arrive in any order but must be assembled into the proper
--      sequence for final output.  There is a Sequencer task interposed
--      before the Driver.  This takes the segments of the message off the
--      Ordering_Queue and those that are in the right order it sends on to
--      the driver; those that are out of order it places back on the end of
--      the queue.
--
--      The test just simulates the arrival of the segments at the Sequencer.
--      The task generating the segments handshakes with the Sequencer during
--      the  "Await Arrival" phase  ensuring that the three segments of a
--      message arrive in REVERSE order (the End-of-Message segment arrives
--      first and the Header last).  In the first cycle the sequencer pulls
--      segments off the queue and puts them back on the end till it
--      encounters the header.  It checks the sequence of the ones it pulls
--      off in case the segments are being put back on in the wrong part of
--      the queue. Having cycled once through it no longer verifies the
--      sequence - it just executes the "application" code for the correct
--      order for dispatch to the driver.
--
--      In this simple example no attempt is made to address segments of
--      another message arriving or any other error conditions (such as
--      missing segments, timing etc.)
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      19 Dec 94   SAIC    Remove parameter from requeue statement
--
--!

with Report;
with Impdef;

procedure C954019 is
begin

   Report.Test ("C954019", "Check Requeue to the same Accept");

   declare  -- encapsulate the test

      type Segment_Sequence is range 1 .. 8;
      Header : constant Segment_Sequence := Segment_Sequence'First;

      type Message_Segment is record
         Id              : Integer;            -- Message ID
         Seg_Sequence_No : Segment_Sequence;   -- Within the message
         Alpha           : String (1 .. 128);
         Eom             : Boolean := False;   -- true for final msg segment
      end record;
      type Acc_Message_Segment is access Message_Segment;

      task Tc_Simulate_Arrival;

      task type Carrier_Task is
         entry Input (Segment : Acc_Message_Segment);
      end Carrier_Task;
      type Acc_Carrier_Task is access Carrier_Task;

      task Sequencer is
         entry Ordering_Queue (Segment : Acc_Message_Segment);
         entry Tc_Handshake_1;
         entry Tc_Handshake_2;
      end Sequencer;

      task Output_Driver is
         entry Input (Segment : Acc_Message_Segment);
      end Output_Driver;

      -- Simulate the arrival of three message segments in REVERSE order
      --
      task body Tc_Simulate_Arrival is
      begin

         for I in 1 .. 3 loop
            declare
               -- Create a task for the next message segment
               Next_Segment_Task : Acc_Carrier_Task := new Carrier_Task;
               -- Create a record for the next segment
               Next_Segment : Acc_Message_Segment := new Message_Segment;
            begin
               if I = 1 then
                  -- Build the EOM segment as the first to "send"
                  Next_Segment.Seg_Sequence_No := Header + 2;
                  Next_Segment.Eom             := True;
               elsif I = 2 then
                  -- Wait for the first segment to arrive at the Sequencer
                  -- before "sending" the second
                  Sequencer.Tc_Handshake_1;
                  -- Build the segment
                  Next_Segment.Seg_Sequence_No := Header + 1;
               else
                  -- Wait for the second segment to arrive at the Sequencer
                  -- before "sending" the third
                  Sequencer.Tc_Handshake_2;
                  -- Build the segment. The last segment in order to arrive
                  -- will be the "header" segment
                  Next_Segment.Seg_Sequence_No := Header;
               end if;
               -- pass the record to its carrier
               Next_Segment_Task.Input (Next_Segment);
            end;
         end loop;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in TC_Simulate_Arrival");
      end Tc_Simulate_Arrival;

      -- One of these is generated for each message segment and the flow of the
      -- segments through the system is controlled by the calls the task makes
      -- and the requeues of those calls
      --
      task body Carrier_Task is
         This_Segment : Acc_Message_Segment := new Message_Segment;
      begin
         accept Input (Segment : Acc_Message_Segment) do
            This_Segment.all := Segment.all;
         end Input;
         null; --:: stub.  Pass the segment around the application as needed

         -- Now output the segment to the Output_Driver. First we have to go
         -- through the Sequencer.
         Sequencer.Ordering_Queue (This_Segment);
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Carrier_Task");
      end Carrier_Task;

      -- Pull segments off the Ordering_Queue and deliver them in the correct
      -- sequence to the Output_Driver.
      --
      task body Sequencer is
         Next_Needed : Segment_Sequence := Header;

         Tc_Await_Arrival     : Boolean          := True;
         Tc_First_Cycle       : Boolean          := True;
         Tc_Expected_Sequence : Segment_Sequence := Header + 2;
      begin
         loop
            select
               accept Ordering_Queue (Segment : Acc_Message_Segment) do

                  --=====================================================
                  -- This part is all Test_Control code

                  if Tc_Await_Arrival then
                     -- We have to arrange that the segments arrive on the
                     -- queue in the right order, so we handshake with the
                     -- TC_Simulate_Arrival task to "send" only one at a time
                     accept Tc_Handshake_1;   -- the first  has arrived
                     -- and has been pulled off the queue

                     -- Wait for the second to arrive (the first has already
                     -- been pulled off the queue
                     while Ordering_Queue'Count < 1 loop
                        delay Impdef.Minimum_Task_Switch;
                     end loop;
                     --
                     accept Tc_Handshake_2;   -- the second has arrived

                     -- Wait for the third to arrive
                     while Ordering_Queue'Count < 2 loop
                        delay Impdef.Minimum_Task_Switch;
                     end loop;

                     -- Subsequent passes through the loop, bypass this code
                     Tc_Await_Arrival := False;

                  end if; -- await arrival

                  if Tc_First_Cycle then
                     -- Check the order of the original three
                     if Segment.Seg_Sequence_No /= Tc_Expected_Sequence then
                        -- The segments are not being pulled off in the
                        -- expected sequence. This could occur if the
                        -- requeue is not putting them back on the end.
                        Report.Failed ("Sequencer: Segment out of sequence");
                     end if; -- sequence check
                     -- Decrement the expected sequence
                     if Tc_Expected_Sequence /= Header then
                        Tc_Expected_Sequence := Tc_Expected_Sequence - 1;
                     else
                        Tc_First_Cycle := False; -- This is the Header - the
                        -- first two segments are
                        -- back on the queue

                     end if; -- decrementing
                  end if; -- first pass
                  --=====================================================

                  -- And this is the Application code
                  if Segment.Seg_Sequence_No = Next_Needed then
                     if Segment.Eom then
                        Next_Needed := Header;  -- reset for next message
                     else
                        Next_Needed := Next_Needed + 1;
                     end if;
                     requeue Output_Driver.Input with abort;
                     Report.Failed ("Requeue did not complete accept body");
                  else
                     -- Not the next needed - put it back on the queue
                     requeue Sequencer.Ordering_Queue;
                     Report.Failed ("Requeue did not complete accept body");
                  end if;
               end Ordering_Queue;
            or
               terminate;
            end select;
         end loop;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Sequencer");
      end Sequencer;

      task body Output_Driver is
         This_Segment : Acc_Message_Segment := new Message_Segment;

         Tc_Expected_Sequence : Segment_Sequence := Segment_Sequence'First;
         Tc_Segment_Total     : Integer          := 0;
         Tc_Expected_Total    : Integer          := 3;
      begin
         loop
            -- Note: normally we would expect this Accept to be in a select
            -- with terminate. For the test we exit the loop on completion
            -- to give better control
            accept Input (Segment : Acc_Message_Segment) do
               This_Segment.all := Segment.all;
            end Input;

            null;  --::: stub - output the next segment of the message

            -- The following is all test control code
            --
            if This_Segment.Seg_Sequence_No /= Tc_Expected_Sequence then
               Report.Failed ("Output_Driver: Segment out of sequence");
            end if;
            Tc_Expected_Sequence := Tc_Expected_Sequence + 1;

            -- Now count the number of segments
            Tc_Segment_Total := Tc_Segment_Total + 1;

            -- Check the number and exit loop when complete There must be
            -- exactly TC_Expected_Total in number and
            --    the last one must be EOM
            --    (test will hang if < TC_Expected_Total arrive
            --    without EOM)
            if This_Segment.Eom then
               -- This is the last segment.
               if Tc_Segment_Total /= Tc_Expected_Total then
                  Report.Failed ("EOM and wrong number of segments");
               end if;
               exit;   -- the loop and terminate the task
            elsif Tc_Segment_Total = Tc_Expected_Total then
               Report.Failed ("No EOM found");
               exit;
            end if;
         end loop;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Output_Driver");
      end Output_Driver;

   begin

      null;

   end; -- encapsulation

   Report.Result;

end C954019;
