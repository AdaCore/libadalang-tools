-- C940013.A
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
--      Check that items queued on a protected entry are handled FIFO and that
--      the  'count attribute of that entry reflects the length of the queue.
--
-- TEST DESCRIPTION:
--      Use a small subset of the freeway ramp simulation shown in other
--      tests.  With the timing pulse off (which prevents items from being
--      removed from the queue) queue up a small number of calls.  Start the
--      timing pulse and, at the first execution of the entry code, check the
--      'count attribute. Empty the queue.   Pass the items being removed from
--      the queue to the Ramp_Sensor_01 task; there check that the items are
--      arriving in FIFO order.  Check the final 'count value
--
--      Send another batch of items at a rate which will, if the delay timing
--      of the implementation is reasonable, cause the queue length to
--      fluctuate in both directions.   Again check that all items arrive
--      FIFO.  At the end check that the 'count returned to zero reflecting
--      the empty queue.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with Impdef;
with Ada.Calendar;

procedure C940013 is

   Tc_Failed_1 : Boolean := False;

begin

   Report.Test
     ("C940013",
      "Check that queues on protected entries are " &
      "handled FIFO and that 'count is correct");

   declare  -- encapsulate the test

      function "+" (Left : Ada.Calendar.Time;
         Right           : Duration) return Ada.Calendar.Time renames
        Ada.Calendar."+";

      -- Weighted load given to each potential problem area and accumulated
      type Load_Factor is range 0 .. 8;
      Clear_Level    : constant Load_Factor := 0;
      Minimum_Level  : constant Load_Factor := 1;
      Moderate_Level : constant Load_Factor := 2;
      Serious_Level  : constant Load_Factor := 4;
      Critical_Level : constant Load_Factor := 6;

      Tc_Expected_Passage_Total : constant Integer := 624;

      -- For this test give each vehicle an integer ID incremented by one
      -- for each successive vehicle. In reality this would be a more complex
      -- alpha-numeric ID assigned at pickup time.
      type Vehicle_Id is range 1 .. 5_000;
      Next_Id : Vehicle_Id := Vehicle_Id'First;

      -- In reality this would be about 5 seconds. The default value of this
      -- constant in the implementation defined package is similar but could,
      -- of course be considerably different - it would not affect the test
      --
      Pulse_Time_Delta : Duration := Impdef.Clear_Ready_Queue;

      task Pulse_Task;       -- task to generate a pulse for each ramp

      -- Carrier task. One is created for each vehicle arriving at the ramp
      task type Vehicle is
         entry Get_Id (Input_Id : in Vehicle_Id);
      end Vehicle;
      type Acc_Vehicle is access Vehicle;

      task Ramp_Sensor_01 is
         entry Accept_Vehicle (Input_Id : in Vehicle_Id);
         entry Tc_First_Three_Handled;
         entry Tc_All_Done;
      end Ramp_Sensor_01;

      protected Pulse_State is
         procedure Start_Pulse;
         procedure Stop_Pulse;
         function Pulsing return Boolean;
      private
         State : Boolean := False;   -- start test will pulse off
      end Pulse_State;

      protected body Pulse_State is

         procedure Start_Pulse is
         begin
            State := True;
         end Start_Pulse;

         procedure Stop_Pulse is
         begin
            State := False;
         end Stop_Pulse;

         function Pulsing return Boolean is
         begin
            return State;
         end Pulsing;

      end Pulse_State;

      --================================================================
      protected Test_Ramp is

         function Meter_In_Use_State return Boolean;
         procedure Time_Pulse_Received;
         entry Wait_At_Meter;
         procedure Tc_Passage (Pass_Point : Integer);
         function Tc_Get_Passage_Total return Integer;
         function Tc_Get_Count return Integer;

      private

         Release_One_Vehicle : Boolean := False;
         -- For this test have Meter_in_Use already set
         Meter_In_Use : Boolean := True;

         Tc_Wait_At_Meter_First : Boolean := True;
         Tc_Entry_Queue_Count   : Integer := 0; -- 'count of Wait_at_Meter
         Tc_Passage_Total       : Integer := 0;
         Tc_Pass_Point_Wam      : Integer := 23;

      end Test_Ramp;
      --================================================================
      protected body Test_Ramp is

         -- External call for Meter_in_Use
         function Meter_In_Use_State return Boolean is
         begin
            return Meter_In_Use;
         end Meter_In_Use_State;

         -- Trace the paths through the various routines by totalling the
         -- weighted call parameters
         procedure Tc_Passage (Pass_Point : Integer) is
         begin
            Tc_Passage_Total := Tc_Passage_Total + Pass_Point;
         end Tc_Passage;

         -- For the final check of the whole test
         function Tc_Get_Passage_Total return Integer is
         begin
            return Tc_Passage_Total;
         end Tc_Get_Passage_Total;

         function Tc_Get_Count return Integer is
         begin
            return Tc_Entry_Queue_Count;
         end Tc_Get_Count;

         -- Here each Vehicle task queues itself awaiting release
         --
         entry Wait_At_Meter when Release_One_Vehicle is
         -- EXAMPLE OF ENTRY WITH BARRIERS AND PERSISTENT SIGNAL
         begin
            --
            Tc_Passage (Tc_Pass_Point_Wam);   -- note passage
            -- For this test three vehicles are queued before the first is
            -- released. If the queueing mechanism is working correctly the
            -- first time we pass through here the entry'count should reflect
            -- this
            if Tc_Wait_At_Meter_First then
               if Wait_At_Meter'Count /= 2 then
                  Tc_Failed_1 := True;
               end if;
               Tc_Wait_At_Meter_First := False;
            end if;
            Tc_Entry_Queue_Count := Wait_At_Meter'Count;  -- note for later

            Release_One_Vehicle := False;   -- Consume the signal
            null; -- stub ::: Decrement count of number of vehicles on ramp
         end Wait_At_Meter;

         procedure Time_Pulse_Received is
            Load : Load_Factor := Minimum_Level;   -- for this version of the
            Freeway_Breakdown : Boolean := False;  -- test, freeway is Minimum
         begin
            -- if broken down, no vehicles are released
            if not Freeway_Breakdown then
               if Load < Moderate_Level then
                  Release_One_Vehicle := True;
               end if;
               null;    -- stub  ::: If other levels, release every other
               --           pulse, every third pulse  etc.
            end if;
         end Time_Pulse_Received;

      end Test_Ramp;
      --================================================================

      -- Simulate the arrival of a vehicle at the Ramp_Receiver and the
      -- generation of an accompanying carrier task
      procedure New_Arrival is
         Next_Vehicle_Task : Acc_Vehicle      := new Vehicle;
         Tc_Pass_Point     : constant Integer := 3;
      begin
         Next_Id := Next_Id + 1;
         Next_Vehicle_Task.Get_Id (Next_Id);
         Test_Ramp.Tc_Passage (Tc_Pass_Point);  -- Note passage through here
         null;
      end New_Arrival;

      -- Carrier task. One is created for each vehicle arriving at the ramp
      task body Vehicle is
         This_Id         : Vehicle_Id;
         Tc_Pass_Point_2 : constant Integer := 21;
      begin
         accept Get_Id (Input_Id : in Vehicle_Id) do
            This_Id := Input_Id;
         end Get_Id;

         if Test_Ramp.Meter_In_Use_State then
            Test_Ramp.Tc_Passage (Tc_Pass_Point_2);  -- note passage
            null;  -- stub::: Increment count of number of vehicles on ramp
            Test_Ramp.Wait_At_Meter;      -- Queue on the meter entry
         end if;

         -- Call to the first in the series of the Ramp_Sensors
         --     this "passes" the vehicle from one sensor to the next
         --     Each sensor will requeue the call to the next thus this
         --     rendezvous will only be completed as the vehicle is released
         --     by the last sensor on the ramp.
         Ramp_Sensor_01.Accept_Vehicle (This_Id);
      exception
         when others =>
            Report.Failed ("Unexpected exception in Vehicle Task");
      end Vehicle;

      task body Ramp_Sensor_01 is
         Tc_Pass_Point : constant Integer := 31;
         This_Id       : Vehicle_Id;
         Tc_Last_Id    : Vehicle_Id       := Vehicle_Id'First;
      begin
         loop
            select
               accept Accept_Vehicle (Input_Id : in Vehicle_Id) do
                  null;   -- stub:::: match up with next Real-Time notification
                  -- from the sensor. Requeue to next ramp sensor
                  This_Id := Input_Id;

                  -- The following is all Test_Control code
                  Test_Ramp.Tc_Passage (Tc_Pass_Point);  -- note passage
                  -- The items arrive in the order they are taken from the
                  -- Wait_at_Meter entry queue
                  if (This_Id - Tc_Last_Id) /= 1 then
                     -- The tasks are being queued (or unqueued) in the wrong
                     -- order
                     Report.Failed
                       ("Queueing on the Wait_at_Meter queue failed");
                  end if;
                  Tc_Last_Id := This_Id;    -- for the next check
                  if Tc_Last_Id = 4 then
                     -- rendezvous with the test driver
                     accept Tc_First_Three_Handled;
                  elsif Tc_Last_Id = 9 then
                     -- rendezvous with the test driver
                     accept Tc_All_Done;
                  end if;
               end Accept_Vehicle;
            or
               terminate;
            end select;
         end loop;
      exception
         when others =>
            Report.Failed ("Unexpected exception in Ramp_Sensor_01");
      end Ramp_Sensor_01;

      -- Task transmits a synchronizing "pulse" to all ramps
      --
      task body Pulse_Task is
         Pulse_Time : Ada.Calendar.Time;
      begin
         while not Pulse_State.Pulsing loop
            -- Starts up in the quiescent state
            delay Impdef.Minimum_Task_Switch;
         end loop;
         Pulse_Time := Ada.Calendar.Clock;
         while Pulse_State.Pulsing loop
            delay until Pulse_Time;
            Test_Ramp.Time_Pulse_Received;   -- Transmit pulse to test_ramp
            -- :::::::::: and to all the other ramps
            Pulse_Time := Pulse_Time + Pulse_Time_Delta; -- calculate next
         end loop;
      exception
         when others =>
            Report.Failed ("Unexpected exception in Pulse_Task");
      end Pulse_Task;

   begin -- declare

      -- Test driver. This is ALL test control code

      -- Arrange to queue three vehicles on the Wait_at_Meter queue. The timing
      -- pulse is quiescent so the queue will build
      for I in 1 .. 3 loop
         New_Arrival;
      end loop;

      delay Pulse_Time_Delta;  -- ensure all is settled

      Pulse_State.Start_Pulse;     -- Start the timing pulse, the queue will
      -- be serviced

      -- wait here until the first three are complete
      Ramp_Sensor_01.Tc_First_Three_Handled;

      if Test_Ramp.Tc_Get_Count /= 0 then
         Report.Failed ("Intermediate Wait_at_Entry'count is incorrect");
      end if;

      -- generate new arrivals at a rate that will make the queue increase and
      -- decrease "randomly"
      for I in 1 .. 5 loop
         New_Arrival;
         delay Pulse_Time_Delta / 2;
      end loop;

      -- wait here till all have been handled
      Ramp_Sensor_01.Tc_All_Done;

      if Test_Ramp.Tc_Get_Count /= 0 then
         Report.Failed ("Final Wait_at_Entry'count is incorrect");
      end if;

      Pulse_State.Stop_Pulse;       -- finish test

      if Tc_Expected_Passage_Total /= Test_Ramp.Tc_Get_Passage_Total then
         Report.Failed ("Unexpected paths taken");
      end if;

   end; -- declare

   if Tc_Failed_1 then
      Report.Failed ("Wait_at_Meter'count incorrect");
   end if;

   Report.Result;

end C940013;
