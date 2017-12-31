with System;                                                            --RT
with Report;
with Impdef;
with Cxe4003_Part_B2;
with Cxe4003_Part_B3;
with Cxe4003_Part_B4;
with Cxe4003_Part_B5;
package body Cxe4003_Call_Test is

   -- Check that remote subprogram calls are executed at most once. Check that
   -- potentially concurrent calls from multiple tasks can be handled by the
   -- PCS.
   --
   -- This is done by having several tasks each calling different RCI packages
   -- in the partition B and passing in a sequence number. The sequence number
   -- is checked to insure that each call arrives and that they come in order.
   -- The tasks are all of different priorities (if the Real-Time --RT annex
   -- is supported. This should result in preemption occurring --RT which will
   -- exercise the PCS better. --RT The Id is a unique value used to identify
   -- each task, tell it what RCI package it is to be using, and initialize the
   -- Cycle so that each task starts out on a slightly different schedule.

   task type Client
     (Low   : Integer;
      High  : Integer;
      Prior : System.Priority;                            --RT
      Id    : Integer) is
      entry Go;
      pragma Priority (Prior);                                            --RT
   end Client;

   task body Client is
      This_Cycle : Integer := Id;
   begin
      for I in Low .. High loop
         case Id is
            when 2 =>
               Cxe4003_Part_B2.Take_Call (I);
            when 3 =>
               Cxe4003_Part_B3.Take_Call (I);
            when 4 =>
               Cxe4003_Part_B4.Take_Call (I);
            when 5 =>
               Cxe4003_Part_B5.Take_Call (I);
            when others =>
               Report.Failed ("internal error in Client");
         end case;
         delay Duration (Impdef.Minimum_Task_Switch * This_Cycle);
         This_Cycle := This_Cycle mod 5 + 1;
      end loop;
   exception
      when others =>
         Report.Failed ("unexpected exception in Call_Test Client");
   end Client;

   procedure Do_Test is
   begin
      -- Report.Comment ("starting call test");
      declare
         C2 : Client
           (200,
            299,
            System.Priority'Last - 5,                            --RT
            2);
         C3 : Client
           (300,
            399,
            System.Priority'Last - 4,                            --RT
            3);
         C4 : Client
           (400,
            499,
            System.Priority'Last - 3,                            --RT
            4);
         -- give the highest priority task the most to do
         C5 : Client
           (500,
            699,
            System.Priority'Last - 2,                            --RT
            5);
      begin
         null;  -- wait for the clients to finish
      end;
      -- Report.Comment ("client tasks have completed");
      Cxe4003_Part_B2.Done;
      Cxe4003_Part_B3.Done;
      Cxe4003_Part_B4.Done;
      Cxe4003_Part_B5.Done;
   end Do_Test;
end Cxe4003_Call_Test;
