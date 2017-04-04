-- C910001.A
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
--      Check that tasks may have discriminants.  Specifically, check where
--      the subtype of the discriminant is a discrete subtype and where it is
--      an access subtype.  Check the case where the default values of the
--      discriminants are used.
--
-- TEST DESCRIPTION:
--      A task is defined with two discriminants, one a discrete subtype and
--      another that is an access subtype.  Tasks are created with various
--      values for discriminants and code within the task checks that these
--      are passed in correctly.  One instance of a default is used.  The
--      values passed to the task as the discriminants are taken from an
--      array of test data and the values received are checked against the
--      same array.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;

procedure C910001 is

   type App_Priority is range 1 .. 10;
   Default_Priority : App_Priority := 5;

   type Message_Id is range 1 .. 10_000;

   type Tc_Number_Of_Messages is range 1 .. 5;

   type Tc_Rec is record
      Tc_Id      : Message_Id;
      A_Priority : App_Priority;
      Tc_Checked : Boolean;
   end record;

   -- This table is used to create the messages and to check them
   Tc_Table : array (1 .. Tc_Number_Of_Messages'Last) of Tc_Rec :=
     ((10, 6, False),
      (20, 2, False),
      (30, 9, False),
      (40, 1, False),
      (50, Default_Priority, False));

begin -- C910001

   Report.Test ("C910001", "Check that tasks may have discriminants");

   declare     -- encapsulate the test

      type Transaction_Record is record
         Id             : Message_Id;
         Account_Number : Integer := 0;
         Stock_Number   : Integer := 0;
         Quantity       : Integer := 0;
         Return_Value   : Integer := 0;
      end record;
      --
      type Acc_Transaction_Record is access Transaction_Record;

      task type Message_Task
        (In_Message  : Acc_Transaction_Record := null;
         In_Priority : App_Priority           := Default_Priority)
      is
         entry Start;
      end Message_Task;
      type Acc_Message_Task is access Message_Task;
      --
      --
      task body Message_Task is
         This_Message   : Acc_Transaction_Record := In_Message;
         This_Priority  : App_Priority           := In_Priority;
         Tc_Match_Found : Boolean                := False;
      begin
         accept Start;
         -- In the example envisioned this task would then queue itself upon
         -- some Distributor task which would send it off (requeue) to the
         -- message processing tasks according to the priority of the message
         -- and the current load on the system. For the test we just verify the
         -- data passed in as discriminants and exit the task
         --
         -- Check for the special case of default discriminants
         if This_Message = null then
            -- The default In_Message has been passed, check that the default
            -- priority was also passed
            if This_Priority /= Default_Priority then
               Report.Failed ("Incorrect Default Priority");
            end if;
            if Tc_Table (Tc_Number_Of_Messages'Last).Tc_Checked then
               Report.Failed ("Duplicate Default messages");
            else
               -- Mark that default has been seen
               Tc_Table (Tc_Number_Of_Messages'Last).Tc_Checked := True;
            end if;
            Tc_Match_Found := True;
         else
            -- Check the data against the table
            for I in Tc_Number_Of_Messages loop
               if Tc_Table (I).Tc_Id = This_Message.Id then
                  -- this is the right slot in the table
                  if Tc_Table (I).Tc_Checked then
                     -- Already checked
                     Report.Failed ("Duplicate Data");
                  else
                     Tc_Table (I).Tc_Checked := True;
                  end if;
                  Tc_Match_Found := True;
                  if Tc_Table (I).A_Priority /= This_Priority then
                     Report.Failed ("ID/Priority mismatch");
                  end if;
                  exit;
               end if;
            end loop;
         end if;

         if not Tc_Match_Found then
            Report.Failed ("No ID match in table");
         end if;

         -- Allow the task to terminate

      end Message_Task;

      -- The Line Driver task accepts data from an external source and builds
      -- them into a transaction record. It then generates a message task. This
      -- message "contains" the record and is given a priority according to
      -- the contents of the message. The priority and transaction records
      -- are passed to the task as discriminants.
      --    In this test we use a dummy record.  Only the ID is of interest
      --    so we pick that and the required priority from an array of
      --    test data.  We artificially limit the endless driver-loop to
      --    the number of messages required for the test and add a special
      --    case to check the defaults.
      --
      task Driver_Task;
      --
      task body Driver_Task is
      begin

         -- Create all but one of the required tasks
         --
         for I in 1 .. Tc_Number_Of_Messages'Last - 1 loop
            declare
               -- Create a record for the next message
               Next_Transaction : Acc_Transaction_Record :=
                 new Transaction_Record;
               -- Create a task for the next message
               Next_Message_Task : Acc_Message_Task :=
                 new Message_Task (Next_Transaction, Tc_Table (I).A_Priority);

            begin
               -- Artificially plug the ID with the next from the table
               --    In reality the whole record would be built here
               Next_Transaction.Id := Tc_Table (I).Tc_Id;

               -- Ensure the task does not start executing till the transaction
               -- record is properly constructed
               Next_Message_Task.Start;

            end;  -- declare
         end loop;

         -- For this subtest create one task with the default discriminants
         --
         declare

            -- Create the task
            Next_Message_Task : Acc_Message_Task := new Message_Task;

         begin

            Next_Message_Task.Start;

         end; -- declare

      end Driver_Task;

   begin
      null;
   end;     -- encapsulation

   -- Now verify that all the tasks executed and checked in
   for I in Tc_Number_Of_Messages loop
      if not Tc_Table (I).Tc_Checked then
         Report.Failed
           ("Task" & Integer'Image (Integer (I)) & " did not verify");
      end if;
   end loop;
   Report.Result;

end C910001;
