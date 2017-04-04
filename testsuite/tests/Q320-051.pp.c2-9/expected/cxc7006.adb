with Report;
with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Exceptions;
with Cxc7006_1;
with Cxc7006_2;
procedure Cxc7006 is
   package Ati renames Ada.Task_Identification;
   package Att renames Ada.Task_Termination;
   package Ae renames Ada.Exceptions;

   Exception_1 : exception;
   Exception_2 : exception;

   Which_Exception : Integer;

   procedure Do_Raise (Code : String) is
   begin
      if Which_Exception = 1 then
         raise Exception_1 with "Do_Raise called: " & Code;
      else
         raise Exception_2 with "Do_Raise called: " & Code;
      end if;
   end Do_Raise;

begin
   Report.Test ("CXC7006", "Test Ada.Task_Termination");

   Test_1 :
      -- Test fallback handler.
       declare
      Raise_Code : constant String := "first";
   begin
      declare
         task type Tt is
            entry Which_Test (Cause : Att.Cause_Of_Termination);
            entry Stall;
         end Tt;

         Tt_Array       : array (1 .. 3) of Tt;
         Id_Array       : Cxc7006_1.Task_Id_Array (Tt_Array'Range);
         Expected_Cause : Cxc7006_1.Cause_Array (Tt_Array'Range);

         task body Tt is
            The_Cause : Att.Cause_Of_Termination;
         begin
            accept Which_Test (Cause : Att.Cause_Of_Termination) do
               The_Cause := Cause;
            end Which_Test;
            case The_Cause is
               when Att.Normal =>
                  delay 1.0;
                  abort Tt_Array (2);
               when Att.Abnormal =>
                  accept Stall;
               when Att.Unhandled_Exception =>
                  Do_Raise (Raise_Code);
            end case;
         end Tt;
      begin
         Cxc7006_1.Start_Test ("Test_1");
         Cxc7006_1.Set_Expected_Message ("Do_Raise called: " & Raise_Code);
         Which_Exception := 1;
         Cxc7006_1.Set_Expected_Exception (Exception_1'Identity);
         Att.Set_Dependents_Fallback_Handler (Cxc7006_1.The_Handler (0));

         for I in Id_Array'Range loop
            Id_Array (I) := Tt_Array (I)'Identity;
         end loop;
         Cxc7006_1.Set_Task_Ids (Id_Array);

         Expected_Cause := (Att.Normal, Att.Abnormal, Att.Unhandled_Exception);
         Cxc7006_1.Set_Expected_Causes (Expected_Cause);

         for I in Tt_Array'Range loop
            Tt_Array (I).Which_Test (Expected_Cause (I));
         end loop;
         -- now wait for all tasks to terminate
      end;

      Cxc7006_1.Test_All_Handlers_Called;

   end Test_1;

   Test_2 :
      -- Test fallback handler set up by a task that is not the direct master
      -- of the tasks that are terminating.
       declare
      Raise_Code : constant String := "second";
   begin
      declare
         Id_Array       : Cxc7006_1.Task_Id_Array (1 .. 5);
         Expected_Cause : Cxc7006_1.Cause_Array (1 .. 5);

         task Grandparent is
            entry Wait_Until_Ready;
            entry Start_Up;
         end Grandparent;

         task body Grandparent is

            task Parent is
               entry Wait_Until_Ready;
               entry Start_Up;
            end Parent;

            task body Parent is

               task type Tt is
                  entry Which_Test (Cause : Att.Cause_Of_Termination);
                  entry Stall;
               end Tt;

               Tt_Array : array (3 .. 5) of Tt;

               task body Tt is
                  The_Cause : Att.Cause_Of_Termination;
               begin
                  accept Which_Test (Cause : Att.Cause_Of_Termination) do
                     The_Cause := Cause;
                  end Which_Test;
                  case The_Cause is
                     when Att.Normal =>
                        delay 1.0;
                        abort Tt_Array (4);
                     when Att.Abnormal =>
                        accept Stall;
                     when Att.Unhandled_Exception =>
                        Do_Raise (Raise_Code);
                  end case;
               end Tt;

            begin
               for I in Tt_Array'Range loop
                  Id_Array (I) := Tt_Array (I)'Identity;
               end loop;
               accept Wait_Until_Ready;
               accept Start_Up;
               for I in Tt_Array'Range loop
                  Tt_Array (I).Which_Test (Expected_Cause (I));
               end loop;
               -- now wait for all tasks to terminate
            end Parent;

         begin
            Id_Array (2) := Parent'Identity;
            Parent.Wait_Until_Ready;
            accept Wait_Until_Ready;
            accept Start_Up;
            Parent.Start_Up;
            -- now wait for all tasks to terminate
         end Grandparent;

      begin
         Cxc7006_1.Start_Test ("Test_2");
         Cxc7006_1.Set_Expected_Message ("Do_Raise called: " & Raise_Code);
         Which_Exception := 2;
         Cxc7006_1.Set_Expected_Exception (Exception_2'Identity);
         Att.Set_Dependents_Fallback_Handler (Cxc7006_1.The_Handler (0));

         Id_Array (1) := Grandparent'Identity;
         Grandparent.Wait_Until_Ready;
         Cxc7006_1.Set_Task_Ids (Id_Array);

         Expected_Cause :=
           (Att.Normal,
            Att.Normal,
            Att.Normal,
            Att.Abnormal,
            Att.Unhandled_Exception);
         Cxc7006_1.Set_Expected_Causes (Expected_Cause);

         Grandparent.Start_Up;
         -- now wait for all tasks to terminate
      end;

      Cxc7006_1.Test_All_Handlers_Called;

   end Test_2;

   Test_3 :
      -- Test fallback handler when fallback handlers are set up by multiple
      -- "ancestors" of a task; make sure the correct handler is called.
       declare
      Raise_Code : constant String := "third";
   begin
      declare
         Id_Array         : Cxc7006_1.Task_Id_Array (1 .. 5);
         Expected_Cause   : Cxc7006_1.Cause_Array (1 .. 5);
         Expected_Handler : Cxc7006_1.Handler_Id_Array (1 .. 5);

         task Grandparent is
            entry Wait_Until_Ready;
            entry Start_Up;
         end Grandparent;

         task body Grandparent is

            task Parent is
               entry Wait_Until_Ready;
               entry Start_Up;
            end Parent;

            task body Parent is

               task type Tt is
                  entry Which_Test (Cause : Att.Cause_Of_Termination);
                  entry Stall;
               end Tt;

               Tt_Array : array (3 .. 5) of Tt;

               task body Tt is
                  The_Cause : Att.Cause_Of_Termination;
               begin
                  accept Which_Test (Cause : Att.Cause_Of_Termination) do
                     The_Cause := Cause;
                  end Which_Test;
                  case The_Cause is
                     when Att.Normal =>
                        delay 1.0;
                        abort Tt_Array (4);
                     when Att.Abnormal =>
                        accept Stall;
                     when Att.Unhandled_Exception =>
                        Do_Raise (Raise_Code);
                  end case;
               end Tt;

            begin
               Att.Set_Dependents_Fallback_Handler (Cxc7006_1.The_Handler (0));
               for I in Tt_Array'Range loop
                  Id_Array (I) := Tt_Array (I)'Identity;
               end loop;
               accept Wait_Until_Ready;
               accept Start_Up;
               for I in Tt_Array'Range loop
                  Tt_Array (I).Which_Test (Expected_Cause (I));
               end loop;
               -- now wait for all tasks to terminate
            end Parent;

         begin
            Att.Set_Dependents_Fallback_Handler (Cxc7006_1.The_Handler (1));
            Id_Array (2) := Parent'Identity;
            Parent.Wait_Until_Ready;
            accept Wait_Until_Ready;
            accept Start_Up;
            Parent.Start_Up;
            -- now wait for all tasks to terminate
         end Grandparent;

      begin
         Cxc7006_1.Start_Test ("Test_3");
         Cxc7006_1.Set_Expected_Message ("Do_Raise called: " & Raise_Code);
         Which_Exception := 2;
         Cxc7006_1.Set_Expected_Exception (Exception_2'Identity);
         Att.Set_Dependents_Fallback_Handler (Cxc7006_1.The_Handler (2));

         Id_Array (1) := Grandparent'Identity;
         Grandparent.Wait_Until_Ready;
         Cxc7006_1.Set_Task_Ids (Id_Array);

         Expected_Cause :=
           (Att.Normal,
            Att.Normal,
            Att.Normal,
            Att.Abnormal,
            Att.Unhandled_Exception);
         Cxc7006_1.Set_Expected_Causes (Expected_Cause);

         Expected_Handler := (1 => 2, 2 => 1, others => 0);
         Cxc7006_1.Set_Expected_Handlers (Expected_Handler);

         Grandparent.Start_Up;
         -- now wait for all tasks to terminate
      end;

      Cxc7006_1.Test_All_Handlers_Called;

   end Test_3;

   Test_4 :
      -- Test Set_Specific_Handler.
       declare
      Raise_Code : constant String := "fourth";
   begin
      declare

         Id_Array         : Cxc7006_1.Task_Id_Array (1 .. 14);
         Expected_Cause   : Cxc7006_1.Cause_Array (1 .. 14);
         Expected_Handler : Cxc7006_1.Handler_Id_Array (1 .. 14);

         Delay_Amounts : constant array (1 .. 8) of Duration :=
           (1.5, 2.7, 2.1, 3.1, 1.3, 2.2, 1.8, 2.9);

         protected Delay_Amount is
            procedure Next_Delay (D : out Duration);
         private
            Index : Integer := Delay_Amounts'First;
         end Delay_Amount;

         protected body Delay_Amount is

            procedure Next_Delay (D : out Duration) is
            begin
               D     := Delay_Amounts (Index);
               Index := Index + 1;
               if Index > Delay_Amounts'Last then
                  Index := Delay_Amounts'First;
               end if;
            end Next_Delay;

         end Delay_Amount;

         task type Tt1 is
            entry Set_Up_Ids (Start_Tt2, Start_Tt3 : Integer);
            entry Start_Up;
         end Tt1;

         task body Tt1 is

            task type Tt2 is
               entry Set_Up_Ids (Start_Tt3 : Integer);
               entry Start_Up;
            end Tt2;

            task body Tt2 is

               task type Tt3 is
                  entry Which_Test (Cause : Att.Cause_Of_Termination);
               end Tt3;

               task body Tt3 is
                  The_Cause : Att.Cause_Of_Termination;
                  D         : Duration;
               begin
                  accept Which_Test (Cause : Att.Cause_Of_Termination) do
                     The_Cause := Cause;
                  end Which_Test;
                  Delay_Amount.Next_Delay (D);
                  delay D;
                  case The_Cause is
                     when Att.Normal =>
                        null;
                     when Att.Abnormal =>
                        raise Program_Error;  -- won't happen in this test
                     when Att.Unhandled_Exception =>
                        Do_Raise (Raise_Code);
                  end case;
               end Tt3;

               Tt3_1 : Tt3;
               Tt3_2 : Tt3;
            begin
               accept Set_Up_Ids (Start_Tt3 : Integer) do
                  Id_Array (Start_Tt3)     := Tt3_1'Identity;
                  Id_Array (Start_Tt3 + 1) := Tt3_2'Identity;
               end Set_Up_Ids;
               Tt3_1.Which_Test (Att.Normal);
               Tt3_2.Which_Test (Att.Unhandled_Exception);
               accept Start_Up;
            end Tt2;

            Tt2_1 : Tt2;
            Tt2_2 : Tt2;
         begin
            accept Set_Up_Ids (Start_Tt2, Start_Tt3 : Integer) do
               Id_Array (Start_Tt2)     := Tt2_1'Identity;
               Id_Array (Start_Tt2 + 1) := Tt2_2'Identity;
               Tt2_1.Set_Up_Ids (Start_Tt3);
               Tt2_2.Set_Up_Ids (Start_Tt3 + 2);
            end Set_Up_Ids;
            accept Start_Up;
            Tt2_1.Start_Up;
            Tt2_2.Start_Up;
         end Tt1;

         Tt1_1 : Tt1;
         Tt1_2 : Tt1;
      begin
         Cxc7006_1.Start_Test ("Test_4");
         Cxc7006_1.Set_Expected_Message ("Do_Raise called: " & Raise_Code);
         Which_Exception := 1;
         Cxc7006_1.Set_Expected_Exception (Exception_1'Identity);
         Att.Set_Dependents_Fallback_Handler (Cxc7006_1.The_Handler (0));

         Id_Array (1) := Tt1_1'Identity;
         Id_Array (2) := Tt1_2'Identity;
         Tt1_1.Set_Up_Ids (3, 7);
         Tt1_2.Set_Up_Ids (5, 11);
         Cxc7006_1.Set_Task_Ids (Id_Array);

         Att.Set_Specific_Handler (Id_Array (1), Cxc7006_1.The_Handler (1));
         Att.Set_Specific_Handler (Id_Array (3), Cxc7006_1.The_Handler (2));
         Att.Set_Specific_Handler (Id_Array (5), Cxc7006_1.The_Handler (3));
         Att.Set_Specific_Handler (Id_Array (7), Cxc7006_1.The_Handler (2));
         Att.Set_Specific_Handler (Id_Array (9), Cxc7006_1.The_Handler (1));
         Att.Set_Specific_Handler (Id_Array (11), Cxc7006_1.The_Handler (3));
         Att.Set_Specific_Handler (Id_Array (13), Cxc7006_1.The_Handler (2));
         Expected_Handler :=
           (1      => 1,
            3      => 2,
            5      => 3,
            7      => 2,
            9      => 1,
            11     => 3,
            13     => 2,
            others => 0);
         Cxc7006_1.Set_Expected_Handlers (Expected_Handler);

         Expected_Cause :=
           (8 | 10 | 12 | 14 => Att.Unhandled_Exception, others => Att.Normal);
         Cxc7006_1.Set_Expected_Causes (Expected_Cause);

         Tt1_1.Start_Up;
         Tt1_2.Start_Up;
         -- wait for all tasks to complete
      end;

      Cxc7006_1.Test_All_Handlers_Called;

   end Test_4;

   Test_5 :
      -- Make sure behavior is correct when tasks that terminate also have
      -- controlled local variables. Make sure that (1) local variables
      -- are finalized before the termination handler is called; (2) if the
      -- task terminates normally but finalization raises an exception, the
      -- termination handler is still called with Cause=Unhandled_Exception;
      -- (3) if the task terminates because of an unhandled exception, and the
      -- finalization then raises the exception, the termination handler is
      -- called with an Exception_Occurrence that identifies Program_Error
      -- (AI05-0202).
       declare
      Raise_Code : constant String := "fifth";
   begin
      declare

         type Test_Type is (Body_Exception, Fin_Exception, Both);

         task type Tt is
            entry Which_Test (Obj_Id : Integer; T : Test_Type);
         end Tt;

         Tt_Array            : array (1 .. 3) of Tt;
         Id_Array            : Cxc7006_1.Task_Id_Array (Tt_Array'Range);
         Expected_Cause      : Cxc7006_1.Cause_Array (Tt_Array'Range);
         Expected_Exceptions : Cxc7006_1.Exception_Array (Tt_Array'Range);

         task body Tt is
            The_Type : Test_Type;
            Obj      : Cxc7006_2.Fin_Type;
         begin
            accept Which_Test (Obj_Id : Integer; T : Test_Type) do
               Obj.Fin_Id := Obj_Id;
               The_Type   := T;
            end Which_Test;
            if The_Type = Fin_Exception or else The_Type = Both then
               Obj.Raise_Exc := True;
            end if;
            if The_Type = Body_Exception or else The_Type = Both then
               Do_Raise (Raise_Code);
            end if;
         end Tt;

      begin
         Cxc7006_1.Start_Test ("Test_5");
         Cxc7006_1.Set_Expected_Message ("Do_Raise called: " & Raise_Code);
         Which_Exception := 2;

         Att.Set_Dependents_Fallback_Handler (Cxc7006_1.The_Handler (0));

         for I in Id_Array'Range loop
            Id_Array (I) := Tt_Array (I)'Identity;
         end loop;
         Cxc7006_1.Set_Task_Ids (Id_Array);

         Expected_Cause := (others => Att.Unhandled_Exception);
         Cxc7006_1.Set_Expected_Causes (Expected_Cause);
         Expected_Exceptions :=
           (Exception_2'Identity,
            Program_Error'Identity,
            Program_Error'Identity);
         Cxc7006_1.Set_Expected_Exceptions (Expected_Exceptions);

         for I in Id_Array'Range loop
            Cxc7006_1.Set_Handler_Test (I, Cxc7006_2.Test_Finalized'Access, I);
            -- Effect is that when termination handler is called, it will call
            -- a procedure that makes sure that the local controlled object has
            -- been finalized before the handler was called.
         end loop;

         Tt_Array (1).Which_Test (1, Body_Exception);
         Tt_Array (2).Which_Test (2, Fin_Exception);
         Tt_Array (3).Which_Test (3, Both);
         -- now wait for all tasks to terminate
      end;

      Cxc7006_1.Test_All_Handlers_Called;

   end Test_5;

   Test_6 :
      -- Make sure behavior is correct when a task propagates an exception
      -- during elaboration.
       declare
      Raise_Code : constant String := "sixth";
   begin
      Cxc7006_1.Start_Test ("Test_6");
      Cxc7006_1.Set_Expected_Message ("Do_Raise called: " & Raise_Code);
      Which_Exception := 1;
      Cxc7006_1.Set_Expected_Exception (Exception_1'Identity);
      Att.Set_Dependents_Fallback_Handler (Cxc7006_1.The_Handler (0));

      declare

         function Set_Up_Task_Id (Id : Ati.Task_Id) return Integer is
         begin
            Cxc7006_1.Set_Task_Ids ((1 => Id));
            Cxc7006_1.Set_Expected_Causes ((1 => Att.Unhandled_Exception));
            return 1;
         end Set_Up_Task_Id;

         function Will_Raise_Exception return Integer is
         begin
            Do_Raise (Raise_Code);
            return 2;
         end Will_Raise_Exception;

         function Should_Not_Get_Here return Integer is
         begin
            Report.Failed ("Elaboration continued after exception");
            return 3;
         end Should_Not_Get_Here;

         task Tt;

         task body Tt is
            V1 : Integer := Set_Up_Task_Id (Tt'Identity);
            V2 : Integer := Will_Raise_Exception;
            V3 : Integer := Should_Not_Get_Here;
         begin
            null;
         end Tt;

      begin
         null;
      exception
         when Tasking_Error =>
            null;
      end;

      Cxc7006_1.Test_All_Handlers_Called;

   end Test_6;

   Report.Result;
end Cxc7006;
