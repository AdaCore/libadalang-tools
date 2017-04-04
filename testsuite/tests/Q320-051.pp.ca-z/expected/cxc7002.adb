------------------------------------------------------------------

with Impdef;
with Report;
with Ada.Task_Identification;
with Ada.Task_Attributes;
with Cxc7002_0;
use type Cxc7002_0.Int_Array;
with Cxc7002_1;
procedure Cxc7002 is
   Verbose : constant Boolean := False;

   package Tid renames Ada.Task_Identification;
   generic package Ta renames Ada.Task_Attributes;
   package Ata renames Cxc7002_1;
   package Ita is new Ta (Integer, 321);
begin
   Report.Test ("CXC7002", "Check the package Task_Attributes");

   -- make sure a copy was made of the initial value for ATA. The following
   -- value should not get used as the initial value for the local task
   -- Check_It.
   Cxc7002_0.Countdown := (7, 6, 5);

   declare
      task Check_It is
         entry Get_Id (Id : out Tid.Task_Id);
         entry Ok_To_Terminate;
      end Check_It;

      task body Check_It is
         -- retrieves attribute value for the current task. make sure that it
         -- is the default value.
         Val    : Integer := Ita.Value;
         Handle : Ata.Attribute_Handle;
      begin
         if Val /= 321 then
            Report.Failed
              ("Check_It - initial value was" &
               Integer'Image (Val) &
               " expected 321");
         elsif Verbose then
            Report.Comment ("Check_It - initial value ok");

         end if;
         Ita.Set_Value (135);

         -- get a handle to the ATA attribute for the current task. make sure
         -- the attribute has the default value.
         Handle := Ata.Reference;
         if Handle.all /= (3, 2, 1) then
            Report.Failed
              ("Check_It - initial ATA value" &
               Cxc7002_0.Small_Integer'Image (Handle (1)) &
               Cxc7002_0.Small_Integer'Image (Handle (2)) &
               Cxc7002_0.Small_Integer'Image (Handle (3)) &
               " expected 3 2 1");
         end if;
         Handle.all := (2, 4, 6);

         accept Get_Id (Id : out Tid.Task_Id) do
            Id := Tid.Current_Task;
         end Get_Id;
         accept Ok_To_Terminate;
      end Check_It;

      Check_It_Tid  : Tid.Task_Id;
      T1_Tid        : Tid.Task_Id;
      T2_Tid        : Tid.Task_Id;
      Null_Tid      : Tid.Task_Id;
      Xi            : Integer;
      Xa            : Cxc7002_0.Int_Array;
      Handle_T1_Ata : Ata.Attribute_Handle;
      Handle_T2_Ata : Ata.Attribute_Handle;
   begin
      Check_It.Get_Id (Check_It_Tid);
      Cxc7002_0.T1.Get_Id (T1_Tid);
      Cxc7002_0.T2.Get_Id (T2_Tid);

      -- don't make it obvious that Null_TID is null
      if Report.Ident_Bool (False) then
         -- never executed
         Null_Tid := Check_It_Tid;
      else
         Null_Tid := Tid.Null_Task_Id;
      end if;

      -- check the values that were set inside Check_It
      Xa := Ata.Value (Check_It_Tid);
      Xi := Ita.Value (Check_It_Tid);

      if Xa = (2, 4, 6) then
         if Verbose then
            Report.Comment ("Check_It 246 ok");
         end if;
      else
         Report.Failed
           ("Expected Check_It values 2,4,6;  found" &
            Cxc7002_0.Small_Integer'Image (Xa (1)) &
            Cxc7002_0.Small_Integer'Image (Xa (2)) &
            Cxc7002_0.Small_Integer'Image (Xa (3)));
      end if;

      if Xi /= 135 then
         Report.Failed
           ("Expected Check_It value 135; found" & Integer'Image (Xi));
      elsif Verbose then
         Report.Comment ("Check_It 135 ok");
      end if;

      -- check the initial values for the tasks that were created before the
      -- attributes were created

      Xa := Ata.Value (T1_Tid);
      if Xa /= (3, 2, 1) then
         Report.Failed
           ("Expected T1 initial values 3,2,1; found" &
            Cxc7002_0.Small_Integer'Image (Xa (1)) &
            Cxc7002_0.Small_Integer'Image (Xa (2)) &
            Cxc7002_0.Small_Integer'Image (Xa (3)));
      elsif Verbose then
         Report.Comment ("T1 321 ok");
      end if;

      Xi := Ita.Value (T2_Tid);
      if Xi /= 321 then
         Report.Failed
           ("Expected T2 initial value 321  found" & Integer'Image (Xi));
      elsif Verbose then
         Report.Comment ("T2 321 ok");
      end if;

      -- restore the initial value to Check_It and then check that the value is
      -- correct
      Ata.Reinitialize (Check_It_Tid);

      Xa := Ata.Value (Check_It_Tid);
      if Xa /= (3, 2, 1) then
         Report.Failed
           ("Expected Check_It reinitialized; found" &
            Cxc7002_0.Small_Integer'Image (Xa (1)) &
            Cxc7002_0.Small_Integer'Image (Xa (2)) &
            Cxc7002_0.Small_Integer'Image (Xa (3)));
      elsif Verbose then
         Report.Comment ("reinitialize ok");
      end if;

      -- check setting attributes for environment task
      Ita.Set_Value (271);
      if Ita.Value /= 271 then
         Report.Failed ("environment task attributes");
      end if;

      -- check the operations on the library level tasks. Interleave the
      -- operations to make sure they are working on the proper attribute
      -- value.
      Handle_T1_Ata     := Ata.Reference (T1_Tid);
      Handle_T2_Ata     := Ata.Reference (T2_Tid);
      Handle_T1_Ata.all := (1, 1, 1);
      Handle_T2_Ata.all := (2, 2, 2);
      if Ata.Value (T1_Tid) /= (1, 1, 1) then
         Report.Failed ("library task t1 array attributes wrong");
      end if;
      if Ata.Value (T2_Tid) /= (2, 2, 2) then
         Report.Failed ("library task t2 array attributes wrong");
      end if;

      -- Reinitialize T1 but not T2 and check the values
      Ata.Reinitialize (T1_Tid);
      if Ata.Value (T1_Tid) /= (3, 2, 1) then
         Report.Failed ("library task t1 array attributes wrong after reinit");
      end if;
      if Ata.Value (T2_Tid) /= (2, 2, 2) then
         Report.Failed ("library task t2 array attributes wrong after reinit");
      end if;

      -- let the tasks terminate
      Check_It.Ok_To_Terminate;
      Cxc7002_0.T1.Ok_To_Terminate;
      Cxc7002_0.T2.Ok_To_Terminate;
      while not Check_It'Terminated loop
         delay Impdef.Switch_To_New_Task;
      end loop;

      -- check the error conditions
      begin
         Ita.Set_Value (786, Check_It_Tid);
         Report.Failed ("Tasking_Error expected");
      exception
         when Tasking_Error =>
            if Verbose then
               Report.Comment ("Tasking_Error properly raised");
            end if;
         when others =>
            Report.Failed
              ("wrong exception raised. " & "Tasking_Error expected");
      end;

      begin
         Ita.Set_Value (941, Null_Tid);
         Report.Failed ("Program_Error expected");
      exception
         when Program_Error =>
            if Verbose then
               Report.Comment ("Program_Error properly raised");
            end if;
         when others =>
            Report.Failed
              ("wrong exception raised. " & "Program_Error expected");

      end;
   exception
      when others =>
         Report.Failed ("unexpected exception");
   end;

   Report.Result;
end Cxc7002;
