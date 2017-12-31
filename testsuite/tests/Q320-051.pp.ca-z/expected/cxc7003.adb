------------------------------------------------------------------

with Cxc7003_0;
with Impdef;
with Report;
with Ada.Task_Identification;
with Ada.Task_Attributes;
with Ada.Exceptions;
procedure Cxc7003 is
   Verbose : constant Boolean := False;
   Not_Supported_Error : exception;
begin
   Report.Test
     ("CXC7003",
      "Check that Set_Value and Reinitialize" &
      " finalize the previous attribute value");

   begin
      declare
         package Tid renames Ada.Task_Identification;
         generic package Ta renames Ada.Task_Attributes;
         package Fta is new Ta (Cxc7003_0.Notes_Finalization,
            Cxc7003_0.Object_4);
      begin
         declare

            -- The following task doesn't really do anything. It is just around
            -- so we can set its attribute.

            task Check_It is
               entry Get_Id (Id : out Tid.Task_Id);
               entry Ok_To_Terminate;
            end Check_It;

            task body Check_It is
            begin
               accept Get_Id (Id : out Tid.Task_Id) do
                  Id := Tid.Current_Task;
               end Get_Id;
               accept Ok_To_Terminate;
            end Check_It;

            Check_It_Tid : Tid.Task_Id;
         begin
            Check_It.Get_Id (Check_It_Tid);

            begin
               -- set Check_It's attribute to 1 and the environment task's to 2
               Fta.Set_Value (Cxc7003_0.Object_1, Check_It_Tid);
               Fta.Set_Value (Cxc7003_0.Object_2);
            exception
               when Info : others =>
                  Report.Not_Applicable
                    ("System not capable of supporting a " &
                     "task attribute of a controlled type -- " &
                     "exception during Set_Value");
                  if Verbose then
                     Report.Comment
                       ("Exception raised was" &
                        Ada.Exceptions.Exception_Name (Info));
                  end if;
                  raise Not_Supported_Error;
            end;

            -- set a new value for Check_It's attribute and make sure that the
            -- object with Id 1 was finalized.
            Cxc7003_0.Last_Finalized := 9; -- state known not to be
            -- produced by finalize
            Fta.Set_Value (Cxc7003_0.Object_3, Check_It_Tid);
            case Cxc7003_0.Last_Finalized is
               when 1 =>
                  if Verbose then
                     Report.Comment ("Set_value finalization ok");
                  end if;
               when 2 =>
                  Report.Failed ("Wrong task's attribute finalized");
               when 9 =>
                  Report.Failed ("No finalization occurred for set_value");
               when others =>
                  Report.Failed
                    ("Unexpected result for set_value:" &
                     Integer'Image (Cxc7003_0.Last_Finalized));
            end case;

            -- restore the default value for the environment task. This should
            -- finalize the object with Id 2
            Cxc7003_0.Last_Finalized := 9; -- state known not to be
            -- produced by finalize
            Fta.Reinitialize;
            case Cxc7003_0.Last_Finalized is
               when 2 =>
                  if Verbose then
                     Report.Comment ("Reinitialize finalization ok");
                  end if;
               when 3 =>
                  Report.Failed
                    ("Wrong task's attribute finalized" & " in reinitialize");
               when 9 =>
                  Report.Failed
                    ("No finalization occurred" & " for reinitialize");
               when others =>
                  Report.Failed
                    ("Unexpected result for" & " reinitialize:" &
                     Integer'Image (Cxc7003_0.Last_Finalized));
            end case;

            -- let the task terminate
            Check_It.Ok_To_Terminate;

         exception
            when Not_Supported_Error =>
               raise;
            when others =>
               Report.Failed ("Unexpected exception (1)");
         end;
      exception
         when Not_Supported_Error =>
            raise;
         when others =>
            Report.Failed ("Unexpected exception (2)");
      end;
   exception
      when Not_Supported_Error =>
         null; -- Not Supported already reported.
      when Info : others =>
         -- This handler should be reachable only for exceptions raised during
         -- the instantiation of task attributes.
         Report.Not_Applicable
           ("System not capable of supporting a task " &
            "attribute of a controlled type -- exception " &
            "during instantiation");
         if Verbose then
            Report.Comment
              ("Exception raised was" & Ada.Exceptions.Exception_Name (Info));
         end if;
   end;

   Report.Result;
end Cxc7003;
