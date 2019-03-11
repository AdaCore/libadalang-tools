with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Drivers;

with Stub.Actions;
with Stub.Command_Lines;

procedure Stub.Main is

   --  Main procedure for lalstub

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch) is
     null;

   Tool : Actions.Stub_Tool;
   Cmd : Command_Line (Stub.Command_Lines.Descriptor'Access);

begin
   Utils.Drivers.Driver
     (Cmd,
      Tool,
      Tool_Package_Name     => "gnatstub",
      Needs_Per_File_Output => True,
      Callback              => Callback'Unrestricted_Access);
end Stub.Main;
