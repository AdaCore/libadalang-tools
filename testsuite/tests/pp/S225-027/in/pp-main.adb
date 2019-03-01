with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Drivers;

with Pp.Actions;
with Pp.Command_Lines;

procedure Pp.Main is

   --  Main procedure for lalpp

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch) is
      pragma Unreferenced (Swit);
      pragma Warnings (Off); -- ???
      use Pp.Command_Lines;
      use String_Ref_Vectors;
      pragma Warnings (On);
   begin
      if Phase = Cmd_Line_1 then

         return;
      end if;
   end Callback;

   Tool : Actions.Pp_Tool;
   Cmd  : Command_Line (Pp.Command_Lines.Descriptor'Access);

begin
   Utils.Drivers.Driver
     (Cmd, Tool, Tool_Package_Name => "pretty_printer",
      Needs_Per_File_Output => True, Callback => Callback'Unrestricted_Access);
--  Should we pass Preprocessing_Allowed => False???
end Pp.Main;
