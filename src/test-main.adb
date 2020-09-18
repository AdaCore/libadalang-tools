with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Drivers;

with Test.Actions;
with Test.Command_Lines;
with Test.Common;

procedure Test.Main is

   --  Main procedure for gnattest

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch) is
      pragma Unreferenced (Swit); -- ????
   begin
      if Phase = Cmd_Line_1 then
         null; -- ????

         return;
      end if;
   end Callback;

   Tool : Actions.Test_Tool;
   Cmd : Command_Line (Test.Command_Lines.Descriptor'Access);

begin
   Test.Actions.Register_Specific_Attributes;

   Utils.Drivers.Driver
     (Cmd,
      Tool,
      Tool_Package_Name     => Test.Common.GT_Package,
      Needs_Per_File_Output => True,
      Callback              => Callback'Unrestricted_Access);
end Test.Main;
