with ASIS_UL.Debug; use ASIS_UL.Debug;

with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Drivers;

with METRICS.Actions;
with METRICS.Command_Lines;

procedure METRICS.Main is

   --  Prototype metrics tool.

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch) is
      use METRICS.Command_Lines;
      use Metrics_String_Seq_Switches;
      use String_Ref_Vectors;
   begin
      if Phase = Cmd_Line_1 then

         if Metrics_String_Seq_Switches.Valid (Swit.Switch) then
            declare
               Switch : constant Metrics_String_Seqs := From_All (Swit.Switch);
            begin
               case Switch is
                  when Gnatmetric_Debug =>
                     for Dbg of Swit.Seq_Val loop
                        Set_Debug_Options (Dbg.all);
                     end loop;
               end case;
            end;
         end if;

         return;
      end if;
   end Callback;

   Tool : Actions.Metrics_Tool;

begin
   LAL_UL.Drivers.Driver
     (METRICS.Command_Lines.Cmd, -- local????
      Tool,
      Tool_Package_Name     => "metrics",
      Needs_Per_File_Output => True,
      Callback              => Callback'Unrestricted_Access);
end METRICS.Main;
