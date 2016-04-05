with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Tools;

package LAL_UL.Drivers is

   procedure Driver
     (Cmd                   : in out Command_Line;
      Tool                  : in out Tools.Tool_State'Class;
      Tool_Package_Name     :        String;
      Needs_Per_File_Output :        Boolean        := False;
      No_Preprocessing      :        Boolean        := False;
      Callback              :        Parse_Callback := null);

end LAL_UL.Drivers;
