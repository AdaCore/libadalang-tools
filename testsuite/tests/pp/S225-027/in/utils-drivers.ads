with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Tools;

package Utils.Drivers is

   procedure Driver
     (Cmd : in out Command_Line; Tool : in out Tools.Tool_State'Class;
      Tool_Package_Name     : String; Needs_Per_File_Output : Boolean := False;
      Preprocessing_Allowed :        Boolean        := True;
      Callback              :        Parse_Callback := null);

end Utils.Drivers;
