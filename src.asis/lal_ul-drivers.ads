with Libadalang.Analysis; use Libadalang.Analysis;

with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;

package LAL_UL.Drivers is

   procedure Driver
     (Cmd                   : in out Command_Line;
      Tool_Package_Name     :        String;
      Needs_Per_File_Output :        Boolean        := False;
      No_Preprocessing      :        Boolean        := False;
      Callback              :        Parse_Callback := null;
      Per_File_Tool_Action  :        not null access procedure
        (Cmd : Command_Line; File_Name : String; Unit : Analysis_Unit);
      Print_Help            : not null access procedure);

end LAL_UL.Drivers;
