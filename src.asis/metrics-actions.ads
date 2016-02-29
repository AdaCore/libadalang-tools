with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with Libadalang.Analysis; use Libadalang.Analysis;

package METRICS.Actions is

   procedure Per_File_Action
     (Cmd : Command_Line; File_Name : String; Unit : Analysis_Unit);

end METRICS.Actions;
