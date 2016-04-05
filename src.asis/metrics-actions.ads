with Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;

with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Tools; use LAL_UL.Tools;

package METRICS.Actions is

   type Metrics_Tool is new Tool_State with private;

   overriding procedure Init (Tool : in out Metrics_Tool; Cmd : Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Metrics_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Metrics_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Metrics_Tool);

private

   type Metrics_Tool is new Tool_State with record
      XML : Text_IO.File_Type;
   end record;

end METRICS.Actions;
