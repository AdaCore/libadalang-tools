with Libadalang.Analysis; use Libadalang.Analysis;
with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Tools; use LAL_UL.Tools;

package Pp.Actions is

   type Pp_Tool is new Tool_State with private;

private

   overriding procedure Init (Tool : in out Pp_Tool; Cmd : Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Pp_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Pp_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Pp_Tool);

   type Pp_Tool is new Tool_State with null record;

   --  For Debugging:

   procedure Dump
     (Tool : in out Pp_Tool;
      Message : String := "");

end Pp.Actions;
