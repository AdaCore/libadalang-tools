with Libadalang.Analysis; use Libadalang.Analysis;
with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Tools;         use Utils.Tools;

pragma Warnings (Off); -- ????
private with Test.Command_Lines; -- ????might want this here, or in body
pragma Warnings (On);

package Test.Actions is

   type Test_Tool is new Tool_State with private;

private

   overriding procedure Init
     (Tool : in out Test_Tool; Cmd : in out Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Test_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Test_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Test_Tool);

   type Test_Tool is new Tool_State with record
      null; -- ????
   end record;

end Test.Actions;
