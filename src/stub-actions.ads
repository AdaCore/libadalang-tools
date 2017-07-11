with Libadalang.Analysis; use Libadalang.Analysis;
with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Tools; use Utils.Tools;

package Stub.Actions is

   type Stub_Tool is new Tool_State with private;

private

   overriding procedure Init (Tool : in out Stub_Tool; Cmd : Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Stub_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Stub_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Stub_Tool);

   type Stub_Tool is new Tool_State with null record;

   --  For Debugging:

   procedure Dump
     (Tool : in out Stub_Tool;
      Message : String := "");

end Stub.Actions;
