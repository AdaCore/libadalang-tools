with GNATCOLL.Projects;

with Libadalang.Analysis; use Libadalang.Analysis;

with Utils.Command_Lines; use Utils.Command_Lines;

package Utils.Tools is

   --  Each tool should derive from Tool_State, and override the ops.
   --  The driver calls Init, then Per_File_Action on each source file,
   --  then Final.

   type Tool_State is abstract tagged limited record
      Project_Tree : GNATCOLL.Projects.Project_Tree_Access;
      --  The only tool that needs access to the Project_Tree is gnatstub.
      --  The driver sets this to the current project. If there is no
      --  -P switch, then the Status will be Empty.
   end record;

   procedure Init (Tool : in out Tool_State; Cmd : Command_Line) is abstract;
   procedure Per_File_Action
     (Tool : in out Tool_State;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit)
     is abstract;
   --  Input is the contents of the file named by File_Name.
   --  BOM_Seen is True if there was a BOM at the start of the file;
   --  the BOM is not included in Input.

   procedure Final (Tool : in out Tool_State; Cmd : Command_Line) is abstract;
   procedure Tool_Help (Tool : Tool_State) is abstract;

end Utils.Tools;
