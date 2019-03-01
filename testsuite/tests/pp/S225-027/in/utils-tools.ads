with GNATCOLL.Projects;

with Libadalang.Analysis; use Libadalang.Analysis;

with Utils.Command_Lines; use Utils.Command_Lines;

package Utils.Tools is

   --  Each tool should derive from Tool_State, and override the ops.
   --  The driver calls Init, then Per_File_Action on each source file,
   --  then Final.

   type Tool_State is abstract tagged limited record
      Project_Tree : GNATCOLL.Projects.Project_Tree_Access;
      --  The driver sets this to the current project. If there is no
      --  -P switch, then the Status will be Empty.

      Project_Env : GNATCOLL.Projects.Project_Environment_Access;

      Context : Analysis_Context := No_Analysis_Context;
   --  The only tool that needs access to the Context is gnatstub.
   end record;

   procedure Init
     (Tool : in out Tool_State; Cmd : in out Command_Line) is abstract;
   procedure Per_File_Action
     (Tool  : in out Tool_State; Cmd : Command_Line; File_Name : String;
      Input :    String; BOM_Seen : Boolean; Unit : Analysis_Unit) is abstract;
   --  Input is the contents of the file named by File_Name.
   --  BOM_Seen is True if there was a BOM at the start of the file;
   --  the BOM is not included in Input.

   procedure Process_File
     (Tool      : in out Tool_State'Class; Cmd : in out Command_Line;
      File_Name :        String; Reparse : Boolean := False);
   --  This class-wide procedure takes care of some bookkeeping, and then
   --  dispatches to Per_File_Action.
   --
   --  If Tool.Context is nil, Process_File creates it. This is necessary
   --  because we have to defer the Create_Context call until after we've read
   --  the first file, because it might set the Wide_Character_Encoding via the
   --  BOM. This makes the somewhat questionable assumption that all files have
   --  the same encoding (which is necessary anyway if it's controlled by the
   --  command line).
   --
   --  Reparse has the same meaning as the parameter of Get_From_File. See
   --  Update_Body in Stub.Actions for the reason (search for the call to
   --  Process_File).

   procedure Final (Tool : in out Tool_State; Cmd : Command_Line) is abstract;
   procedure Tool_Help (Tool : Tool_State) is abstract;

end Utils.Tools;
