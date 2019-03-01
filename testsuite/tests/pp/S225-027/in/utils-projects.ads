with Ada.Containers.Hashed_Maps;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Hash;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Utils.Command_Lines; use Utils.Command_Lines;

package Utils.Projects is

   function Hash is new GNAT.String_Hash.Hash (Character, String,
      Ada.Containers.Hash_Type);

   function Hash (S : String_Access) return Ada.Containers.Hash_Type is
     (Hash (S.all));

   function Equivalent_Keys (S1, S2 : String_Access) return Boolean is
     (S1.all = S2.all);

   package String_String_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => String_Access, Element_Type => String_Access, Hash => Hash,
      Equivalent_Keys => Equivalent_Keys);
   use String_String_Maps;
   subtype String_String_Map is String_String_Maps.Map;

   package String_String_List_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => String_Access, Element_Type => String_List_Access,
      Hash     => Hash, Equivalent_Keys => Equivalent_Keys);
   use String_String_List_Maps;
   subtype String_String_List_Map is String_String_List_Maps.Map;

   procedure Process_Command_Line
     (Cmd                                        : in out Command_Line;
      Cmd_Text, Cmd_Cargs, Project_Switches_Text :    out Argument_List_Access;
      Global_Report_Dir                          :    out String_Ref;
      Compiler_Options                           :    out String_List_Access;
      Project_RTS                                :    out String_Access;
      Individual_Source_Options                  : out String_String_List_Map;
      Result_Dirs                                :    out String_String_Map;
      The_Project_Tree :    out not null Project_Tree_Access;
      The_Project_Env :    out not null Project_Environment_Access;
      Needs_Per_File_Output :        Boolean; Preprocessing_Allowed : Boolean;
      Tool_Package_Name :    String; Compute_Project_Closure : Boolean := True;
      Callback                                   :    Parse_Callback := null;
      Post_Cmd_Line_1_Action : not null access procedure (Cmd : Command_Line);
      Tool_Temp_Dir :        String; Print_Help : not null access procedure);
   --  Processes the command line and (if specified on the command line) the
   --  project file.
   --
   --  Cmd_Text and Project_Switches_Text are the textual arguments from
   --  the command line and project file, if any. Project_Switches_Text is
   --  empty if there is no project file. These are needed temporarily so
   --  we can process "-cargs" the old way.
   --
   --  Global_Report_Dir is set to the directory name in which to place global
   --  tool results, if this information comes from the project file (see
   --  Set_Global_Result_Dirs). Otherwise it is null.
   --
   --  Compiler_Options are options that should be passed to gcc, based on the
   --  content of the project file.
   --
   --  Project_RTS is the value Runtime of the project.
   --
   --  Individual_Source_Options is a mapping from source file names to
   --  switches specified specifically for that source file.
   --
   --  Result_Dirs is a mapping from source file names to file-specific result
   --  directories. Only used if Needs_Per_File_Output is ON.
   --
   --  Tool_Package_Name is the name of the project-file package to use for the
   --  tool.
   --
   --  ????? Use Compiler_Options for more stuff,
   --  where we currently have actions that call Store_GNAT_Option_With_Path
   --  and friends.
   --
   --  Compute_Project_Closure is True if source files should be extracted from
   --  the project if no argument file is specified explicitly. There are at
   --  least two tools that pass False - gnatelim and gnatstub.
   --
   --  Callback is called for each switch, and can be used when some immediate
   --  action is required as soon as the switch is seen.
   --
   --  Post_Cmd_Line_1_Action is called after the Cmd_Line_1 pass.
   --
   --  Tool_Temp_Dir is the name of the directory to which temp files should be
   --  written.
   --
   --  Print_Help is called if --help appears on the command line.

   procedure Read_File_Names_From_File
     (Par_File_Name : String;
      Action        : not null access procedure (File_Name : String));
   --  Read each file name from the named file, and call Action for each
   --  one. This is used to implement the --files and --ignore switches.

   procedure Print_Command_Line (Incremental_Mode, Mimic_gcc : Boolean);
--  Prints the command line to standard output for debugging

end Utils.Projects;
