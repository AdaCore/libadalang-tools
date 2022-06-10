------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.Directory_Operations;
with GNAT.Traceback.Symbolic;

with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Traces;

with Libadalang.Analysis;            use Libadalang.Analysis;
with Libadalang.Project_Provider;    use Libadalang.Project_Provider;

with Utils.Command_Lines.Common;     use Utils.Command_Lines.Common;
with Utils.Environment;
with Utils.Err_Out;
with Utils.Formatted_Output;
with Utils.Projects.Aggregate;
with Utils.String_Utilities; use Utils.String_Utilities;
with Utils.Tool_Names; use Utils.Tool_Names;
with Utils.Versions;

package body Utils.Projects is
   use Ada.Text_IO;

   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Source_Selection_Switches;

   My_Project_Tree : aliased Project_Tree;
   Project_Env : Project_Environment_Access;

   function Project_File_Name (Cmd : Command_Line) return String with
      Pre => Arg (Cmd, Project_File) /= null;
      --  Returns the project file name with ".gpr" appended if necessary

   function Main_Unit_Names
     (Cmd : Command_Line) return String_Ref_Array is
     (if
        Arg (Cmd) = Update_All
      then
        (if Num_File_Names (Cmd) = 0 then []
         else File_Names (Cmd))
      else []);
   --  If "-U main_unit_1 main_unit_2 ..." was specified, this returns the list
   --  of main units. Otherwise (-U was not specified, or was specified without
   --  main unit names), returns empty array.

   procedure Post_Cmd_Line_1 (Cmd : Command_Line);
   --  This is called by Process_Command_Line after the first pass through
   --  the command-line arguments.

   procedure Recompute_View_Errors (S : String);
   --  Print out all errors but the warnings about missing directories.

   procedure Gnatstub_Special_Case (Cmd : in out Command_Line);
   --  Gnatstub accepts a command line of the form "src dir", where "dir" is
   --  treated as "--output-dir=dir". This feature is not documented, but is
   --  used by GPS. It is unfortunate that this general-purpose driver has to
   --  know about a particular tool, but Init is called too late.

   procedure Process_Project
     (Cmd                       : in out Command_Line;
      Cmd_Text                  :        Argument_List_Access;
      Global_Report_Dir         :    out String_Ref;
      Preprocessing_Allowed     :        Boolean;
      My_Project_Tree           : in out Project_Tree;
      My_Project_Env            :        Project_Environment_Access;
      Tool_Package_Name         :        String;
      Compute_Project_Closure   :        Boolean;
      Callback                  :        Parse_Callback);

   function Project_File_Name (Cmd : Command_Line) return String is
      Name : String renames Arg (Cmd, Project_File).all;
      Ext : constant String :=
        (if Has_Suffix (Name, Suffix => ".gpr") then "" else ".gpr");
   begin
      return Name & Ext;
   end Project_File_Name;

   ---------------------------
   -- Recompute_View_Errors --
   ---------------------------

   procedure Recompute_View_Errors (S : String) is
   begin
      if Index (S, "warning") /= 0
        and then Index (S, "directory") /= 0
        and then Index (S, "not found") /= 0
      then
         return;
      else
         Cmd_Error_No_Tool_Name (S);
      end if;
   end Recompute_View_Errors;

   procedure Process_Project
     (Cmd                       : in out Command_Line;
      Cmd_Text                  :        Argument_List_Access;
      Global_Report_Dir         :    out String_Ref;
      Preprocessing_Allowed     :        Boolean;
      My_Project_Tree           : in out Project_Tree;
      My_Project_Env            :        Project_Environment_Access;
      Tool_Package_Name         :        String;
      Compute_Project_Closure   :        Boolean;
      Callback                  :        Parse_Callback)
   is
      use String_Access_Vectors;

      procedure Initialize_Environment;
      --  Initializes the environment for extracting the information from the
      --  project file. This includes setting the parameters specific to the
      --  given tool version assuming that the tools for cross environment are
      --  named in a standard way (that is, <cross-prefix>-<tool_name>).

      function Is_Ada_File (File : Virtual_File) return Boolean;
      --  Checks if the given source file is an Ada file.

      function Is_Externally_Built (File : Virtual_File) return Boolean;
      --  Checks if the given source file belongs to an externally build
      --  library.

      procedure Load_Tool_Project;
      --  Does the same as GNATCOLL.Projects.Load, the only difference is that
      --  all the parameters except the project are hidden. This procedure
      --  never generates any error or warning message, because here we do
      --  not know values of external variables. ????????????????Inspect all
      --  comments below.

      procedure Get_Files_From_Closure;
      --  Provided that the tool arguments contain '-U main_unit' parameter,
      --  tries to get the full closure of main_unit and to store it as tool
      --  argument files.

      procedure Get_Sources_From_Project;
      --  Extracts and stores the list of sources of the project to process as
      --  tool arguments.
      --
      --  More documentation is needed:
      --
      --  * when we extract the sources from the project * what happens when
      --    o there is no -U option
      --    o -U option is specified, but without the main unit
      --    o -U option is specified with the main unit name
      --
      --  ??? Extended projects???

      procedure Set_External_Values;
      --  For each value of an external variable that has been stored as a
      --  result of the initial parameter processing, checks that the given
      --  variable indeed is defined in the project, and the value specified
      --  for it is the valid value for this variable. If both checks are
      --  successful sets the given value as the value of a given variable,
      --  otherwise raises Parameter_Error. If all the stored pairs of external
      --  variable names and corresponding values are successfully processed,
      --  recomputes the view of the project with these values of external
      --  variables.

      procedure Extract_Tool_Options;
      --  Extracts tool attributes from the project file. The default does the
      --  following: * if there is exactly one source file specified, tries to
      --  get the
      --    tool options from the Switches attribute with the corresponding
      --    index. If there is no such Switches attribute, tries to get tool
      --    attributes from the Default_Switches attribute.
      --  * otherwise tries to get the tool attributes from the
      --    Default_Switches attribute.

      procedure Load_Aggregated_Project;
      --  Loads My_Project_Tree (that is supposed to be an aggregate project),
      --  then unloads it and loads in the same environment the project passed
      --  as a parameter of '--aggregated_project_file option' (which is
      --  supposed to be a (non-aggregate) project aggregated by
      --  My_Project_Tree.

      procedure Set_Global_Result_Dirs;
      --  Sets the directory to place the global tool results into.

      procedure Set_Individual_Source_Options;
      --  Also for each file from the project that is a tool argument computes
      --  and stores in the source table the list of compiler options needed
      --  to create the tree for this particular file. It also defines the
      --  directory the file-specific results of the tool should be placed
      --  into. This procedure should be called *after* storing the argument
      --  files in the source table. It is NOT a part of the actions combined
      --  in Process_Project_File procedure.????????????????

      function Needed_For_Tree_Creation (Option : String) return Boolean;
      --  Checks if the argument is the compilation option that is needed for
      --  tree creation. Also gives an error message if there is a preprocessor
      --  switch, and Preprocessing_Allowed is False.

      ----------------------------
      -- Initialize_Environment --
      ----------------------------

      procedure Initialize_Environment is

         Target_Opt : constant String := (if Arg (Cmd, Target) /= null
                                 then Arg (Cmd, Target).all
                                 else "");
         RTS_Opt : constant String := (if Arg (Cmd, Run_Time_System) /= null
                              then Arg (Cmd, Run_Time_System).all
                              else "");
      begin
         GNATCOLL.Traces.Parse_Config_File;
         Initialize (Project_Env);

         Project_Env.Set_Target_And_Runtime
           (Target  => (if Target_Opt /= "" then Target_Opt else Target),
            Runtime => RTS_Opt);

         if Arg (Cmd, Follow_Symbolic_Links) then
            Project_Env.Set_Trusted_Mode (True);
         end if;

         Set_Automatic_Config_File (Project_Env.all);
      end Initialize_Environment;

      -----------------------
      -- Load_Tool_Project --
      -----------------------

      procedure Load_Tool_Project is
         Error_Printed : Boolean := False;

         procedure Errors (S : String);
         --  Load calls this in case of certain (but not all) errors

         procedure Errors (S : String) is
         begin
            if Index (S, " not a regular file") /= 0 then
               Err_Out.Put
                 ("\1: project file \2 not found\n",
                  Tool_Name, Project_File_Name (Cmd));
            else
               Err_Out.Put ("\1: \2\n", Tool_Name, S);
            end if;
            Error_Printed := True;
         end Errors;
      begin
         My_Project_Tree.Load
           (GNATCOLL.VFS.Create (+Project_File_Name (Cmd)),
            Project_Env,
            Errors => Errors'Unrestricted_Access,
            Recompute_View => False,
            Report_Missing_Dirs => False);

         My_Project_Tree.Recompute_View
           (Errors => Recompute_View_Errors'Unrestricted_Access);

         if Is_Aggregate_Project (My_Project_Tree.Root_Project) then

            if My_Project_Tree.Root_Project = No_Project then
               Cmd_Error ("project not loaded");
            end if;

            Aggregate.Collect_Aggregated_Projects
              (My_Project_Tree.Root_Project);

            if Aggregate.Use_Subprocesses_For_Aggregated_Projects then
               --  General case - more than one project is aggregated. We
               --  process them one by one spawning the tool for each
               --  project. See Process_Aggregated_Projects in
               --  Utils.Projects.Aggregate.

               null;

            else
               --  Important and useful particular case - exactly one
               --  project is aggregated, so we load it in the environment
               --  that already has all the settings from the argument
               --  aggregate project:

               My_Project_Tree.Unload;
               Load
                 (Self                => My_Project_Tree,
                  Root_Project_Path   =>
                    Create (Filesystem_String
                              (Aggregate.Get_Aggregated_Prj_Src.all)),
                  Env                 => Project_Env,
                  Errors              => Errors'Unrestricted_Access,
                  Report_Missing_Dirs => False);
            end if;
         end if;

      exception
         when Invalid_Project =>
            if Error_Printed then
               raise Command_Line_Error;
            end if;

            Cmd_Error (Project_File_Name (Cmd) & ": invalid project");
      end Load_Tool_Project;

      -----------------------------
      -- Load_Aggregated_Project --
      -----------------------------

      procedure Load_Aggregated_Project is
         pragma Assert (Arg (Cmd, Aggregated_Project_File) /= null);

         procedure Errors (S : String);

         procedure Errors (S : String) is
         begin
            if Index (S, " not a regular file") /= 0 then
               Cmd_Error ("project file " &
                          Project_File_Name (Cmd) &
                          " not found");
            elsif Index (S, "is illegal for typed string") /= 0 then
               Cmd_Error (S);
            elsif Index (S, "warning") /= 0
                 and then Index (S, "directory") /= 0
                 and then Index (S, "not found") /= 0
            then
               return;
            else
               Cmd_Error (S);
            end if;
         end Errors;

         Aggregated_Name : constant Filesystem_String :=
           Filesystem_String (Arg (Cmd, Aggregated_Project_File).all);

      --  Start of processing for Load_Aggregated_Project

      begin
         My_Project_Tree.Load
           (GNATCOLL.VFS.Create (+Project_File_Name (Cmd)),
            Project_Env,
            Errors              => Errors'Unrestricted_Access,
            Report_Missing_Dirs => False);

         if My_Project_Tree.Root_Project = No_Project then
            Cmd_Error ("project not loaded");
         end if;

         pragma Assert (Is_Aggregate_Project (My_Project_Tree.Root_Project));

         My_Project_Tree.Unload;

         Load
           (Self                => My_Project_Tree,
            Root_Project_Path   => Create (Aggregated_Name),
            Env                 => Project_Env,
            Errors              => Errors'Unrestricted_Access,
            Report_Missing_Dirs => False);

         pragma Assert
           (not Is_Aggregate_Project (My_Project_Tree.Root_Project));
      end Load_Aggregated_Project;

      -----------------
      -- Is_Ada_File --
      -----------------

      function Is_Ada_File (File : Virtual_File) return Boolean is
         use Ada.Characters.Handling;
      begin
         return To_Lower (Language (Info (My_Project_Tree, File))) = "ada";
      end Is_Ada_File;

      -------------------------
      -- Is_Externally_Built --
      -------------------------

      function Is_Externally_Built (File : Virtual_File) return Boolean is
         F_Info : constant File_Info := Info (My_Project_Tree, File);
         Proj   : constant Project_Type := Project (F_Info);
         Attr   : constant Attribute_Pkg_String :=
           Build ("", "externally_built");

         use Ada.Characters.Handling;
      begin
         if Has_Attribute (Proj, Attr) then
            if To_Lower (Attribute_Value (Proj, Attr)) = "true" then
               return True;
            end if;
         end if;
         return False;
      end Is_Externally_Built;

      ----------------------------
      -- Get_Files_From_Closure --
      ----------------------------

      procedure Get_Files_From_Closure is
         Provider : constant Unit_Provider_Reference :=
           Create_Project_Unit_Provider
             (Tree             => My_Project_Tree'Unchecked_Access,
              Env              => My_Project_Env,
              Is_Project_Owner => False);

         Ctx  : constant Analysis_Context :=
           Create_Context (Unit_Provider => Provider);

         Mains : constant String_Ref_Array := Main_Unit_Names (Cmd);

         Mains_From_Prj : GNAT.OS_Lib.String_List_Access :=
           My_Project_Tree.Root_Project.Attribute_Value
             (Attribute    => Main_Attribute,
              Use_Extended => True);

         package String_Sets is new
           Ada.Containers.Indefinite_Ordered_Sets (String);
         use String_Sets;

         Closure_Incomplete : Boolean := False;

         Closure : Set;
         --  Cumulative closure of given main(s)

         package Spec_To_Separates_Maps is new
           Ada.Containers.Indefinite_Ordered_Maps (String, Set);
         use Spec_To_Separates_Maps;

         Separates : Map;
         --  Stores all separates from the project hierarchy. At the moment
         --  P_Unit_Dependencies does not return units containing separates
         --  so we need to add them manually: once we have a spec that
         --  corresponds to any separates added to the closure, we need
         --  to add corresponding separates as well.

         procedure Process_Main_Unit (Main_Full : String);
         --  Adds closure for given unit to the overall closure

         procedure Update_Closure (New_Source : String);
         --  Calculate unit dependencies with LAL, for resulting specs
         --  recursively process bodies if they exist.

         function Is_Source_Of_Interest (Full_Name : String) return Boolean;
         --  Checks whether given file is a source of user project. Filters out
         --  runtime units, sources from externally built projects and unknown
         --  files that are not sources of any project.

         ---------------------------
         -- Is_Source_Of_Interest --
         ---------------------------

         function Is_Source_Of_Interest (Full_Name : String) return Boolean
         is
            Inf : constant File_Info :=
              My_Project_Tree.Info (Create (+Full_Name));
         begin
            return Inf.Project /= No_Project
              and then not Is_Externally_Built (Create (+Full_Name));
         end Is_Source_Of_Interest;

         -----------------------
         -- Process_Main_Unit --
         -----------------------

         procedure Process_Main_Unit (Main_Full : String) is
         begin
            Update_Closure (Main_Full);

            --  If main is a spec we also need to include the corresponding
            --  body in the closure if it exists.
            if My_Project_Tree.Info (Create (+Main_Full)).Unit_Part = Unit_Spec
            then
               declare
                  Main_Other : constant String :=
                    My_Project_Tree.Other_File
                      (Create (+Main_Full)).Display_Full_Name;
               begin
                  if Is_Source_Of_Interest (Main_Other) then
                     Update_Closure (Main_Other);
                  end if;
               end;
            end if;
         end Process_Main_Unit;

         --------------------
         -- Update_Closure --
         --------------------

         procedure Update_Closure (New_Source : String) is
            Unit : Analysis_Unit;
            CU   : Compilation_Unit;
         begin
            if Closure.Contains (New_Source)
              or else not Is_Source_Of_Interest (New_Source)
            then
               return;
            end if;

            Closure.Insert (New_Source);

            Unit := Ctx.Get_From_File (New_Source);
            CU   := Unit.Root.As_Compilation_Unit;

            for Dep of CU.P_Unit_Dependencies loop

               declare
                  Src    : constant String       := Dep.Unit.Get_Filename;
                  Src_VF : constant Virtual_File := Create (+Src);
                  Inf    : constant File_Info    :=
                    My_Project_Tree.Info (Src_VF);
               begin

                  if not Closure.Contains (Src)
                    and then Is_Source_Of_Interest (Src)
                  then
                     Closure.Insert (Src);
                     if Inf.Unit_Part = Unit_Spec then
                        Update_Closure
                          (My_Project_Tree.Other_File (Src_VF).
                               Display_Full_Name);

                        if Separates.Contains (Src) then
                           for Sep of Separates.Element (Src) loop
                              Update_Closure (Sep);
                           end loop;
                        end if;
                     end if;
                  end if;

               end;

            end loop;

         exception
            when Ex : others =>
               Closure_Incomplete := True;
               Formatted_Output.Put
                 ("\1\n",
                  "could not get dependencies of "
                  & GNAT.Directory_Operations.Base_Name (New_Source));
               if Debug_Flag_U then
                  Formatted_Output.Put
                    ("\1\n",
                     Ada.Exceptions.Exception_Name (Ex)
                     & " : "
                     & Ada.Exceptions.Exception_Message (Ex)
                     & ASCII.LF
                     & GNAT.Traceback.Symbolic.Symbolic_Traceback (Ex));
               end if;
         end Update_Closure;

      begin
         --  Populating Separates. This is a temporary solution until
         --  P_Unit_Dependencies starts returning separates.
         declare
            Sources : File_Array_Access :=
              My_Project_Tree.Root_Project.Source_Files (Recursive => True);
            Tmp_Set : Set;
            Spec_VF : Virtual_File;
         begin
            for Src of Sources.all loop

               if My_Project_Tree.Info (Src).Unit_Part = Unit_Separate
               then
                  Spec_VF := My_Project_Tree.Other_File (Src);

                  if Separates.Contains (Spec_VF.Display_Full_Name) then
                     Tmp_Set := Separates.Element (Spec_VF.Display_Full_Name);
                     Tmp_Set.Include (Src.Display_Full_Name);
                     Separates.Replace (Spec_VF.Display_Full_Name, Tmp_Set);
                  else
                     Tmp_Set.Include (Src.Display_Full_Name);
                     Separates.Include (Spec_VF.Display_Full_Name, Tmp_Set);
                  end if;

                  Tmp_Set.Clear;
               end if;
            end loop;

            Unchecked_Free (Sources);
         end;

         --  Mains on the command line take precedence over the ones specified
         --  in the project file.
         if Mains'Length > 0 then
            for Main of Mains loop
               Process_Main_Unit
                 (My_Project_Tree.Create (+Main.all).Display_Full_Name);
            end loop;
         else
            for Main of Mains_From_Prj.all loop
               Process_Main_Unit
                 (My_Project_Tree.Create (+Main.all).Display_Full_Name);
            end loop;
         end if;

         Free (Mains_From_Prj);

         if Closure_Incomplete then
            Formatted_Output.Put ("could not get complete closure\n");
         end if;

         --  We first need to erase the main unit names from the command
         --  line to avoid dulicates.
         Clear_File_Names (Cmd);

         if Debug_Flag_U then
            Formatted_Output.Put ("Closure:\n");
         end if;
         for Src of Closure loop
            Append_File_Name (Cmd, Src);
            if Debug_Flag_U then
               Formatted_Output.Put ("\1\n", Src);
            end if;
         end loop;

      exception
         when others =>
            Cmd_Error_No_Tool_Name
              ("could not get closure of specified sources");
      end Get_Files_From_Closure;

      ------------------------------
      -- Get_Sources_From_Project --
      ------------------------------

      procedure Get_Sources_From_Project is
         Prj   : Project_Type;
         Files : File_Array_Access;
--         Success  : Boolean := False;

         Num_Names : constant Natural := Num_File_Names (Cmd);
         --  Number of File_Names on the command line
         Num_Files_Switches : constant Natural :=
           Arg_Length (Cmd, Common.Files);
         --  Number of "-files=..." switches on the command line
         Argument_File_Specified : constant Boolean :=
           (if Arg (Cmd) = Update_All then Num_Files_Switches > 0
            else Num_Names > 0 or else Num_Files_Switches > 0);
      --  True if we have source files specified on the command line. If -U
      --  (Update_All) was specified, then the "file name" (if any) is taken
      --  to be the main unit name, not a file name.

         function Has_Ada_Mains_Only return Boolean;
         --  Checks that root project has mains specified and all of them
         --  are Ada mains, no C/C++ or other languages.

         ------------------------
         -- Has_Ada_Mains_Only --
         ------------------------

         function Has_Ada_Mains_Only return Boolean is
            Mains_From_Prj : GNAT.OS_Lib.String_List_Access :=
              My_Project_Tree.Root_Project.Attribute_Value
                (Attribute    => Main_Attribute,
                 Use_Extended => True);
         begin
            if Mains_From_Prj = null
              or else Mains_From_Prj.all'Length = 0
            then
               Free (Mains_From_Prj);
               return False;
            end if;

            for Main of Mains_From_Prj.all loop
               if not Is_Ada_File (My_Project_Tree.Create (+Main.all)) then
                  Free (Mains_From_Prj);
                  return False;
               end if;
            end loop;

            Free (Mains_From_Prj);
            return True;
         end Has_Ada_Mains_Only;

      begin

         --  We get file names from the project file if Compute_Project_Closure
         --  is True, and no file names were given on the command line, either
         --  directly, or via one or more "-files=par_file_name" switches.

         if Compute_Project_Closure and then not Argument_File_Specified then
            if Arg (Cmd) = No_Subprojects
              or else (Main_Unit_Names (Cmd)'Length = 0
                       and then not (Arg (Cmd) = No_Source_Selection
                                     and then Has_Ada_Mains_Only))
            then
               Prj := My_Project_Tree.Root_Project;

               Files := Prj.Source_Files
                 (Recursive => Arg (Cmd) /= No_Subprojects,
                  Include_Externally_Built => False);

               if Arg (Cmd) = No_Subprojects then
                  Prj := Prj.Extended_Project;
                  while Prj /= No_Project loop
                     Append
                       (Files,
                        Prj.Source_Files
                          (Recursive                => False,
                           Include_Externally_Built => False).all);

                     Prj := Prj.Extended_Project;
                  end loop;
               end if;

               for F in Files'Range loop
                  if not Is_Externally_Built (Files (F))
                    and then Is_Ada_File (Files (F))
                  then
                     Append_File_Name (Cmd, Files (F).Display_Base_Name);
                     --  No need to call Callback for non-switches
                  end if;
               end loop;

               if Arg (Cmd) = Update_All then
                  if Num_File_Names (Cmd) = 0 then
                     Cmd_Error
                       (Project_File_Name (Cmd) &
                        "does not contain source files");
                  end if;
               end if;

            else
               Get_Files_From_Closure;
            end if;
         end if;

      end Get_Sources_From_Project;

      -------------------------
      -- Set_External_Values --
      -------------------------

      procedure Set_External_Values is
         X_Vars : constant String_Ref_Array := Arg (Cmd, External_Variable);
         GPR_TOOL_Set : Boolean := False;
         --  True if -XGPR_TOOL=... appears on the command line
      begin
         for X of X_Vars loop
            --  X is of the form "VAR=value"

            declare
               pragma Assert (X'First = 1);
               Equal : constant Natural := Index (X.all, "=");
               X_Var : String renames X (1 .. Equal - 1);
               X_Val : String renames X (Equal + 1 .. X'Last);
            begin
               if Equal = 0 then -- "=" not found (????say so)
                  Cmd_Error ("wrong parameter of -X option: " & X.all);
               end if;

               if X_Var = "GPR_TOOL" then
                  GPR_TOOL_Set := True;
               end if;

               Project_Env.Change_Environment (X_Var, X_Val);
            end;
         end loop;

         --  Set GPR_TOOL, unless it is already set via an environment variable
         --  or on the command line.

         if not Ada.Environment_Variables.Exists ("GPR_TOOL")
           and then not GPR_TOOL_Set
         then
            Project_Env.Change_Environment ("GPR_TOOL", Basic_Tool_Name);
         end if;
      end Set_External_Values;

      --------------------------
      -- Extract_Tool_Options --
      --------------------------

      procedure Extract_Tool_Options is
         Arg_File_Name : String_Access;

         Proj : constant Project_Type := Root_Project (My_Project_Tree);

         Attr_Switches : constant Attribute_Pkg_List :=
           Build (Tool_Package_Name, "Switches");
         Attr_Def_Switches : constant Attribute_Pkg_List :=
           Build (Tool_Package_Name, "Default_Switches");
         Attr_GT_Switches : constant Attribute_Pkg_List :=
           Build (Tool_Package_Name, "GNATtest_Switches");

         Attr_Indexes : String_List_Access;
         Index_Found  : Boolean := False;
         Project_Switches_Text : Argument_List_Access;
      begin
         if Num_File_Names (Cmd) = 1 then
            --  ????What if the "one file" comes from -files=
            Arg_File_Name := new String'(File_Names (Cmd) (1).all);

            Attr_Indexes :=
              new String_List'(Attribute_Indexes (Proj, Attr_Switches));

            for J in Attr_Indexes'Range loop
               if Arg_File_Name.all = Attr_Indexes (J).all then
                  --  What about non-case-sensitive system?
                  Index_Found := True;
                  exit;
               end if;
            end loop;
         end if;

         if not Index_Found then
            --  We have to get tool options from Default_Sources

            if Has_Attribute (Proj, Attr_Def_Switches, "ada") then
               Project_Switches_Text :=
                 Attribute_Value (Proj, Attr_Def_Switches, "ada");
            elsif Has_Attribute (Proj, Attr_GT_Switches) then
               Project_Switches_Text :=
                 Attribute_Value (Proj, Attr_GT_Switches);
            end if;
         else
            if Has_Attribute (Proj, Attr_Switches) then
               Project_Switches_Text :=
                 Attribute_Value (Proj, Attr_Switches, Arg_File_Name.all);
            end if;
         end if;

         if Project_Switches_Text /= null then
            Parse
              (Project_Switches_Text,
               Cmd,
               Phase              => Project_File,
               Callback           => Callback,
               Collect_File_Names => False);
            --  Collect_File_Names doesn't matter, because we're only parsing
            --  switches.
         end if;
      end Extract_Tool_Options;

      ----------------------------
      -- Set_Global_Result_Dirs --
      ----------------------------

      procedure Set_Global_Result_Dirs is
         Global_Report_Dir : Virtual_File;
      begin
         if not Arg (Cmd, No_Objects_Dir) then
            if Arg (Cmd, Subdirs) /= null then
               Set_Object_Subdir (Project_Env.all, +Arg (Cmd, Subdirs).all);
               Recompute_View
                 (My_Project_Tree,
                  Errors => Recompute_View_Errors'Unrestricted_Access);
            end if;

            Global_Report_Dir := My_Project_Tree.Root_Project.Object_Dir;

            if Global_Report_Dir = No_File then
               Global_Report_Dir := My_Project_Tree.Root_Project.Project_Path;
            end if;

            Process_Project.Global_Report_Dir :=
              new String'(Display_Dir_Name (Global_Report_Dir));
         end if;
      end Set_Global_Result_Dirs;

      -----------------------------------
      -- Set_Individual_Source_Options --
      -----------------------------------

      procedure Set_Individual_Source_Options is

         Sources : constant File_Array_Access :=
           My_Project_Tree.Root_Project.Source_Files (Recursive => True);

         Project_U   : Project_Type;
         Attr_Proj   : Project_Type;
         Source_Info : File_Info;
         Name        : String_Access;

         Sws        : String_List_Access;
         Is_Default : Boolean := False;

         File_Switches : String_Access_Vector;

         procedure Scan_Switches;
         --  Works on Sws as on a global object. Scans the argument, checks if
         --  the element being visited is needed for tree creation, and if it
         --  is, stores it in File_Switches.

         procedure Add_Switch (S : String);
         --  Adds S to File_Switches;

         Compiler_Local_Configuration_Pragmas : constant Attribute_Pkg_String
           := Build (Compiler_Package, "Local_Configuration_Pragmas");
         Compiler_Local_Config_File : constant Attribute_Pkg_String
           := Build (Compiler_Package, "Local_Config_File");

         function Normalize_Switch (S : String) return String;
         --  If the switch contains a path, normalizes this path. This is
         --  needed because the switch will be used from the temporary
         --  directory created by a tool.

         procedure Add_Switch (S : String) is
         begin
            Append (File_Switches, new String'(S));
         end Add_Switch;

         procedure Scan_Switches is
         begin
            for J in Sws'Range loop
               if Debug_Flag_C then
                  Put (Sws (J).all & ' ');
               end if;

               if Needed_For_Tree_Creation (Sws (J).all) then
                  Add_Switch (Normalize_Switch (Sws (J).all));
               end if;
            end loop;

            if Debug_Flag_C then
               if Is_Default then
                  Put ("(default)");
               end if;

               Put_Line ("");
            end if;

            Free (Sws);
         end Scan_Switches;

         function Normalize_Switch (S : String) return String is
            Res : constant String := Trim (S, Both);
            Opt_Start  : constant Natural := S'First;
            Opt_End    :          Natural;
            Path_Start :          Natural;
            Path_End   : constant Natural := S'Last;
         begin
            if Res'Length >= 9
              and then
               Res (Opt_Start .. Opt_Start + 5) = "-gnate"
              and then
               Res (Opt_Start + 6) in 'e' | 'p'
            then
               Opt_End    := Opt_Start + 6;
               Path_Start := Opt_End + 1;

               while Path_Start < Path_End and then
                     Res (Path_Start) in ' ' | '='
               loop
                  Path_Start := Path_Start + 1;
               end loop;

               return Res (Opt_Start .. Opt_End) &
                           Normalize_Pathname (Res (Path_Start .. Path_End));
            else
               return Res;
            end if;
         end Normalize_Switch;

      --  Start of processing for Set_Individual_Source_Options

      begin
         for S in Sources'Range loop
            Source_Info := My_Project_Tree.Info (Sources (S));
            Project_U   := Project (Source_Info);
            Name        := new String'(Display_Base_Name (Sources (S)));

            if Debug_Flag_C then
               Put_Line ("Switches defined for " & Name.all);
            end if;

            Switches
              (Project          => Project_U,
               In_Pkg           => Compiler_Package,
               File             => Sources (S),
               Language         => "ada",
               Value            => Sws,
               Is_Default_Value => Is_Default);

            Scan_Switches;

            Switches
              (Project          => Project_U,
               In_Pkg           => Builder_Package,
               File             => Sources (S),
               Language         => "ada",
               Value            => Sws,
               Is_Default_Value => Is_Default);

            Scan_Switches;

            if Arg (Cmd) /= Update_All
              and then Has_Attribute
                (Project_U,
                 Compiler_Local_Configuration_Pragmas)
            then
               Attr_Proj :=
                 Attribute_Project
                   (Project   => Project_U,
                    Attribute => Compiler_Local_Configuration_Pragmas);
               declare
                  Attr_Val : constant String :=
                    Attribute_Value
                      (Project_U,
                       Compiler_Local_Configuration_Pragmas);
               begin
                  Add_Switch
                    ("-gnatec=" &
                     Normalize_Pathname
                       (Name      => Attr_Val,
                        Directory =>
                          GNAT.Directory_Operations.Dir_Name
                            (Display_Full_Name (Project_Path (Attr_Proj)))));
               end;
            end if;

            if Arg (Cmd) /= Update_All
              and then Has_Attribute
                (Project_U,
                 Compiler_Local_Config_File,
                 "ada")
            then
               Attr_Proj :=
                 Attribute_Project
                   (Project   => Project_U,
                    Attribute => Compiler_Local_Config_File,
                    Index     => "ada");

               declare
                  Attr_Val : constant String :=
                    Attribute_Value
                      (Project_U,
                       Compiler_Local_Config_File,
                       "ada");
               begin
                  Add_Switch
                    ("-gnatec=" &
                     Normalize_Pathname
                       (Name      => Attr_Val,
                        Directory =>
                          GNAT.Directory_Operations.Dir_Name
                            (Display_Full_Name (Project_Path (Attr_Proj)))));
               end;
            end if;

            if Is_Empty (File_Switches) then
               if Debug_Flag_C then
                  Put_Line ("No stored switches");
               end if;
            end if;

            --  Defining the directory to place the file-specific results into:
         end loop;
      end Set_Individual_Source_Options;

      ------------------------------
      -- Needed_For_Tree_Creation --
      ------------------------------

      function Needed_For_Tree_Creation (Option : String) return Boolean is
         Result : Boolean := False;
      begin
         if Has_Prefix (Option, Prefix => "-gnateD")
           or else Has_Prefix (Option, Prefix => "-gnatep")
         then
            if Preprocessing_Allowed then
               Result := True;
            else
               Cmd_Error ("cannot preprocess argument file, " &
                            "do preprocessing as a separate step");
            end if;
         elsif Option = "-gnat83"
           or else Option = "-gnat95"
           or else Option = "-gnat05"
           or else Option = "-gnat12"
           or else Option = "-gnatdm"
           or else Option = "-gnatd.V"
           or else Option = "-gnatI"
           or else Has_Prefix (Option, Prefix => "--RTS=")
         then
            Result := True;
         end if;

         return Result;
      end Needed_For_Tree_Creation;

   --  Start of processing for Process_Project

   begin
      Initialize_Environment;
      Set_External_Values;

      if Arg (Cmd, Aggregated_Project_File) = null then
         Load_Tool_Project;
      else
         Load_Aggregated_Project;
      end if;

      if Aggregate.Use_Subprocesses_For_Aggregated_Projects then

         if Num_File_Names (Cmd) /= 0 then
            Cmd_Error
              ("argument file cannot be specified for aggregate project");
         end if;

         if Arg (Cmd) = Update_All then
            Cmd_Error ("'-U' cannot be specified for aggregate project");
         end if;

         --  Information in 'else' below is not extracted from the aggregate
         --  project itself.

      else
         Extract_Tool_Options;

         --  Now we need to Parse again, so command-line args override project
         --  file args. This needs to be done before getting sources from the
         --  project, as -U/--no-subprojects affect source selection and may
         --  override each other.
         Parse
           (Cmd_Text,
            Cmd,
            Phase              => Cmd_Line_2,
            Callback           => Callback,
            Collect_File_Names => False);

         Get_Sources_From_Project;
         Set_Global_Result_Dirs;
         Set_Individual_Source_Options;
      end if;

   end Process_Project;

   -------------------------
   -- Read_File_Names_From_File --
   -------------------------

   procedure Read_File_Names_From_File
     (Par_File_Name : String;
      Action        : not null access procedure (File_Name : String))
   is
      Arg_File    : File_Type;
      Next_Ch     : Character;
      End_Of_Line : Boolean;

      function Get_File_Name return String;
      --  Reads from Par_File_Name the name of the next file (the file to read
      --  from should exist and be opened). Returns an empty string if there is
      --  no file names in Par_File_Name any more

      function Get_File_Name return String is
         File_Name_Buffer : String (1 .. 16 * 1_024);
         File_Name_Len    : Natural := 0;
      begin
         if not End_Of_File (Arg_File) then
            Get (Arg_File, Next_Ch);

            while Next_Ch in ' ' | ASCII.HT | ASCII.LF | ASCII.CR loop
               exit when End_Of_File (Arg_File);
               Get (Arg_File, Next_Ch);
            end loop;

            --  If we are here. Next_Ch is neither a white space nor
            --  end-of-line character. Two cases are possible, they
            --  require different processing:
            --
            --  1. Next_Ch = '"', this means that the file name is surrounded
            --     by quotation marks and it can contain spaces inside.
            --
            --  2. Next_Ch /= '"', this means that the file name is bounded by
            --     a white space or end-of-line character

            if Next_Ch = '"' then

               --  We do not generate any warning for badly formatted content
               --  of the file such as
               --
               --    file_name_1
               --    "file name 2
               --    file_name_3
               --
               --  (We do not check that quotation marks correctly go by pairs)

               --  Skip leading '"'
               Get (Arg_File, Next_Ch);

               while Next_Ch not in '"' | ASCII.LF | ASCII.CR loop
                  File_Name_Len                    := File_Name_Len + 1;
                  File_Name_Buffer (File_Name_Len) := Next_Ch;

                  Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

                  exit when End_Of_Line or else End_Of_File (Arg_File);

                  Get (Arg_File, Next_Ch);
               end loop;

               if Next_Ch = '"'
                 and then not Ada.Text_IO.End_Of_Line (Arg_File)
               then
                  --  skip trailing '"'
                  Get (Arg_File, Next_Ch);
               end if;
            else
               while Next_Ch not in ' ' | ASCII.HT | ASCII.LF | ASCII.CR loop
                  File_Name_Len                    := File_Name_Len + 1;
                  File_Name_Buffer (File_Name_Len) := Next_Ch;

                  Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

                  exit when End_Of_Line or else End_Of_File (Arg_File);

                  Get (Arg_File, Next_Ch);
               end loop;
            end if;

         end if;

         return File_Name_Buffer (1 .. File_Name_Len);
      end Get_File_Name;

   --  Start of processing for Read_File_Names_From_File

   begin
      if not Is_Regular_File (Par_File_Name) then
         Cmd_Error (Par_File_Name & " does not exist");
      end if;

      Open (Arg_File, In_File, Par_File_Name);

      loop
         declare
            Tmp_Str : constant String := Get_File_Name;
         begin
            exit when Tmp_Str = "";
            Action (Tmp_Str);
         end;
      end loop;

      Close (Arg_File);
   exception
      when others =>
         Cmd_Error ("cannot read arguments from " & Par_File_Name);
   end Read_File_Names_From_File;

   procedure Gnatstub_Special_Case (Cmd : in out Command_Line) is
   begin
      if Basic_Tool_Name = "gnatstub" then
         if Num_File_Names (Cmd) = 2 then
            declare
               Old : constant String_Ref_Array := File_Names (Cmd);
            begin
               if Is_Directory (Old (2).all) then
                  --  Change "gnatstub src dir" to
                  --   "gnatstub src --output-dir=dir".

                  Clear_File_Names (Cmd);
                  Append_File_Name (Cmd, Old (1).all);
                  Set_Arg (Cmd, Output_Directory, Old (2).all);
               end if;
            end;
         end if;
      end if;
   end Gnatstub_Special_Case;

   procedure Post_Cmd_Line_1 (Cmd : Command_Line) is
      use Utils.Environment;
   begin
      for Dbg of Arg (Cmd, Command_Lines.Common.Debug) loop
         Set_Debug_Options (Dbg.all);
      end loop;

      Tool_Current_Dir := new String'(Initial_Dir);
      --  Leave Tool_Inner_Dir = null
   end Post_Cmd_Line_1;

   procedure Process_Command_Line
     (Cmd                             : in out Command_Line;
      Global_Report_Dir               :    out String_Ref;
      The_Project_Tree                :    out not null Project_Tree_Access;
      The_Project_Env                : out not null Project_Environment_Access;
      Preprocessing_Allowed           :        Boolean;
      Tool_Package_Name               :        String;
      Compute_Project_Closure         :        Boolean        := True;
      Callback                        :        Parse_Callback := null;
      Print_Help                      : not null access procedure)
   is
      --  We have to Parse the command line BEFORE we Parse the project file,
      --  because command-line args tell us the name of the project file, and
      --  options for processing it.

      --  We have to Parse the command line AFTER we Parse the project file,
      --  because command-line switches should override those from the project
      --  file.

      --  So we do both.

      --  In addition, we parse the command line ignoring errors first, for
      --  --version and --help switches. ???This also sets debug flags, etc.

      Cmd_Text : constant Argument_List_Access :=
        Text_Args_From_Command_Line (Tool_Package_Name);
   begin
      The_Project_Tree := My_Project_Tree'Access;
      The_Project_Env := Project_Env;

      --  First, process --version or --help switches, if present

      Parse
        (Cmd_Text,
         Cmd,
         Collect_File_Names => True,
         Phase              => Cmd_Line_1,
         Callback           => Callback,
         Ignore_Errors      => True);
      if Incremental_Mode (Cmd) then
         Cmd_Error ("--incremental not yet supported");
      end if;

      Post_Cmd_Line_1 (Cmd);

      if Debug_Flag_C then
         Print_Command_Line (Incremental_Mode (Cmd), Mimic_gcc (Cmd));
      end if;

      if Arg (Cmd, Version) then
         Versions.Print_Tool_Version;
         Environment.Clean_Up;
         OS_Exit (0);
      end if;

      if Arg (Cmd, Help) then
         Print_Help.all;
         Environment.Clean_Up;
         OS_Exit (0);
      end if;

      if Arg (Cmd, Cargs) then
         Cmd_Error_No_Tool_Name
           ("-cargs switch is no longer supported; use " &
            "e.g. --wide-character-encoding=8 instead of -cargs -gnatW8");
      end if;

      if Arg (Cmd, Verbose)
        and then Arg (Cmd, Aggregated_Project_File) = null
      then
         Versions.Print_Version_Info;
      end if;
      if Error_Detected (Cmd) then
         Parse
           (Cmd_Text,
            Cmd,
            Phase              => Cmd_Line_1,
            Callback           => null,
            Collect_File_Names => False);

         --  Can't get here, because Parse will have raised Command_Line_Error
         raise Program_Error;
      end if;

      declare
         procedure Update_File_Name (File_Name : in out String_Ref);
         --  Set File_Name to the full name if -P specified. If the file
         --  doesn't exist, or is not a regular file, give an error.

         procedure Look_For_GPR (File_Name : in out String_Ref);
         --  Look for a project file among argument sources. This allows
         --  to support invocation of tool with a project file without -P
         --  for example:
         --    gnatmetric simple.gpr

         procedure Append_One (File_Name : String);
         --  Append one file name onto Cmd

         procedure Look_For_GPR (File_Name : in out String_Ref) is
            use GNAT.Directory_Operations;
         begin
            if File_Extension (File_Name.all) = ".gpr" then
               Set_Arg (Cmd, Project_File, File_Name.all);
            end if;
         end Look_For_GPR;

         procedure Update_File_Name (File_Name : in out String_Ref) is
         begin
            if Is_Regular_File (File_Name.all) then
               return;
            end if;

            if Arg (Cmd, Project_File) /= null then
               declare
                  Res : constant Virtual_File :=
                    GNATCOLL.Projects.Create (My_Project_Tree, +File_Name.all);
               begin
                  if Res = No_File then
                     Cmd_Error ("file not found: " & File_Name.all);
                  end if;

                  declare
                     F_Inf : constant File_Info := Info (My_Project_Tree, Res);
                     Proj  : constant Project_Type := Project (F_Inf);
                     Attr  : constant Attribute_Pkg_String :=
                       Build ("", "externally_built");

                     use Ada.Characters.Handling;
                  begin
                     if Has_Attribute (Proj, Attr) then
                        if
                          To_Lower (Attribute_Value (Proj, Attr)) = "true"
                        then
                           Cmd_Error_No_Help
                             (File_Name.all
                              & " is from externally built project "
                              & Proj.Name);
                        end if;
                     end if;
                  end;

                  File_Name := new String'(Res.Display_Full_Name);
               end;
            end if;

            if Ada.Directories.Exists (File_Name.all)
              and then not Is_Regular_File (File_Name.all)
            then
               Cmd_Error ("not a regular file: " & File_Name.all);
            end if;
         end Update_File_Name;

         procedure Append_One (File_Name : String) is
         begin
            Append_File_Name (Cmd, File_Name);
         end Append_One;

      begin
         if Arg (Cmd, Project_File) = null then
            Iter_File_Names (Cmd, Look_For_GPR'Access);

            if Arg (Cmd, Project_File) /= null then
               --  We need to remove the project file from argument sources
               declare
                  Old : constant String_Ref_Array := File_Names (Cmd);
               begin
                  Clear_File_Names (Cmd);
                  for F of Old loop
                     if F.all /= Arg (Cmd, Project_File).all then
                        Append_File_Name (Cmd, F.all);
                     end if;
                  end loop;
               end;
            end if;
         end if;

         if Arg (Cmd, Project_File) /= null then
            Process_Project
              (Cmd,
               Cmd_Text,
               Global_Report_Dir,
               Preprocessing_Allowed,
               My_Project_Tree,
               Project_Env,
               Tool_Package_Name,
               Compute_Project_Closure,
               Callback);

            Environment.Create_Temp_Dir
              (My_Project_Tree.Root_Project.Object_Dir.Display_Full_Name);
         else
            Environment.Create_Temp_Dir;
         end if;

         --  Subsequent call to Parse command line again is performed inside
         --  Process_Project to happen in time for possible closure
         --  computation. And if there is no project file we already have
         --  all the switches from the first command line parsing.

--  Environment has:
--               if not Mimic_gcc then
--                  --  Ignore unrecognized switches in the inner invocation
--                  Error ...

         --  The following could just as well happen before the above
         --  Cmd_Line_2 Parse, because file names and "-files=par_file_name"
         --  switches came from the Cmd_Line_1 Parse, or from the project file.

         --  We process the "-files=par_file_name" switches by reading file
         --  names from the file(s) and appending those to the command line.
         --  Then we update the file names to contain directory information
         --  if appropriate.

         for Par_File_Name of Arg (Cmd, Files) loop
            Read_File_Names_From_File (Par_File_Name.all, Append_One'Access);
         end loop;

         if not Aggregate.Use_Subprocesses_For_Aggregated_Projects
           and then Num_File_Names (Cmd) = 0
         then
            Cmd_Error ("No input source file set");
         end if;

         Gnatstub_Special_Case (Cmd);

         Sort_File_Names (Cmd);
         Iter_File_Names (Cmd, Update_File_Name'Access);
      end;
   end Process_Command_Line;

   ------------------------
   -- Print_Command_Line --
   ------------------------

   procedure Print_Command_Line (Incremental_Mode, Mimic_gcc : Boolean) is
      use Ada.Command_Line, Ada.Directories;
   begin
      if Incremental_Mode then
         Formatted_Output.Put ("(outer)\n  ");
      end if;
      if Mimic_gcc then
         Formatted_Output.Put ("(inner)\n  ");
      end if;

      Formatted_Output.Put ("current directory = \1\n", Current_Directory);
      --  A4G.A_Debug.Print_Env; -- disable for now

      Formatted_Output.Put ("  \1", Command_Name);

      for X in 1 .. Argument_Count loop
         Formatted_Output.Put (" \1", Argument (X));
      end loop;
      Formatted_Output.Put ("\n");
   end Print_Command_Line;

end Utils.Projects;
