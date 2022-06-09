------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2019-2022, AdaCore                    --
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

with Ada.Command_Line;
with Ada.Containers; use type Ada.Containers.Count_Type;
with Ada.Environment_Variables;

with Interfaces; use type Interfaces.Unsigned_16;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Traces;

with Libadalang;     use Libadalang;
with Libadalang.Project_Provider;

with Utils.Command_Lines.Common; use Utils; use Utils.Command_Lines.Common;
pragma Unreferenced (Utils.Command_Lines.Common); -- ????
with Utils.Formatted_Output;

with Utils_Debug; use Utils_Debug;

with Test.Command_Lines; use Test.Command_Lines;

with Test.Aggregator;
with Test.Skeleton;
with Test.Harness;
with Test.Mapping;
with Test.Common;
with Test.Skeleton.Source_Table;
with Test.Harness.Source_Table;

with Ada.Directories; use Ada.Directories;
with Utils.Projects; use Utils.Projects;
with GNAT.Directory_Operations;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with TGen.Context;
with TGen.Gen_Strategies;

package body Test.Actions is

   SPT : GNATCOLL.Projects.Project_Tree renames
     Test.Common.Source_Project_Tree;

   function Is_Externally_Built (File : Virtual_File) return Boolean;
   --  Checks if the given source file belongs to an externally build library

   procedure Process_Exclusion_List
     (Value        : String;
      From_Project : Boolean := False);
   --  Processes value of --exclude-from-stubbing switch. If values come from
   --  project attributes they do not override already stored ones.

   procedure Check_Direct;
   --  Checks if there are no intersections between target and source dirs.
   --  If everything is fine, tries to create target dirs.

   procedure Check_Subdir;
   --  Checks if there are no intersections between target and source dirs.
   --  If everything is fine, tries to create all target subdirs.

   procedure Check_Separate_Root;
   --  Checks if there are no intersections between target and source dirs.
   --  If everything is fine, tries to create a directory hierarchy similar
   --  to one of the tested sources.

   procedure Check_Stub;
   --  Checks if there are no intersections between stub and source dirs and
   --  between stub and test dirs.

   function Non_Null_Intersection
     (Left  : File_Array_Access;
      Right : File_Array)
      return Boolean;
   --  Returns True if two file arrays have at least one common file.

   procedure Process_Additional_Tests
     (Env : Project_Environment_Access; Cmd : Command_Line);
   --  Loads the project containing additional tests and processes them.
   --  This project needs to get loaded with the same environment as the
   --  argument one.

   use Utils.Formatted_Output;

   pragma Warnings (Off); -- ????
   --  These use clauses will be necessary later.
   --  At least some of them.

   use Common_Flag_Switches,
       Common_Boolean_Switches,
       Ada_Version_Switches,
       Common_String_Switches,
       Common_String_Seq_Switches,
       Common_Nat_Switches;

   use Test_Boolean_Switches, Test_String_Switches, Test_String_Seq_Switches;
   pragma Warnings (On);

   ----------
   -- Init --
   ----------

   procedure Init
     (Tool : in out Test_Tool; Cmd : in out Command_Line)
   is
      Tmp   : GNAT.OS_Lib.String_Access;
      Files : File_Array_Access;

      Root_Prj : Project_Type;

      type Output_Mode_Type is (Root_Mode, Subdir_Mode, Direct_Mode);
      Output_Mode : Output_Mode_Type := Direct_Mode;

      Subdir_Mode_Att : constant Attribute_Pkg_String :=
        Build (Test.Common.GT_Package, "subdir");
      Root_Mode_Att   : constant Attribute_Pkg_String :=
        Build (Test.Common.GT_Package, "tests_root");
      Direct_Mode_Att : constant Attribute_Pkg_String :=
        Build (Test.Common.GT_Package, "tests_dir");

      function Build_Att_String
        (Attribute_Name : String) return Attribute_Pkg_String
      is
        (Build (Test.Common.GT_Package, Attribute_Name));

      --  Flags for default output dirs being set explicitly:
      Stub_Dir_Set    : Boolean := False;
      Tests_Dir_Set   : Boolean := False;
      Harness_Dir_Set : Boolean := False;

      Ignored : Test.Common.String_Set.Set;
      --  Set of file names mentioned in the --ignore=... switch

      procedure Report_Multiple_Output
        (Second_Output_Mode : Output_Mode_Type;
         From_Project       : Boolean := False);
      --  Issue message about switches that correspond to Output_Mode and
      --  Second_Output_Mode are mutually exclusive and raise
      --  Command_Line_Error.

      procedure Report_Multiple_Output
        (Second_Output_Mode : Output_Mode_Type;
         From_Project       : Boolean := False)
      is
         function Mode_Image_Cmd (M : Output_Mode_Type) return String is
           (case M is
                when Root_Mode   => "--tests-root",
                when Subdir_Mode => "--subdirs",
                when Direct_Mode => "--tests-dir");

         function Mode_Image_Att (M : Output_Mode_Type) return String is
           (case M is
                when Root_Mode   => "Tests_Root",
                when Subdir_Mode => "Subdir",
                when Direct_Mode => "Tests_Dir");
      begin
         Test.Common.Report_Err
           ("multiple output modes are not allowed");
         if From_Project then
            Cmd_Error_No_Help
              ("attributes "
               & Mode_Image_Att (Output_Mode)
               & " and "
               & Mode_Image_Att (Second_Output_Mode)
               & " are mutually exclusive");
         else
            Cmd_Error_No_Help
              ("options "
               & Mode_Image_Cmd (Output_Mode)
               & " and "
               & Mode_Image_Cmd (Second_Output_Mode)
               & " are mutually exclusive");
         end if;
      end Report_Multiple_Output;

   begin
      GNATCOLL.Traces.Parse_Config_File;
      Test.Common.Verbose := Arg (Cmd, Verbose);
      Test.Common.Quiet   := Arg (Cmd, Quiet);

      if Arg (Cmd, Passed_Tests) /= null then
         if Arg (Cmd, Passed_Tests).all = "hide" then
            Test.Common.Show_Passed_Tests := False;
         elsif Arg (Cmd, Passed_Tests).all = "show" then
            Test.Common.Show_Passed_Tests := True;
         else
            Cmd_Error_No_Help
              ("--passed-tests should be either show or hide");
         end if;
      end if;

      if Status (Tool.Project_Tree.all) = Empty then

         if Arg (Cmd, Subdirs) /= null then
            GNAT.OS_Lib.Free (Test.Common.Aggregate_Subdir_Name);
            Test.Common.Aggregate_Subdir_Name := new String'
              (Arg (Cmd, Subdirs).all);
         end if;

         for File of File_Names (Cmd) loop

            if GNAT.Directory_Operations.File_Extension
              (File.all) in ".ads" | ".adb"
            then
               --  No project is specified but there are argument sources.
               --  Most probably user forgot to specify the project, and since
               --  gnattest cannot work without a project file it only makes
               --  sense to stop here.
               Cmd_Error_No_Help ("project file not specified");
            end if;

            Tmp := new String'
              (GNAT.OS_Lib.Normalize_Pathname
                 (File.all,
                  Resolve_Links  => False,
                  Case_Sensitive => False));
            if not GNAT.OS_Lib.Is_Regular_File (Tmp.all) then
               Cmd_Error_No_Help ("cannot find " & Tmp.all);
            end if;
            Test.Aggregator.Add_Drivers_To_List (Tmp.all);
            GNAT.OS_Lib.Free (Tmp);
         end loop;

         if Arg (Cmd, Jobs) = 0 then
            Cmd_Error_No_Help (" -j should be a positive number");
         else
            Test.Common.Queues_Number := Arg (Cmd, Jobs);
         end if;

         --  Dealing with environment dir to copy
         if Arg (Cmd, Copy_Environment) /= null then
            Test.Common.Environment_Dir := new String'
              (Normalize_Pathname
                 (Arg (Cmd, Copy_Environment).all,
                  Resolve_Links  => False,
                  Case_Sensitive => False));
            if not Is_Directory (Test.Common.Environment_Dir.all) then
               Cmd_Error_No_Help
                 ("environment dir "
                  & Test.Common.Environment_Dir.all & " does not exist");
            end if;
         end if;

         --  Clearing argument files so that the driver does not try to process
         --  them as ada sources.
         Clear_File_Names (Cmd);

         Test.Common.Queues_Number := Arg (Cmd, Jobs);

         --  Aggregation mode does not require any further processing
         return;
      end if;

      SPT := GNATCOLL.Projects.Project_Tree (Tool.Project_Tree.all);

      Test.Common.Target_Val :=
        new String'(SPT.Root_Project.Get_Target (Default_To_Host => False));
      Test.Common.RTS_Attribute_Val :=
        new String'(SPT.Root_Project.Get_Runtime);

      --  Most output directories should be calculated relatively to original
      --  object dirs, so possible side effect of --subdirs must be undone.
      if Arg (Cmd, Subdirs) /= null then
         SPT.Root_Project.Get_Environment.Set_Object_Subdir ("");
         SPT.Recompute_View;
      end if;
      Root_Prj := SPT.Root_Project;

      declare
         procedure Include_One (File_Name : String);
         --  Include File_Name in the Ignored set

         procedure Include_One (File_Name : String) is
         begin
            Ignored.Include (File_Name);
         end Include_One;
      begin
         for Ignored_Arg of Arg (Cmd, Ignore) loop
            Read_File_Names_From_File (Ignored_Arg.all, Include_One'Access);
         end loop;
      end;

      if Root_Prj.Has_Attribute (Runtime_Attribute) then
         Test.Common.RTS_Attribute_Val := new String'(Root_Prj.Get_Runtime);
      end if;

      if Arg (Cmd, Recursive) then
         --  We need to override the list of argument sources. Switch -r is
         --  a legacy switch equal to -U without parameter that other tools
         --  do not have. We can also optimise a bit, since gnattest only cares
         --  about units specs as entry points of analysis.
         Clear_File_Names (Cmd);
         declare
            All_Sources : File_Array_Access :=
              Root_Prj.Source_Files (Recursive => True);
         begin
            for S of All_Sources.all loop
               if not Ignored.Contains (Simple_Name (S.Display_Full_Name))
                 and then To_Lower (SPT.Info (S).Language) = "ada"
                 and then SPT.Info (S).Unit_Part = Unit_Spec
               then
                  Append_File_Name (Cmd, S.Display_Full_Name);
               end if;
            end loop;
            Unchecked_Free (All_Sources);
         end;
      end if;

      if Arg (Cmd, Harness_Only) then
         Test.Common.Harness_Only := True;

         if Arg (Cmd, Additional_Tests) /= null then
            Cmd_Error_No_Help
              ("--harness only and --additional-tests are mutually exclusive");
         elsif Root_Prj.Has_Attribute (Build_Att_String ("additional_tests"))
         then
            Cmd_Error_No_Help
              ("--harness only and Gnattest.Additional_Tests "
               & "are mutually exclusive");
         end if;
      end if;

      --  Check for multiple output modes
      if Arg (Cmd, Tests_Dir) /= null then

         Output_Mode := Direct_Mode;

         if Arg (Cmd, Tests_Root) /= null then
            Report_Multiple_Output (Root_Mode);
         elsif Arg (Cmd, Subdirs) /= null then
            Report_Multiple_Output (Subdir_Mode);
         end if;

         Tests_Dir_Set := True;
         Free (Test.Common.Test_Dir_Name);
         Test.Common.Test_Dir_Name := new String'
           (Arg (Cmd, Tests_Dir).all);

      elsif Arg (Cmd, Tests_Root) /= null then

         Output_Mode := Root_Mode;

         if Arg (Cmd, Subdirs) /= null then
            Report_Multiple_Output (Subdir_Mode);
         end if;

         Tests_Dir_Set := True;
         Test.Common.Separate_Root_Dir := new String'
           (Arg (Cmd, Tests_Root).all);

      elsif Arg (Cmd, Subdirs) /= null then

         Output_Mode := Subdir_Mode;
         Tests_Dir_Set := True;
         Test.Common.Test_Subdir_Name := new String'
           (Arg (Cmd, Subdirs).all);

      else

         if Root_Prj.Has_Attribute (Direct_Mode_Att) then

            Output_Mode := Direct_Mode;

            if Root_Prj.Has_Attribute (Root_Mode_Att) then
               Report_Multiple_Output (Root_Mode, True);
            elsif Root_Prj.Has_Attribute (Subdir_Mode_Att) then
               Report_Multiple_Output (Subdir_Mode, True);
            end if;

            Tests_Dir_Set := True;
            Free (Test.Common.Test_Dir_Name);
            Test.Common.Test_Dir_Name := new String'
              (Root_Prj.Attribute_Value (Direct_Mode_Att));

         elsif Root_Prj.Has_Attribute (Root_Mode_Att) then

            Output_Mode := Root_Mode;

            if Root_Prj.Has_Attribute (Subdir_Mode_Att) then
               Report_Multiple_Output (Subdir_Mode, True);
            end if;

            Tests_Dir_Set := True;
            Test.Common.Separate_Root_Dir := new String'
              (Root_Prj.Attribute_Value (Root_Mode_Att));

         elsif Root_Prj.Has_Attribute (Subdir_Mode_Att) then

            Output_Mode := Subdir_Mode;
            Tests_Dir_Set := True;
            Test.Common.Test_Subdir_Name := new String'
              (Root_Prj.Attribute_Value (Subdir_Mode_Att));

         end if;

      end if;

      if Arg (Cmd, Stubs_Dir) /= null then

         Free (Test.Common.Stub_Dir_Name);
         Test.Common.Stub_Dir_Name := new String'(Arg (Cmd, Stubs_Dir).all);
         Stub_Dir_Set := True;

      elsif Root_Prj.Has_Attribute (Build_Att_String ("stubs_dir")) then

         Free (Test.Common.Stub_Dir_Name);
         Test.Common.Stub_Dir_Name := new String'
           (Root_Prj.Attribute_Value (Build_Att_String ("stubs_dir")));
         Stub_Dir_Set := True;

      end if;

      if Arg (Cmd, Harness_Dir) /= null then

         Free (Test.Common.Harness_Dir_Str);
         Test.Common.Harness_Dir_Str := new String'
           (Arg (Cmd, Harness_Dir).all);
         Harness_Dir_Set := True;

      elsif Root_Prj.Has_Attribute (Build_Att_String ("harness_dir")) then

         Free (Test.Common.Harness_Dir_Str);
         Test.Common.Harness_Dir_Str := new String'
           (Root_Prj.Attribute_Value (Build_Att_String ("harness_dir")));
         Harness_Dir_Set := True;

      end if;

      --  Checking if argument project has IDE package specified.
      declare
         S : constant Attribute_Pkg_String := Build (Ide_Package, "");
      begin
         if Has_Attribute (Root_Prj, S) then
            Test.Common.IDE_Package_Present := True;
         else
            Test.Common.IDE_Package_Present := False;
         end if;
      end;

      --  Checking if argument project has Make package specified.
      declare
         S : constant Attribute_Pkg_String := Build ("make", "");
      begin
         if Has_Attribute (Root_Prj, S) then
            Test.Common.Make_Package_Present := True;
         else
            Test.Common.Make_Package_Present := False;
         end if;
      end;

      --  We need to fill a local source table since gnattest actually needs
      --  info not only on current source but on any particular one or even
      --  all of them at once.

      declare
         --  For now repeating code from Utils.Drivers to get rid of ignored
         --  files, this should be optimized.
         use Test.Common.String_Set;

         Source_Info : File_Info;

      begin
         Common.Stub_Mode_ON := Arg (Cmd, Stub);

         for File of File_Names (Cmd) loop
            if not Contains (Ignored, Simple_Name (File.all)) then

               Source_Info := Info (SPT, Create (SPT, +File.all));

               if Source_Info.Unit_Part = Unit_Spec then
                  if Test.Common.Harness_Only then
                     Test.Harness.Source_Table.Add_Source_To_Process
                       (Source_Info.File.Display_Full_Name);
                  else
                     Test.Skeleton.Source_Table.Add_Source_To_Process
                       (Source_Info.File.Display_Full_Name);
                  end if;
               end if;
            end if;
         end loop;
      end;

      Test.Common.Substitution_Suite := Arg (Cmd, Validate_Type_Extensions);
      Test.Common.Inheritance_To_Suite := Arg (Cmd, Inheritance_Check);
      Test.Common.Test_Case_Only := Arg (Cmd, Test_Case_Only);
      Test.Common.Omit_Sloc := Arg (Cmd, Omit_Sloc);
      Test.Common.Show_Test_Duration := Arg (Cmd, Test_Duration);
      Test.Common.Relocatable_Harness := Arg (Cmd, Relocatable_Harness);

      Test.Common.Strict_Execution := Arg (Cmd, Strict)
        or else (Ada.Environment_Variables.Exists ("GNATTEST_STRICT")
                  and then Ada.Environment_Variables.Value
                    ("GNATTEST_STRICT") = "TRUE");

      --  Command line support

      if not Arg (Cmd, Command_Line_Support) then
         Test.Common.No_Command_Line := True;
      else
         declare
            Files : constant GNATCOLL.VFS.File_Array :=
              Predefined_Source_Files (Root_Prj.Get_Environment);
            A_Comlin_Found : Boolean := False;
         begin
            for I in Files'Range loop
               if Files (I).Display_Base_Name = "a-comlin.ads" then
                  A_Comlin_Found := True;
                  exit;
               end if;
            end loop;

            Test.Common.No_Command_Line := not A_Comlin_Found;
         end;
      end if;

      --  Default behaviour of tests
      declare
         Skeleton_Default_Att : constant Attribute_Pkg_String :=
           Build_Att_String ("skeletons_default");
         Skeleton_Default_Val : constant String :=
           (if Arg (Cmd, Skeleton_Default) = null then
               (if Root_Prj.Has_Attribute (Skeleton_Default_Att) then
                   Root_Prj.Attribute_Value (Skeleton_Default_Att)
                else
                    "")
            else Arg (Cmd, Skeleton_Default).all);
      begin
         if Skeleton_Default_Val = "pass" then
            Test.Common.Skeletons_Fail := False;
         elsif Skeleton_Default_Val = "fail" then
            Test.Common.Skeletons_Fail := True;
         elsif Skeleton_Default_Val /= "" then
            if Arg (Cmd, Skeleton_Default) = null then
               Cmd_Error_No_Help
                 ("--skeleton-default should be either fail or pass");
            else
               Cmd_Error_No_Help
                 ("Gnattest.Skeletons_Default should be either fail or pass");
            end if;
         end if;
      end;

      --  Exit status
      if Arg (Cmd, Exit_Status) /= null then
         if Arg (Cmd, Exit_Status).all = "off" then
            Test.Common.Show_Passed_Tests := False;
         elsif Arg (Cmd, Exit_Status).all = "on" then
            Test.Common.Add_Exit_Status := True;
         else
            Cmd_Error_No_Help
              ("--exit-status should be either on or off");
         end if;
      end if;

      --  Separate drivers
      if Arg (Cmd, Separate_Drivers) /= null then
         if Arg (Cmd, Separate_Drivers).all in "unit" | "" then
            Test.Common.Separate_Drivers := True;
            Test.Common.Driver_Per_Unit := True;
         elsif Arg (Cmd, Separate_Drivers).all = "test" then
            Test.Common.Separate_Drivers := True;
            Test.Common.Driver_Per_Unit := False;
         else
            Cmd_Error_No_Help
              ("--separate-drivers should be either unit or test"
               & " >" & Arg (Cmd, Separate_Drivers).all & "<");
         end if;
      end if;

      --  Reporter
      if Arg (Cmd, Reporter) /= null then
         Free (Test.Common.Reporter_Name);
         Test.Common.Reporter_Name := new String'(Arg (Cmd, Reporter).all);
         if Arg (Cmd, Stub) or else Arg (Cmd, Separate_Drivers) /= null then
            Test.Common.Report_Std
              ("warning: (gnattest) --reporter has no effect");
         end if;
      end if;

      if Arg (Cmd, Stub) then

         if Arg (Cmd, Harness_Only) then
            Cmd_Error_No_Help
              ("options --harness-only and --stub are incompatible");
         end if;

         if Arg (Cmd, Additional_Tests) /= null then
            Cmd_Error_No_Help
              ("options --additional-tests and --stub are incompatible");
         end if;

         if not Tests_Dir_Set then
            Free (Test.Common.Test_Dir_Name);
            Test.Common.Test_Dir_Name := new String'
              ("gnattest_stub" & Directory_Separator & "tests");
         end if;

         if not Stub_Dir_Set then
            Free (Test.Common.Stub_Dir_Name);
            Test.Common.Stub_Dir_Name := new String'
              ("gnattest_stub" & Directory_Separator & "stubs");
         end if;

         if not Harness_Dir_Set then
            Free (Test.Common.Harness_Dir_Str);
            Test.Common.Harness_Dir_Str := new String'
              ("gnattest_stub" & Directory_Separator & "harness");
         end if;

         Test.Skeleton.Source_Table.Initialize_Project_Table (SPT);

         Files := SPT.Root_Project.Source_Files (True);
         for F in Files'Range loop
            if
              To_Lower (SPT.Info (Files (F)).Language) = "ada"
              and then not Is_Externally_Built (Files (F))
            then
               case SPT.Info (Files (F)).Unit_Part is
                  when Unit_Body =>
                     declare
                        P : Project_Type :=
                          SPT.Info (Files (F)).Project;
                     begin
                        --  The name of the project here will be used to create
                        --  stub projects. Those extend original projects, so
                        --  if a source belongs to an extended project we need
                        --  the extending on here instead, so that we do not
                        --  end up with different extensions of same project.
                        while Extending_Project (P) /= No_Project loop
                           P := Extending_Project (P);
                        end loop;

                        Test.Skeleton.Source_Table.Add_Body_To_Process
                          (Files (F).Display_Full_Name,
                           P.Name,
                           SPT.Info (Files (F)).Unit_Name);
                     end;
                  when Unit_Spec =>
                     Test.Skeleton.Source_Table.Add_Body_Reference
                       (Files (F).Display_Full_Name);
                  when others =>
                     null;
               end case;
            end if;
         end loop;
         Unchecked_Free (Files);

      end if;

      --  Processing harness dir specification

      if Is_Absolute_Path
        (GNATCOLL.VFS.Create (+Test.Common.Harness_Dir_Str.all))
      then
         Tmp := Test.Common.Harness_Dir_Str;
         Test.Common.Harness_Dir_Str := new String'
           (Normalize_Pathname
              (Tmp.all,
               Resolve_Links  => False,
               Case_Sensitive => False)
            & Directory_Separator);
         Free (Tmp);
      else
         Tmp := Test.Common.Harness_Dir_Str;
         Test.Common.Harness_Dir_Str := new String'
           (Normalize_Pathname
              (Root_Prj.Object_Dir.Display_Full_Name & Tmp.all,
               Resolve_Links  => False,
               Case_Sensitive => False)
            & Directory_Separator);
         Free (Tmp);
      end if;

      for Dir of Root_Prj.Source_Dirs (Recursive => True) loop
         if Test.Common.Harness_Dir_Str.all =
           Normalize_Pathname
             (Dir.Display_Full_Name,
              Resolve_Links  => False,
              Case_Sensitive => False)
           & Directory_Separator
         then
            Cmd_Error_No_Help
              ("invalid harness directory, cannot mix up "
               & "infrastructure and sources");
         end if;
      end loop;

      if Is_Regular_File (Test.Common.Harness_Dir_Str.all) then
         Cmd_Error_No_Help ("cannot create harness directory");
      elsif not Is_Directory (Test.Common.Harness_Dir_Str.all) then

         declare
            Dir : File_Array_Access;
         begin
            Append
              (Dir, GNATCOLL.VFS.Create (+Test.Common.Harness_Dir_Str.all));
            Test.Common.Create_Dirs (Dir);
         exception
            when GNAT.Directory_Operations.Directory_Error =>
               Cmd_Error_No_Help ("cannot create harness directory");
         end;

      end if;

      case Output_Mode is
         when Direct_Mode =>
            Check_Direct;
         when Subdir_Mode =>
            Check_Subdir;
         when Root_Mode   =>
            Check_Separate_Root;
      end case;

      --  Test vectors

      if Arg (Cmd, Gen_Test_Vectors) then
         Test.Common.Generate_Test_Vectors := True;
         Test.Common.JSON_Test_Dir := new String'
           (Test.Common.Harness_Dir_Str.all & "JSON_tests"
            & GNAT.OS_Lib.Directory_Separator);
         if Debug_Flag_1 then
            Put ("Requested test vectors generation at"
                 & " <harness_dir>/JSON_tests\n");
         end if;

         declare
            Dir : File_Array_Access;
         begin
            Append
              (Dir, GNATCOLL.VFS.Create (+Test.Common.JSON_Test_Dir.all));
            Test.Common.Create_Dirs (Dir);
         exception
            when GNAT.Directory_Operations.Directory_Error =>
               Cmd_Error_No_Help ("cannot create JSON test directory");
         end;

         TGen.Gen_Strategies.Initialize
           (Test.Common.TGen_Ctx,
            Root_Prj,
            Ada.Strings.Unbounded.To_Unbounded_String
              (Test.Common.JSON_Test_Dir.all));

         if Arg (Cmd, Gen_Test_Num) /= null then
            begin
               Test.Common.TGen_Num_Tests :=
                 Positive'Value (Arg (Cmd, Gen_Test_Num).all);
            exception
               when others =>
                  Cmd_Error_No_Help
                    ("--gen-test-num should be a positive integer");
            end;
         end if;

         if Arg (Cmd, Gen_Unsupported_Behavior) /= null then
            if Arg (Cmd, Gen_Unsupported_Behavior).all = "no-test" then
               Test.Common.TGen_Ctx.Unsupported_Type_Behavior :=
                 TGen.Context.No_Test;
            elsif Arg (Cmd, Gen_Unsupported_Behavior).all = "commented-out"
            then
               Test.Common.TGen_Ctx.Unsupported_Type_Behavior :=
                 TGen.Context.Commented_Out;
            else
               Cmd_Error_No_Help
                 ("--gen-unsupported-behavior must be one of ""no-test"" or"
                  & " ""commented-out""");
            end if;
         else
            Test.Common.TGen_Ctx.Unsupported_Type_Behavior :=
              TGen.Context.No_Test;
         end if;
      end if;

      if Common.Stub_Mode_ON then
         Check_Stub;
         declare
            Excludes : constant String_Ref_Array :=
              Arg (Cmd, Exclude_From_Stubbing);
         begin
            for Exclude of Excludes loop
               Process_Exclusion_List (Exclude.all);
            end loop;
         end;

         declare
            Default_Exclude_Attr : constant Attribute_Pkg_String :=
              Build (Test.Common.GT_Package, "default_stub_exclusion_list");
            Exclude_Attr         : constant Attribute_Pkg_String :=
              Build (Test.Common.GT_Package, "stub_exclusion_list");
            Indexes              : constant String_List          :=
              Attribute_Indexes (Root_Prj, Exclude_Attr);
         begin
            if Has_Attribute (Root_Prj, Default_Exclude_Attr) then
               Process_Exclusion_List
                 (Attribute_Value (Root_Prj, Default_Exclude_Attr),
                  From_Project => True);
            end if;
            for Index of Indexes loop
               Process_Exclusion_List
                 (Attribute_Value (Root_Prj, Exclude_Attr, Index.all),
                  From_Project => True);
            end loop;
         end;
      end if;

      --  Process additional tests
      if Arg (Cmd, Additional_Tests) /= null then
         Test.Common.Additional_Tests_Prj := new String'
           (Normalize_Pathname
              (Arg (Cmd, Additional_Tests).all,
               Resolve_Links  => False,
               Case_Sensitive => False));
      elsif Root_Prj.Has_Attribute (Build_Att_String ("additional_tests")) then
         Test.Common.Additional_Tests_Prj := new String'
           (Normalize_Pathname
              (Root_Prj.Attribute_Value
                   (Build_Att_String ("additional_tests")),
               Resolve_Links  => False,
               Case_Sensitive => False));
      end if;

      if Test.Common.Additional_Tests_Prj /= null and then
        not Is_Regular_File (Test.Common.Additional_Tests_Prj.all)
      then
         Cmd_Error_No_Help
           ("cannot find " & Test.Common.Additional_Tests_Prj.all);
      end if;

      if Root_Prj.Has_Attribute (Compiler_Default_Switches_Attribute) then
         declare
            Switches : String_List_Access :=
              Attribute_Value (Root_Prj,
                               Compiler_Default_Switches_Attribute,
                               "ada");
         begin
            if Switches = null then
               return;
            end if;

            for I in Switches'Range loop
               if Switches (I).all = "-gnatE" then
                  Test.Common.Inherited_Switches.Append (Switches (I).all);
               end if;
            end loop;

            Free (Switches);
         end;
      end if;

      Ignored.Clear;

   end Init;

   -----------
   -- Final --
   -----------

   procedure Final (Tool : in out Test_Tool; Cmd : Command_Line) is
      Src_Prj : constant String :=
        Tool.Project_Tree.Root_Project.Project_Path.Display_Full_Name;
   begin

      if Status (Tool.Project_Tree.all) = Empty then
         Test.Aggregator.Process_Drivers_List;
      else
         if Arg (Cmd, Stub) then
            Test.Harness.Generate_Stub_Test_Driver_Projects (Src_Prj);
         elsif Arg (Cmd, Separate_Drivers) /= null then
            Test.Skeleton.Generate_Project_File (Src_Prj);
            Test.Harness.Generate_Test_Driver_Projects (Src_Prj);
         else
            if not Arg (Cmd, Harness_Only) then
               if Test.Common.Additional_Tests_Prj /= null then
                  Process_Additional_Tests (Tool.Project_Env, Cmd);
               end if;
               Test.Skeleton.Report_Unused_Generic_Tests;
               Test.Skeleton.Generate_Project_File (Src_Prj);
               if Test.Common.Verbose then
                  Test.Skeleton.Report_Tests_Total;
               end if;
            end if;
            Test.Harness.Test_Runner_Generator  (Src_Prj);
            Test.Harness.Project_Creator        (Src_Prj);
         end if;
         Test.Harness.Generate_Makefile (Src_Prj);
         Test.Common.Generate_Common_File;
         Test.Mapping.Generate_Mapping_File;
      end if;

      if Test.Common.Strict_Execution
        and then Test.Common.Source_Processing_Failed
      then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Final;

   ---------------------
   -- Per_File_Action --
   ---------------------

   procedure Per_File_Action
     (Tool : in out Test_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit)
   is
      pragma Unreferenced (Tool, Input, BOM_Seen); -- ????
   begin
      if Debug_Flag_V then
         Print (Unit);
         Put ("With trivia\n");
         PP_Trivia (Unit);
      end if;

      if Test.Common.Harness_Only then
         Test.Harness.Process_Source (Unit);
      else
         Test.Skeleton.Process_Source (Unit);
      end if;
   end Per_File_Action;

   -----------------------------
   -- Per_Invalid_File_Action --
   -----------------------------

   overriding procedure Per_Invalid_File_Action
     (Tool      : in out Test_Tool;
      Cmd       :        Command_Line;
      File_Name :        String)
   is
      pragma Unreferenced (Tool, Cmd, File_Name);
   begin
      Test.Common.Source_Processing_Failed := True;
   end Per_Invalid_File_Action;

   ---------------
   -- Tool_Help --
   ---------------

   procedure Tool_Help (Tool : Test_Tool) is
      pragma Unreferenced (Tool);
   begin
      pragma Style_Checks ("M200"); -- Allow long lines
      Put ("usage: gnattest -Pprj [opts] {filename}\n");
      Put ("        - generates the unit testing framework\n");
      Put ("\n");
      Put (" or   gnattest test_drivers.list [opts]\n");
      Put ("        - executes tests and aggregates the results\n");
      Put ("\n");
      Put (" --version        - Display version and exit\n");
      Put (" --help           - Display usage and exit\n");
      Put (" -v, --verbose    - Verbose mode\n");
      Put (" -q, --quiet      - Quiet mode\n");
      Put ("\n");

      Put ("Framework generation mode options:\n");
      Put ("\n");
      Put (" -Pproject        - Use project file project. Only one such switch can be used\n");
      Put (" -U               - Process all sources of the argument project\n");
      Put (" -U main          - Process the closure of units rooted at unit main\n");
      Put (" --no-subprojects - Process sources of root project only\n");
      Put (" -Xname=value     - Specify an external reference for argument project file\n");
      Put (" -eL              - Follow all symbolic links when processing project files\n");
      Put (" --target=target  - Specify a target\n");
      Put (" --RTS=runtime    - Specify runtime for Ada\n");
      Put (" --files=file     - Name of a text file containing a list of Ada\n");
      Put ("                    source files to process\n");
      Put (" --ignore=file    - Name of a text file containing a list of sources\n");
      Put ("                    to be excluded from processing\n");

      Put (" --strict                - Return error exit code if there are parsing errors\n");
      Put (" --additional-tests=prj  - Treat sources from project prj as additional\n");
      Put ("                           manual tests to add to the test suite\n");
      Put (" --harness-only          - Treat argument sources as tests to add to the suite\n");
      Put (" --stub                  - Generate testing framework that uses stubs\n");
      Put ("\n");

      Put (" --exclude-from-stubbing=file       - List of sources whose bodies should not\n");
      Put ("                                      be stubbed\n");
      Put (" --exclude-from-stubbing:unit=file  - List of sources whose bodies should not\n");
      Put ("                                      be stubbed when testing unit\n");
      Put ("\n");

      Put (" --harness-dir=dirname  - Output dir for test harness\n");
      Put (" --tests-dir=dirname    - Test files are put in dirname\n");
      Put (" --subdirs=dirname      - Test files are put in subdirs dirname of source dirs\n");
      Put (" --tests-root=dirname   - Test files are put in the same directory hierarchy\n");
      Put ("                          as sources but rooted at dirname\n");
      Put (" --stubs-dir=dirname    - Stub files are put in subdirs of dirname\n");
      Put ("\n");

      Put (" --validate-type-extensions     - Run all tests from all parents to check LSP\n");
      Put (" --inheritance-check            - Run inherited tests for descendants\n");
      Put (" --no-inheritance-check         - Do not run inherited tests for descendants\n");
      Put (" --test-case-only               - Create tests only when Test_Case is specified\n");
      Put (" --skeleton-default=(pass|fail) - Default behavior of unimplemented tests\n");
      Put (" --passed-tests=(show|hide)     - Default output of passed tests\n");
      Put (" --exit-status=(on|off)         - Default usage of the exit status\n");
      Put (" --omit-sloc                    - Don't record subprogram sloc in test package\n");
      Put (" --no-command-line              - Don't add command line support to test driver\n");
      Put (" --test-duration                - Show timing for each test\n");
      Put ("\n");

      Put ("Tests execution mode options:\n");
      Put ("\n");
      Put (" --passed-tests=(show|hide)  - Default output of passed tests\n");
      Put (" --queues=n, -jn             - Run n tests in parallel (default n=1)\n");
      Put (" --copy-environment=dir      - Copy contents of dir to temp dirs where test\n");
      Put ("                               drivers are spawned\n");
      Put (" --subdirs=dirname           - Look for test drivers in subdirs\n");
      pragma Style_Checks ("M79");
   end Tool_Help;

   -------------------------
   -- Is_Externally_Built --
   -------------------------

   function Is_Externally_Built (File : Virtual_File) return Boolean is
      F_Info : constant File_Info    := Info (SPT, File);
      Proj   : constant Project_Type := Project (F_Info);
      Attr   : constant Attribute_Pkg_String := Build ("", "externally_built");
   begin
      if Has_Attribute (Proj, Attr) then
         if To_Lower (Attribute_Value (Proj, Attr)) = "true" then
            return True;
         end if;
      end if;
      return False;
   end Is_Externally_Built;

   ----------------------------
   -- Process_Exclusion_List --
   ----------------------------

   procedure Process_Exclusion_List
     (Value        : String;
      From_Project : Boolean := False)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      Idx   : Natural;
      First : constant Natural := Value'First;

      F : File_Type;

      Exclude_For_One_UUT : constant Boolean :=
        Value'Length > 3    and then
        Value (First) = ':' and then
        Index (Value, "=") > First + 1;

      S : String_Access;

      function Is_Comment (S : String) return Boolean is
        (S'Length >= 2 and then S (S'First .. S'First + 1) = "--");
   begin
      if Exclude_For_One_UUT then
         Idx := Index (Value, "=");
         declare
            Unit   : constant String := Value (First + 1 .. Idx - 1);
            F_Path : constant String :=
              Normalize_Pathname
                (Name           => Value (Idx + 1 .. Value'Last),
                 Resolve_Links  => False,
                 Case_Sensitive => False);
         begin
            if not Is_Regular_File (F_Path) then
               Cmd_Error_No_Help ("cannot find " & F_Path);
            end if;

            if From_Project and then
              Test.Common.Stub_Exclusion_Lists.Contains (Unit)
            then
               return;
            end if;

            Open (F, In_File, F_Path);
            while not End_Of_File (F) loop
               S := new String'(Get_Line (F));
               if not Is_Comment (S.all) then
                  Test.Common.Store_Excluded_Stub (Unit, S.all);
               end if;
               Free (S);
            end loop;
            Close (F);
         end;
         return;
      end if;

      if From_Project and then
        not Test.Common.Default_Stub_Exclusion_List.Is_Empty
      then
         return;
      end if;

      declare
         F_Path : constant String :=
           Normalize_Pathname
             (Name           => Value,
              Resolve_Links  => False,
              Case_Sensitive => False);
      begin
         if not Is_Regular_File (F_Path) then
            Cmd_Error_No_Help ("cannot find " & F_Path);
         end if;
         Open (F, In_File, F_Path);
         while not End_Of_File (F) loop
            S := new String'(Get_Line (F));
            if not Is_Comment (S.all) then
               Test.Common.Store_Default_Excluded_Stub (S.all);
            end if;
            Free (S);
         end loop;
         Close (F);
      end;

   end Process_Exclusion_List;

   ----------------------------------
   -- Register_Specific_Attributes --
   ----------------------------------

   procedure Register_Specific_Attributes is
      procedure Report_If_Err (S : String);
      --  Outputs warning when attribute cannot be registered

      procedure Report_If_Err (S : String) is
      begin
         if S = "" then
            return;
         else
            Common.Report_Std (S);
         end if;
      end Report_If_Err;
   begin
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name    => "gnattest_switches",
            Pkg     => Test.Common.GT_Package,
            Is_List => True,
            Indexed => False));
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name => "harness_dir",
            Pkg  => Test.Common.GT_Package));
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name => "subdir",
            Pkg  => Test.Common.GT_Package));
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name => "tests_root",
            Pkg  => Test.Common.GT_Package));
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name => "tests_dir",
            Pkg  => Test.Common.GT_Package));
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name => "additional_tests",
            Pkg  => Test.Common.GT_Package));
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name    => "stubs_dir",
            Pkg     => Test.Common.GT_Package));
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name => "skeletons_default",
            Pkg  => Test.Common.GT_Package));
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name => "default_stub_exclusion_list",
            Pkg  => Test.Common.GT_Package));
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name    => "stub_exclusion_list",
            Pkg     => Test.Common.GT_Package,
            Indexed => True));

      --  Not really a gnattest specific attribute, but we still need to
      --  inherit makefile attribute in test driver.
      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name => "makefile",
            Pkg  => "make"));

      --  Needed for gnatcov integration

      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name                 => "Switches",
            Pkg                  => "Coverage",
            Is_List              => True,
            Indexed              => True,
            Case_Sensitive_Index => False));

      Report_If_Err
        (GNATCOLL.Projects.Register_New_Attribute
           (Name    => "Board",
            Pkg     => "Emulator",
            Is_List => False,
            Indexed => False));

   end Register_Specific_Attributes;

   ---------------------------
   -- Non_Null_Intersection --
   ---------------------------

   function Non_Null_Intersection
     (Left  : File_Array_Access;
      Right : File_Array) return Boolean is
   begin
      for J in Left'Range loop
         declare
            Left_Str : constant String :=
              Normalize_Pathname
                (Name           => Left.all (J).Display_Full_Name,
                 Resolve_Links  => False,
                 Case_Sensitive => False);
         begin
            for K in Right'Range loop

               if Left_Str =
                 Normalize_Pathname
                   (Name           => Right (K).Display_Full_Name,
                    Resolve_Links  => False,
                    Case_Sensitive => False)
               then
                  Test.Common.Report_Std
                    ("gnattest: "
                     & Left_Str & " is used for more than one purpose");
                  return True;
               end if;
            end loop;
         end;
      end loop;

      return False;
   end Non_Null_Intersection;

   ------------------
   -- Check_Direct --
   ------------------

   procedure Check_Direct is
      use Test.Common;

      Tmp : String_Access;
      TD_Name : constant Virtual_File :=
        GNATCOLL.VFS.Create (+Test_Dir_Name.all);
      Future_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);
      Harness_Dir_Ar : constant File_Array (1 .. 1) :=
        [1 => Create (+(Harness_Dir_Str.all))];

      Obj_Dir : String_Access;

      All_Source_Locations : constant File_Array :=
        Source_Project_Tree.Root_Project.Source_Dirs (Recursive => True);

      Project  : Project_Type;
      Iterator : Project_Iterator :=
        Start (Source_Project_Tree.Root_Project);
   begin

      if TD_Name.Is_Absolute_Path then
         Append (Future_Dirs, GNATCOLL.VFS.Create (+Test_Dir_Name.all));
      else
         loop
            Project := Current (Iterator);
            exit when Project = No_Project;

            Obj_Dir := new String'(Project.Object_Dir.Display_Full_Name);
            Tmp := new String'(Obj_Dir.all & Test_Dir_Name.all);
            Append (Future_Dirs, GNATCOLL.VFS.Create (+Tmp.all));
            Free (Tmp);
            Free (Obj_Dir);

            Next (Iterator);
         end loop;
      end if;

      if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
         Cmd_Error_No_Help
           ("invalid output directory, cannot mix up "
            & "tests and sources");
      end if;

      if Non_Null_Intersection (Future_Dirs, Harness_Dir_Ar) then
         Cmd_Error_No_Help
           ("invalid output directory, cannot mix up "
            & "tests and infrastructure");
      end if;

      Unchecked_Free (Future_Dirs);

      Test.Skeleton.Source_Table.Set_Direct_Output;
   end Check_Direct;

   ------------------
   -- Check_Subdir --
   ------------------

   procedure Check_Subdir is
      use Test.Common;

      Future_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);
      --  List of dirs to be generated. The list is checked for intersections
      --  with source dirs before any new directories are created.

      Harness_Dir_Ar : constant File_Array (1 .. 1) :=
        [1 => Create (+(Harness_Dir_Str.all))];

      All_Source_Locations : constant File_Array :=
        Source_Project_Tree.Root_Project.Source_Dirs (Recursive => True);
   begin
      for Loc of All_Source_Locations loop
         Append (Future_Dirs, Loc / (+Test_Subdir_Name.all));
      end loop;

      if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
         Cmd_Error_No_Help
           ("invalid output directory, cannot mix up "
            & "tests and sources");
      end if;

      if Non_Null_Intersection (Future_Dirs, Harness_Dir_Ar) then
         Cmd_Error_No_Help
           ("invalid output directory, cannot mix up "
            & "tests and infrastructure");
      end if;

      Test.Skeleton.Source_Table.Set_Subdir_Output;
   end Check_Subdir;

   -------------------------
   -- Check_Separate_Root --
   -------------------------

   procedure Check_Separate_Root is
      use Test.Common;

      RD_Name : constant Virtual_File :=
        GNATCOLL.VFS.Create (+Separate_Root_Dir.all);

      Tmp, Buff    : String_Access;
      Maximin_Root : String_Access;
      Root_Length  : Integer;

      Future_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);
      --  List of dirs to be generated. The list is checked for intersections
      --  with source dirs before any new directories are created.

      Harness_Dir_Ar : constant File_Array (1 .. 1) :=
        [1 => Create (+(Harness_Dir_Str.all))];

      All_Source_Locations : constant File_Array :=
        Source_Project_Tree.Root_Project.Source_Dirs (Recursive => True);

      Files : File_Array_Access;
      Project  : Project_Type;
      Iterator : Project_Iterator :=
        Start_Reversed (Source_Project_Tree.Root_Project);

      Ext_Bld : constant Attribute_Pkg_String :=
        Build ("", "externally_built");

      Obj_Dir                 : String_Access;
      Local_Separate_Root_Dir : String_Access;

      function Common_Root (Left : String; Right : String) return String;
      --  Returns the coincident beginning of both paths or an empty string.

      -------------------
      --  Common_Root  --
      -------------------

      function Common_Root (Left : String; Right : String) return String is
         Idxl : Integer := Left'First;
         Idxr : Integer := Right'First;

         Last_Dir_Sep_Index : Integer := Idxl - 1;
         --  We need to check for the following:
         --  ...somepath/dir/
         --  ...somepath/directory/

      begin
         if Left = "" or Right = "" then
            return "";
         end if;

         loop
            if Left (Idxl) = Directory_Separator
              and then Right (Idxr) = Directory_Separator
            then
               Last_Dir_Sep_Index := Idxl;
            end if;

            if Left (Idxl) /= Right (Idxr) then
               return Left (Left'First .. Last_Dir_Sep_Index);
            end if;

            exit when Idxl = Left'Last or Idxr = Right'Last;

            Idxl := Idxl + 1;
            Idxr := Idxr + 1;
         end loop;

         return Left (Left'First .. Idxl);
      end Common_Root;

   begin

      if RD_Name.Is_Absolute_Path then

         Test.Skeleton.Source_Table.Reset_Location_Iterator;
         Tmp := new String'
           (Test.Skeleton.Source_Table.Next_Source_Location);
         Maximin_Root := new String'(Tmp.all);

         loop
            Tmp := new String'
              (Test.Skeleton.Source_Table.Next_Source_Location);
            exit when Tmp.all = "";

            Buff := new String'(Common_Root (Tmp.all, Maximin_Root.all));

            if Buff.all = "" then
               Cmd_Error_No_Help
                 ("gnattest: sources have different root dirs, "
                  & "cannot apply separate root output");
            end if;

            Free (Maximin_Root);
            Maximin_Root := new String'(Buff.all);
            Free (Buff);
            Free (Tmp);
         end loop;

         Root_Length := Maximin_Root.all'Length;

         Separate_Root_Dir := new String'
           (Normalize_Pathname
              (Name           => Separate_Root_Dir.all,
               Resolve_Links  => False,
               Case_Sensitive => False));

         Test.Skeleton.Source_Table.Reset_Location_Iterator;

         loop
            Tmp := new String'
              (Test.Skeleton.Source_Table.Next_Source_Location);
            exit when Tmp.all = "";

            Append (Future_Dirs, GNATCOLL.VFS.Create
              (+(Separate_Root_Dir.all & Directory_Separator &
                 Tmp.all (Root_Length + 1 .. Tmp.all'Last))));

            Free (Tmp);
         end loop;

         if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
            Cmd_Error_No_Help
              ("invalid output directory, cannot mix up "
               & "tests and sources");
         end if;

         if Non_Null_Intersection (Future_Dirs, Harness_Dir_Ar) then
            Cmd_Error_No_Help
              ("invalid output directory, cannot mix up "
               & "tests and infrastructure");
         end if;

         Test.Skeleton.Source_Table.Set_Separate_Root (Maximin_Root.all);
      else

         loop
            Project := Current (Iterator);
            exit when Project = No_Project;

            declare
               Dirs : constant File_Array := Project.Source_Dirs (False);

               Common_Root_Dir : String_Access;
            begin
               if Dirs'Length > 0 then
                  Common_Root_Dir := new String'
                    (Dirs (Dirs'First).Display_Full_Name);

                  for J in Dirs'Range loop
                     Tmp := new String'(Dirs (J).Display_Full_Name);
                     Buff := new String'
                       (Common_Root (Tmp.all, Common_Root_Dir.all));

                     if Buff.all = "" then
                        Cmd_Error_No_Help
                          ("gnattest: sources have different root dirs, "
                           & "cannot apply separate root output");
                     end if;

                     Free (Common_Root_Dir);
                     Common_Root_Dir := new String'(Buff.all);
                     Free (Buff);
                     Free (Tmp);
                  end loop;

                  for J in Dirs'Range loop
                     if Dirs (J).Display_Full_Name = Common_Root_Dir.all then
                        Maximin_Root := Common_Root_Dir;
                        exit;
                     end if;
                  end loop;
               end if;
            end;

            Files := Project.Source_Files;

            if Files'Length > 0 then
               if Maximin_Root = null then
                  Maximin_Root := new String'
                    (Files (Files'First).Display_Dir_Name);
               end if;

               for F in Files'Range loop
                  Tmp := new String'(Files (F).Display_Dir_Name);
                  Buff := new String'(Common_Root (Tmp.all, Maximin_Root.all));

                  if Buff.all = "" then
                     Cmd_Error_No_Help
                       ("gnattest: sources have different root dirs, "
                        & "cannot apply separate root output");
                  end if;

                  Free (Maximin_Root);
                  Maximin_Root := new String'(Buff.all);
                  Free (Buff);
                  Free (Tmp);
               end loop;

               Root_Length := Maximin_Root.all'Length;

               Obj_Dir := new String'(Project.Object_Dir.Display_Full_Name);

               Local_Separate_Root_Dir := new String'
                 (Normalize_Pathname
                    (Name => Obj_Dir.all & Separate_Root_Dir.all,
                     Case_Sensitive => False));

               for F in Files'Range loop

                  if
                    Source_Project_Tree.Info (Files (F)).Unit_Part = Unit_Spec
                    and then Test.Skeleton.Source_Table.Source_Present
                      (Files (F).Display_Full_Name)
                  then
                     Tmp := new String'(Files (F).Display_Dir_Name);

                     Append (Future_Dirs, GNATCOLL.VFS.Create
                       (+(Local_Separate_Root_Dir.all & Directory_Separator &
                          Tmp.all (Root_Length + 1 .. Tmp.all'Last))));

                     Test.Skeleton.Source_Table.Set_Output_Dir
                       (Files (F).Display_Full_Name,
                        Local_Separate_Root_Dir.all & Directory_Separator &
                        Tmp.all (Root_Length + 1 .. Tmp.all'Last));
                  end if;

               end loop;

            end if;

            --  Externally built projects should be skipped.
            loop
               Next (Iterator);

               if
                 Current (Iterator) = No_Project
                 or else (not Has_Attribute (Current (Iterator), Ext_Bld))
                 or else
                   To_Lower
                     (Attribute_Value (Current (Iterator), Ext_Bld)) /= "true"
               then
                  exit;
               end if;
            end loop;
         end loop;

         if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
            Cmd_Error_No_Help
              ("invalid output directory, cannot mix up "
               & "tests and sources");
         end if;

         if Non_Null_Intersection (Future_Dirs, Harness_Dir_Ar) then
            Cmd_Error_No_Help
              ("invalid output directory, cannot mix up "
               & "tests and infrastructure");
         end if;

      end if;

   end Check_Separate_Root;

   ----------------
   -- Check_Stub --
   ----------------

   procedure Check_Stub is
      use Test.Common;

      Tmp : String_Access;
      SD_Name : constant Virtual_File :=
        GNATCOLL.VFS.Create (+Stub_Dir_Name.all);
      Future_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);

      All_Source_Locations : constant File_Array :=
        Source_Project_Tree.Root_Project.Source_Dirs (Recursive => True);

      Obj_Dir : String_Access;

      Project  : Project_Type;
      Iterator : Project_Iterator :=
        Start (Source_Project_Tree.Root_Project);
   begin

      --  look for collisions with source dirs
      if SD_Name.Is_Absolute_Path then
         Append (Future_Dirs, GNATCOLL.VFS.Create (+Test_Dir_Name.all));
      else
         loop
            Project := Current (Iterator);
            exit when Project = No_Project;

            Obj_Dir := new String'(Project.Object_Dir.Display_Full_Name);
            Tmp := new String'(Obj_Dir.all & Stub_Dir_Name.all);
            Append (Future_Dirs, GNATCOLL.VFS.Create (+Tmp.all));
            Free (Tmp);
            Free (Obj_Dir);

            Next (Iterator);
         end loop;
      end if;

      if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
         Cmd_Error_No_Help
           ("gnattest: invalid stub directory, cannot mix up "
            & "stubs and source files");
      end if;

      Test.Skeleton.Source_Table.Set_Direct_Stub_Output;

      --  Once stub dirs are set we can compare them with test dirs per source.
      Skeleton.Source_Table.Reset_Source_Iterator;
      Tmp := new String'(Skeleton.Source_Table.Next_Source_Name);
      while Tmp.all /= "" loop
         if
           Skeleton.Source_Table.Get_Source_Output_Dir (Tmp.all) =
           Skeleton.Source_Table.Get_Source_Stub_Dir (Tmp.all)
         then
            Test.Common.Report_Std
              ("gnattest: "
               & Skeleton.Source_Table.Get_Source_Stub_Dir (Tmp.all)
               & " is used for more than one purpose");
            Cmd_Error_No_Help
              ("gnattest: invalid stub directory, cannot mix up "
               & "stubs and tests");
         end if;
         Free (Tmp);
         Tmp := new String'(Skeleton.Source_Table.Next_Source_Name);
      end loop;

      Skeleton.Source_Table.Reset_Source_Iterator;
   end Check_Stub;

   ------------------------------
   -- Process_Additional_Tests --
   ------------------------------

   procedure Process_Additional_Tests
     (Env : Project_Environment_Access; Cmd : Command_Line)
   is
      PT       : Project_Tree_Access := new Project_Tree;
      Sources  : File_Array_Access;
      Src_Info : File_Info;

      Context  : Analysis_Context;
      Provider : Unit_Provider_Reference;
      Unit     : Analysis_Unit;

      Current_Source : String_Access;

      use Libadalang.Project_Provider;
   begin
      PT.Load (Create (+Test.Common.Additional_Tests_Prj.all), Env);
      Sources := PT.Root_Project.Source_Files;
      for Src in Sources.all'Range loop
         Src_Info := PT.Info (Sources (Src));
         if Src_Info.Unit_Part = Unit_Spec then
            Test.Harness.Source_Table.Add_Source_To_Process
              (Sources (Src).Display_Full_Name);
         end if;
      end loop;
      Unchecked_Free (Sources);

      Provider := Create_Project_Unit_Provider
        (Tree             => PT,
         Env              => Env,
         Is_Project_Owner => False);

      Context := Create_Context
        (Charset       => Wide_Character_Encoding (Cmd),
         Unit_Provider => Provider);

      Current_Source := new String'
        (Test.Harness.Source_Table.Next_Non_Processed_Source);
      while Current_Source.all /= "" loop

         Unit := Get_From_File
           (Context,
            Test.Harness.Source_Table.Get_Source_Full_Name
              (Current_Source.all));

         Test.Harness.Process_Source (Unit);

         Free (Current_Source);
         Current_Source := new String'
           (Test.Harness.Source_Table.Next_Non_Processed_Source);
      end loop;
      Free (Current_Source);

      PT.Unload;
      Free (PT);
   end Process_Additional_Tests;

end Test.Actions;
