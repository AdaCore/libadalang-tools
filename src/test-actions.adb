------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                     G N A T T E S T . A C T I O N S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers; use type Ada.Containers.Count_Type;
with Interfaces; use type Interfaces.Unsigned_16;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Traces;

with Libadalang;     use Libadalang;
--  with LAL_Extensions; use LAL_Extensions;

with Utils.Command_Lines.Common; use Utils; use Utils.Command_Lines.Common;
pragma Unreferenced (Utils.Command_Lines.Common); -- ????
with Utils.Formatted_Output;

with Utils_Debug; use Utils_Debug;

with Test.Command_Lines; use Test.Command_Lines;

with Test.Skeleton;
with Test.Harness;
with Test.Mapping;
with Test.Common;
with Test.Skeleton.Source_Table;

with Ada.Directories; use Ada.Directories;
with Utils.Projects; use Utils.Projects;
with GNAT.Directory_Operations;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Test.Actions is

   SPT : GNATCOLL.Projects.Project_Tree renames
     Test.Common.Source_Project_Tree;

   function Is_Externally_Built (File : Virtual_File) return Boolean;
   --  Checks if the given source file belongs to an externally build library

   procedure Process_Exclusion_List (Value : String);
   --  Processes value of --exclude-from-stubbing switch

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
   begin
      GNATCOLL.Traces.Parse_Config_File;

      --  We need to fill a local source table since gnattest actually needs
      --  info not only on current source but on any particular one or even
      --  all of them at once.

      SPT := GNATCOLL.Projects.Project_Tree (Tool.Project_Tree.all);

      declare
         --  For now repeating code from Utils.Drivers to get rid of ignored
         --  files, this should be optimized.
         use Test.Common.String_Set;

         procedure Include_One (File_Name : String);
         --  Include File_Name in the Ignored set below

         Ignored : Test.Common.String_Set.Set;
         --  Set of file names mentioned in the --ignore=... switch

         procedure Include_One (File_Name : String) is
         begin
            Include (Ignored, File_Name);
         end Include_One;

      begin
         Common.Stub_Mode_ON := Arg (Cmd, Stub);

         for Ignored_Arg of Arg (Cmd, Ignore) loop
            Read_File_Names_From_File (Ignored_Arg.all, Include_One'Access);
         end loop;

         for File of File_Names (Cmd) loop
            if not Contains (Ignored, Simple_Name (File.all)) then

               if Info (SPT, Create (+File.all)).Unit_Part = Unit_Spec then
                  Test.Skeleton.Source_Table.Add_Source_To_Process (File.all);
               end if;
            end if;
         end loop;
      end;

      if Arg (Cmd, Stub) then
         Free (Test.Common.Test_Dir_Name);
         Test.Common.Test_Dir_Name := new String'
           ("gnattest_stub" & Directory_Separator & "tests");
         Free (Test.Common.Stub_Dir_Name);
         Test.Common.Stub_Dir_Name := new String'
           ("gnattest_stub" & Directory_Separator & "stubs");
         Free (Test.Common.Harness_Dir_Str);
         Test.Common.Harness_Dir_Str := new String'
           ("gnattest_stub" & Directory_Separator & "harness");
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
      Test.Skeleton.Source_Table.Set_Direct_Output;

      --  Processing harness dir specification
      --  --harness-dir is a string switch, so Arg returns null or a pointer to
      --  the specified string.

      if Arg (Cmd, Harness_Dir) /= null then
         Free (Test.Common.Harness_Dir_Str);
         if
           Is_Absolute_Path (GNATCOLL.VFS.Create (+Arg (Cmd, Harness_Dir).all))
         then
            Test.Common.Harness_Dir_Str :=
              new String'(Arg (Cmd, Harness_Dir).all);
         else
            Test.Common.Harness_Dir_Str := new String'
              (Tool.Project_Tree.Root_Project.Object_Dir.Display_Full_Name
               & Arg (Cmd, Harness_Dir).all);
         end if;
      else
         Tmp := new String'
           (Tool.Project_Tree.Root_Project.Object_Dir.Display_Full_Name
            & Test.Common.Harness_Dir_Str.all);
         Free (Test.Common.Harness_Dir_Str);
         Test.Common.Harness_Dir_Str := Tmp;
      end if;

      if Is_Regular_File (Test.Common.Harness_Dir_Str.all) then
         Cmd_Error_No_Help ("gnattest: cannot create harness directory");
      elsif not Is_Directory (Test.Common.Harness_Dir_Str.all) then

         declare
            Dir : File_Array_Access;
         begin
            Append
              (Dir, GNATCOLL.VFS.Create (+Test.Common.Harness_Dir_Str.all));
            Test.Common.Create_Dirs (Dir);
         exception
            when GNAT.Directory_Operations.Directory_Error =>
               Cmd_Error_No_Help ("gnattest: cannot create harness directory");
         end;

      end if;

      Tmp := new String'(Normalize_Pathname
        (Name           => Test.Common.Harness_Dir_Str.all,
         Case_Sensitive => False));
      Free (Test.Common.Harness_Dir_Str);
      Test.Common.Harness_Dir_Str :=
        new String'(Tmp.all & Directory_Separator);
      Free (Tmp);

      if Common.Stub_Mode_ON then
         Test.Skeleton.Source_Table.Set_Direct_Stub_Output;
         declare
            Excludes : constant String_Ref_Array :=
              Arg (Cmd, Exclude_From_Stubbing);
         begin
            for Exclude of Excludes loop
               Process_Exclusion_List (Exclude.all);
            end loop;
         end;
      end if;
   end Init;

   -----------
   -- Final --
   -----------

   procedure Final (Tool : in out Test_Tool; Cmd : Command_Line) is
      Src_Prj : constant String :=
        Tool.Project_Tree.Root_Project.Project_Path.Display_Full_Name;
   begin
      if not Arg (Cmd, Harness_Only) then
         if Arg (Cmd, Stub) then
            Test.Harness.Generate_Stub_Test_Driver_Projects (Src_Prj);
         else
            Test.Skeleton.Generate_Project_File (Src_Prj);
            Test.Harness.Test_Runner_Generator  (Src_Prj);
            Test.Harness.Project_Creator        (Src_Prj);
         end if;
         Test.Common.Generate_Common_File;
         Test.Mapping.Generate_Mapping_File;
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

      Test.Skeleton.Process_Source (Unit);
   end Per_File_Action;

   ---------------
   -- Tool_Help --
   ---------------

   procedure Tool_Help (Tool : Test_Tool) is
      pragma Unreferenced (Tool);
   begin
      pragma Style_Checks ("M200"); -- Allow long lines
      Put ("usage: gnattest [options] {filename}\n");
      Put (" options:\n");
      Put (" --version - Display version and exit\n");
      Put (" --help    - Display usage and exit\n");
      Put ("\n");

      Put (" -Pproject        - Use project file project. Only one such switch can be used\n");
      Put (" -U               - process all sources of the argument project\n");
      Put (" -U main          - process the closure of units rooted at unit main\n");
      Put (" -Xname=value     - specify an external reference for argument project file\n");
      Put (" --subdirs=dir    - specify subdirectory to place the result files into\n");
      Put (" -eL              - follow all symbolic links when processing project files\n");
      Put ("\n");

      Put (" --verbose    - verbose mode\n");
      Put (" --quiet      - quiet mode\n");
      Put ("\n");

      Put (" --etc ????\n");
      Put ("\n");
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

   procedure Process_Exclusion_List (Value : String) is
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
                 Case_Sensitive => False);
         begin
            if not Is_Regular_File (F_Path) then
               Cmd_Error_No_Help ("cannot find " & F_Path);
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

      declare
         F_Path : constant String :=
           Normalize_Pathname
             (Name           => Value,
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

end Test.Actions;
