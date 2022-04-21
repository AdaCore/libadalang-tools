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
--
--  Suppress Separate Tool
--
--  Usage:
--  suppress_separate -P <project_file> -S <source_code_file> -L <line_number>
--  -R <column_number>
--
--  -P, --project          Project file to use
--  -S, --source           Source code file of the node
--  -L, --line             Line number of the node
--  -R, --column           Column number of the node
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Suppress_Separate;
use Laltools.Refactor.Suppress_Separate;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

procedure Suppress_Separate is

   procedure Suppress_Separate_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  Main procedure of this program

   function Action_Description (Target_Separate : Basic_Decl) return String;
   --  Prints a description of the separate that is being suppressed

   package Suppress_Separate_App is new Libadalang.Helpers.App
     (Name             => "suppress_separate",
      Description      => "Suppress Separate",
      App_setup        => Suppress_Separate_App_Setup);

   package Source is new Parse_Option
     (Parser      => Suppress_Separate_App.Args.Parser,
      Short       => "-S",
      Long        => "--source",
      Help        => "Source code file of the node",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Default_Val => Null_Unbounded_String,
      Enabled     => True);

   package Line is new Parse_Option
     (Parser      => Suppress_Separate_App.Args.Parser,
      Short       => "-L",
      Long        => "--line",
      Help        => "Line of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package Column is new Parse_Option
     (Parser      => Suppress_Separate_App.Args.Parser,
      Short       => "-R",
      Long        => "--column",
      Help        => "Column of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   ------------------------
   -- Action_Description --
   ------------------------

   function Action_Description (Target_Separate : Basic_Decl) return String is
   begin
      return "Suppressing " & Image (Target_Separate);
   end Action_Description;

   --------------------------------
   -- Remove_Parameter_App_Setup --
   --------------------------------

   procedure Suppress_Separate_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      Source_File : constant Unbounded_String := Source.Get;

      Sloc : constant Source_Location :=
        (Line_Number (Line.Get), Column_Number (Column.Get));

      Files : constant File_Array_Access :=
        Context.Provider.Project.Root_Project.Source_Files;

      Main_Unit       : Analysis_Unit;
      Node            : Ada_Node;
      Number_Of_Units : constant Positive := Files'Length;
      Units_Index     : Positive := 1;
      Units           : Analysis_Unit_Array (1 .. Number_Of_Units);

      function Analysis_Units return Analysis_Unit_Array is (Units);

      Target_Separate : Basic_Decl;

      Edits : Refactoring_Edits;

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File
        (To_String (Source_File));

      Node := Main_Unit.Root.Lookup (Sloc);

      for File of Files.all loop
         declare
            Filename : constant Filesystem_String := File.Full_Name;

         begin
            Units (Units_Index) :=
              Node.Unit.Context.Get_From_File (String (Filename));
            Units_Index := Units_Index + 1;
         end;
      end loop;

      if Is_Suppress_Separate_Available (Node, Target_Separate) then
         declare
            Suppressor : constant Separate_Suppressor :=
               Create (Target_Separate);

         begin
            Put_Line (Action_Description (Target_Separate));

            Edits := Suppressor.Refactor (Analysis_Units'Access);

            Print (Edits);
            New_Line;
         end;

      else
         Put_Line
           ("Not possible to suppress any separate given node "
            & Node.Image);
      end if;
   end Suppress_Separate_App_Setup;

begin
   Suppress_Separate_App.Run;
end Suppress_Separate;
