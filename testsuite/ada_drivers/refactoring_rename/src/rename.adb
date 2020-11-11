------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

with Laltools.Refactor.Rename; use Laltools.Refactor.Rename;

with Printers; use Printers;

--  This procedure defines the Refactor Rename Tool. Given the location of
--  an identifier in a source code file, and the project it belongs to, it
--  finds all references of the node's refereced declaration and checks
--  if the rename will cause an issue.

--  Usage:
--  outgoing_calls -P <project_file> -S <source_code_file> -L <line_number>
--  -R <column_number> -N <new_name> -A <algorithm>
--
--  -P, --project          Project file to use
--  -S, --source           Source code file of the identifier
--  -L, --line             Line number of the identifier
--  -R, --column           Column number of the identifier
--  -N, --new-name         New name
--  -A, --algorithm        Algorithm used to check for rename conflicts:
--                         'map_references' or 'analyse_ast'

procedure Rename is

   Main_Unit : Analysis_Unit;
   Node : Ada_Node;

   procedure App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package App is new Libadalang.Helpers.App
     (Name             => "rename",
      Description      => "Rename",
      App_setup        => App_Setup);

   package Source is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Short       => "-S",
      Long        => "--source",
      Help        => "Source code file of the node",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String,
      Enabled     => True);

   package Line is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Short       => "-L",
      Long        => "--line",
      Help        => "Line of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package Column is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Short       => "-R",
      Long        => "--column",
      Help        => "Column of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package New_Name is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Short       => "-N",
      Long        => "--new-name",
      Help        => "New name",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String,
      Enabled     => True);

   package Algorithm is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Long        => "--algorithm",
      Help        => "Algorithm used to check for rename conflicts: "
      & "'map_references' or 'analyse_ast'",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.To_Unbounded_String
        ("map_references"),
      Enabled     => True);

   ---------------
   -- App_Setup --
   ---------------

   procedure App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array)
   is
      Source_File : constant Unbounded_String :=
        Source.Get;
      Sloc : constant Source_Location :=
        (Line   => Line_Number (Line.Get),
         Column => Column_Number (Column.Get));
      NN : constant Unbounded_String := New_Name.Get;
      Rename_Algorithm : constant Unbounded_String := Algorithm.Get;

      Files : constant GNATCOLL.VFS.File_Array_Access :=
        Context.Provider.Project.Root_Project.Source_Files;

      Number_Of_Units : constant Positive := Files'Length;

      Idx : Positive := 1;

      Units : Analysis_Unit_Array (1 .. Number_Of_Units);

      References : Renamable_References;

      use type Ada.Strings.Unbounded.Unbounded_String;

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File
        (Ada.Strings.Unbounded.To_String (Source_File));
      Node := Main_Unit.Root.Lookup (Sloc);

      Put_Line ("# Renaming node " & Image (Node.Full_Sloc_Image));

      for F of Files.all loop
         declare
            FN : constant GNATCOLL.VFS.Filesystem_String := F.Full_Name;
         begin
            Units (Idx) := Node.Unit.Context.Get_From_File (String (FN));
            Idx := Idx + 1;
         end;
      end loop;

      if Rename_Algorithm = "map_references" then
         References := Find_All_Renamable_References
           (Node           => Node,
            New_Name       => To_Unbounded_Text
              (To_Text (Ada.Strings.Unbounded.To_String (NN))),
            Units          => Units,
            Algorithm_Kind => Map_References);

      elsif Rename_Algorithm = "analyse_ast" then
         References := Find_All_Renamable_References
           (Node           => Node,
            New_Name       => To_Unbounded_Text
              (To_Text (Ada.Strings.Unbounded.To_String (NN))),
            Units          => Units,
            Algorithm_Kind => Analyse_AST);

      else
         Put_Line ("Unrecognized algorithm");
      end if;

      PP (References);
      New_Line;
   end App_Setup;

begin
   App.Run;
end Rename;
