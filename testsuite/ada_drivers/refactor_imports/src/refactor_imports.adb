------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2020-2022, AdaCore                      --
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

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Wide_Wide_Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Helpers;

with Laltools.Refactor_Imports;

--  This procedure defines the Refactor Imports Tool. Given the location of
--  an identifier in a source code file and the project it belongs to, prints
--  all possible "with" clauses and prefixes that can be added so that a
--  declaration with the same identifier's text becomes visible.

--  Usage:
--  refactor_imports -P <project_file> -S <source_code_file> -L <line_number>
--  -R <column_number>
--
--  -P, --project          Project file to use
--  -S, --source           Source code file of the identifier
--  -L, --line             Line number of the identifier
--  -R, --column           Column number of the identifier

procedure Refactor_Imports is

   package LALAnalysis renames Libadalang.Analysis;
   package LALHelpers renames Libadalang.Helpers;
   package LKSSlocs renames Langkit_Support.Slocs;

   Units : LALHelpers.Unit_Vectors.Vector;
   Main_Unit : LALAnalysis.Analysis_Unit;
   Node : LALAnalysis.Ada_Node;

   procedure App_Setup
     (Context : LALHelpers.App_Context;
      Jobs : LALHelpers.App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   procedure Process_Unit
     (Context : LALHelpers.App_Job_Context; Unit : LALAnalysis.Analysis_Unit);
   --  This procedure will be called once right after a unit is parsed

   procedure Job_Post_Process (Context : LALHelpers.App_Job_Context);
   --  This procedure will be called once after all units have been parsed.
   --  Note it will be called once per job.

   package App is new LALHelpers.App
     (Name             => "refactor_imports",
      Description      => "refactor imports",
      App_setup        => App_Setup,
      Process_Unit     => Process_Unit,
      Job_Post_Process => Job_Post_Process);

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
      Help        => "Collumn of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   ---------------
   -- App_Setup --
   ---------------

   procedure App_Setup
     (Context : LALHelpers.App_Context;
      Jobs : LALHelpers.App_Job_Context_Array)
   is
      Source_File : constant Ada.Strings.Unbounded.Unbounded_String
        := Source.Get;
      Line_Number : constant Natural
        := Line.Get;
      Column_Number : constant Natural
        := Column.Get;
      Sloc : constant LKSSlocs.Source_Location
        := (Line   => LKSSlocs.Line_Number (Line_Number),
            Column => LKSSlocs.Column_Number (Column_Number));
      Files : constant GNATCOLL.VFS.File_Array
        := Context.Provider.Project.Root_Project.Get_Environment.
          Predefined_Source_Files;
   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File
        (Ada.Strings.Unbounded.To_String (Source_File));
      Node := Main_Unit.Root.Lookup (Sloc);
      for F of Files loop
         Units.Append (Jobs (1).Analysis_Ctx.Get_From_File (+F.Full_Name));
      end loop;
   end App_Setup;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Context : LALHelpers.App_Job_Context; Unit : LALAnalysis.Analysis_Unit)
   is
      pragma Unreferenced (Context);
   begin
      Units.Append (Unit);
   end Process_Unit;

   ----------------------
   -- Job_Post_Process --
   ----------------------

   procedure Job_Post_Process (Context : LALHelpers.App_Job_Context) is
      pragma Unreferenced (Context);
   begin
      if Main_Unit.Has_Diagnostics then
         for D of Main_Unit.Diagnostics loop
            Ada.Text_IO.Put_Line ("Format_GNU_Diagnostics: " &
                                    Main_Unit.Format_GNU_Diagnostic (D));
         end loop;
      else
         if Node.Kind /= Ada_Identifier then
            return;
         end if;
         declare
            use Laltools.Refactor_Imports;
            Suggestions : constant Import_Suggestions_Vector.Vector :=
              Get_Import_Suggestions (Node, Units);
         begin
            for S of Suggestions loop
               Ada.Text_IO.Put_Line (S.Declaration.Image);
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                    (S.With_Clause_Text));
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                    (S.Prefix_Text));
            end loop;
         end;
      end if;
   end Job_Post_Process;

begin
   App.Run;
end Refactor_Imports;
