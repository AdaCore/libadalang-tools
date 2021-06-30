------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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
--  Add Parameter Tool
--
--  This tool adds a paremeter to the subprogram specified by the input.
--  The input must be the subprogram specification location, or a parameter
--  location. The new parameter is always added to the front of the given
--  location.
--
--  Usage:
--  remove_parameter -P <project_file> -S <source_code_file> -L <line_number>
--  -R <column_number> -N <name> -M <mode> -T <type> -D <default>
--
--  -P, --project          Project file to use
--  -S, --source           Source code file of the node
--  -L, --line             Line number of the node
--  -R, --column           Column number of the node
--  -N, --name             Parameter name
--  -M, --mode             Parameter mode
--  -T, --type             Parameter type
--  -D, --default          Parameter default expression
--
--  Example: procedure Foo (Bar, Baz : in out Float; Qux : in out Integer);
--
--  1) If the location given refers to 'Foo', then the new parameter will be
--     the first parameter.
--
--  2) If the location given refers to Bar, then the new parameter will be
--     added between Bar and Baz.
--
--  3) If the location given refers to Qux, then the new parameter will be
--     added as the last parameter.

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Laltools.Common; use Laltools.Common;
with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Subprogram_Signature;
use Laltools.Refactor.Subprogram_Signature;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;
with Libadalang.Common; use Libadalang.Common;

procedure Add_Parameter is

   procedure Add_Parameter_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);

   package Add_Parameter_App is new App
     (Name             => "add_parameter",
      Description      => "Add Parameter",
      App_setup        => Add_Parameter_App_Setup);

   package Source is new Parse_Option
     (Parser      => Add_Parameter_App.Args.Parser,
      Short       => "-S",
      Long        => "--source",
      Help        => "Source code file of the node",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Default_Val => Null_Unbounded_String,
      Enabled     => True);

   package Line is new Parse_Option
     (Parser      => Add_Parameter_App.Args.Parser,
      Short       => "-L",
      Long        => "--line",
      Help        => "Line of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package Column is new Parse_Option
     (Parser      => Add_Parameter_App.Args.Parser,
      Short       => "-R",
      Long        => "--column",
      Help        => "Column of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package Parameter is new Parse_Option
     (Parser      => Add_Parameter_App.Args.Parser,
      Short       => "-N",
      Long        => "--new_parameter",
      Help        => "Parameter identifier or full specification",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Default_Val => Null_Unbounded_String,
      Enabled     => True);

   -----------------------------
   -- Add_Parameter_App_Setup --
   -----------------------------

   procedure Add_Parameter_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array)
   is
      Source_File   : constant Unbounded_String := Source.Get;
      Location      : constant Source_Location :=
        (Line_Number (Line.Get), Column_Number (Column.Get));
      New_Parameter : constant Unbounded_String := Parameter.Get;

      Files : constant File_Array_Access :=
        Context.Provider.Project.Root_Project.Source_Files (Recursive => True);

      Unit            : Analysis_Unit;
      Number_Of_Units : constant Positive := Files'Length;
      Units_Index     : Positive := 1;
      Units           : Analysis_Unit_Array (1 .. Number_Of_Units);

      function Analysis_Units return Analysis_Unit_Array is (Units);

      Requires_Full_Specification : Boolean;

      Edits : Refactoring_Edits;

   begin
      Unit := Jobs (1).Analysis_Ctx.Get_From_File
        (Ada.Strings.Unbounded.To_String (Source_File));

      if Is_Add_Parameter_Available
        (Unit, Location, Requires_Full_Specification)
      then
         for File of Files.all loop
            declare
               Filename : constant GNATCOLL.VFS.Filesystem_String :=
                 File.Full_Name;

            begin
               Units (Units_Index) :=
                 Jobs (1).Analysis_Ctx.Get_From_File (String (Filename));
               Units_Index := Units_Index + 1;
            end;
         end loop;

         --  Full parameter specification is required, so check if the input
         --  is syntactically correct against Param_Spec_Rule.

         if Requires_Full_Specification
           and then not Validate_Syntax (New_Parameter, Param_Spec_Rule)
         then
            Put_Line ("Failed to add a new parameter. "
                      & "Full parameter specification is required.");
            New_Line;
            return;
         end if;

         --  Full parameter specification is not required, so check if
         --  the input is syntactically correct against Param_Spec_Rule or
         --  Identifier_Rule.

         if not Requires_Full_Specification
           and then not (Validate_Syntax (New_Parameter, Param_Spec_Rule)
                         or Validate_Syntax (New_Parameter, Defining_Id_Rule)
                         or Validate_Syntax
                           (New_Parameter, Defining_Id_List_Rule))
         then
            Put_Line ("Failed to add a new parameter. "
                      & "Full parameter specification or an identifier is "
                      & "required.");
            New_Line;
            return;
         end if;

         --  Inputs have been sanitized and it is possible to add a parameter
         --  to the given location.

         declare
            Adder : constant Parameter_Adder :=
              Create (Unit, Location, New_Parameter);

         begin
            Put_Line
              ("Adding parameter "
               & To_String (New_Parameter));

            Edits := Adder.Refactor (Analysis_Units'Access);

            Print (Edits.Text_Edits);
         end;

      else
         Put_Line
           ("Not possible to add any parameter given location "
            & To_String (Source_File) & ":" & Trim (Location.Line'Image, Both)
            & ":" & Trim (Location.Column'Image, Both));
         New_Line;
      end if;
   end Add_Parameter_App_Setup;

begin
   Add_Parameter_App.Run;
end Add_Parameter;
