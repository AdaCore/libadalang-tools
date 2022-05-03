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
--  Remove Parameter Tool
--
--  Depending on the provided source code location, this tool can perform three
--  different methods to remove parameters and their respective arguments in
--  the subprogram calls.
--
--  Usage:
--  remove_parameter -P <project_file> -S <source_code_file> -L <line_number>
--  -R <column_number>
--
--  -P, --project          Project file to use
--  -S, --source           Source code file of the node
--  -L, --line             Line number of the node
--  -R, --column           Column number of the node
--
--  Example: 'Qux (Foo, Bar : in Integer);'
--
--  1) Remove a single parameter:
--     If the provided source location is within Foo's location, Foo is
--     removed.
--
--  2) Remove all parameters of a Param_Spec:
--     If the provided source location in within the subtype indication of
--     the Param_Spec, then the intire Param_Spec is removed.
--
--  3) Remove all parameters of the subprogram:
--     If the provided source location in within the subprogram name, then
--     all parameters are moved.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;

with Laltools.Common; use Laltools.Common;
with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Subprogram_Signature;
use Laltools.Refactor.Subprogram_Signature;
with Laltools.Refactor.Subprogram_Signature.Remove_Parameter;
use Laltools.Refactor.Subprogram_Signature.Remove_Parameter;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

procedure Remove_Parameter is

   procedure Remove_Parameter_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  Main procedure of this program.

   function Action_Description
     (Target_Subp              : Basic_Decl'Class;
      Target_Parameter_Indices : Parameter_Indices_Range_Type)
      return String;
   --  Return an description of the action this program will do.
   --  Subp is the target subprogram and Target_Parameter_Indices are the
   --  indices of the parameters to be removed.
   --
   --  The description is generated based on the amount of parameters to be
   --  removed.
   --
   --  1) Only one parameter
   --     Example: Removing parameter A
   --
   --  2) Two parameters
   --     Example: Removing parameters A and B
   --
   --  3) Three or more parameters
   --     Example: Removing parameters A to C

   package Remove_Parameter_App is new Libadalang.Helpers.App
     (Name             => "remove_parameter",
      Description      => "Remove Parameter",
      App_setup        => Remove_Parameter_App_Setup);

   package Source is new Parse_Option
     (Parser      => Remove_Parameter_App.Args.Parser,
      Short       => "-S",
      Long        => "--source",
      Help        => "Source code file of the node",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Default_Val => Null_Unbounded_String,
      Enabled     => True);

   package Line is new Parse_Option
     (Parser      => Remove_Parameter_App.Args.Parser,
      Short       => "-L",
      Long        => "--line",
      Help        => "Line of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package Column is new Parse_Option
     (Parser      => Remove_Parameter_App.Args.Parser,
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

   function Action_Description
     (Target_Subp              : Basic_Decl'Class;
      Target_Parameter_Indices : Parameter_Indices_Range_Type)
      return String is
   begin
      if Target_Parameter_Indices.First = Target_Parameter_Indices.Last then
         return Image
           ("Removing parameter "
            & Get_Parameter_Name
              (Target_Subp, Target_Parameter_Indices.First));

      elsif Target_Parameter_Indices.Last =
        Target_Parameter_Indices.First + 1
      then
         return Image
           ("Removing parameters "
            & Get_Parameter_Name
              (Target_Subp, Target_Parameter_Indices.Last - 1)
            & " and "
            & Get_Parameter_Name
              (Target_Subp, Target_Parameter_Indices.Last));

      else
         return Image
           ("Removing parameters "
            & Get_Parameter_Name (Target_Subp, Target_Parameter_Indices.First)
            & " to "
            & Get_Parameter_Name (Target_Subp, Target_Parameter_Indices.Last));
      end if;
   end Action_Description;

   --------------------------------
   -- Remove_Parameter_App_Setup --
   --------------------------------

   procedure Remove_Parameter_App_Setup
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

      Target_Subp              : Basic_Decl;
      Target_Parameter_Indices : Parameter_Indices_Range_Type;

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

      if Is_Remove_Parameter_Available
        (Node, Target_Subp, Target_Parameter_Indices)
      then
         declare
            Remover : constant Parameter_Remover :=
              Create (Target_Subp, Target_Parameter_Indices);

         begin
            Put_Line
              (Action_Description (Target_Subp, Target_Parameter_Indices));

            Edits := Remover.Refactor (Analysis_Units'Access);

            Print (Edits.Text_Edits);
         end;

      else
         Put_Line
           ("Not possible to remove any parameter given node " & Node.Image);
      end if;
   end Remove_Parameter_App_Setup;

begin
   Remove_Parameter_App.Run;
end Remove_Parameter;
