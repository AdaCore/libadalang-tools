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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Subprogram_Signature;
use Laltools.Refactor.Subprogram_Signature;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

procedure Add_Parameter is

   function Is_Data_Valid
     (Parameter_Data : Parameter_Data_Type;
      Requires_Type  : Boolean)
      return Boolean is
     (Parameter_Data.Name /= Null_Unbounded_String
      and then (if Requires_Type
        then Parameter_Data.Type_Indication /= Null_Unbounded_String
        else True));
   --  This functions checks if the new parameter data given as input to this
   --  program is valid.

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

   package Parameter_Name is new Parse_Option
     (Parser      => Add_Parameter_App.Args.Parser,
      Short       => "-N",
      Long        => "--name",
      Help        => "Parameter name",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Default_Val => Null_Unbounded_String,
      Enabled     => True);

   package Parameter_Mode is new Parse_Option
     (Parser      => Add_Parameter_App.Args.Parser,
      Short       => "-M",
      Long        => "--mode",
      Help        => "Parameter mode",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Default_Val => Null_Unbounded_String,
      Enabled     => True);

   package Parameter_Type is new Parse_Option
     (Parser      => Add_Parameter_App.Args.Parser,
      Short       => "-T",
      Long        => "--type",
      Help        => "Parameter type",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Default_Val => Null_Unbounded_String,
      Enabled     => True);

   package Parameter_Default is new Parse_Option
     (Parser      => Add_Parameter_App.Args.Parser,
      Short       => "-D",
      Long        => "--default",
      Help        => "Parameter default value",
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

      Target_Subp            : Basic_Decl;
      Target_Parameter_Index : Positive;
      Requires_Type          : Boolean;
      Parameter_Data         : constant Parameter_Data_Type :=
        (Name            => Parameter_Name.Get,
         Mode            => Parameter_Mode.Get,
         Type_Indication => Parameter_Type.Get,
         Default_Expr    => Parameter_Default.Get);

      Edits : Edit_Map;

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File
        (Ada.Strings.Unbounded.To_String (Source_File));

      Node := Main_Unit.Root.Lookup (Sloc);

      for File of Files.all loop
         declare
            Filename : constant GNATCOLL.VFS.Filesystem_String :=
              File.Full_Name;

         begin
            Units (Units_Index) :=
              Node.Unit.Context.Get_From_File (String (Filename));
            Units_Index := Units_Index + 1;
         end;
      end loop;

      if Is_Add_Parameter_Available
        (Node, Target_Subp, Target_Parameter_Index, Requires_Type)
      then
         if Is_Data_Valid (Parameter_Data, Requires_Type)
         then
            declare
               Adder : constant Parameter_Adder :=
                 Create (Target_Subp, Parameter_Data, Target_Parameter_Index);

            begin
               Put_Line
                 ("Adding parameter ("
                  & To_String (Image (Parameter_Data))
                  & ") with index "
                  & Target_Parameter_Index'Image);

               Edits := Adder.Refactor (Analysis_Units'Access);

               Print (Edits);
            end;

         else
            Put_Line
              ("Not possible to add any parameter due to invalid input");
            New_Line;
         end if;

      else
         Put_Line
           ("Not possible to add any parameter given node " & Node.Image);
         New_Line;
      end if;
   end Add_Parameter_App_Setup;

begin
   Add_Parameter_App.Run;
end Add_Parameter;
