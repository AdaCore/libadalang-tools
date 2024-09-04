------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GPR2.Containers;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.View;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

with Pp.Command_Lines;
with Pp.Scanner;

with Utils.Char_Vectors;
with Utils.Command_Lines;
with Utils.Command_Lines.Common;

with Laltools.Partial_GNATPP;

--  This procedure defines the partial gnatpp formatting tool

--  Usage:
--  partial_gnatpp -S <source-file> -SL <start-line> -EL <end-line>
--  -SC <start-column> -EC <end-column>
--
--  -S,  --source-file     Source code file of the selection to reformat
--  -SL, --start-line      Line of the first statement to extract
--  -EL, --end-line        Line of the last statement to extract
--  -SC, --start-column    Column of the first statement to extract
--  -EC, --end-column      Column of the last statement to extract

procedure Partial_GNATpp is

   PP_Pack                  : constant GPR2.Package_Id :=
     GPR2."+" ("pretty_printer");
   PP_Default_Switches_Attr : constant GPR2.Q_Attribute_Id :=
     (PP_Pack, GPR2."+" ("default_switches"));

   procedure Partial_GNATpp_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array);

   package Partial_GNATpp_App is new Libadalang.Helpers.App
     (Name             => "Partial_GNATpp",
      Description      => "Partial_GNATpp",
      App_setup        => Partial_GNATpp_App_Setup);

   package Args is

      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Partial_GNATpp_App.Args.Parser,
         Short       => "-S",
         Long        => "--source-file",
         Help        => "Source code file of the selection",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Start_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Partial_GNATpp_App.Args.Parser,
         Short       => "-SL",
         Long        => "--start-line",
         Help        => "Start line",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 1,
         Enabled     => True);

      package Start_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Partial_GNATpp_App.Args.Parser,
         Short       => "-SC",
         Long        => "--start-column",
         Help        => "Start column",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 1,
         Enabled     => True);

      package End_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Partial_GNATpp_App.Args.Parser,
         Short       => "-EL",
         Long        => "--end-line",
         Help        => "End line",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 1,
         Enabled     => True);

      package End_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Partial_GNATpp_App.Args.Parser,
         Short       => "-EC",
         Long        => "--end-column",
         Help        => "End column",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 1,
         Enabled     => True);

      package Source_Line_Breaks is new GNATCOLL.Opt_Parse.Parse_Flag
        (Parser      => Partial_GNATpp_App.Args.Parser,
         Long        => "--source-line-breaks",
         Help        => "Take line breaks only from source",
         Enabled     => True);

   end Args;

   --------------------------------
   --  Partial_GNATpp_App_Setup  --
   --------------------------------

   procedure Partial_GNATpp_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      use Pp.Command_Lines;
      use Utils.Command_Lines;
      use Laltools.Partial_GNATPP;

      procedure Setup_Pretty_Printer_Switches;

      --  Setups PP_Options by doing the first pass and then checks if this
      --  project has a "Pretty_Printer" package with additional switches.
      --  If so, do a second and final parse to update PP_Options with these.

      Source_File : constant String := To_String (Args.Source.Get);

      Selection_Range : constant Source_Location_Range :=
        (Line_Number (Args.Start_Line.Get),
         Line_Number (Args.End_Line.Get),
         Column_Number (Args.Start_Column.Get),
         Column_Number (Args.End_Column.Get));

      Unit : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_File);

      PP_Options : Command_Line (Pp.Command_Lines.Descriptor'Access);

      -----------------------------------
      -- Setup_Pretty_Printer_Switches --
      -----------------------------------

      procedure Setup_Pretty_Printer_Switches is
         Dummy : GNAT.Strings.String_List_Access :=
           new GNAT.Strings.String_List (1 .. 0);

      begin
         Parse
           (Dummy,
            PP_Options,
            Phase              => Cmd_Line_1,
            Callback           => null,
            Collect_File_Names => False,
            Ignore_Errors      => True);
         GNAT.OS_Lib.Free (Dummy);

         --  If Context.Provider.Kind is in Project_File, it means that a
         --  project was given by the -P option.
         --  Partial_GNATpp_App.Args.Project_File cannot be an empty string
         --  in that case.
         if Context.Provider.Kind in Project_File then
            Ada.Assertions.Assert
               (To_String (Partial_GNATpp_App.Args.Project_File.Get) /= "");

            --  Set the Project_File option in PP_Options
            Utils.Command_Lines.Common.Common_String_Switches.Set_Arg
               (PP_Options,
               Utils.Command_Lines.Common.Project_File,
               To_String (Partial_GNATpp_App.Args.Project_File.Get));

            --  If this project has a "Pretty_Printer" package with additional
            --  switches, do a second and final parse to update PP_Options with
            --  these.
            declare
               Project          : constant GPR2.Project.View.Object :=
                  Context.Provider.Project.Root_Project;
               Attr_Value       : constant GPR2.Project.Attribute.Object :=
                 Project.Attribute
                   (Name  => PP_Default_Switches_Attr,
                    Index => GPR2.Project.Attribute_Index.Create ("ada"));
            begin
               if Attr_Value.Is_Defined then
                  declare
                     use GNAT.OS_Lib;

                     GPR_Values : constant GPR2.Containers.Source_Value_List :=
                       Attr_Value.Values;
                     Values     : Argument_List_Access :=
                       new Argument_List (1 .. Natural (GPR_Values.Length));
                  begin
                     for I in Values.all'Range loop
                        Values (I) := new String'(GPR_Values (I).Text);
                     end loop;
                     Parse
                       (Values,
                        PP_Options,
                        Phase              => Project_File,
                        Callback           => null,
                        Collect_File_Names => False,
                        Ignore_Errors      => True);
                     Free (Values);
                  end;
               end if;
            end;
         end if;
      end Setup_Pretty_Printer_Switches;

   begin
      Setup_Pretty_Printer_Switches;

      if Args.Source_Line_Breaks.Get then

         --  Format the selected range of the text. If the --source-line-breaks
         --  switch is passed then the formetted text will be filtered and only
         --  the reformatted initial selected lines will be returned.
         --  Otherwise, the enclosing parent node will be rewritten, the node
         --  is returned as value of Formatted_Node parameter in this call.
         --  This is based on the gnatpp engine and has as entry point the
         --  Format_Vector of PP.Actions.

         declare
            Enclosing_Node  : Ada_Node;
            Output          : Utils.Char_Vectors.Char_Vector;
            Output_SL_Range : Source_Location_Range;
            Messages        : Pp.Scanner.Source_Message_Vector;

         begin

            Format_Selection
              (Main_Unit                => Unit,
               Input_Selection_Range    => Selection_Range,
               Output                   => Output,
               Output_Selection_Range   => Output_SL_Range,
               PP_Messages              => Messages,
               Formatted_Node           => Enclosing_Node,
               PP_Options               => PP_Options,
               Force_Source_Line_Breaks => Args.Source_Line_Breaks.Get);

            --  Create the text edits to be passed to the IDE related to the
            --  rewritten selection.

            declare
               Output_Str : constant String :=
                 Utils.Char_Vectors.Char_Vectors.Elems (Output)
                   (1 .. Utils.Char_Vectors.Char_Vectors.Last_Index (Output));

               Edit : Partial_Formatting_Edit;

            begin
               Edit := Partial_Formatting_Edit'
                 (Diagnostics    => Messages,
                  Formatted_Node => Enclosing_Node,
                  Indentation    => 0,
                  Edit           =>
                    Text_Edit'
                      (Location => Output_SL_Range,
                       Text     =>
                         Ada.Strings.Unbounded.To_Unbounded_String
                           (Output_Str)));
               Ada.Text_IO.Put_Line (Image (Edit));
               New_Line;
            end;
         end;
      else
         Ada.Text_IO.Put_Line
           (Image (Format_Selection (Unit, Selection_Range, PP_Options)));
         New_Line;
      end if;

   end Partial_GNATpp_App_Setup;

begin
   GPR2.Project.Registry.Pack.Add
     (PP_Pack, GPR2.Project.Registry.Pack.Everywhere);
   GPR2.Project.Registry.Attribute.Add
     (Name                 => PP_Default_Switches_Attr,
      Index_Type           => GPR2.Project.Registry.Attribute.Language_Index,
      Value                => GPR2.Project.Registry.Attribute.List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);

   Partial_GNATpp_App.Run;
end Partial_GNATpp;
