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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

with Pp.Command_Lines;
with Pp.Scanner;

with Utils.Char_Vectors;
with Utils.Command_Lines;

with Laltools.Partial_GNATPP;
with Laltools.Refactor;

with GNAT.Strings;

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

      package Keep_Source_LB is new GNATCOLL.Opt_Parse.Parse_Flag
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
      pragma Unreferenced (Context);
      Source_File : constant String := To_String (Args.Source.Get);

      Selection_Range      : constant Source_Location_Range :=
        (Line_Number (Args.Start_Line.Get),
         Line_Number (Args.End_Line.Get),
         Column_Number (Args.Start_Column.Get),
         Column_Number (Args.End_Column.Get));

      Partial_Gnatpp_SLB   : constant Boolean := Args.Keep_Source_LB.Get;

      Main_Unit            : Analysis_Unit;
      Enclosing_Node       : Ada_Node;

      use Pp.Command_Lines;
      use Utils.Char_Vectors;
      use Utils.Command_Lines;
      use Laltools.Partial_GNATPP;

      PP_Options      : Command_Line (Pp.Command_Lines.Descriptor'Access);
      Output          : Char_Vector;
      Output_SL_Range : Source_Location_Range;
      Messages        : Pp.Scanner.Source_Message_Vector;
      Validated       : GNAT.Strings.String_List_Access :=
        new GNAT.Strings.String_List (1 .. 0);

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File (Source_File);

      Parse
        (Validated,
         PP_Options,
         Phase              => Cmd_Line_1,
         Callback           => null,
         Collect_File_Names => False,
         Ignore_Errors      => True);
      GNAT.Strings.Free (Validated);

      --  Format the selected range of the text. If the --source-line-breaks
      --  switch is passed then the formetted text will be filtered and only
      --  the reformatted initial selected lines will be returned. Otherwise,
      --  the enclosing parent node will be rewritten, the node is returned
      --  as value of Formatted_Node parameter in this call.
      --  This is based on the gnatpp engine and has as entry point the
      --  Format_Vector of PP.Actions.

      Format_Selection
        (Main_Unit              => Main_Unit,
         Input_Selection_Range  => Selection_Range,
         Output                 => Output,
         Output_Selection_Range => Output_SL_Range,
         PP_Messages            => Messages,
         Formatted_Node         => Enclosing_Node,
         PP_Options             => PP_Options,
         Keep_Source_LB         => Partial_Gnatpp_SLB);

      --  Create the text edits to be passed to the IDE related to the
      --  rewritten selection.

      declare
         Output_Str : constant String :=
           Char_Vectors.Elems (Output)
           (1 .. Char_Vectors.Last_Index (Output));

         Edits : Partial_Select_Edits;

         use Laltools.Refactor;
      begin
         Edits := Partial_Select_Edits'
           (Unit => Main_Unit,
            Node => Enclosing_Node,
            Edit => Text_Edit'
              (Location => Output_SL_Range,
               Text     => Ada.Strings.Unbounded.To_Unbounded_String
                 (Output_Str)));
         Print (Edits);
      end;

      New_Line;
   end Partial_GNATpp_App_Setup;

begin
   Partial_GNATpp_App.Run;
end Partial_GNATpp;
