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

with Pp.Actions;
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
   end Args;

   --------------------------------
   --  Partial_GNATpp_App_Setup  --
   --------------------------------

   procedure Partial_GNATpp_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);
      Source_File : constant String := To_String (Args.Source.Get);

      Selection_Range : constant Source_Location_Range :=
        (Line_Number (Args.Start_Line.Get),
         Line_Number (Args.End_Line.Get),
         Column_Number (Args.Start_Column.Get),
         Column_Number (Args.End_Column.Get));

      Main_Unit  : Analysis_Unit;

      Start_Node, End_Node : Ada_Node;
      Enclosing_Node       : Ada_Node;
      Offset               : Natural := 0;

   begin
      --  Ada.Text_IO.Put_Line ("MKU Source_File = " & Source_File);
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File (Source_File);

      declare
         use Pp.Actions;
         use Pp.Command_Lines;
         use Utils.Char_Vectors;
         use Utils.Command_Lines;
         use Laltools.Partial_GNATPP;

         PP_Options : Command_Line (Pp.Command_Lines.Descriptor'Access);
         Input_Sel  : Char_Vector;
         Output     : Char_Vector;
         Messages   : Pp.Scanner.Source_Message_Vector;

         Validated  : GNAT.Strings.String_List_Access :=
           new GNAT.Strings.String_List (1 .. 0);
      begin

         Parse
           (Validated,
            PP_Options,
            Phase              => Cmd_Line_1,
            Callback           => null,
            Collect_File_Names => False,
            Ignore_Errors      => True);
         GNAT.Strings.Free (Validated);

         --  Find the corresponding Start_Node and End_Node given the initial
         --  selection range
         Get_Selected_Region_Enclosing_Node (Main_Unit,
                                             Selection_Range,
                                             Start_Node,
                                             End_Node,
                                             Enclosing_Node,
                                             Input_Sel);

         --  Ada.Text_IO.Put_Line
         --    ("MKU Enclosing_Node = " & Enclosing_Node.Image);
         --
         --  Ada.Text_IO.Put_Line
         --    ("MKU Enclosing_Node START SLOC = ("
         --     & Enclosing_Node.Sloc_Range.Start_Line'Img & ","
         --     & Enclosing_Node.Sloc_Range.Start_Column'Img
         --     & ")");
         --
         --  Ada.Text_IO.Put_Line
         --    ("MKU Enclosing_Node END SLOC = ("
         --     & Enclosing_Node.Sloc_Range.End_Line'Img & ","
         --     & Enclosing_Node.Sloc_Range.End_Column'Img
         --     & ")");

         pragma Assert (Enclosing_Node /= No_Ada_Node);

         --  Determine the offset for the indentation of the enclosing node
         --  based on the previous or next sibling starting column position
         --  and set this value for further usage by Insert_Indentation in
         --  the post phases processing of the tree.

         Offset := Get_Starting_Offset (Enclosing_Node,
                                        PP_Indentation (PP_Options),
                                        PP_Indent_Continuation (PP_Options));
         --  Ada.Text_IO.Put_Line (" MKU   Offset = " & Offset'Img);

         if Offset /= 0 then
            Set_Partial_Gnatpp_Offset (Offset - 1);
         end if;

         declare
            Input_Sel_Str : constant String :=
              Char_Vectors.Elems (Input_Sel)
              (1 .. Char_Vectors.Last_Index (Input_Sel));
            pragma Unreferenced (Input_Sel_Str);

         begin
            --  Ada.Text_IO.Put_Line (Input_Sel_Str);
            null;
         end;

         begin
            Format_Vector
              (Cmd       => PP_Options,
               Input     => Input_Sel,
               Node      => Enclosing_Node,
               Output    => Output,
               Messages  => Messages,
               Partial_Gnatpp => True);
         exception
            when others =>
               --  Ada.Text_IO.Put_Line ("Partial_Gnatpp: Unknown error");
               Output := Input_Sel;
         end;

         declare
            Output_Str : constant String :=
              Char_Vectors.Elems (Output)
              (1 .. Char_Vectors.Last_Index (Output));

            New_Sel_Range : constant Source_Location_Range :=
              (Enclosing_Node.Sloc_Range.Start_Line,
               Enclosing_Node.Sloc_Range.End_Line,
               Enclosing_Node.Sloc_Range.Start_Column,
               Enclosing_Node.Sloc_Range.End_Column);

            Edits : Partial_Select_Edits;

            use Laltools.Refactor;
         begin
            Edits := Partial_Select_Edits'
              (Unit => Main_Unit,
               Node => Enclosing_Node,
               Edit => Text_Edit'
                 (Location => New_Sel_Range,
                  Text     => Ada.Strings.Unbounded.To_Unbounded_String
                    (Output_Str)));
            Print (Edits);
         end;
      end;

      New_Line;
   end Partial_GNATpp_App_Setup;

begin
   Partial_GNATpp_App.Run;
end Partial_GNATpp;
