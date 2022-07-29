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

      Partial_Gnatpp_SLB   : constant Boolean := Args.Keep_Source_LB.Get;

   begin
      --  Ada.Text_IO.Put_Line ("MKU Source_File = " & Source_File);
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File (Source_File);

      declare
         use Pp.Actions;
         use Pp.Command_Lines;
         use Utils.Char_Vectors;
         use Utils.Command_Lines;
         use Laltools.Partial_GNATPP;

         procedure Set_Args (PP_Options : in out Command_Line; Opt : Boolean);
         --  pragma Unreferenced (Set_Args);
         --  This procedure will updates the gnatpp command line switches
         --  if the flag --source-line-breaks is passed to patial_gnatpp.
         --  The gnatpp command line will get --source-line-breaks switch and
         --  order gnatpp switches, that might be used and potentially
         --  incompatibles with this one, in order to get the expected gnatpp
         --  behavior.

         procedure Set_Args (PP_Options : in out Command_Line; Opt : Boolean)
         is
            use Pp.Command_Lines.Pp_Boolean_Switches;
            use Pp.Command_Lines.Pp_Flag_Switches;
            use Pp.Command_Lines.Pp_Nat_Switches;
         begin
            if not Opt then
               return;
            end if;

            Set_Arg (PP_Options, Source_Line_Breaks, True);
            Set_Arg (PP_Options, Comments_Fill, False);
            Set_Arg (PP_Options, Separate_Loop_Then, False);
            Set_Arg (PP_Options, Separate_Then, False);
            Set_Arg (PP_Options, Separate_Loop, False);
            Set_Arg (PP_Options, No_Separate_Loop, False);
            Set_Arg (PP_Options, No_Separate_Then, False);
            Set_Arg (PP_Options, No_Separate_Loop_Then, False);
            Set_Arg (PP_Options, Separate_Label, False);
            Set_Arg (PP_Options, Separate_Stmt_Name, False);
            Set_Arg (PP_Options, Separate_Is, False);
            Set_Arg (PP_Options, Use_On_New_Line, False);
            Set_Arg (PP_Options, Split_Line_Before_Op, False);
            Set_Arg (PP_Options, Split_Line_Before_Record, False);
            Set_Arg (PP_Options, Insert_Blank_Lines, False);
            Set_Arg (PP_Options, Preserve_Blank_Lines, False);
            Set_Arg (PP_Options, Preserve_Line_Breaks, False);
            Set_Arg (PP_Options, Vertical_Enum_Types, False);
            Set_Arg (PP_Options, Vertical_Array_Types, False);
            Set_Arg (PP_Options, Vertical_Named_Aggregates, False);
            Set_Arg (PP_Options, Vertical_Case_Alternatives, False);
            Set_Arg (PP_Options, Call_Threshold, Natural'Last);
            Set_Arg (PP_Options, Par_Threshold, Natural'Last);
            Set_Arg (PP_Options, Case_Threshold, Natural'Last);

         end Set_Args;

         PP_Options : Command_Line (Pp.Command_Lines.Descriptor'Access);
         Input_Sel  : Char_Vector;
         Output     : Char_Vector;
         Messages   : Pp.Scanner.Source_Message_Vector;

         Validated  : GNAT.Strings.String_List_Access :=
           new GNAT.Strings.String_List (1 .. 0);
         Output_SL_Range : Source_Location_Range;
      begin

         Parse
           (Validated,
            PP_Options,
            Phase              => Cmd_Line_1,
            Callback           => null,
            Collect_File_Names => False,
            Ignore_Errors      => True);
         GNAT.Strings.Free (Validated);

         --  Pass --source-line-breaks to gnatpp and update other potentially
         --  used switches if this is needed based on the partial gnatpp
         --  command line flag
         Set_Args (PP_Options, Partial_Gnatpp_SLB);

         --  Find the corresponding Start_Node and End_Node given the initial
         --  selection range
         Get_Selected_Region_Enclosing_Node (Main_Unit,
                                             Selection_Range,
                                             Start_Node,
                                             End_Node,
                                             Enclosing_Node,
                                             Input_Sel,
                                             Output_SL_Range);

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

         if Offset /= 0 then
            Set_Partial_Gnatpp_Offset (Offset - 1);
         end if;

         --  Format_Vector will rewrite the input selection and returns the
         --  formatted text corresponding to the Enclosing_Node. The output
         --  contains more than the initial selected text since it is based
         --  on the closest enclosing parent of the initial selection.

         Format_Vector
           (Cmd       => PP_Options,
            Input     => Input_Sel,
            Node      => Enclosing_Node,
            Output    => Output,
            Messages  => Messages,
            Partial_Gnatpp => True);

         --  In the case of preserving source line breaks switch usage, get the
         --  filtered output of the significant lines based on the initial
         --  selection to make text edits only on that part of the code.

         if Partial_Gnatpp_SLB then
            declare
               New_Output          : Char_Vector;
               New_Output_SL_Range : Source_Location_Range;
            begin
               Filter_Initially_Selected_Lines_From_Output
                 (Unit             => Main_Unit,
                  Initial_SL_Range => Selection_Range,
                  Output           => Output,
                  Output_SL_Range  => Output_SL_Range,
                  New_Output       => New_Output,
                  New_SL_Range     => New_Output_SL_Range);

               Output := New_Output;
               Output_SL_Range := New_Output_SL_Range;
            end;
         end if;

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
      end;

      New_Line;
   end Partial_GNATpp_App_Setup;

begin
   Partial_GNATpp_App.Run;
end Partial_GNATpp;
