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
with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

with Pp.Actions;
with Pp.Command_Lines;
with Pp.Scanner;
with Utils.Char_Vectors;
with Utils.Command_Lines;

with GNAT.Strings;

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
         Long        => "--source",
         Help        => "Source code file of the node",
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

   end Args;

   procedure Partial_GNATpp_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);
      Source_File : constant String          := To_String (Args.Source.Get);
      SLOC        : constant Source_Location :=
        (Line_Number (Args.Start_Line.Get),
         Column_Number (Args.Start_Column.Get));

      Main_Unit : Analysis_Unit;
      Node      : Ada_Node;

      function Process_Node (Node : Ada_Node'Class) return Ada_Node;

      function Process_Node (Node : Ada_Node'Class) return Ada_Node
      is
      begin
         return Node.As_Ada_Node;
      end Process_Node;

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File (Source_File);

      Node := Process_Node (Main_Unit.Root.Lookup (SLOC));

      Put_Line ("# GNATpp node " & Node.Image);

      declare
         use Pp.Actions;
         use Utils.Char_Vectors;
         use Utils.Command_Lines;

         PP_Options : Command_Line (Pp.Command_Lines.Descriptor'Access);
         Input      : Char_Vector;
         In_Range   : constant Char_Subrange := (1, 0);
         Output     : Char_Vector;
         Out_Range  : Char_Subrange;
         Messages   : Pp.Scanner.Source_Message_Vector;

         Validated  : GNAT.Strings.String_List_Access :=
           new GNAT.Strings.String_List (1 .. 0);

         S : GNAT.Strings.String_Access;

      begin
         Parse
           (Validated,
            PP_Options,
            Phase              => Cmd_Line_1,
            Callback           => null,
            Collect_File_Names => False,
            Ignore_Errors      => True);
         GNAT.Strings.Free (Validated);

         S := new String'(To_UTF8 (Node.Text));
         Input.Append (S.all);
         GNAT.Strings.Free (S);
         Format_Vector
           (PP_Options,
            Input,
            Node,
            In_Range,
            Output,
            Out_Range,
            Messages);

         declare
            Output_String : constant String :=
              Char_Vectors.Elems (Output)
                (1 .. Char_Vectors.Last_Index (Output));
         begin
            Ada.Text_IO.Put_Line (Output_String);
         end;
      end;

      New_Line;
   end Partial_GNATpp_App_Setup;

begin
   Partial_GNATpp_App.Run;
end Partial_GNATpp;
