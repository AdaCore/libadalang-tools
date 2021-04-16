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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Opt_Parse;
with GNATCOLL.Traces;

with Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Helpers;

with Laltools.Common;
with Laltools.Call_Hierarchy;

--  This procedure defines the Outgoing Calls Tool. Given the location of
--  an identifier in a source code file and the project it belongs to,
--  if the identifier is a subprogram call, prints all subprogram calls called
--  by it.

--  Usage:
--  outgoing_calls -P <project_file> -S <source_code_file> -L <line_number>
--  -R <column_number>
--
--  -P, --project          Project file to use
--  -S, --source           Source code file of the identifier
--  -L, --line             Line number of the identifier
--  -R, --column           Column number of the identifier

procedure Outgoing_Calls is

   package LALAnalysis renames Libadalang.Analysis;
   package LALHelpers renames Libadalang.Helpers;
   package LKSSlocs renames Langkit_Support.Slocs;

   Main_Unit : LALAnalysis.Analysis_Unit;
   Node : LALAnalysis.Ada_Node;

   procedure App_Setup
     (Context : LALHelpers.App_Context;
      Jobs : LALHelpers.App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   procedure Job_Post_Process (Context : LALHelpers.App_Job_Context);
   --  This procedure will be called once after all units have been parsed.
   --  Note it will be called once per job.

   package App is new LALHelpers.App
     (Name             => "outgoing_oalls",
      Description      => "Print outgoing calls of a subprogram, i.e., all " &
        "subprograms called by it.",
      App_setup        => App_Setup,
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
      pragma Unreferenced (Context);
      Source_File : constant Ada.Strings.Unbounded.Unbounded_String
        := Source.Get;
      Line_Number : constant Natural
        := Line.Get;
      Column_Number : constant Natural
        := Column.Get;
      Sloc : constant LKSSlocs.Source_Location
        := (Line   => LKSSlocs.Line_Number (Line_Number),
            Column => LKSSlocs.Column_Number (Column_Number));
   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File
        (Ada.Strings.Unbounded.To_String (Source_File));
      Node := Main_Unit.Root.Lookup (Sloc);
   end App_Setup;

   ----------------------
   -- Job_Post_Process --
   ----------------------

   procedure Job_Post_Process (Context : LALHelpers.App_Job_Context) is
      pragma Unreferenced (Context);

      Calls : Laltools.Common.References_By_Subprogram.Map;
      Dummy_Trace        : GNATCOLL.Traces.Trace_Handle;
      Dummy_Imprecise    : Boolean := False;

      use type LALAnalysis.Defining_Name;

      procedure Callback (Subp_Call : LALAnalysis.Ada_Node'Class);
      --  Adds Subp_Call to Calls.

      procedure Print_References_By_Subprogram_Map
        (Map : Laltools.Common.References_By_Subprogram.Map);
      --  Prints a References_By_Subprogram.Map in a human-readable format.

      procedure Callback (Subp_Call : LALAnalysis.Ada_Node'Class) is
         Call_Definition : LALAnalysis.Defining_Name;
         Subp_Call_Name  : constant LALAnalysis.Name :=
           Laltools.Common.Get_Node_As_Name (Subp_Call.As_Ada_Node);
      begin
         --  First try to resolve the called function.

         Call_Definition := Laltools.Common.Resolve_Name
           (Subp_Call_Name, Dummy_Trace, Dummy_Imprecise);

         if Call_Definition /= LALAnalysis.No_Defining_Name then
            if Calls.Contains (Call_Definition) then
               declare
                  R : constant
                    Laltools.Common.References_By_Subprogram.Reference_Type
                    := Calls.Reference (Call_Definition);
               begin
                  R.Include (Subp_Call.As_Base_Id);
               end;
            else
               declare
                  L : Laltools.Common.References_Sets.Set;
               begin
                  L.Include (Subp_Call.As_Base_Id);
                  Calls.Insert (Call_Definition, L);
               end;
            end if;
         end if;
      end Callback;

      procedure Print_References_By_Subprogram_Map
        (Map : Laltools.Common.References_By_Subprogram.Map) is
         use Laltools.Common;
         Cursor : References_By_Subprogram.Cursor := Map.First;

      begin
         while References_By_Subprogram.Has_Element (Cursor)
         loop
            for E of References_By_Subprogram.Element (Cursor) loop
               Ada.Text_IO.Put_Line (References_By_Subprogram.Key (Cursor)
                                     .P_Parent_Basic_Decl.Image
                                       & " "
                                     & E.Image);
            end loop;
            References_By_Subprogram.Next (Cursor);
         end loop;
      end Print_References_By_Subprogram_Map;

      Node_Name          : constant LALAnalysis.Name :=
        Laltools.Common.Get_Node_As_Name (Node);
      Node_Defining_Name : LALAnalysis.Defining_Name :=
        LALAnalysis.No_Defining_Name;

      use type LALAnalysis.Name;
   begin
      if Node_Name = LALAnalysis.No_Name then
         Ada.Text_IO.Put_Line ("Node is not a name.");
         return;
      end if;

      Node_Defining_Name :=
        Laltools.Common.Resolve_Name (Node_Name, Dummy_Trace, Dummy_Imprecise);

      if Node_Defining_Name = LALAnalysis.No_Defining_Name then
         Ada.Text_IO.Put_Line ("Node is not a defining name.");
         return;
      elsif not Node_Defining_Name.P_Basic_Decl.P_Is_Subprogram then
         Ada.Text_IO.Put_Line ("Node is not a subprogram.");
         return;
      end if;

      Laltools.Call_Hierarchy.Find_Outgoing_Calls
        (Definition => Node_Defining_Name,
         Callback   => Callback'Access,
         Trace      => Dummy_Trace,
         Imprecise  => Dummy_Imprecise);

      Print_References_By_Subprogram_Map (Calls);
   end Job_Post_Process;
begin
   App.Run;
end Outgoing_Calls;
