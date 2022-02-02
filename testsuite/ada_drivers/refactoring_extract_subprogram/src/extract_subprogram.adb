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
with Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Extract_Subprogram;
use Laltools.Refactor.Extract_Subprogram;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Helpers; use Libadalang.Helpers;

--  This procedure defines the Extract Subprogram Tool.

--  Usage:
--  extract_subprogram -P <project> -S <source> -SL <start-line> -EL <end-line>
--  -SC <start-column> -EC <end-column> -N <name>
--
--  -P,  --project         Project file to use
--  -S,  --source          Source code file of the statements to extract
--  -SL, --start-line      Line of the first statement to extract
--  -EL, --end-line        Line of the last statement to extract
--  -SC, --start-column    Column of the first statement to extract
--  -EC, --end-column      Column of the last statement to extract
--  -N,  --name            Name of the extracted subprogram

procedure Extract_Subprogram is

   procedure Extract_Subprogram_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Extract_Subprogram_App is new Libadalang.Helpers.App
     (Name             => "Extract_Subprogram",
      Description      => "Extract Subprogram",
      App_setup        => Extract_Subprogram_App_Setup);

   package Args is
      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Extract_Subprogram_App.Args.Parser,
         Short       => "-S",
         Long        => "--source",
         Help        => "Source code file of the statements to extract",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Start_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Extract_Subprogram_App.Args.Parser,
         Short       => "-SL",
         Long        => "--start-line",
         Help        => "Line of the first statement to extract",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package End_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Extract_Subprogram_App.Args.Parser,
         Short       => "-EL",
         Long        => "--end-line",
         Help        => "Line of the last statement to extract",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package Start_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Extract_Subprogram_App.Args.Parser,
         Short       => "-SC",
         Long        => "--start-column",
         Help        => "Column of the first statement to extract",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package End_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Extract_Subprogram_App.Args.Parser,
         Short       => "-EC",
         Long        => "--end-column",
         Help        => "Column of the last statement to extract",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package Name is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Extract_Subprogram_App.Args.Parser,
         Short       => "-N",
         Long        => "--name",
         Help        => "Name of the extracted subprogram",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => To_Unbounded_String (""),
         Enabled     => True);
   end Args;

   ---------------------------
   -- Extract_Subprogram_App_Setup --
   ---------------------------

   procedure Extract_Subprogram_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      Source_File        : constant String := To_String (Args.Source.Get);
      Unit               : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_File);
      Section_To_Extract : constant Source_Location_Range :=
        (Line_Number (Args.Start_Line.Get),
         Line_Number (Args.End_Line.Get),
         Column_Number (Args.Start_Column.Get),
         Column_Number (Args.End_Column.Get));

      Available_Subprogram_Kinds : Available_Subprogram_Kinds_Type;

      Edits : Refactoring_Edits;

   begin
      if Is_Extract_Subprogram_Available
        (Unit, Section_To_Extract, Available_Subprogram_Kinds)
      then
         for Subprogram_Kind in Ada_Subp_Kind loop
            if Available_Subprogram_Kinds (Subprogram_Kind) then
               declare
                  Subprogram_Name : constant Unbounded_String :=
                    (if Args.Name.Get = "" then
                       Default_Extracted_Subprogram_Name
                        (Unit,
                         (Section_To_Extract.Start_Line,
                          Section_To_Extract.Start_Column))
                     else Args.Name.Get);
               begin
                  Ada.Text_IO.Put_Line
                    ("Extracting as a " & Subprogram_Kind'Image & ":");

                  Edits := Create_Subprogram_Extractor
                    (Unit,
                     Section_To_Extract,
                     Subprogram_Kind,
                     Subprogram_Name).Refactor (null);

                  Print (Edits);
               end;
            end if;
         end loop;
      end if;

   end Extract_Subprogram_App_Setup;

begin
   Extract_Subprogram_App.Run;
end Extract_Subprogram;
