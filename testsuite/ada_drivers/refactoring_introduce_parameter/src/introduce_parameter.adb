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

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Introduce_Parameter;
use Laltools.Refactor.Introduce_Parameter;

--  This procedure defines the Introduce Parameter Tool

--  Usage:
--  introduce_parameter -P <project> --source <source>
--                      --start-line <start-line>
--                      --start-column <start-column>
--                      --end-line <end-line>
--                      --end-column <end-column>
--
--  -P, --project    Project file
--  --source         Source code file of the parameter to introduce
--  --start-line     Start line of the parameter to introduce
--  --start-column   Start column of the parameter to introduce
--  --end-line       End line of the parameter to introduce
--  --end-column     End column of the parameter to introduce

procedure Introduce_Parameter is

   procedure Introduce_Parameter_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Introduce_Parameter_App is new Libadalang.Helpers.App
     (Name             => "Introduce_Parameter",
      Description      => "Introduce Parameter",
      App_setup        => Introduce_Parameter_App_Setup);

   package Args is
      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Introduce_Parameter_App.Args.Parser,
         Long        => "--source",
         Help        => "Source code file of the parameter to introduce",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Start_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Introduce_Parameter_App.Args.Parser,
         Long        => "--start-line",
         Help        => "Start line of the parameter to introduce",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package Start_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Introduce_Parameter_App.Args.Parser,
         Long        => "--start-column",
         Help        => "Start column of the parameter to introduce",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package End_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Introduce_Parameter_App.Args.Parser,
         Long        => "--end-line",
         Help        => "End line of the parameter to introduce",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package End_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Introduce_Parameter_App.Args.Parser,
         Long        => "--end-column",
         Help        => "End column of the parameter to introduce",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

   end Args;

   ----------------------------------
   -- Introduce_Parameter_App_Setup --
   ----------------------------------

   procedure Introduce_Parameter_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      Source_File : constant String := To_String (Args.Source.Get);
      Unit        : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_File);
      SLOC_Range  : constant Source_Location_Range :=
        (Line_Number (Args.Start_Line.Get),
         Line_Number (Args.End_Line.Get),
         Column_Number (Args.Start_Column.Get),
         Column_Number (Args.End_Column.Get) - 1);

      Edits : Refactoring_Edits;

      function Analysis_Units return Analysis_Unit_Array is ([Unit]);

   begin
      if Is_Introduce_Parameter_Available (Unit, SLOC_Range) then
         Edits := Create_Parameter_Introducer
                    (Unit, SLOC_Range).Refactor (Analysis_Units'Access);
         Print (Edits);
      end if;
   end Introduce_Parameter_App_Setup;

begin
   Introduce_Parameter_App.Run;
end Introduce_Parameter;
