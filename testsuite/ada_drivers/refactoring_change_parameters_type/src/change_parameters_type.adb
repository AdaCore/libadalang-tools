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

with Laltools.Common; use Laltools.Common;
with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Subprogram_Signature.Change_Parameters_Type;
use Laltools.Refactor.Subprogram_Signature.Change_Parameters_Type;

--  This procedure defines the Change Parameters Type Tool

--  Usage:
--  change_parameters_type -P <project> --source <source>
--     --start-line <start-line> --start-column <start-column>
--     --end-line <start-line> --end-column <start-column>
--     --new-parameter-type <new-parameter-type>
--
--  -P, --project    Project file
--  --source         Source code file of the parameters
--  --start-line     Start line of the parameters selection
--  --start-column   Start column of the parameters selection
--  --end-line       End line of the parameters selection
--  --end-column     End column of the parameters selection

procedure Change_Parameters_Type is

   procedure Change_Parameters_Type_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Change_Parameters_Type_App is new Libadalang.Helpers.App
     (Name             => "Change_Parameter_Type",
      Description      => "Change Parameter Type",
      App_setup        => Change_Parameters_Type_App_Setup);

   package Args is
      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Change_Parameters_Type_App.Args.Parser,
         Long        => "--source",
         Help        => "Source code file of the parameters",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Start_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Change_Parameters_Type_App.Args.Parser,
         Long        => "--start-line",
         Help        => "Start line of the parameters selection",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package Start_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Change_Parameters_Type_App.Args.Parser,
         Long        => "--start-column",
         Help        => "Start column of the parameters selection",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package End_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Change_Parameters_Type_App.Args.Parser,
         Long        => "--end-line",
         Help        => "End line of the parameters selection",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package End_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Change_Parameters_Type_App.Args.Parser,
         Long        => "--end-column",
         Help        => "End column of the parameters selection",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package New_Parameter_Type is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Change_Parameters_Type_App.Args.Parser,
         Long        => "--new-parameter-type",
         Help        => "New parameters type",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);
   end Args;

   --------------------------------------
   -- Change_Parameters_Type_App_Setup --
   --------------------------------------

   procedure Change_Parameters_Type_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      use Args;

      Source_File                      : constant String :=
        To_String (Args.Source.Get);
      Unit                             : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_File);
      Parameters_Source_Location_Range : constant Source_Location_Range :=
        (Line_Number (Start_Line.Get),
         Line_Number (End_Line.Get),
         Column_Number (Start_Column.Get),
         Column_Number (End_Column.Get));
      New_Parameter_Syntax_Rules       : Grammar_Rule_Vector;
      Units                            : constant Analysis_Unit_Array :=
        Get_Ada_Analysis_Units (Context.Provider, Jobs (1).Analysis_Ctx);

      function Analysis_Units return Analysis_Unit_Array is (Units);

      Edits : Refactoring_Edits;

   begin
      if Is_Change_Parameters_Type_Available
           (Unit                             => Unit,
            Parameters_Source_Location_Range =>
              Parameters_Source_Location_Range,
            New_Parameter_Syntax_Rules       =>
              New_Parameter_Syntax_Rules)
      then
         Edits :=
           Create_Parameters_Type_Changer
             (Unit                             => Unit,
              Parameters_Source_Location_Range =>
                Parameters_Source_Location_Range,
              New_Parameters_Type              => New_Parameter_Type.Get).
             Refactor (Analysis_Units'Access);
         Print (Edits);
      end if;
   end Change_Parameters_Type_App_Setup;

begin
   Change_Parameters_Type_App.Run;
end Change_Parameters_Type;
