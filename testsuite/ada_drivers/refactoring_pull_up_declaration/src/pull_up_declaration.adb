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
with Laltools.Refactor.Pull_Up_Declaration;
use Laltools.Refactor.Pull_Up_Declaration;

--  This procedure defines the Pull Up Declaration Tool

--  Usage:
--  pull_up_declaration -P <project> -S <source> -L <line> -C <column>
--
--  -P, --project   Project file
--  --source    Source code file of the declaration to pull up
--  --line      Line of the declaration to pull up
--  --column    Column of the declaration to pull up

procedure Pull_Up_Declaration is

   procedure Pull_Up_Declaration_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Pull_Up_Declaration_App is new Libadalang.Helpers.App
     (Name             => "Pull_Up_Declaration",
      Description      => "Pull Up Declaration",
      App_setup        => Pull_Up_Declaration_App_Setup);

   package Args is
      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Pull_Up_Declaration_App.Args.Parser,
         Long        => "--source",
         Help        => "Source code file of the declaration to pull up",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Pull_Up_Declaration_App.Args.Parser,
         Long        => "--line",
         Help        => "Line of the declaration to pull up",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Pull_Up_Declaration_App.Args.Parser,
         Long        => "--column",
         Help        => "Column of the declaration to pull up",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

   end Args;

   ----------------------------------
   -- Pull_Up_Declaration_App_Setup --
   ----------------------------------

   procedure Pull_Up_Declaration_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      Source_File      : constant String := To_String (Args.Source.Get);
      Unit             : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_File);
      Declaration_SLOC : constant Source_Location :=
        ((Line_Number (Args.Line.Get),
          Column_Number (Args.Column.Get)));

      Edits : Refactoring_Edits;

      function Analysis_Units return Analysis_Unit_Array is
        ([Unit]);

   begin
      if Is_Pull_Up_Declaration_Available (Unit, Declaration_SLOC) then
         Edits := Create_Declaration_Pull_Upper
                    (Unit, Declaration_SLOC).Refactor (Analysis_Units'Access);
         Print (Edits);
      end if;

   end Pull_Up_Declaration_App_Setup;

begin
   Pull_Up_Declaration_App.Run;
end Pull_Up_Declaration;
