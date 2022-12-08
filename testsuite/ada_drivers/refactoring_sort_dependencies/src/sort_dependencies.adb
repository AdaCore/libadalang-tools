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

with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Sort_Dependencies;
use Laltools.Refactor.Sort_Dependencies;

with Libadalang.Common; use Libadalang.Common;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

--  This procedure defines the Sort Dependencies Tool

--  Usage:
--  sort_dependencies --project <project> --source <source>
--
--  --project, -P   Project file
--  --source, -S    Source code file to sort dependencies

procedure Sort_Dependencies is

   procedure Sort_Dependencies_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Sort_Dependencies_App is new Libadalang.Helpers.App
     (Name             => "Sort_Dependencies",
      Description      => "Sort Dependencies",
      App_setup        => Sort_Dependencies_App_Setup);

   package Args is
      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Sort_Dependencies_App.Args.Parser,
         Short       => "-S",
         Long        => "--source",
         Help        => "Source code file to sort dependencies",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

   end Args;

   ---------------------------------
   -- Sort_Dependencies_App_Setup --
   ---------------------------------

   procedure Sort_Dependencies_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      Source_File : constant String := To_String (Args.Source.Get);
      Unit        : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_File);

   begin
      if Unit.Root.Kind in Ada_Compilation_Unit_List then
         for Compilation_Unit of Unit.Root.As_Compilation_Unit_List loop
            Print
              (Create_Dependencies_Sorter
                 (Compilation_Unit.As_Compilation_Unit).Refactor (null));
         end loop;
      elsif Unit.Root.Kind in Ada_Compilation_Unit then
         Print
           (Create_Dependencies_Sorter
              (Unit.Root.As_Compilation_Unit).Refactor (null));
      end if;
   end Sort_Dependencies_App_Setup;

begin
   Sort_Dependencies_App.Run;
end Sort_Dependencies;
