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
with GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Replace_Type;
use Laltools.Refactor.Replace_Type;

--  This procedure defines the Replace Type rafactoring tool

--  Usage:
--  replace_type -P <project> -S <source> -SL <line> -SC <column>
--
--  -P, --project           Project file
--  -S, --source            Source code file of the source type (type to be
--                          replaced)
--  -SL, --source-line      Line of the source type
--  -SC, --source-column    Column of the source type

procedure Replace_Type is

   procedure Replace_Type_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Replace_Type_App is new Libadalang.Helpers.App
     (Name             => "Replace_Type",
      Description      => "Replace Type",
      App_setup        => Replace_Type_App_Setup);

   package Args is
      package Source_Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Replace_Type_App.Args.Parser,
         Short       => "-S",
         Long        => "--source",
         Help        =>
            "Source code file of the source type (type to be replaced)",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Source_Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Replace_Type_App.Args.Parser,
         Short       => "-SL",
         Long        => "--source-line",
         Help        => "Line of the source type",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

      package Source_Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Replace_Type_App.Args.Parser,
         Short       => "-SC",
         Long        => "--source-column",
         Help        => "Column of the source type",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0,
         Enabled     => True);

   end Args;

   ----------------------------------
   -- Replace_Type_App_Setup --
   ----------------------------------

   procedure Replace_Type_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      Source_Source_File      : constant String :=
        To_String (Args.Source_Source.Get);
      Source_Unit             : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_Source_File);
      Source_Type_SLOC        : constant Source_Location :=
        ((Line_Number (Args.Source_Line.Get),
         Column_Number (Args.Source_Column.Get)));

      Files : constant GNATCOLL.VFS.File_Array_Access :=
        Context.Provider.Project.Root_Project.Source_Files (Recursive => True);

      Max_Number_Of_Units  : constant Natural := Files'Length;
      True_Number_Of_Units : Natural := 0;
      Units                : Analysis_Unit_Array (1 .. Max_Number_Of_Units);

      function Analysis_Units return Analysis_Unit_Array is
        (Units (1 .. True_Number_Of_Units));

   begin
      for File of Files.all loop
         True_Number_Of_Units := @ + 1;
         declare
            Filename : constant GNATCOLL.VFS.Filesystem_String :=
              File.Full_Name;

         begin
            Units (True_Number_Of_Units) :=
              Jobs (1).Analysis_Ctx.Get_From_File (String (Filename));
         end;
      end loop;

      if Is_Replace_Type_Available
           (Source_Unit, Source_Type_SLOC)
      then
         Print
           (Create_Type_Replacer
              (Source_Unit, Source_Type_SLOC, To_Unbounded_String ("Foo")).
                Refactor (Analysis_Units'Access));
      end if;

   end Replace_Type_App_Setup;

begin
   Replace_Type_App.Run;
end Replace_Type;
