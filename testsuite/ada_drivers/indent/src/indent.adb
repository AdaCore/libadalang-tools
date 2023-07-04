------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

with Laltools.Partial_GNATPP; use Laltools.Partial_GNATPP;

--  This procedure estimates what the indentation of a line in a source file
--  should be.

--  Usage:
--  indent -S <source-file> -L <line>
--
--  -S, --source-file     Source code file of the line to indent
--  -L, --lines           Lines to indent

procedure Indent is
   procedure Indent_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array);

   package Indent_App is new Libadalang.Helpers.App
     (Name             => "Indent",
      Description      => "Indent",
      App_setup        => Indent_App_Setup);

   package Args is
      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Indent_App.Args.Parser,
         Short       => "-S",
         Long        => "--source-file",
         Help        => "ource code file of the line to indent",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String);

      package Lines is new GNATCOLL.Opt_Parse.Parse_Option_List
        (Parser      => Indent_App.Args.Parser,
         Short       => "-L",
         Long        => "--lines",
         Help        => "Lines to indent",
         Arg_Type    => Natural,
         Convert     => Natural'Value);
   end Args;

   --------------------------------
   --  Indent_App_Setup  --
   --------------------------------

   procedure Indent_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      Unit : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (To_String (Args.Source.Get));

   begin
      for Line of Args.Lines.Get loop
         Put_Line (Estimate_Indentation (Unit, Line_Number (Line))'Image);
      end loop;
   end Indent_App_Setup;

begin
   Indent_App.Run;
end Indent;
