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
--
--  Rejuvenate tools command line utilities

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Tools;
with Ada.Strings.Unbounded;

package Command_Line is

   Parser : Argument_Parser :=
     Create_Argument_Parser (Help => "Rejuvenate tools");

   package Tool is new Parse_Positional_Arg
     (Parser   => Parser,
      Name     => "tool",
      Help     => Tools.Tool_List,
      Arg_Type => Tools.Tool,
      Convert  => Tools.Convert);

   package Verbose is new Parse_Flag
     (Parser   => Parser,
      Short    => "-v",
      Long     => "--verbose",
      Help     => "Verbose output");

   package Project is new Parse_Option
     (Parser      => Parser,
      Short       => "-P",
      Long        => "--project",
      Help        => "Project",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   package Source is new Parse_Option
     (Parser      => Parser,
      Short       => "-S",
      Long        => "--source",
      Help        => "Source",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   package Pipe is new Parse_Flag
     (Parser      => Parser,
      Long        => "--pipe",
      Help        => "Print result to stdout");

   package Interactive is new Parse_Flag
     (Parser      => Parser,
      Short       => "-i",
      Long        => "--interactive",
      Help        => "Interactive mode");

   package Output_Dir is new Parse_Option
     (Parser      => Parser,
      Short       => "-o",
      Long        => "--output",
      Help        => "Save the output to given file",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

end Command_Line;
