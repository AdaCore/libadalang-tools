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
--  Rejuvenate tools driver

with Ada.Command_Line; use Ada.Command_Line;
--  with Ada.Exceptions; use Ada.Exceptions;

with GNATCOLL.Strings; use GNATCOLL.Strings;

with Command_Line;

with Tools;
with Tools.Record_Components_Tool;
with Tools.Array_Aggregates_Tool;

procedure Rejuvenate is

   function "+" (Str : String) return XString renames To_XString;
   --  TODO: Move this to an utils package

   First_Tool_Index : constant Natural := Tools.Find_First_Tool_Index;

   Command_Line_Arguments : XString_Array :=
     (if First_Tool_Index = 0 then []
      else [1 .. First_Tool_Index => +""]);

   --  TODO: Hide the Command_Line_Arguments and Tool_Args logic.
   --  TODO: Rename Command_Line_Arguments since this represents the
   --  arguments that common to all tools. Possible new name: Common_Arguments.

begin
   for J in 1 .. First_Tool_Index loop
      Command_Line_Arguments (J) := +Argument (J);
   end loop;

   if Command_Line.Parser.Parse (Command_Line_Arguments) then
      declare
         Tool_Args : XString_Array :=
           (if Argument_Count = First_Tool_Index then [+"--help"]
            else [1 .. Argument_Count - First_Tool_Index => +""]);
            --  GNAT bug
            --  else [for J in First_Tool_Index + 1 .. Argument_Count
            --        => +Argument (J)]);
            --  TODO: Raise a ticket with this bug and ammend this comment
            --  with the ticket number.

      begin
         for J in First_Tool_Index + 1 .. Argument_Count loop
            Tool_Args (J - First_Tool_Index) := +Argument (J);
         end loop;

         case Command_Line.Tool.Get is
            when Tools.Record_Components =>
               if Tools.Record_Components_Tool.Parser.Parse (Tool_Args) then
                  Tools.Record_Components_Tool.Run;
               end if;
            when Tools.Array_Aggregates =>
               if Tools.Array_Aggregates_Tool.Parser.Parse (Tool_Args) then
                  Tools.Array_Aggregates_Tool.Run;
               end if;
         end case;
      end;
   end if;
--  GNAT bug
--  exception
--     when Parse_Tool_Exception =>
--        Ada.Text_IO.Put_Line (Args.Parser.Help);
--     when others =>
--        Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
--  TODO: Raise a ticket with this bug and ammend this comment with the
--  ticket number.
end Rejuvenate;
