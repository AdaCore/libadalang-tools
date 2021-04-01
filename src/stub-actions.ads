------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Libadalang.Analysis; use Libadalang.Analysis;
with Utils.Char_Vectors; use Utils.Char_Vectors;
with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Tools; use Utils.Tools;
with Pp.Scanner; use Pp;

package Stub.Actions is

   type Stub_Tool is new Tool_State with private;

private

   overriding procedure Init
     (Tool : in out Stub_Tool; Cmd : in out Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Stub_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Stub_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Stub_Tool);

   type Stub_Tool is new Tool_State with record
      Ignored_Out_Range : Char_Subrange;
      Ignored_Messages : Scanner.Source_Message_Vector;
   end record;

   --  For Debugging:

   procedure Dump
     (Tool : in out Stub_Tool;
      Message : String := "");

end Stub.Actions;
