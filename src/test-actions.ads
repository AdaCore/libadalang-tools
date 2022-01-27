------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2019-2022, AdaCore                    --
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
with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Tools;         use Utils.Tools;

package Test.Actions is

   type Test_Tool is new Tool_State with private;

   procedure Register_Specific_Attributes;
   --  Registers gnattest specific project attributes so that they can be
   --  queried later.

private

   overriding procedure Init
     (Tool : in out Test_Tool; Cmd : in out Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Test_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Test_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Test_Tool);
   overriding procedure Per_Invalid_File_Action
     (Tool      : in out Test_Tool;
      Cmd       :        Command_Line;
      File_Name :        String);

   type Test_Tool is new Tool_State with null record;

end Test.Actions;
