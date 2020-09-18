------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                     G N A T T E S T . A C T I O N S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
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

   type Test_Tool is new Tool_State with null record;

end Test.Actions;
