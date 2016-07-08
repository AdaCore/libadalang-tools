------------------------------------------------------------------------------
--                                                                          --
--                  COMMON ASIS TOOLS COMPONENTS LIBRARY                    --
--                                                                          --
--                       A S I S _ U L . O U T P U T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2016, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Gnatvsn;

with LAL_UL.Tool_Names;
with LAL_UL.Formatted_Output; use LAL_UL.Formatted_Output;

package body LAL_UL.Versions is

   ------------------------
   -- Print_Tool_Version --
   ------------------------

   procedure Print_Tool_Version is
   begin
      Put ("\1 \2\n",
           To_Upper (Tool_Names.Tool_Name), Gnatvsn.Gnat_Version_String);
      Put ("Copyright (C) \1, \2\n",
           Gnatvsn.Current_Year, Gnatvsn.Copyright_Holder);
      Put ("\1", Gnatvsn.Gnat_Free_Software);
      Put ("\n");
   end Print_Tool_Version;

   ------------------------
   -- Print_Version_Info --
   ------------------------

   procedure Print_Version_Info is
   begin
      Put ("\1 \2\n", Tool_Names.Tool_Name, Gnatvsn.Gnat_Version_String);
      Put ("Copyright (C) \1, \2.\n",
           Gnatvsn.Current_Year, "AdaCore");
   end Print_Version_Info;

end LAL_UL.Versions;
