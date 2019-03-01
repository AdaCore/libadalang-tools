------------------------------------------------------------------------------
--                                                                          --
--                  COMMON ASIS TOOLS COMPONENTS LIBRARY                    --
--                                                                          --
--                       A S I S _ U L . O U T P U T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2017, AdaCore                      --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Utils.Formatted_Output;  use Utils.Formatted_Output;
with Utils.Tool_Names;
with Libadalang;

package body Utils.Versions is

   --  Much of the following is copied from gnatvsn.ads in the GNAT sources.

   Copyright_Holder : constant String := "AdaCore.";

   function Gnat_Free_Software return String;
   --  Text to be displayed by the different GNAT tools when switch --version
   --  is used. This text depends on the GNAT build type.

   ------------------------
   -- Gnat_Free_Software --
   ------------------------

   function Gnat_Free_Software return String is
   begin
      case Build_Type is
         when GPL =>
            return "This is free software; see the source for copying conditions." &
              ASCII.LF &
              "There is NO warranty; not even for MERCHANTABILITY or FITNESS" &
              " FOR A PARTICULAR PURPOSE.";

         when Gnatpro =>
            return "This is free software; see the source for copying conditions." &
              ASCII.LF &
              "See your AdaCore support agreement for details of warranty" &
              " and support." & ASCII.LF &
              "If you do not have a current support agreement, then there" &
              " is absolutely" & ASCII.LF &
              "no warranty; not even for MERCHANTABILITY or FITNESS FOR" &
              " A PARTICULAR" & ASCII.LF & "PURPOSE.";
      end case;
   end Gnat_Free_Software;

   ------------------------
   -- Print_Tool_Version --
   ------------------------

   Initial_Year : constant String := "2004";
   --  This is the first year in which any of the sources used by these tools
   --  was written.

   function Edition return String is
     (case Build_Type is when Gnatpro => "Pro", when GPL => "Community");

   procedure Print_Tool_Version is
   begin
      Put
        ("\1 \2 \3\n", To_Upper (Tool_Names.Tool_Name), Edition,
         Libadalang.Version);
      Put
        ("Copyright (C) \1-\2, \3\n", Initial_Year, Libadalang.Current_Year,
         Copyright_Holder);
      Put ("\1", Gnat_Free_Software);
      Put ("\n");
   end Print_Tool_Version;

   ------------------------
   -- Print_Version_Info --
   ------------------------

   procedure Print_Version_Info is
   begin
      Put ("\1 \2 \3\n", Tool_Names.Tool_Name, Edition, Libadalang.Version);
      Put
        ("Copyright (C) \1-\2, \3\n", Initial_Year, Libadalang.Current_Year,
         Copyright_Holder);
   end Print_Version_Info;

end Utils.Versions;
