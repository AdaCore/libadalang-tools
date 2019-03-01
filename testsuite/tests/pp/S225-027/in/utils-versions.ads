------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                       A S I S _ U L . O U T P U T                        --
--                                                                          --
--                                 S p e c                                  --
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

package Utils.Versions is

   type Gnat_Build_Type is (Gnatpro, GPL);

   Build_Type : constant Gnat_Build_Type := Gnatpro;
   --  Kind of GNAT Build:
   --
   --    Gnatpro
   --       GNAT Professional version. This version of GNAT is supported by Ada
   --       Core Technologies.
   --
   --    GPL
   --       GNAT Community Edition. This is a special version of GNAT, released
   --       by Ada Core Technologies and intended for academic users, and free
   --       software developers.

   procedure Print_Tool_Version;
   --  Similar to Print_Version_Info, but sends the output into Stdout and
   --  the format of the information printed is similar to what is printed
   --  for '--version' option by all the other GNAT tools.
   --
   --  This is used when --version is specified.

   procedure Print_Version_Info;
--  Prints the tool version information in the following format:
--
--  <toolname>
--  Copyright <current year>, AdaCore.
--
--  This is used when -v is specified.

end Utils.Versions;
