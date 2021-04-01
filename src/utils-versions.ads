------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2004-2021, AdaCore                    --
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

package Utils.Versions is

   Version      : constant String := "dev";
   Current_Year : constant String := "unknown";

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

   procedure Print_Version_Info;
   --  Prints the tool version information to standard error in the following
   --  format:
   --
   --  <toolname>
   --  Copyright <current year>, AdaCore.
   --
   --  This is used when -v is specified. It is meant to mimic old versions of
   --  the tools.

   procedure Print_Tool_Version;
   --  Similar to Print_Version_Info, except the output is sent into Stdout,
   --  and the format of the information printed is similar to what is printed
   --  for '--version' option by all the other GNAT tools.
   --
   --  This is used when --version is specified. It is meant to mimic other
   --  GNAT tools.

end Utils.Versions;
