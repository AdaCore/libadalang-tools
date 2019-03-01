------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                               D B G _ O U T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                  Copyright (C) 2012-2017, AdaCore, Inc.                  --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

with Utils.Generic_Formatted_Output;
with Utils.String_Utilities;

package Utils.Dbg_Out is new Utils.Generic_Formatted_Output
  (Char_Type                => Character, Str_Type => String,
   Basic_Put_Char           => Utils.String_Utilities.Std_Err_Put_Char,
   Output_Enabled_Initially => False);
--  All data is sent to Text_IO.Standard_Error. In gdb, set:
--     Utils.Dbg_Out.Output_Enabled := True
--  to enable debugging output, which is turned off by default.
