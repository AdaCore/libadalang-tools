------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                      F O R M A T T E D _ O U T P U T                     --
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
with Utils.String_Utilities; use Utils.String_Utilities;

package Utils.Formatted_Output is new Utils.Generic_Formatted_Output
  (Char_Type      => Character, Str_Type => String,
   Basic_Put_Char => Text_IO_Put_Char);
--  All data is sent to Text_IO.Current_Output, so Text_IO.Set_Output
--  maybe used to send it to a particular file.
