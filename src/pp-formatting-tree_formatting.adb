------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                       Pp.Formatting.Tree_Formatting                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2001-2016, AdaCore                      --
--                                                                          --
-- GNATPP  is free software; you can redistribute it and/or modify it under --
-- terms  of  the  GNU  General  Public  License  as  published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);
with Unchecked_Deallocation;

with Libadalang.Lexer;
with LAL_Extensions; use LAL_Extensions;
with Pp.Command_Lines; use Pp.Command_Lines;

package body Pp.Formatting.Tree_Formatting is
   use Utils.Command_Lines;

   pragma Style_Checks ("M85");

--   use Common_Flag_Switches, Common_String_Switches,
--     Common_String_Seq_Switches, Common_Nat_Switches;

   use Pp_Flag_Switches,
     Pp_Boolean_Switches,
     Attribute_Casing_Switches,
     Keyword_Casing_Switches,
     Name_Casing_Switches,
     Enum_Casing_Switches,
     Type_Casing_Switches,
     Number_Casing_Switches,
     Pragma_Casing_Switches,
     Pp_String_Switches,
     Pp_Nat_Switches,
     Pp_String_Seq_Switches;

end Pp.Formatting.Tree_Formatting;
