------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                                    Pp                                    --
--                                                                          --
--                                 S p e c                                  --
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

pragma Ada_2012;

pragma Warnings (Off); -- imported for children
with ASIS_UL.Debug;
with ASIS_UL.Dbg_Out; use ASIS_UL;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;
use Ada;
pragma Warnings (On);

package Pp is
   --  Root of pretty-printing utilities

   pragma Elaborate_Body;

   Debug_Mode : Boolean renames ASIS_UL.Debug.Debug_Flag_9;

   Assert_Enabled : Boolean := False;
   --  Set to True in body if assertions are enabled. This should really be a
   --  constant, but there's no easy mechanism for that.
end Pp;
