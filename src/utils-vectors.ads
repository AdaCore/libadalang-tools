------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                               V E C T O R S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                  Copyright (C) 2013-2017, AdaCore, Inc.                  --
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

pragma Warnings (Off); -- One of these will be unreferenced
with Utils.Fast_Vectors;
with Utils.Slow_Vectors;
pragma Warnings (On);

generic package Utils.Vectors renames Utils.Fast_Vectors;
--  Utils.Slow_Vectors;

--  This renaming is so that we can easily switch back to Slow_Vectors for
--  debugging. Unfortunately, for that to work you have to uncomment all
--  the "use all type" clauses on vector types; they're commented out due
--  to warnings.
