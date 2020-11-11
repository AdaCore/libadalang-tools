------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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
--
--  This package contains pretty print utilities of datastructures used by
--  procedure Rename.

with Ada.Containers.Ordered_Maps;

with Laltools.Refactor.Rename; use Laltools.Refactor.Rename;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;

package Printers is

   function "<" (Left, Right : Analysis_Unit) return Boolean is
     (Left.Get_Filename < Right.Get_Filename);

   package Unit_Slocs_Ordered_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type        => Analysis_Unit,
      Element_Type    => Slocs_Maps.Map,
      "<"             => "<",
      "="             => Slocs_Maps."=");

   procedure PP (Sloc : Source_Location_Range);

   procedure PP (Set : Slocs_Sets.Set);

   procedure PP (Map : Slocs_Maps.Map);

   procedure PP (Map : Unit_Slocs_Ordered_Maps.Map);

   procedure PP (References : Renamable_References);

end Printers;
