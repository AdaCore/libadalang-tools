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
--  This package contains LAL_Tools contructs to represent refactoring
--  problems.

with Ada.Containers.Indefinite_Vectors;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package Laltools.Refactor.Problems is

   type Refactor_Problem is interface;

   function Filename (Self : Refactor_Problem) return String is abstract;
   --  Returns the filename of the analysis unit where Self happens.

   function Location (Self : Refactor_Problem)
                      return Source_Location_Range is abstract;
   --  Return a location in the file where Self happens.

   function Info (Self : Refactor_Problem) return String is abstract;
   --  Returns a human readable message with the description of Self.

   package Refactor_Problem_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Natural,
      Element_Type => Refactor_Problem'Class);

end Laltools.Refactor.Problems;
