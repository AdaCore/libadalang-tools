------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
--  This package contains refactoring tools that allow sorting a compilation
--  unit prelude.

package Laltools.Refactor.Sort_Dependencies is

   function Is_Sort_Dependencies_Available
     (Unit             : Analysis_Unit;
      Sloc             : Source_Location)
      return Boolean;
   --  Returns True if Sloc is inside a compilation unit prelude

   type Dependencies_Sorter is new Refactoring_Tool with private;

   function Create_Dependencies_Sorter
     (Compilation_Unit : Libadalang.Analysis.Compilation_Unit)
     return Dependencies_Sorter;
   --  Dependencies_Sorter constructor

   overriding
   function Refactor
     (Self           : Dependencies_Sorter;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Sorts the Compilation_Unit's preludes passed to Dependencies_Sorter's
   --  constructor Create_Dependencies_Sorter.

private

   type Dependencies_Sorter is new Refactoring_Tool with
      record
         Compilation_Unit : Libadalang.Analysis.Compilation_Unit;
      end record;

end Laltools.Refactor.Sort_Dependencies;
