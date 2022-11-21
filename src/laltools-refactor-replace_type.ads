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
--  TODO

with Langkit_Support.Slocs;

package Laltools.Refactor.Replace_Type is

   function Is_Replace_Type_Available
     (Source_Unit     : Analysis_Unit;
      Source_Location : Langkit_Support.Slocs.Source_Location)
      return Boolean;
   --  TODO

   type Type_Replacer is new Refactoring_Tool with private;

   function Create_Type_Replacer
     (Source_Unit      : Analysis_Unit;
      Source_Type_SLOC : Langkit_Support.Slocs.Source_Location;
      New_Type         : Unbounded_String)
      return Type_Replacer;
   --  TODO

   overriding
   function Refactor
     (Self           : Type_Replacer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  TODO

private

   type Type_Replacer is new Refactoring_Tool with
      record
         Source_Type : Base_Type_Decl;
         New_Type    : Unbounded_String;
      end record;

end Laltools.Refactor.Replace_Type;
