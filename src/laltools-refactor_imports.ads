------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2020-2022, AdaCore                      --
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
--  This package contains the Refactor Imports Tool utilities

with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

with Langkit_Support.Text; use Langkit_Support.Text;

package Laltools.Refactor_Imports is

   type Import_Suggestion is record
      Declaration      : Basic_Decl := No_Basic_Decl;
      With_Clause_Text : Unbounded_Text_Type :=
        Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
      Prefix_Text      : Unbounded_Text_Type :=
        Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
   end record;

   function "<" (Left, Right : Import_Suggestion) return Boolean;

   package Import_Suggestions_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Import_Suggestion);

   package Import_Suggestions_Vector_Sorting is new
     Import_Suggestions_Vector.Generic_Sorting;

   function Get_Import_Suggestions
     (Node  : Ada_Node'Class;
      Units : Unit_Vectors.Vector)
      return Import_Suggestions_Vector.Vector;
   --  For each declaration of Reachable_Declarations, determines a vector of
   --  valid with clauses and corresponding prefixes so that Identifier becomes
   --  visible.

end Laltools.Refactor_Imports;
