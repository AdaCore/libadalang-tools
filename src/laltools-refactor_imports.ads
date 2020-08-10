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
--  This package contains the Refactor Imports Tool utilities

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Libadalang.Analysis;
with Libadalang.Helpers;

with Langkit_Support.Text;

with Ada.Strings.Wide_Wide_Hash;

package Laltools.Refactor_Imports is

   package LALAnalysis renames Libadalang.Analysis;
   package LALHelpers renames Libadalang.Helpers;
   package LKSText renames Langkit_Support.Text;

   function Basic_Decl_Hash (Decl : LALAnalysis.Basic_Decl)
                             return Ada.Containers.Hash_Type;
   --  Casts Decl as Ada_Node and uses Hash from Libadalang.Analysis.
   --  This is convenient for containers with Basic_Decl elements.

   package Reachable_Declarations_Hashed_Set is new Ada.Containers.Hashed_Sets
     (Element_Type => LALAnalysis.Basic_Decl,
      Hash         => Basic_Decl_Hash,
      Equivalent_Elements => LALAnalysis."=",
      "=" => LALAnalysis."=");

   function Text_Type_Equivalent
     (Left, Right : LKSText.Text_Type) return Boolean is
     (Left = Right);
   --  True if two Text_Type elements are the same.

   package Reachable_Declarations_Result is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => LKSText.Text_Type,
        Element_Type    => Reachable_Declarations_Hashed_Set.Set,
        Hash            => Ada.Strings.Wide_Wide_Hash,
        Equivalent_Keys => Text_Type_Equivalent,
        "="             => Reachable_Declarations_Hashed_Set."=");

   type Import_Suggestion is record
      Declaration : LALAnalysis.Basic_Decl := LALAnalysis.No_Basic_Decl;
      Import_Text : LKSText.Unbounded_Text_Type;
      Prefix_Text : LKSText.Unbounded_Text_Type;
   end record;

   package Import_Suggestions_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Import_Suggestion);

   function Get_Reachable_Declarations
     (Identifier : LALAnalysis.Identifier;
      Units      : LALHelpers.Unit_Vectors.Vector)
      return Reachable_Declarations_Hashed_Set.Set;
   --  Finds all the declarations that are reachable by Identifier. A reachable
   --  declaration is one that is visible by adding a with clause of the
   --  respective package or that is visible because it is declared in a
   --  visible part of the local unit.

   function Get_Import_Suggestions
     (Id              : LALAnalysis.Identifier;
      Reachable_Decls : Reachable_Declarations_Hashed_Set.Set)
      return Import_Suggestions_Vector.Vector;
   --  For each declaration of Reachable_Decls, determines a vector of valid
   --  with clauses and corresponding prefixes so that Id becomes visible.

end Laltools.Refactor_Imports;
