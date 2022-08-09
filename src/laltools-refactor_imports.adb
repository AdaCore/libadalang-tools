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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings;
with Ada.Strings.Wide_Wide_Hash;

with Langkit_Support.Errors;

with Libadalang.Common; use Libadalang.Common;

with Laltools.Common;

package body Laltools.Refactor_Imports is

   function Basic_Decl_Hash (Decl : Basic_Decl)
                             return Ada.Containers.Hash_Type;
   --  Casts Decl as Ada_Node and uses Hash from Libadalang.Analysis.
   --  This is convenient for containers with Basic_Decl elements.

   package Reachable_Declarations_Hashed_Set is new Ada.Containers.Hashed_Sets
     (Element_Type        => Basic_Decl,
      Hash                => Basic_Decl_Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

   function Text_Type_Equivalent
     (Left, Right : Text_Type) return Boolean is (Left = Right);
   --  True if two Text_Type elements are the same.

   package Reachable_Declarations_Map is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Text_Type,
        Element_Type    => Reachable_Declarations_Hashed_Set.Set,
        Hash            => Ada.Strings.Wide_Wide_Hash,
        Equivalent_Keys => Text_Type_Equivalent,
        "="             => Reachable_Declarations_Hashed_Set."=");

   package Aliases_Hashed_Set is new Ada.Containers.Hashed_Sets
     (Element_Type => Basic_Decl,
      Hash                => Basic_Decl_Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

   package Reachable_Declarations_Aliases_Map is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Basic_Decl,
        Element_Type    => Aliases_Hashed_Set.Set,
        Hash            => Basic_Decl_Hash,
        Equivalent_Keys => "=",
        "="             => Aliases_Hashed_Set."=");

   type Reachable_Declarations is record
      Decls_Map         : Reachable_Declarations_Map.Map;
      Aliased_Decls_Map : Reachable_Declarations_Aliases_Map.Map;
   end record;

   function Get_Reachable_Declarations
     (Identifier : Libadalang.Analysis.Identifier;
      Units      : Unit_Vectors.Vector)
      return Reachable_Declarations;
   --  Finds all the declarations that are reachable by Identifier. A reachable
   --  declaration is one that is visible by adding a with clause of the
   --  respective package or that is visible because it is declared in a
   --  visible part of the local unit.

   function "<" (L, R : Unbounded_Text_Type) return Boolean
   renames Ada.Strings.Wide_Wide_Unbounded."<";
   --  Shortcut for the unbounded text type less than operator

   function "&" (L, R : Unbounded_Text_Type) return Unbounded_Text_Type
   renames Ada.Strings.Wide_Wide_Unbounded."&";
   --  Shortcut for the unbounded text type and operator

   package Parent_Packages_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Basic_Decl,
      "="        => "=");

   function Weak_Equivalent (Left, Right : Import_Suggestion) return Boolean;
   --  True if Declaration's defining name text is the same, as well as the
   --  with clause and the prefix. For instance, overloaded subprograms.

   function Is_Specification_Unit
     (Unit : Analysis_Unit) return Boolean;
   --  Check if Unit is an analysis unit of a package specification file.
   --  ??? Possibly move this function to Libadalang.

   function Get_Generic_Package_Internal
     (Gen_Pkg_Instantiation : Generic_Package_Instantiation)
      return Generic_Package_Internal;
   --  Finds the Generic_Package_Internal node given a
   --  Generic_Package_Instantiation. Note that P_Designated_Generic_Decl
   --  sometimes raises an unexpected exception when Gen_Pkg_Instantiation is
   --  a node from an Ada runtime file. At the moment, this is considered
   --  a LAL bug: [T814-031]
   --  ??? Possibly move this function to Libadalang as a property of
   --  Generic_Package_Instantiation nodes.

   function List_And_Expand_Package_Declarations
     (Pkg_Decl : Base_Package_Decl'Class; Incl_Private : Boolean)
      return Reachable_Declarations_Hashed_Set.Set;
   --  Add every declaration of Pkg_Decl to a list. If any declaration is
   --  also a Package_Declaration (in case of nested packages) then the nested
   --  package is also expanded and its declarations are added to the same
   --  list. Incl_Private controls whether or not private declarations are
   --  also added.

   procedure Add_Declaration
     (Decl          :        Basic_Decl;
      Reach_Decls   : in out Reachable_Declarations;
      Incl_Private  :        Boolean);
   --  Add Decl to Declarations by creating a key with the text of Decl's first
   --  identifier. The key's element is a list of all the declarations that
   --  share the same identifier text.

   function Get_Specification_Unit_Declarations
     (Unit : Analysis_Unit; Incl_Private : Boolean)
      return Reachable_Declarations;
   --  If Unit is a specification unit, then creates a map where the keys
   --  identify a declaration by its identifier text, and the key's value is
   --  a list of all declarations with such identifier text.

   function Get_Local_Unit_Reachable_Declarations
     (Node : Ada_Node'Class)
      return Reachable_Declarations;
   --  Finds all the declarations declared in a Node's analysis unit, that are
   --  visible to Node.

   function Get_Parent_Packages
     (Node : Ada_Node'Class) return Parent_Packages_Vector.Vector;
   --  Finds all parent packages of a node that is inside nested packages

   procedure Remove_Duplicated_Suggestions
     (Suggestions_Vector : in out Import_Suggestions_Vector.Vector);
   --  Removes duplicate suggestions using the Weak_Equivalent function.
   --  Suggestions_Vector must be sorted.

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Import_Suggestion) return Boolean is
   begin
      if Left.With_Clause_Text = Right.With_Clause_Text then
         return Left.Prefix_Text < Right.Prefix_Text;
      end if;
      return Left.With_Clause_Text < Right.With_Clause_Text;
   end "<";

   ---------------------
   -- Weak_Equivalent --
   ---------------------

   function Weak_Equivalent (Left, Right : Import_Suggestion) return Boolean is
   begin
      return Left.Declaration.P_Defining_Name.Text
               = Right.Declaration.P_Defining_Name.Text
        and then Left.With_Clause_Text = Right.With_Clause_Text
        and then Left.Prefix_Text = Right.Prefix_Text;
   end Weak_Equivalent;

   ---------------------
   -- Basic_Decl_Hash --
   ---------------------

   function Basic_Decl_Hash (Decl : Basic_Decl)
                             return Ada.Containers.Hash_Type is
   begin
      return Hash (Decl.As_Ada_Node);
   end Basic_Decl_Hash;

   ----------------------------------
   -- Get_Generic_Package_Internal --
   ----------------------------------

   function Get_Generic_Package_Internal
     (Gen_Pkg_Instantiation : Generic_Package_Instantiation)
      return Generic_Package_Internal is
   begin
      return Gen_Pkg_Instantiation.P_Designated_Generic_Decl.
        As_Generic_Package_Decl.F_Package_Decl.As_Generic_Package_Internal;
   exception
      when E : Langkit_Support.Errors.Property_Error =>
         pragma Unreferenced (E);
         return No_Generic_Package_Internal;
   end Get_Generic_Package_Internal;

   ---------------------------
   -- Is_Specification_Unit --
   ---------------------------

   function Is_Specification_Unit
     (Unit : Analysis_Unit) return Boolean is
   begin
      if Unit /= No_Analysis_Unit then
         declare
            Root_Node : constant Ada_Node := Unit.Root;
         begin
            if not Root_Node.Is_Null
              and then Root_Node.Kind = Ada_Compilation_Unit
            then
               declare
                  Compilation_Unit :
                    constant Libadalang.Analysis.Compilation_Unit :=
                      As_Compilation_Unit (Root_Node);
               begin
                  if not Compilation_Unit.Is_Null then
                     return Compilation_Unit.P_Unit_Kind = Unit_Specification;
                  end if;
               end;
            end if;
         end;
      end if;
      return False;
   end Is_Specification_Unit;

   ------------------------------------------
   -- List_And_Expand_Package_Declarations --
   ------------------------------------------

   function List_And_Expand_Package_Declarations
     (Pkg_Decl : Base_Package_Decl'Class; Incl_Private : Boolean)
      return Reachable_Declarations_Hashed_Set.Set
   is
      All_Decls : Reachable_Declarations_Hashed_Set.Set;

      procedure Explore_Declarative_Part (Decls : Ada_Node_List);
      --  Iterates through Decls adding each declaration to All_Decls. If
      --  a declaration is a Package_Decl then it is expanded and nested
      --  declarations are also added to All_Decls

      ------------------------------
      -- Explore_Declarative_Part --
      ------------------------------

      procedure Explore_Declarative_Part (Decls : Ada_Node_List) is
      begin
         for Node of Decls loop
            if Node.Kind in Ada_Basic_Decl then
               --  F_Decls can return nodes that not not inherit from
               --  Basic_Decl type, so ignore those.

               if Node.Kind = Ada_Package_Decl then
                  --  Expand again nested Package_Decl nodes.

                  for Nested_Node of List_And_Expand_Package_Declarations
                    (Node.As_Package_Decl, False)
                  loop
                     All_Decls.Include (Nested_Node.As_Basic_Decl);
                  end loop;
               elsif Node.Kind = Ada_Generic_Package_Instantiation
               then
                  --  Expand again nested Generic_Package_Instantiation nodes.

                  for Nested_Node of List_And_Expand_Package_Declarations
                    (Get_Generic_Package_Internal
                       (Node.As_Generic_Package_Instantiation),
                     False)
                  loop
                     All_Decls.Include (Nested_Node.As_Basic_Decl);
                  end loop;
               else
                  --  All other nodes that inherit from Basic_Decl type except
                  --  Package_Decl and Generic_Package_Instantiation can
                  --  be added.

                  All_Decls.Include (Node.As_Basic_Decl);
               end if;
            end if;
         end loop;
      end Explore_Declarative_Part;

   begin
      --  Return an empty set if Pkg_Decl is null

      if Pkg_Decl.Is_Null then
         return All_Decls;
      end if;

      if not Pkg_Decl.F_Public_Part.Is_Null then
         begin
            Explore_Declarative_Part (Pkg_Decl.F_Public_Part.F_Decls);

         exception
            --  Ignore Constraint_Error thrown by F_Decls due to invalid code.
            --  Continue execution since Pkg_Decl.F_Private_Part.F_Decls might
            --  not have the same issue.
            --  This has been seen in V516-059.
            when Constraint_Error =>
               null;
         end;
      end if;

      if Incl_Private and then not Pkg_Decl.F_Private_Part.Is_Null then
         begin
            Explore_Declarative_Part (Pkg_Decl.F_Private_Part.F_Decls);

         exception
            --  Ignore Constraint_Error thrown by F_Decls due to invalid code.
            --  Continue execution since Pkg_Decl.F_Public_Part.F_Decls might
            --  have executed correctly.
            --  This has been seen in V516-059.
            when Constraint_Error =>
               null;
         end;
      end if;

      return All_Decls;
   end List_And_Expand_Package_Declarations;

   ---------------------
   -- Add_Declaration --
   ---------------------

   procedure Add_Declaration
     (Decl          :        Basic_Decl;
      Reach_Decls   : in out Reachable_Declarations;
      Incl_Private  :        Boolean)
   is
      Decls_Map         : Reachable_Declarations_Map.Map renames
        Reach_Decls.Decls_Map;
      Aliased_Decls_Map : Reachable_Declarations_Aliases_Map.Map renames
        Reach_Decls.Aliased_Decls_Map;

      procedure Append (Decl : Basic_Decl);
      --  If Decls_Map already has a key equal to Decl's first identifier text
      --  then its set is expanded with Decl, otherwise and new set is created
      --  with Decl as the only element.

      procedure Append_Alias (Alias : Basic_Decl;
                              Decl  : Basic_Decl);
      --  If Aliased_Decls_Map already has a key equal to Alias then its set
      --  is expanded with Decl, otherwise and new set is created with Decl
      --  as the only element.

      ------------
      -- Append --
      ------------

      procedure Append (Decl : Basic_Decl) is
      begin
         if Decl.Is_Null or else Decl.P_Defining_Name.Is_Null then
            return;
         end if;

         declare
            Text : constant Text_Type := Decl.P_Defining_Name.F_Name.Text;

         begin
            if Decls_Map.Contains (Text) then
               Decls_Map.Reference (Text).Include (Decl);
            else
               declare
                  Decls_Set : Reachable_Declarations_Hashed_Set.Set;
               begin
                  Decls_Set.Include (Decl);
                  Decls_Map.Insert (Text, Decls_Set);
               end;
            end if;
         end;
      end Append;

      ------------------
      -- Append_Alias --
      ------------------

      procedure Append_Alias (Alias : Basic_Decl;
                              Decl  : Basic_Decl) is
      begin
         if Aliased_Decls_Map.Contains (Alias) then
            if not Aliased_Decls_Map.Reference (Alias).Contains (Decl) then
               Aliased_Decls_Map.Reference (Alias).Insert (Decl);
            end if;
         else
            declare
               V : Aliases_Hashed_Set.Set;
            begin
               V.Insert (Decl);
               Aliased_Decls_Map.Insert (Alias, V);
            end;
         end if;
      end Append_Alias;

   begin
      if Decl.Kind in Ada_Error_Decl then
         --  Do not add nodes of Error_Declaration type.

         return;
      elsif Decl.Kind = Ada_Package_Decl then
         --  Nodes of type Package_Decl are nested packages and need to be
         --  expanded since they contain more declarations

         for D of List_And_Expand_Package_Declarations
           (Decl.As_Package_Decl, Incl_Private)
         loop
            Append (D);

            if D.Kind = Ada_Package_Renaming_Decl then
               Append_Alias (D.As_Package_Renaming_Decl.P_Renamed_Package,
                             D);
            end if;
         end loop;
      elsif Decl.Kind = Ada_Generic_Package_Instantiation then
         for D of List_And_Expand_Package_Declarations
           (Get_Generic_Package_Internal
              (Decl.As_Generic_Package_Instantiation),
            False)
         loop
            Append (D);

            if D.Kind = Ada_Package_Renaming_Decl then
               Append_Alias
                 (D.As_Package_Renaming_Decl.P_Renamed_Package, D);
            end if;
         end loop;
      else
         Append (Decl.As_Basic_Decl);

         if Decl.Kind = Ada_Package_Renaming_Decl then
            Append_Alias (Decl.As_Package_Renaming_Decl.P_Renamed_Package,
                          Decl);
         end if;

         --  ??? The same code should be applicable to
         --  Ada_Generic_Renaming_Decl and
         --  Ada_Subp_Renaming_Decl_Range. However, for now, LAL does
         --  not provide property P_Renamed_Package for such types.
      end if;
   end Add_Declaration;

   -----------------------------------------
   -- Get_Specification_Unit_Declarations --
   -----------------------------------------

   function Get_Specification_Unit_Declarations
     (Unit         : Analysis_Unit;
      Incl_Private : Boolean)
      return Reachable_Declarations
   is
      Reach_Decls : Reachable_Declarations;

      procedure Add_Declarations (Decls : Ada_Node_List);
      --  Iterates through all declarations of the list and adds them to Map by
      --  calling Add_Declaration. Add_Declaration expands also expands nested
      --  package declarations.

      ----------------------
      -- Add_Declarations --
      ----------------------

      procedure Add_Declarations (Decls : Ada_Node_List) is
      begin
         for Node of Decls loop
            if Node.Kind in Ada_Basic_Decl then
               Add_Declaration
                 (Decl         => Node.As_Basic_Decl,
                  Reach_Decls  => Reach_Decls,
                  Incl_Private => False);
            end if;
         end loop;
      end Add_Declarations;

      Comp_Unit_Decl      : Basic_Decl;
      Comp_Unit_Decl_Kind : Ada_Node_Kind_Type;

   begin
      if not Is_Specification_Unit (Unit)
        or else not (not Unit.Root.Is_Null
                     and then Unit.Root.Kind = Ada_Compilation_Unit)
      then
         return Reach_Decls;
      end if;

      Comp_Unit_Decl      := Unit.Root.As_Compilation_Unit.P_Decl;
      Comp_Unit_Decl_Kind := Unit.Root.As_Compilation_Unit.P_Decl.Kind;

      case Comp_Unit_Decl_Kind is
         when Ada_Package_Decl =>
            --  Add all public declarations inside an Ada_Package_Decl node,
            --  expanding nested Ada_Package_Decl and
            --  Ada_Generic_Package_Instantiation nodes. Private declarations
            --  of this Ada_Package_Decl are added according to Incl_Private.
            --  Private declarations of nested package declarations /
            --  instantiations are not added.

            declare
               Pkg_Decl : constant Package_Decl :=
                 Comp_Unit_Decl.As_Package_Decl;
            begin
               if not Pkg_Decl.F_Public_Part.Is_Null then
                  Add_Declarations
                    (Pkg_Decl.F_Public_Part.As_Public_Part.F_Decls);
               end if;

               if Incl_Private and then not Pkg_Decl.F_Private_Part.Is_Null
               then
                  Add_Declarations
                    (Pkg_Decl.F_Private_Part.As_Private_Part.F_Decls);
               end if;
            end;

         when Ada_Generic_Package_Instantiation =>
            --  Add all public declarations inside
            --  Ada_Generic_Package_Instantiation, expanding nested
            --  Ada_Package_Decl and Ada_Generic_Package_Instantiation nodes.
            --  Private declarations are never added.

            declare
               Gen_Pkg_Internal : constant
                 Generic_Package_Internal :=
                   Get_Generic_Package_Internal
                     (Comp_Unit_Decl.As_Generic_Package_Instantiation);
            begin
               if not Gen_Pkg_Internal.Is_Null
                 and then not Gen_Pkg_Internal.F_Public_Part.Is_Null
               then
                  Add_Declarations
                    (Gen_Pkg_Internal.F_Public_Part.As_Public_Part.
                       F_Decls);
               end if;
            end;

         when Ada_Package_Renaming_Decl
            | Ada_Subp_Renaming_Decl
            | Ada_Generic_Package_Renaming_Decl
            | Ada_Generic_Subp_Renaming_Decl_Range
            | Ada_Generic_Package_Decl
            | Ada_Subp_Decl
            | Ada_Generic_Subp_Instantiation
            | Ada_Generic_Subp_Decl_Range
         =>
            --  Other kinds of specification units are considered as basic
            --  declarations and are added to the Reach_Decls.

            Add_Declaration
              (Decl         => Comp_Unit_Decl,
               Reach_Decls  => Reach_Decls,
               Incl_Private => False);

         when others =>
            null;
      end case;
      return Reach_Decls;
   end Get_Specification_Unit_Declarations;

   -------------------------------------------
   -- Get_Local_Unit_Reachable_Declarations --
   -------------------------------------------

   function Get_Local_Unit_Reachable_Declarations
     (Node : Ada_Node'Class)
      return Reachable_Declarations
   is
      Reach_Decls : Reachable_Declarations;

   begin
      if Node.Parent.Is_Null then
         return Reach_Decls;
      end if;

      declare
         Stop_Decl : Basic_Decl := No_Basic_Decl;

      begin
         for Parent of Node.Parent.Parents loop
            if Parent.Kind in Ada_Basic_Decl
              and then Stop_Decl.Is_Null
            then
               Stop_Decl := Parent.As_Basic_Decl;
            end if;

            if Parent.Kind = Ada_Handled_Stmts then
               --  If this Handled_Statements has a corresponding
               --  Declarative_Part, add all those declarations to Map.
               --  If a declaration is a package declaration, then only add
               --  the visible declarations.

               declare
                  Decl_Part : constant Declarative_Part :=
                    Laltools.Common.Get_Declarative_Part
                      (Parent.As_Handled_Stmts);
               begin
                  if not Decl_Part.Is_Null then
                     for Decl of Decl_Part.F_Decls loop
                        if Decl.Kind in Ada_Basic_Decl then
                           Add_Declaration
                             (Decl         => Decl.As_Basic_Decl,
                              Reach_Decls  => Reach_Decls,
                              Incl_Private => False);
                        end if;
                     end loop;
                  end if;
               end;
            elsif Parent.Kind = Ada_Declarative_Part then
               declare
                  Decl_Part : constant Declarative_Part :=
                    Parent.As_Declarative_Part;
               begin
                  if not Decl_Part.Is_Null then
                     --  For a node in a Declarative_Part of a Package_Body
                     --  only the previously declared declarations are
                     --  visible.

                     Decl_Loop :
                     for Decl of Decl_Part.F_Decls loop
                        --  Allow recursion for declarations that are not
                        --  packages

                        if Decl.Kind in Ada_Basic_Decl
                          and then Decl.As_Basic_Decl = Stop_Decl
                        then
                           if not (Decl.Kind in Ada_Package_Decl |
                                   Ada_Package_Body)
                           then
                              Add_Declaration
                                (Decl         =>
                                   Decl.As_Basic_Decl.P_Defining_Name.
                                     P_Canonical_Part.P_Basic_Decl,
                                 Reach_Decls  => Reach_Decls,
                                 Incl_Private => False);
                           end if;
                           exit Decl_Loop;
                        end if;

                        --  Do not add Package_Body

                        if Decl.Kind in Ada_Basic_Decl
                          and then Decl.Kind /= Ada_Package_Body
                        then
                           Add_Declaration
                             (Decl         =>
                                Decl.As_Basic_Decl.P_Defining_Name.
                                  P_Canonical_Part.P_Basic_Decl,
                              Reach_Decls  => Reach_Decls,
                              Incl_Private => False);
                        end if;
                     end loop Decl_Loop;
                  end if;
               end;
            elsif Parent.Kind = Ada_Package_Body then
               --  If Node is inside a Package_Body then all private
               --  declarations found in the Package_Declaration are
               --  added to Map.

               if not Parent.As_Basic_Decl.P_Defining_Name.P_Canonical_Part
                 .P_Basic_Decl.Is_Null
               then
                  Add_Declaration
                    (Decl        =>
                       Parent.As_Basic_Decl.P_Defining_Name.P_Canonical_Part
                     .P_Basic_Decl,
                     Reach_Decls => Reach_Decls, Incl_Private => True);
               end if;
            end if;
         end loop;
      end;
      return Reach_Decls;
   end Get_Local_Unit_Reachable_Declarations;

   -----------------------------------
   -- Remove_Duplicated_Suggestions --
   -----------------------------------

   procedure Remove_Duplicated_Suggestions
     (Suggestions_Vector : in out Import_Suggestions_Vector.Vector)
   is
      Unique_Suggestions_Vector : Import_Suggestions_Vector.Vector;
      J : Import_Suggestions_Vector.Extended_Index := 0;
      use type Ada.Containers.Count_Type;
   begin
      if Suggestions_Vector.Length < 2 then
         return;
      end if;

      Unique_Suggestions_Vector.Append (Suggestions_Vector.First_Element);
      for K in 1 .. Import_Suggestions_Vector.Extended_Index
        (Suggestions_Vector.Length) - 1
      loop
         if not Weak_Equivalent
           (Suggestions_Vector.Element (J),
            Suggestions_Vector.Element (K))
         then
            Unique_Suggestions_Vector.Append (Suggestions_Vector.Element (K));
         end if;
         J := J + 1;
      end loop;
      Suggestions_Vector := Unique_Suggestions_Vector;
   end Remove_Duplicated_Suggestions;

   --------------------------------
   -- Get_Reachable_Declarations --
   --------------------------------

   function Get_Reachable_Declarations
     (Identifier : Libadalang.Analysis.Identifier;
      Units      : Unit_Vectors.Vector)
      return Reachable_Declarations
   is

      Reach_Decls       : Reachable_Declarations;
      Decls_Map         : Reachable_Declarations_Map.Map renames
        Reach_Decls.Decls_Map;
      Aliased_Decls_Map : Reachable_Declarations_Aliases_Map.Map renames
        Reach_Decls.Aliased_Decls_Map;

   begin

      if Identifier.Is_Null then
         return Reach_Decls;
      end if;

      Reach_Decls := Get_Local_Unit_Reachable_Declarations (Identifier);

      for Unit of Units loop
         declare
            Unit_Decls : constant Reachable_Declarations :=
              Get_Specification_Unit_Declarations (Unit, False);
            C1 : Reachable_Declarations_Map.Cursor
              := Unit_Decls.Decls_Map.First;
            C2 : Reachable_Declarations_Aliases_Map.Cursor
              := Unit_Decls.Aliased_Decls_Map.First;

            use Reachable_Declarations_Map;
            use Reachable_Declarations_Aliases_Map;
            use Aliases_Hashed_Set;
         begin
            --  Merge declarations maps

            while Has_Element (C1) loop
               if Decls_Map.Contains (Key (C1)) then
                  declare
                     List : constant Reachable_Declarations_Hashed_Set.Set :=
                       Element (C1);
                     C3 : Reachable_Declarations_Hashed_Set.Cursor :=
                       List.First;
                     use Reachable_Declarations_Hashed_Set;
                  begin
                     while Has_Element (C3) loop
                        Decls_Map.Reference (Key (C1)).Include (Element (C3));
                        Next (C3);
                     end loop;
                  end;
               else
                  Decls_Map.Insert (Key (C1), Element (C1));
               end if;

               Next (C1);
            end loop;

            --  Merge aliased declaration maps

            while Has_Element (C2) loop
               if Aliased_Decls_Map.Contains (Key (C2)) then
                  declare
                     V  : constant Aliases_Hashed_Set.Set
                       := Element (C2);
                     C3 : Aliases_Hashed_Set.Cursor :=
                       V.First;
                  begin
                     while Has_Element (C3) loop
                        if not Aliased_Decls_Map.Reference (Key (C2)).
                          Contains (Element (C3))
                        then
                           Aliased_Decls_Map.Reference (Key (C2))
                             .Insert (Element (C3));
                        end if;

                        Next (C3);
                     end loop;
                  end;
               else
                  Aliased_Decls_Map.Insert (Key (C2), Element (C2));
               end if;

               Next (C2);
            end loop;
         end;
      end loop;

      return Reach_Decls;

   end Get_Reachable_Declarations;

   ------------------------
   -- Get_Parent_Package --
   ------------------------

   function Get_Parent_Packages
     (Node : Ada_Node'Class) return Parent_Packages_Vector.Vector
   is
      Parent_Pkgs : Parent_Packages_Vector.Vector;

   begin
      for P of Node.Parent.Parents loop
         if P.Kind in Ada_Package_Body
           | Ada_Package_Decl
             | Ada_Generic_Package_Decl
         then
            Parent_Pkgs.Append (P.As_Basic_Decl);
         end if;
      end loop;

      declare
         Top_Level : constant Basic_Decl :=
           Node.P_Top_Level_Decl (Node.Unit);
      begin
         if Top_Level /= No_Basic_Decl
           and then Top_Level.Kind in Ada_Subp_Body_Range
         then
            --  Inside a main file: add the main name as the last element
            --  it will be used to filter out all the elements defined inside
            --  the unit itself.
            Parent_Pkgs.Append (Top_Level);
         end if;
      end;

      if Node.P_Parent_Basic_Decl.Kind =
        Ada_Generic_Package_Instantiation
      then
         Parent_Pkgs.Delete_Last;
         Parent_Pkgs.Append (Node.P_Parent_Basic_Decl);
         for P of Get_Parent_Packages (Node.P_Parent_Basic_Decl) loop
            Parent_Pkgs.Append (P);
         end loop;
      end if;
      return Parent_Pkgs;
   end Get_Parent_Packages;

   ----------------------------
   -- Get_Import_Suggestions --
   ----------------------------

   function Get_Import_Suggestions
     (Node  : Ada_Node'Class;
      Units : Unit_Vectors.Vector)
      return Import_Suggestions_Vector.Vector
   is
      type Vectorized_Suggestion is
         record
            Prefix_Decls      : Parent_Packages_Vector.Vector;
            With_Clause_Decls : Parent_Packages_Vector.Vector;
         end record;

      package Vectorized_Suggestion_Vectors is
        new Ada.Containers.Vectors
          (Index_Type   => Natural,
           Element_Type => Vectorized_Suggestion);

      function Apply_Aliases
        (Self    : Vectorized_Suggestion;
         Aliases : Reachable_Declarations_Aliases_Map.Map)
         return Vectorized_Suggestion_Vectors.Vector;
      --  ???  A package can have a visible rename in many places in the
      --  code. This function will check if the rename is applicable
      --  in a specific position of the prefix / with clause.

      -------------------
      -- Apply_Aliases --
      -------------------

      function Apply_Aliases
        (Self    : Vectorized_Suggestion;
         Aliases : Reachable_Declarations_Aliases_Map.Map)
         return Vectorized_Suggestion_Vectors.Vector
      is
         Suggestions : Vectorized_Suggestion_Vectors.Vector;

         procedure Branch (Vec_Sug   : Vectorized_Suggestion;
                           Start_Idx : Parent_Packages_Vector.Extended_Index);
         --  Starting on the Basic_Decl of Vec_Sug.Prefix_Decls that
         --  corresponds to index given by Start_Idx, iterates through the
         --  the declarations and checks if they have any rename. If so,
         --  checks if it's valid. If so, branches the suggestion in the
         --  renamed declaration.

         --  Example:
         --  Package E renames package A
         --  Package F renames package B
         --  Vec_Sug has the following decls: A.B.C.
         --  This procedure will create the following branches
         --  (\ indicates a branch):

         --  C
         --  |\
         --  B F
         --  | |
         --  A A
         --   \ \
         --    E E

         --  Producing the following suggestions:
         --  A.B.C.
         --  A.F.C.
         --  E.B.C.
         --  E.F.C.

         --  ??? Future optimization: use graph algorithms.

         -----------------
         -- Valid_Alias --
         -----------------

         function Valid_Alias return Boolean is (True);

         ------------
         -- Branch --
         ------------

         procedure Branch (Vec_Sug   : Vectorized_Suggestion;
                           Start_Idx : Parent_Packages_Vector.Extended_Index)
         is
            Aliased_Sug : Vectorized_Suggestion;
         begin
            for Index in Start_Idx .. Vec_Sug.Prefix_Decls.Last_Index loop
               if Aliases.Contains (Vec_Sug.Prefix_Decls.Element (Index)) then
                  for Alias of Aliases.Element (Vec_Sug.Prefix_Decls
                                                .Element (Index))
                  loop
                     if Valid_Alias then
                        Aliased_Sug := Vec_Sug;
                        Aliased_Sug.Prefix_Decls.Replace_Element
                          (Index, Alias);
                        declare
                           Branch_Idx : constant
                             Parent_Packages_Vector.Extended_Index
                               := Aliased_Sug.With_Clause_Decls.Find_Index
                                 (Vec_Sug.Prefix_Decls.Element (Index));
                        begin
                           if Branch_Idx /= Parent_Packages_Vector.No_Index
                           then
                              Aliased_Sug.With_Clause_Decls.Replace_Element
                                (Branch_Idx, Alias);
                           end if;
                        end;
                        Suggestions.Append (Aliased_Sug);
                        Branch (Aliased_Sug, Index);
                     end if;
                  end loop;
               end if;
            end loop;
         end Branch;

      begin
         Suggestions.Append (Self);
         Branch (Self, Self.Prefix_Decls.First_Index);
         return Suggestions;
      end Apply_Aliases;

      Identifier        : constant Libadalang.Analysis.Identifier :=
        (if not Node.Is_Null and then Node.Kind in Ada_Identifier_Range then
           Node.As_Identifier
         else
           No_Identifier);
      Identifier_Text   : constant Text_Type :=
        (if not Identifier.Is_Null then Identifier.Text
         else "");
      Name              : Libadalang.Analysis.Name :=
        (if not Identifier.Is_Null and then Identifier.Kind in Ada_Name then
           Identifier.As_Name
         else
           No_Name);
      Resolved_Name     : Defining_Name := No_Defining_Name;

      Reach_Decls       : Reachable_Declarations;
      Decls_List        : Reachable_Declarations_Hashed_Set.Set;
      Id_Parent_Pkgs    : Parent_Packages_Vector.Vector;

      Suggestions       : Import_Suggestions_Vector.Vector;

      use type Ada.Containers.Count_Type;

   begin
      if Name.Is_Null then
         return Suggestions;
      end if;

      while not Name.Is_Null
        and then not Name.Parent.Is_Null
        and then Name.Parent.Kind in Ada_Dotted_Name_Range
      loop
         Name := Name.Parent.As_Name;
      end loop;

      if Name.Is_Null or else Name.P_Is_Defining then
         return Suggestions;
      end if;

      if Name.Kind in Ada_Dotted_Name_Range then
         Name := Name.As_Dotted_Name.F_Suffix.As_Name;
      end if;

      Resolved_Name := Laltools.Common.Resolve_Name_Precisely (Name);

      if Resolved_Name.Is_Null then
         Reach_Decls :=
           Get_Reachable_Declarations
             (Identifier =>
                (if Name.Kind in Ada_Dotted_Name_Range then
                        Name.As_Dotted_Name.F_Suffix.As_Identifier
                 else
                    Name.As_Identifier),
              Units      => Units);

         if Reach_Decls.Decls_Map.Length = 0 then
            return Suggestions;
         end if;

         if Reach_Decls.Decls_Map.Contains (Identifier_Text) then
            Decls_List := Reach_Decls.Decls_Map.Element (Identifier_Text);
         end if;

         Id_Parent_Pkgs := Get_Parent_Packages (Name);

         for D of Decls_List loop
            declare
               Decl_Parent_Pkgs : constant Parent_Packages_Vector.Vector :=
                 Get_Parent_Packages (D);
               Vec_Sug          : Vectorized_Suggestion;

               Prefix_Decls      : Parent_Packages_Vector.Vector
               renames Vec_Sug.Prefix_Decls;
               With_Clause_Decls : Parent_Packages_Vector.Vector
               renames Vec_Sug.With_Clause_Decls;
               Aliased_Decls_Map :
               Reachable_Declarations_Aliases_Map.Map renames
                 Reach_Decls.Aliased_Decls_Map;

            begin

               if Id_Parent_Pkgs.Length = 0 then
                  --  Id is in a unit that does not have a parent package
                  --  (for instance, a standalone subprogram).

                  if Decl_Parent_Pkgs.Length /= 0 then
                     --  Otherwise, the prefix must contain all the parent
                     --  packages of D but the with clause must only contain
                     --  the top level parent package of D.

                     Prefix_Decls := Decl_Parent_Pkgs;
                     With_Clause_Decls.Append
                       (Decl_Parent_Pkgs.Last_Element);
                  end if;
               else
                  if Decl_Parent_Pkgs.Length /= 0
                    and then Id_Parent_Pkgs.Last_Element.P_Defining_Name.
                      P_Canonical_Part.P_Basic_Decl =
                        Decl_Parent_Pkgs.Last_Element.P_Defining_Name.
                          P_Canonical_Part.P_Basic_Decl
                  then
                     --  Nested packages cases. If both D and Id share the
                     --  same parent, no with clause or prefix is needed.

                     --  If they do not share the same parent, then the with
                     --  clause is still not needed but the prefix is.
                     --  Prefix depends on the hierarchy. All packages are
                     --  added to the prefix up to the one that is shared by
                     --  Id and D.

                     if Id_Parent_Pkgs.First_Element.P_Defining_Name.
                       P_Canonical_Part.P_Basic_Decl /=
                         Decl_Parent_Pkgs.First_Element.P_Defining_Name.
                           P_Canonical_Part.P_Basic_Decl
                     then
                        for P of Decl_Parent_Pkgs loop
                           exit when P.P_Defining_Name.P_Canonical_Part.
                             P_Basic_Decl = Id_Parent_Pkgs.First_Element.
                               P_Defining_Name.P_Canonical_Part.
                                 P_Basic_Decl;

                           Prefix_Decls.Append (P);
                        end loop;
                     end if;
                  else
                     Prefix_Decls := Decl_Parent_Pkgs;
                     With_Clause_Decls.Append
                       (Decl_Parent_Pkgs.Last_Element);
                  end if;
               end if;

               declare
                  Vectorized_Suggestion_Vector : constant
                    Vectorized_Suggestion_Vectors.Vector
                      := Apply_Aliases (Vec_Sug, Aliased_Decls_Map);
                  Suggestion                   : Import_Suggestion;
               begin
                  for Vec_Sug of Vectorized_Suggestion_Vector loop
                     Suggestion.Declaration := D;
                     Suggestion.Prefix_Text :=
                       Ada.Strings.Wide_Wide_Unbounded.
                         Null_Unbounded_Wide_Wide_String;
                     Suggestion.With_Clause_Text :=
                       Ada.Strings.Wide_Wide_Unbounded.
                         Null_Unbounded_Wide_Wide_String;

                     for D of Vec_Sug.Prefix_Decls loop
                        Suggestion.Prefix_Text :=
                          To_Unbounded_Text (D.P_Defining_Name.Text)
                          & To_Unbounded_Text (To_Text ("."))
                          & Suggestion.Prefix_Text;
                     end loop;

                     for D of Vec_Sug.With_Clause_Decls loop
                        if D /= Vec_Sug.With_Clause_Decls.First_Element then
                           Suggestion.With_Clause_Text :=
                             To_Unbounded_Text (D.P_Defining_Name.Text)
                             & To_Unbounded_Text (To_Text ("."))
                             & Suggestion.With_Clause_Text;
                        else
                           Suggestion.With_Clause_Text :=
                             To_Unbounded_Text (D.P_Defining_Name.Text);
                        end if;
                     end loop;

                     if Suggestion.Prefix_Text /=
                       Ada.Strings.Wide_Wide_Unbounded.
                         Null_Unbounded_Wide_Wide_String
                     then
                        Suggestions.Append (Suggestion);
                     end if;
                  end loop;
               end;
            end;
         end loop;

         Import_Suggestions_Vector_Sorting.Sort (Suggestions);
         Remove_Duplicated_Suggestions (Suggestions);

      else
         declare
            Resolved_Name_CU  : constant Compilation_Unit :=
              Resolved_Name.P_Enclosing_Compilation_Unit;
            Unit_Dependencies : constant Compilation_Unit_Array :=
              Node.P_Enclosing_Compilation_Unit.P_Unit_Dependencies;

         begin
            if Name.P_Enclosing_Compilation_Unit /= Resolved_Name_CU
              and then not (for some Unit of Unit_Dependencies
                            => Unit = Resolved_Name_CU)
            then
               declare
                  Top_Level_Decl : constant Basic_Decl :=
                    Resolved_Name_CU.P_Top_Level_Decl
                      (Resolved_Name_CU.Unit);
                  Suggestion     : constant Import_Suggestion :=
                    (Declaration      =>
                       Resolved_Name.P_Basic_Decl,
                     With_Clause_Text =>
                       Langkit_Support.Text.To_Unbounded_Text
                         (Top_Level_Decl.P_Fully_Qualified_Name),
                     Prefix_Text      =>
                       Ada.Strings.Wide_Wide_Unbounded.
                         Null_Unbounded_Wide_Wide_String);

               begin
                  Suggestions.Append (Suggestion);
               end;
            end if;
         end;
      end if;

      return Suggestions;
   exception
      when others =>
         return Import_Suggestions_Vector.Empty_Vector;
   end Get_Import_Suggestions;

end Laltools.Refactor_Imports;
