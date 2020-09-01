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

with Ada.Strings;

with Langkit_Support.Errors;

with Libadalang.Common;
with Libadalang.Iterators;

package body Laltools.Refactor_Imports is

   package LALCommon renames Libadalang.Common;
   package LALIterators renames Libadalang.Iterators;

   package Parent_Packages_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => LALAnalysis.Basic_Decl,
      "="        => LALAnalysis."=");

   function Is_Specification_Unit
     (Unit : LALAnalysis.Analysis_Unit) return Boolean;
   --  Check if Unit is an analysis unit of a package specification file.
   --  ??? Possibly move this function to Libadalang.

   function Get_First_Identifier_From_Declaration
     (Decl : LALAnalysis.Basic_Decl'Class) return LALAnalysis.Identifier;
   --  Return the first identifier found in a basic declaration.

   function Get_Generic_Package_Internal
     (Gen_Pkg_Instantiation : LALAnalysis.Generic_Package_Instantiation)
      return LALAnalysis.Generic_Package_Internal;
   --  Finds the Generic_Package_Internal node given a
   --  Generic_Package_Instantiation. Note that P_Designated_Generic_Decl
   --  sometimes raises an unexpected exception when Gen_Pkg_Instantiation is
   --  a node from an Ada runtime file. At the moment, this is considered
   --  a LAL bug: [T814-031]
   --  ??? Possibly move this function to Libadalang as a property of
   --  Generic_Package_Instantiation nodes.

   function List_And_Expand_Package_Declarations
     (Pkg_Decl : LALAnalysis.Base_Package_Decl'Class; Incl_Private : Boolean)
      return Reachable_Declarations_Hashed_Set.Set;
   --  Add every declaration of Pkg_Decl to a list. If any declaration is
   --  also a Package_Declaration (in case of nested packages) then the nested
   --  package is also expanded and its declarations are added to the same
   --  list. Incl_Private controls whether or not private declarations are
   --  also added.

   procedure Add_Declaration
     (Decl          :        LALAnalysis.Basic_Decl;
      Reach_Decls   : in out Reachable_Declarations;
      Incl_Private  :        Boolean);
   --  Add Decl to Declarations by creating a key with the text of Decl's first
   --  identifier. The key's element is a list of all the declarations that
   --  share the same identifier text.

   function Get_Specification_Unit_Declarations
     (Unit : LALAnalysis.Analysis_Unit; Incl_Private : Boolean)
      return Reachable_Declarations;
   --  If Unit is a specification unit, then creates a map where the keys
   --  identify a declaration by its identifier text, and the key's value is
   --  a list of all declarations with such identifier text.

   function Get_Declarative_Part
     (Stmts : LALAnalysis.Handled_Stmts) return LALAnalysis.Declarative_Part;
   --  Finds the Handled_Stmts's respective Declarative_Part, if it exists.
   --  ??? Possibly move this function to Libadalang.

   function Get_Local_Unit_Reachable_Declarations
     (Node : LALAnalysis.Ada_Node'Class)
      return Reachable_Declarations;
   --  Finds all the declarations declared in a Node's analysis unit, that are
   --  visible to Node.

   function Get_Parent_Packages
     (Node : LALAnalysis.Ada_Node'Class) return Parent_Packages_Vector.Vector;
   --  Finds all parent packages of a node that is inside nested packages

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Import_Suggestion) return Boolean is
      use type LKSText.Unbounded_Text_Type;
   begin
      if Left.With_Clause_Text = Right.With_Clause_Text then
         return Left.Prefix_Text < Right.Prefix_Text;
      end if;
      return Left.With_Clause_Text < Right.With_Clause_Text;
   end "<";

   ---------------------
   -- Basic_Decl_Hash --
   ---------------------

   function Basic_Decl_Hash (Decl : LALAnalysis.Basic_Decl)
                             return Ada.Containers.Hash_Type is
   begin
      return LALAnalysis.Hash (Decl.As_Ada_Node);
   end Basic_Decl_Hash;

   ----------------------------------
   -- Get_Generic_Package_Internal --
   ----------------------------------

   function Get_Generic_Package_Internal
     (Gen_Pkg_Instantiation : LALAnalysis.Generic_Package_Instantiation)
      return LALAnalysis.Generic_Package_Internal is
   begin
      return Gen_Pkg_Instantiation.P_Designated_Generic_Decl.
        As_Generic_Package_Decl.F_Package_Decl.As_Generic_Package_Internal;
   exception
      when E : Langkit_Support.Errors.Property_Error =>
         pragma Unreferenced (E);
         return LALAnalysis.No_Generic_Package_Internal;
   end Get_Generic_Package_Internal;

   ---------------------------
   -- Is_Specification_Unit --
   ---------------------------

   function Is_Specification_Unit
     (Unit : LALAnalysis.Analysis_Unit) return Boolean
   is
      use type LALAnalysis.Analysis_Unit;
      use type LALAnalysis.Ada_Node;
      use type LALAnalysis.Compilation_Unit;
      use type LALCommon.Ada_Node_Kind_Type;
      use type LALCommon.Analysis_Unit_Kind;
   begin
      if Unit /= LALAnalysis.No_Analysis_Unit then
         declare
            Root_Node : constant LALAnalysis.Ada_Node := Unit.Root;
         begin
            if Root_Node /= LALAnalysis.No_Ada_Node
              and then Root_Node.Kind = LALCommon.Ada_Compilation_Unit
            then
               declare
                  Compilation_Unit : constant LALAnalysis.Compilation_Unit :=
                    LALAnalysis.As_Compilation_Unit (Root_Node);
               begin
                  if Compilation_Unit /= LALAnalysis.No_Compilation_Unit then
                     return Compilation_Unit.P_Unit_Kind =
                       LALCommon.Unit_Specification;
                  end if;
               end;
            end if;
         end;
      end if;
      return False;
   end Is_Specification_Unit;

   --------------------------------------------
   --  Get_First_Identifier_From_Declaration --
   --------------------------------------------

   function Get_First_Identifier_From_Declaration
     (Decl : LALAnalysis.Basic_Decl'Class) return LALAnalysis.Identifier
   is
      Node : constant LALAnalysis.Ada_Node :=
        LALIterators.Find_First
          (Decl, LALIterators.Kind_Is (LALCommon.Ada_Identifier));
      use type LALAnalysis.Ada_Node;
   begin
      if Node /= LALAnalysis.No_Ada_Node then
         return Node.As_Identifier;
      else
         return LALAnalysis.No_Identifier;
      end if;
   end Get_First_Identifier_From_Declaration;

   ------------------------------------------
   -- List_And_Expand_Package_Declarations --
   ------------------------------------------

   function List_And_Expand_Package_Declarations
     (Pkg_Decl : LALAnalysis.Base_Package_Decl'Class; Incl_Private : Boolean)
      return Reachable_Declarations_Hashed_Set.Set
   is
      All_Decls : Reachable_Declarations_Hashed_Set.Set;

      procedure Explore_Declarative_Part (Decls : LALAnalysis.Ada_Node_List);
      --  Iterates through Decls adding each declaration to All_Decls. If
      --  a declaration is a Package_Decl then it is expanded and nested
      --  declarations are also added to All_Decls

      ------------------------------
      -- Explore_Declarative_Part --
      ------------------------------

      procedure Explore_Declarative_Part (Decls : LALAnalysis.Ada_Node_List) is
         use type LALCommon.Ada_Node_Kind_Type;
      begin
         for Node of Decls loop
            if Node.Kind in LALCommon.Ada_Basic_Decl then
               --  F_Decls can return nodes that not not inherit from
               --  Basic_Decl type, so ignore those.

               if Node.Kind = LALCommon.Ada_Package_Decl then
                  --  Expand again nested Package_Decl nodes.

                  for Nested_Node of List_And_Expand_Package_Declarations
                    (Node.As_Package_Decl, False)
                  loop
                     All_Decls.Include (Nested_Node.As_Basic_Decl);
                  end loop;
               elsif Node.Kind = LALCommon.Ada_Generic_Package_Instantiation
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

      use type LALCommon.Ada_Node_Kind_Type;
      use type LALAnalysis.Base_Package_Decl;
      use type LALAnalysis.Generic_Package_Internal;
      use type LALAnalysis.Package_Decl;
      use type LALAnalysis.Public_Part;
      use type LALAnalysis.Private_Part;
   begin
      --  Return an empty set if Pkg_Decl is No_Base_Package_Decl,
      --  No_Generic_Package_Internal, or No_Package_Decl.

      if Pkg_Decl.As_Base_Package_Decl = LALAnalysis.No_Base_Package_Decl then
         return All_Decls;
      elsif Pkg_Decl.Kind = LALCommon.Ada_Generic_Package_Internal then
         if Pkg_Decl.As_Generic_Package_Internal =
           LALAnalysis.No_Generic_Package_Internal
         then
            return All_Decls;
         end if;
      elsif Pkg_Decl.Kind = LALCommon.Ada_Package_Decl then
         if Pkg_Decl.As_Package_Decl = LALAnalysis.No_Package_Decl then
            return All_Decls;
         end if;
      end if;

      if Pkg_Decl.F_Public_Part /= LALAnalysis.No_Public_Part then
         Explore_Declarative_Part (Pkg_Decl.F_Public_Part.F_Decls);
      end if;

      if Incl_Private
        and then Pkg_Decl.F_Private_Part /= LALAnalysis.No_Private_Part
      then
         Explore_Declarative_Part (Pkg_Decl.F_Private_Part.F_Decls);
      end if;

      return All_Decls;
   end List_And_Expand_Package_Declarations;

   ---------------------
   -- Add_Declaration --
   ---------------------

   procedure Add_Declaration
     (Decl          :        LALAnalysis.Basic_Decl;
      Reach_Decls   : in out Reachable_Declarations;
      Incl_Private  :        Boolean)
   is
      Decls_Map         : Reachable_Declarations_Map.Map renames
        Reach_Decls.Decls_Map;
      Aliased_Decls_Map : Reachable_Declarations_Aliases_Map.Map renames
        Reach_Decls.Aliased_Decls_Map;

      procedure Append (Decl : LALAnalysis.Basic_Decl);
      --  If Decls_Map already has a key equal to Decl's first identifier text
      --  then its set is expanded with Decl, otherwise and new set is created
      --  with Decl as the only element.

      procedure Append_Alias (Alias : LALAnalysis.Basic_Decl;
                              Decl  : LALAnalysis.Basic_Decl);
      --  If Aliased_Decls_Map already has a key equal to Alias then its set
      --  is expanded with Decl, otherwise and new set is created with Decl
      --  as the only element.

      ------------
      -- Append --
      ------------

      procedure Append (Decl : LALAnalysis.Basic_Decl)
      is
         Identifier : constant LALAnalysis.Identifier :=
           Get_First_Identifier_From_Declaration (Decl);
         use type LALAnalysis.Identifier;
      begin
         if Identifier = LALAnalysis.No_Identifier then
            return;
         end if;

         if Decls_Map.Contains (Identifier.Text) then
            Decls_Map.Reference (Identifier.Text).Include (Decl);
         else
            declare
               Decls_Set : Reachable_Declarations_Hashed_Set.Set;
            begin
               Decls_Set.Include (Decl);
               Decls_Map.Insert (Identifier.Text, Decls_Set);
            end;
         end if;
      end Append;

      ------------------
      -- Append_Alias --
      ------------------

      procedure Append_Alias (Alias : LALAnalysis.Basic_Decl;
                              Decl  : LALAnalysis.Basic_Decl) is
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

      use type LALCommon.Ada_Node_Kind_Type;

   begin
      if Decl.Kind in LALCommon.Ada_Error_Decl then
         --  Do not add nodes of Error_Declaration type.

         return;
      elsif Decl.Kind = LALCommon.Ada_Package_Decl then
         --  Nodes of type Package_Decl are nested packages and need to be
         --  expanded since they contain more declarations

         for D of List_And_Expand_Package_Declarations
           (Decl.As_Package_Decl, Incl_Private)
         loop
            Append (D);

            if D.Kind = LALCommon.Ada_Package_Renaming_Decl then
               Append_Alias (D.As_Package_Renaming_Decl.P_Renamed_Package,
                             D);
            end if;
         end loop;
      elsif Decl.Kind = LALCommon.Ada_Generic_Package_Instantiation then
         for D of List_And_Expand_Package_Declarations
           (Get_Generic_Package_Internal
              (Decl.As_Generic_Package_Instantiation),
            False)
         loop
            Append (D);

            if D.Kind = LALCommon.Ada_Package_Renaming_Decl then
               Append_Alias
                 (D.As_Package_Renaming_Decl.P_Renamed_Package, D);
            end if;
         end loop;
      else
         Append (Decl.As_Basic_Decl);

         if Decl.Kind = LALCommon.Ada_Package_Renaming_Decl then
            Append_Alias (Decl.As_Package_Renaming_Decl.P_Renamed_Package,
                          Decl);
         end if;

         --  ??? The same code should be applicable to
         --  LALCommon.Ada_Generic_Renaming_Decl and
         --  LALCommon.Ada_Subp_Renaming_Decl_Range. However, for now, LAL does
         --  not provide property P_Renamed_Package for such types.
      end if;
   end Add_Declaration;

   -----------------------------------------
   -- Get_Specification_Unit_Declarations --
   -----------------------------------------

   function Get_Specification_Unit_Declarations
     (Unit : LALAnalysis.Analysis_Unit; Incl_Private : Boolean)
      return Reachable_Declarations
   is
      Reach_Decls : Reachable_Declarations;

      use type LALCommon.Ada_Node_Kind_Type;

      procedure Add_Declarations (Decls : LALAnalysis.Ada_Node_List);
      --  Iterates through all declarations of the list and adds them to Map by
      --  calling Add_Declaration. Add_Declaration expands also expands nested
      --  package declarations.

      ----------------------
      -- Add_Declarations --
      ----------------------

      procedure Add_Declarations (Decls : LALAnalysis.Ada_Node_List) is
      begin
         for Node of Decls loop
            if Node.Kind in LALCommon.Ada_Basic_Decl then
               Add_Declaration
                 (Decl         => Node.As_Basic_Decl,
                  Reach_Decls  => Reach_Decls,
                  Incl_Private => False);
            end if;
         end loop;
      end Add_Declarations;

      Comp_Unit_Decl      : LALAnalysis.Basic_Decl;
      Comp_Unit_Decl_Kind : LALCommon.Ada_Node_Kind_Type;

      use type LALAnalysis.Ada_Node;
      use type LALAnalysis.Public_Part;
      use type LALAnalysis.Private_Part;
      use type LALAnalysis.Generic_Package_Internal;
   begin
      if not Is_Specification_Unit (Unit)
        or else not (Unit.Root /= LALAnalysis.No_Ada_Node
                     and then Unit.Root.Kind = LALCommon.Ada_Compilation_Unit)
      then
         return Reach_Decls;
      end if;

      Comp_Unit_Decl      := Unit.Root.As_Compilation_Unit.P_Decl;
      Comp_Unit_Decl_Kind := Unit.Root.As_Compilation_Unit.P_Decl.Kind;

      case Comp_Unit_Decl_Kind is
         when LALCommon.Ada_Package_Decl =>
            --  Add all public declarations inside an Ada_Package_Decl node,
            --  expanding nested Ada_Package_Decl and
            --  Ada_Generic_Package_Instantiation nodes. Private declarations
            --  of this Ada_Package_Decl are added according to Incl_Private.
            --  Private declarations of nested package declarations /
            --  instantiations are not added.

            declare
               Pkg_Decl : constant LALAnalysis.Package_Decl :=
                 Comp_Unit_Decl.As_Package_Decl;
            begin
               if Pkg_Decl.F_Public_Part /= LALAnalysis.No_Public_Part then
                  Add_Declarations
                    (Pkg_Decl.F_Public_Part.As_Public_Part.F_Decls);
               end if;

               if Incl_Private
                 and then Pkg_Decl.F_Private_Part /=
                   LALAnalysis.No_Private_Part
               then
                  Add_Declarations
                    (Pkg_Decl.F_Private_Part.As_Private_Part.F_Decls);
               end if;
            end;

         when LALCommon.Ada_Generic_Package_Instantiation =>
            --  Add all public declarations inside
            --  Ada_Generic_Package_Instantiation, expanding nested
            --  Ada_Package_Decl and Ada_Generic_Package_Instantiation nodes.
            --  Private declarations are never added.

            declare
               Gen_Pkg_Internal : constant
                 LALAnalysis.Generic_Package_Internal :=
                   Get_Generic_Package_Internal
                     (Comp_Unit_Decl.As_Generic_Package_Instantiation);
            begin
               if Gen_Pkg_Internal /=
                 LALAnalysis.No_Generic_Package_Internal
                 and then Gen_Pkg_Internal.F_Public_Part /=
                   LALAnalysis.No_Public_Part
               then
                  Add_Declarations
                    (Gen_Pkg_Internal.F_Public_Part.As_Public_Part.
                       F_Decls);
               end if;
            end;

         when LALCommon.Ada_Package_Renaming_Decl
            | LALCommon.Ada_Subp_Renaming_Decl
            | LALCommon.Ada_Generic_Package_Renaming_Decl
            | LALCommon.Ada_Generic_Subp_Renaming_Decl_Range
            | LALCommon.Ada_Generic_Package_Decl
            | LALCommon.Ada_Subp_Decl
            | LALCommon.Ada_Generic_Subp_Instantiation
            | LALCommon.Ada_Generic_Subp_Decl_Range
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

   --------------------------
   -- Get_Declarative_Part --
   --------------------------

   function Get_Declarative_Part
     (Stmts : LALAnalysis.Handled_Stmts) return LALAnalysis.Declarative_Part
   is
      use type LALAnalysis.Handled_Stmts;
      use type LALAnalysis.Ada_Node;
   begin
      if Stmts = LALAnalysis.No_Handled_Stmts
        or else Stmts.Parent = LALAnalysis.No_Ada_Node
      then
         return LALAnalysis.No_Declarative_Part;
      end if;

      case Stmts.Parent.Kind is
         when LALCommon.Ada_Decl_Block =>
            return Stmts.Parent.As_Decl_Block.F_Decls;

         when LALCommon.Ada_Entry_Body =>
            return Stmts.Parent.As_Entry_Body.F_Decls;

         when LALCommon.Ada_Package_Body =>
            return Stmts.Parent.As_Package_Body.F_Decls;

         when LALCommon.Ada_Subp_Body =>
            return Stmts.Parent.As_Subp_Body.F_Decls;

         when LALCommon.Ada_Task_Body =>
            return Stmts.Parent.As_Task_Body.F_Decls;

         when others =>
            return LALAnalysis.No_Declarative_Part;
      end case;
   end Get_Declarative_Part;

   -------------------------------------------
   -- Get_Local_Unit_Reachable_Declarations --
   -------------------------------------------

   function Get_Local_Unit_Reachable_Declarations
     (Node : LALAnalysis.Ada_Node'Class)
      return Reachable_Declarations
   is
      Reach_Decls : Reachable_Declarations;
      use type LALAnalysis.Ada_Node;
   begin
      if Node.Parent = LALAnalysis.No_Ada_Node then
         return Reach_Decls;
      end if;

      declare
         Stop_Decl : LALAnalysis.Basic_Decl := LALAnalysis.No_Basic_Decl;

         use type LALAnalysis.Declarative_Part;
         use type LALAnalysis.Basic_Decl;
         use type LALCommon.Ada_Node_Kind_Type;
      begin
         for Parent of Node.Parent.Parents loop
            if Parent.Kind in LALCommon.Ada_Basic_Decl
              and then Stop_Decl = LALAnalysis.No_Basic_Decl
            then
               Stop_Decl := Parent.As_Basic_Decl;
            end if;

            if Parent.Kind = LALCommon.Ada_Handled_Stmts then
               --  If this Handled_Statements has a corresponding
               --  Declarative_Part, add all those declarations to Map.
               --  If a declaration is a package declaration, then only add
               --  the visible declarations.

               declare
                  Decl_Part : constant LALAnalysis.Declarative_Part :=
                    Get_Declarative_Part (Parent.As_Handled_Stmts);
               begin
                  if Decl_Part /= LALAnalysis.No_Declarative_Part then
                     for Decl of Decl_Part.F_Decls loop
                        if Decl.Kind in LALCommon.Ada_Basic_Decl then
                           Add_Declaration
                             (Decl         => Decl.As_Basic_Decl,
                              Reach_Decls  => Reach_Decls,
                              Incl_Private => False);
                        end if;
                     end loop;
                  end if;
               end;
            elsif Parent.Kind = LALCommon.Ada_Declarative_Part then
               declare
                  Decl_Part : constant LALAnalysis.Declarative_Part :=
                    Parent.As_Declarative_Part;
               begin
                  if Decl_Part /= LALAnalysis.No_Declarative_Part then
                     --  For a node in a Declarative_Part of a Package_Body
                     --  only the previously declared declarations are
                     --  visible.

                     Decl_Loop :
                     for Decl of Decl_Part.F_Decls loop
                        --  Allow recursion for declarations that are not
                        --  packages

                        if Decl.Kind in LALCommon.Ada_Basic_Decl
                          and then Decl.As_Basic_Decl = Stop_Decl
                        then
                           if not (Decl.Kind in LALCommon.Ada_Package_Decl |
                                   LALCommon.Ada_Package_Body)
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

                        if Decl.Kind in LALCommon.Ada_Basic_Decl
                          and then Decl.Kind /= LALCommon.Ada_Package_Body
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
            elsif Parent.Kind = LALCommon.Ada_Package_Body then
               --  If Node is inside a Package_Body then all private
               --  declarations found in the Package_Declaration are
               --  added to Map.

               if Parent.As_Basic_Decl.P_Defining_Name.P_Canonical_Part
                 .P_Basic_Decl /=
                   LALAnalysis.No_Basic_Decl
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

   --------------------------------
   -- Get_Reachable_Declarations --
   --------------------------------

   function Get_Reachable_Declarations
     (Identifier : LALAnalysis.Identifier;
      Units      : LALHelpers.Unit_Vectors.Vector)
      return Reachable_Declarations
   is

      Reach_Decls       : Reachable_Declarations;
      Decls_Map         : Reachable_Declarations_Map.Map renames
        Reach_Decls.Decls_Map;
      Aliased_Decls_Map : Reachable_Declarations_Aliases_Map.Map renames
        Reach_Decls.Aliased_Decls_Map;

      use type LALAnalysis.Identifier;
   begin

      if Identifier = LALAnalysis.No_Identifier then
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
     (Node : LALAnalysis.Ada_Node'Class) return Parent_Packages_Vector.Vector
   is
      Parent_Pkgs : Parent_Packages_Vector.Vector;
      use type LALCommon.Ada_Node_Kind_Type;
   begin
      for P of Node.Parent.Parents loop
         if P.Kind in LALCommon.Ada_Package_Body
           | LALCommon.Ada_Package_Decl
             | LALCommon.Ada_Generic_Package_Decl
         then
            Parent_Pkgs.Append (P.As_Basic_Decl);
         end if;
      end loop;

      if Node.P_Parent_Basic_Decl.Kind =
        LALCommon.Ada_Generic_Package_Instantiation
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
     (Identifier : LALAnalysis.Identifier;
      Units      : LALHelpers.Unit_Vectors.Vector)
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

      use type LALAnalysis.Basic_Decl;
      use type LALAnalysis.Identifier;
      use type LALAnalysis.Ada_Node;
      use type Ada.Containers.Count_Type;

      package LKSText renames Langkit_Support.Text;

      Identifier_Text : constant LKSText.Text_Type  := Identifier.Text;
      Reach_Decls       : Reachable_Declarations;
      Decls_List        : Reachable_Declarations_Hashed_Set.Set;
      Id_Parent_Pkgs    : Parent_Packages_Vector.Vector;
      Suggestions       : Import_Suggestions_Vector.Vector;

      Decls_Map         : Reachable_Declarations_Map.Map renames
        Reach_Decls.Decls_Map;
   begin

      if Identifier = LALAnalysis.No_Identifier
        or else Identifier.Parent = LALAnalysis.No_Ada_Node
      then
         return Suggestions;
      end if;

      Reach_Decls := Get_Reachable_Declarations (Identifier, Units);

      if Decls_Map.Length = 0 then
         return Suggestions;
      end if;

      if Decls_Map.Contains (Identifier_Text) then
         Decls_List := Decls_Map.Element (Identifier_Text);
      end if;

      Id_Parent_Pkgs := Get_Parent_Packages (Identifier);

      for D of Decls_List loop
         declare
            Decl_Parent_Pkgs : constant Parent_Packages_Vector.Vector :=
              Get_Parent_Packages (D);
            Vec_Sug : Vectorized_Suggestion;

            Prefix_Decls      : Parent_Packages_Vector.Vector
            renames Vec_Sug.Prefix_Decls;
            With_Clause_Decls : Parent_Packages_Vector.Vector
            renames Vec_Sug.With_Clause_Decls;
            Aliased_Decls_Map : Reachable_Declarations_Aliases_Map.Map renames
              Reach_Decls.Aliased_Decls_Map;

            use type LKSText.Unbounded_Text_Type;
         begin

            if Id_Parent_Pkgs.Length = 0 then
               --  Id is in a unit that does not have a parent package
               --  (for instance, a standalone subprogram).

               if Decl_Parent_Pkgs.Length = 0 then
                  --  If D is a standalone subprogram, then it does not need a
                  --  prefix, and the with clause is simply it's defining name.

                  Prefix_Decls.Append (D);
               else
                  --  Otherwise, the prefix must contain all the parent
                  --  packages of D but the with clause must only contain
                  --  the top level parent package of D.

                  Prefix_Decls := Decl_Parent_Pkgs;
                  With_Clause_Decls.Append (Decl_Parent_Pkgs.Last_Element);
               end if;
            else
               if Decl_Parent_Pkgs.Length = 0 then
                  --  If D is a standalone subprogram, then it does not need a
                  --  prefix, and the with clause is simply it's defining name.

                  Prefix_Decls.Append (D);
               elsif Id_Parent_Pkgs.Last_Element.P_Defining_Name.
                 P_Canonical_Part.P_Basic_Decl =
                   Decl_Parent_Pkgs.Last_Element.P_Defining_Name.
                     P_Canonical_Part.P_Basic_Decl
               then
                  --  Nested packages cases. If both D and Id share the same
                  --  parent, no with clause or prefix is needed.

                  --  If they do not share the same parent, then the with
                  --  clause is still not needed but the prefix is. Prefix
                  --  depends on the hierarchy. All packages are added to the
                  --  prefix up to the one that is shared by Id and D.

                  if Id_Parent_Pkgs.First_Element.P_Defining_Name.
                    P_Canonical_Part.P_Basic_Decl /=
                      Decl_Parent_Pkgs.First_Element.P_Defining_Name.
                        P_Canonical_Part.P_Basic_Decl
                  then
                     for P of Decl_Parent_Pkgs loop
                        exit when P.P_Defining_Name.P_Canonical_Part.
                          P_Basic_Decl = Id_Parent_Pkgs.First_Element.
                            P_Defining_Name.P_Canonical_Part.P_Basic_Decl;

                        Prefix_Decls.Append (P);
                     end loop;
                  end if;
               else
                  Prefix_Decls := Decl_Parent_Pkgs;
                  With_Clause_Decls.Append (Decl_Parent_Pkgs.Last_Element);
               end if;
            end if;

            declare
               Vectorized_Suggestion_Vector : constant
                 Vectorized_Suggestion_Vectors.Vector
                   := Apply_Aliases (Vec_Sug, Aliased_Decls_Map);
               Suggestion       : Import_Suggestion;
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
                       LKSText.To_Unbounded_Text (D.P_Defining_Name.Text)
                       & "."
                       & Suggestion.Prefix_Text;
                  end loop;

                  for D of Vec_Sug.With_Clause_Decls loop
                     if D /= Vec_Sug.With_Clause_Decls.First_Element then
                        Suggestion.With_Clause_Text :=
                          LKSText.To_Unbounded_Text (D.P_Defining_Name.Text)
                          & "."
                          & Suggestion.With_Clause_Text;
                     else
                        Suggestion.With_Clause_Text :=
                          LKSText.To_Unbounded_Text (D.P_Defining_Name.Text);
                     end if;
                  end loop;

                  Suggestions.Append (Suggestion);
               end loop;
            end;
         end;
      end loop;

      Import_Suggestions_Vector_Sorting.Sort (Suggestions);
      return Suggestions;
   end Get_Import_Suggestions;

end Laltools.Refactor_Imports;
