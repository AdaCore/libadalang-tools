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

with Libadalang.Common;
with Libadalang.Iterators;

with Ada.Strings;

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

   function List_And_Expand_Package_Declarations
     (Pkg_Decl : LALAnalysis.Package_Decl; Incl_Private : Boolean)
      return Reachable_Declarations_Hashed_Set.Set;
   --  Add every declaration of Pkg_Decl to a list. If any declaration is
   --  also a Package_Declaration (in case of nested packages) then the nested
   --  package is also expanded and its declarations are added to the same
   --  list. Incl_Private controls whether or not private declarations are
   --  also added.

   procedure Add_Declaration
     (Decl         :        LALAnalysis.Basic_Decl'Class;
      Declarations : in out Reachable_Declarations_Result.Map;
      Incl_Private :        Boolean);
   --  Add Decl to Declarations by creating a key with the text of Decl's first
   --  identifier. The key's element is a list of all the declarations that
   --  share the same identifier text.

   function Get_Specification_Unit_Declarations
     (Unit : LALAnalysis.Analysis_Unit; Incl_Private : Boolean)
      return Reachable_Declarations_Result.Map;
   --  If Unit is a specification unit, then creates a map where the keys
   --  identify a declaration by its identifier text, and the key's value is
   --  a list of all declarations with such identifier text.

   function Get_Declarative_Part
     (Stmts : LALAnalysis.Handled_Stmts) return LALAnalysis.Declarative_Part;
   --  Finds the Handled_Stmts's respective Declarative_Part, if it exists.
   --  ??? Possibly move this function to Libadalang.

   function Get_Local_Unit_Reachable_Declarations
     (Node : LALAnalysis.Ada_Node'Class)
      return Reachable_Declarations_Result.Map;
   --  Finds all the declarations declared in a Node's analysis unit, that are
   --  visible to Node.

   function Get_Parent_Packages
     (Node : LALAnalysis.Ada_Node'Class) return Parent_Packages_Vector.Vector;
   --  Finds all parent packages of a node that is inside nested packages

   ---------------------
   -- Basic_Decl_Hash --
   ---------------------

   function Basic_Decl_Hash (Decl : LALAnalysis.Basic_Decl)
                                return Ada.Containers.Hash_Type is
   begin
      return LALAnalysis.Hash (Decl.As_Ada_Node);
   end Basic_Decl_Hash;

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
     (Pkg_Decl : LALAnalysis.Package_Decl; Incl_Private : Boolean)
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
                     if Node.Kind in LALCommon.Ada_Basic_Decl then
                        All_Decls.Include (Nested_Node.As_Basic_Decl);
                     end if;
                  end loop;
               else
                  --  All other nodes that inherit from Basic_Decl type except
                  --  Package_Decl can be added.
                  All_Decls.Include (Node.As_Basic_Decl);
               end if;
            end if;
         end loop;
      end Explore_Declarative_Part;

      use type LALAnalysis.Package_Decl;
      use type LALAnalysis.Public_Part;
      use type LALAnalysis.Private_Part;

   begin
      if Pkg_Decl = LALAnalysis.No_Package_Decl then
         return All_Decls;
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
     (Decl         :        LALAnalysis.Basic_Decl'Class;
      Declarations : in out Reachable_Declarations_Result.Map;
      Incl_Private :        Boolean)
   is
      use type LALCommon.Ada_Node_Kind_Type;

      procedure Append (D : LALAnalysis.Basic_Decl);
      --  If Declarations already has a key equalto D's first identifier text
      --  is then its list is expanded with D, otherwise and new list is
      --  created with D as the only element.

      ------------
      -- Append --
      ------------

      procedure Append (D : LALAnalysis.Basic_Decl) is
         Identifier : constant LALAnalysis.Identifier :=
           Get_First_Identifier_From_Declaration (D);
         use type LALAnalysis.Identifier;
      begin
         if Identifier /= LALAnalysis.No_Identifier
           and then Declarations.Contains (Identifier.Text)
         then
            Declarations.Reference (Identifier.Text).Include (D.As_Basic_Decl);
         else
            declare
               Decl_List : Reachable_Declarations_Hashed_Set.Set;
            begin
               Decl_List.Include (D);
               Declarations.Insert (Identifier.Text, Decl_List);
            end;
         end if;
      end Append;

   begin
      --  Do not add nodes of Error_Declaration type
      if Decl.Kind = LALCommon.Ada_Error_Decl then
         return;
      elsif Decl.Kind = LALCommon.Ada_Package_Decl then
         --  Nodes of type Package_Decl are nested packages and need to be
         --  expanded since they contain more declarations
         for D of List_And_Expand_Package_Declarations
           (Decl.As_Package_Decl, Incl_Private)
         loop
            Append (D);
         end loop;
      else
         Append (Decl.As_Basic_Decl);
      end if;
   end Add_Declaration;

   -----------------------------------------
   -- Get_Specification_Unit_Declarations --
   -----------------------------------------

   function Get_Specification_Unit_Declarations
     (Unit : LALAnalysis.Analysis_Unit; Incl_Private : Boolean)
      return Reachable_Declarations_Result.Map
   is
      Map : Reachable_Declarations_Result.Map;

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
                 (Decl         => Node.As_Basic_Decl, Declarations => Map,
                  Incl_Private => False);
            end if;
         end loop;
      end Add_Declarations;

      use type LALAnalysis.Ada_Node;
      use type LALAnalysis.Public_Part;
      use type LALAnalysis.Private_Part;
   begin
      if not Is_Specification_Unit (Unit) then
         return Map;
      end if;

      if Unit.Root /= LALAnalysis.No_Ada_Node
        and then Unit.Root.Kind = LALCommon.Ada_Compilation_Unit
        and then Unit.Root.As_Compilation_Unit.P_Decl.Kind =
          LALCommon.Ada_Package_Decl
      then
         declare
            Pkg_Decl : constant LALAnalysis.Package_Decl :=
              Unit.Root.As_Compilation_Unit.P_Decl.As_Package_Decl;
         begin
            if Pkg_Decl.F_Public_Part /= LALAnalysis.No_Public_Part then
               Add_Declarations
                 (Pkg_Decl.F_Public_Part.As_Public_Part.F_Decls);
            end if;

            if Incl_Private
              and then Pkg_Decl.F_Private_Part /= LALAnalysis.No_Private_Part
            then
               Add_Declarations
                 (Pkg_Decl.F_Private_Part.As_Private_Part.F_Decls);
            end if;
         end;
      end if;
      return Map;
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
      return Reachable_Declarations_Result.Map
   is
      Map : Reachable_Declarations_Result.Map;
      use type LALAnalysis.Ada_Node;
   begin
      if Node.Parent = LALAnalysis.No_Ada_Node then
         return Map;
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
                             (Decl => Decl.As_Basic_Decl, Declarations => Map,
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
                     Decl_Loop :
                     for Decl of Decl_Part.F_Decls loop

                        --  For a node in a Declarative_Part of a Package_Body
                        --  only the previously declared declarations are
                        --  visible.

                        if Decl.Kind in LALCommon.Ada_Basic_Decl
                          and then Decl.As_Basic_Decl = Stop_Decl
                        then

                           --  Allow recursion
                           if not (Decl.Kind in LALCommon.Ada_Package_Decl |
                                   LALCommon.Ada_Package_Body)
                           then
                              Add_Declaration
                                (Decl =>
                                   Decl.As_Basic_Decl.P_Defining_Name
                                 .P_Canonical_Part
                                 .P_Basic_Decl,
                                 Declarations => Map, Incl_Private => False);
                           end if;
                           exit Decl_Loop;
                        end if;

                        --  Do not add Package_Body
                        if Decl.Kind in LALCommon.Ada_Basic_Decl
                          and then Decl.Kind /= LALCommon.Ada_Package_Body
                        then
                           Add_Declaration
                             (Decl =>
                                Decl.As_Basic_Decl.P_Defining_Name
                              .P_Canonical_Part
                              .P_Basic_Decl,
                              Declarations => Map, Incl_Private => False);
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
                    (Decl =>
                       Parent.As_Basic_Decl.P_Defining_Name.P_Canonical_Part
                     .P_Basic_Decl,
                     Declarations => Map, Incl_Private => True);
               end if;
            end if;
         end loop;
      end;
      return Map;
   end Get_Local_Unit_Reachable_Declarations;

   --------------------------------
   -- Get_Reachable_Declarations --
   --------------------------------

   function Get_Reachable_Declarations
     (Identifier : LALAnalysis.Identifier;
      Units      : LALHelpers.Unit_Vectors.Vector)
      return Reachable_Declarations_Hashed_Set.Set
   is
      package LKSText renames Langkit_Support.Text;
      Identifier_Text : LKSText.Text_Type                 := Identifier.Text;
      Decls_List      : Reachable_Declarations_Hashed_Set.Set;
      Decls_Map       : Reachable_Declarations_Result.Map :=
        Get_Local_Unit_Reachable_Declarations (Identifier);
      use type LALAnalysis.Identifier;
   begin

      if Identifier = LALAnalysis.No_Identifier then
         return Decls_List;
      else
         Identifier_Text := Identifier.Text;
      end if;

      for Unit of Units loop
         declare
            Unit_Decls_Map : constant Reachable_Declarations_Result.Map :=
              Get_Specification_Unit_Declarations (Unit, False);
            Unit_Decls_Map_Cursor : Reachable_Declarations_Result.Cursor :=
              Unit_Decls_Map.First;
            use Reachable_Declarations_Result;
         begin
            while Has_Element (Unit_Decls_Map_Cursor) loop
               if Decls_Map.Contains (Key (Unit_Decls_Map_Cursor)) then
                  --  Merge both lists
                  declare
                     List : constant Reachable_Declarations_Hashed_Set.Set :=
                       Element (Unit_Decls_Map_Cursor);
                     List_Cursor : Reachable_Declarations_Hashed_Set.Cursor :=
                       List.First;
                     use Reachable_Declarations_Hashed_Set;
                  begin
                     while Has_Element (List_Cursor) loop
                        Decls_Map.Reference (Key (Unit_Decls_Map_Cursor)).
                          Include (Element (List_Cursor));
                        Next (List_Cursor);
                     end loop;
                  end;
               else
                  --  Add key and elements to Map
                  Decls_Map.Insert
                    (Key (Unit_Decls_Map_Cursor),
                     Element (Unit_Decls_Map_Cursor));
               end if;

               Next (Unit_Decls_Map_Cursor);
            end loop;
         end;
      end loop;

      if Decls_Map.Contains (Identifier_Text) then
         Decls_List := Decls_Map.Element (Identifier_Text);
      end if;

      return Decls_List;
   end Get_Reachable_Declarations;

   ------------------------
   -- Get_Parent_Package --
   ------------------------

   function Get_Parent_Packages
     (Node : LALAnalysis.Ada_Node'Class) return Parent_Packages_Vector.Vector
   is
      Parent_Pkgs : Parent_Packages_Vector.Vector;
   begin
      for P of Node.Parent.Parents loop
         if P.Kind in LALCommon.Ada_Package_Body | LALCommon.Ada_Package_Decl
         then
            Parent_Pkgs.Append (P.As_Basic_Decl);
         end if;
      end loop;
      return Parent_Pkgs;
   end Get_Parent_Packages;

   ----------------------------
   -- Get_Import_Suggestions --
   ----------------------------

   function Get_Import_Suggestions
     (Id              : LALAnalysis.Identifier;
      Reachable_Decls : Reachable_Declarations_Hashed_Set.Set)
      return Import_Suggestions_Vector.Vector
   is
      use type LALAnalysis.Basic_Decl;
      use type LALAnalysis.Identifier;
      use type LALAnalysis.Ada_Node;
      use type Ada.Containers.Count_Type;

      Id_Parent_Pkgs : Parent_Packages_Vector.Vector;
      Suggestions    : Import_Suggestions_Vector.Vector;
   begin

      if Id = LALAnalysis.No_Identifier
        or else Id.Parent = LALAnalysis.No_Ada_Node
        or else Reachable_Decls.Length = 0
      then
         return Suggestions;
      end if;

      Id_Parent_Pkgs := Get_Parent_Packages (Id);

      for D of Reachable_Decls loop
         declare
            Suggestion       : Import_Suggestion;
            Decl_Parent_Pkgs : constant Parent_Packages_Vector.Vector :=
              Get_Parent_Packages (D);
            use type LKSText.Unbounded_Text_Type;
         begin
            Suggestion.Declaration := D;

            if Id_Parent_Pkgs.Length = 0 then

               if Decl_Parent_Pkgs.Length = 0 then

                  --  If D is a standalone subprogram, then it does not need a
                  --  prefix, and the with clause is simply it's defining name.

                  Suggestion.Import_Text :=
                    LKSText.To_Unbounded_Text (D.P_Defining_Name.Text);
               else

                  --  Id is in a unit that does not have a parent package
                  --  (for instance, a standalone subprogram), the prefix must
                  --  contain all the parent packages of D but the with clause
                  --  must only contain the top level parent package of D.

                  for P of Decl_Parent_Pkgs loop
                     Suggestion.Prefix_Text :=
                       LKSText.To_Unbounded_Text (P.P_Defining_Name.Text) &
                       "." & Suggestion.Prefix_Text;
                  end loop;
                  Suggestion.Import_Text :=
                    LKSText.To_Unbounded_Text
                      (Decl_Parent_Pkgs.Last_Element.P_Defining_Name.Text);
               end if;
            else
               if Decl_Parent_Pkgs.Length = 0 then
                  Suggestion.Import_Text :=
                    LKSText.To_Unbounded_Text (D.P_Defining_Name.Text);
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

                        Suggestion.Prefix_Text :=
                          LKSText.To_Unbounded_Text
                            (P.P_Defining_Name.Text) &
                          "." & Suggestion.Prefix_Text;
                     end loop;
                  end if;
               else
                  for P of Decl_Parent_Pkgs loop
                     Suggestion.Prefix_Text :=
                       LKSText.To_Unbounded_Text (P.P_Defining_Name.Text) &
                       "." & Suggestion.Prefix_Text;
                  end loop;
                  Suggestion.Import_Text :=
                    LKSText.To_Unbounded_Text
                      (Decl_Parent_Pkgs.Last_Element.P_Defining_Name.Text);
               end if;
            end if;
            Suggestions.Append (Suggestion);
         end;
      end loop;

      return Suggestions;
   end Get_Import_Suggestions;

end Laltools.Refactor_Imports;
