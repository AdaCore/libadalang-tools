------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
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

with TGen.Types.Array_Types;
with TGen.Types.Constraints;  use TGen.Types.Constraints;
with TGen.Types.Record_Types; use TGen.Types.Record_Types;

package body TGen.Dependency_Graph is

   -----------------------
   -- Type_Dependencies --
   -----------------------

   function Type_Dependencies
     (T : SP.Ref; Transitive : Boolean := False) return Typ_Set
   is
      use TGen.Types.Array_Types;

      Res : Typ_Set;

      procedure Inspect_Variant (Var : Variant_Part_Acc);
      --  Include the types of the components defined in Var in the set of type
      --  on which T depends on, and inspect them transitively if needed.

      procedure Inspect_Variant (Var : Variant_Part_Acc) is
      begin
         if Var = null then
            return;
         end if;
         for Choice of Var.Variant_Choices loop
            for Comp of Choice.Components loop
               Res.Include (Comp);
               if Transitive or else Comp.Get.Kind = Anonymous_Kind then
                  Res.Union (Type_Dependencies (Comp, Transitive));
               end if;
            end loop;
            Inspect_Variant (Choice.Variant);
         end loop;
      end Inspect_Variant;

   begin
      case T.Get.Kind is
         when Anonymous_Kind =>
            Res.Include (As_Anonymous_Typ (T).Named_Ancestor);
            if Transitive then
               Res.Union (Type_Dependencies
                            (As_Anonymous_Typ (T).Named_Ancestor, Transitive));
            end if;
         when Array_Typ_Range =>
            declare
               Comp_Ty : constant SP.Ref := As_Array_Typ (T).Component_Type;
            begin
               Res.Include (Comp_Ty);
               if Transitive or else Comp_Ty.Get.Kind = Anonymous_Kind then
                  Res.Union (Type_Dependencies (Comp_Ty, Transitive));
               end if;
            end;

            --  Index type are discrete types, and thus do not depend on
            --  any type.

            for Idx_Typ of As_Array_Typ (T).Index_Types loop
                  Res.Include (Idx_Typ);
               if Idx_Typ.Get.Kind = Anonymous_Kind then
                  Res.Union (Type_Dependencies (Idx_Typ, Transitive));
               end if;
            end loop;
         when Non_Disc_Record_Kind =>
            for Comp_Typ of As_Nondiscriminated_Record_Typ (T).Component_Types
            loop
               Res.Include (Comp_Typ);
               if Transitive or else Comp_Typ.Get.Kind = Anonymous_Kind then
                  Res.Union (Type_Dependencies (Comp_Typ, Transitive));
               end if;
            end loop;
         when Function_Kind =>
            for Comp_Typ of As_Function_Typ (T).Component_Types
            loop
               Res.Include (Comp_Typ);
               if Transitive then
                  Res.Union (Type_Dependencies (Comp_Typ, Transitive));
               end if;
            end loop;
         when Disc_Record_Kind =>
            for Comp_Typ of As_Discriminated_Record_Typ (T).Component_Types
            loop
               Res.Include (Comp_Typ);
               if Transitive or else Comp_Typ.Get.Kind = Anonymous_Kind then
                  Res.Union (Type_Dependencies (Comp_Typ, Transitive));
               end if;
            end loop;

            --  Discriminant types are discrete types, and thus do not depend
            --  on any type.

            for Disc_Typ of As_Discriminated_Record_Typ (T).Discriminant_Types
            loop
               Res.Include (Disc_Typ);
               if Transitive or else Disc_Typ.Get.Kind = Anonymous_Kind then
                  Res.Union (Type_Dependencies (Disc_Typ, Transitive));
               end if;
            end loop;
            Inspect_Variant (As_Discriminated_Record_Typ (T).Variant);
         when others =>
            null;
      end case;
      return Res;
   end Type_Dependencies;

   -----------------
   -- Create_Node --
   -----------------

   procedure Create_Node (G : in out Graph_Type; New_Node : SP.Ref) is
   begin
      G.Nodes.Insert (New_Node);
      G.Succ.Insert (New_Node, Typ_Sets.Empty);
      G.Pred.Insert (New_Node, Typ_Sets.Empty);
   end Create_Node;

   -----------------
   -- Create_Edge --
   -----------------

   procedure Create_Edge (G : in out Graph_Type; From, To : SP.Ref) is
   begin
      G.Succ.Reference (From).Insert (New_Item => To);
      G.Pred.Reference (To).Insert (New_Item => From);
   end Create_Edge;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (G        : Graph_Type;
      Callback : access procedure (N : SP.Ref))
   is
      G_Copy : Graph_Type := G;
      Roots : Typ_List;
   begin
      --  Start by getting the roots of the DAG

      for Node of G_Copy.Nodes loop
         if G_Copy.Pred.Element (Node).Is_Empty then
            Roots.Append (Node);
         end if;
      end loop;

      --  The current roots have no dependencies:
      --     * Call callback on them
      --     * Consider their successors. If their successors have no
      --       predecessors left, add them to the roots list.

      while not Roots.Is_Empty loop
         declare
            Next_Roots : Typ_List := Typ_Lists.Empty;
         begin
            for Root of Roots loop
               --  TODO: improve the performance as set deletions are not
               --  constant operations.

               Callback (Root);
               G_Copy.Nodes.Delete (Root);
               for Succ of G_Copy.Succ (Root) loop
                  G_Copy.Pred.Reference (Succ).Delete (Root);
                  if G_Copy.Pred.Element (Succ).Is_Empty then
                     Next_Roots.Append (Succ);
                  end if;
               end loop;

               --  Delete this root in the list of pred / succ

               G_Copy.Pred.Delete (Root);
               G_Copy.Succ.Delete (Root);
            end loop;
            Roots := Next_Roots;
         end;
      end loop;

      --  Check that we traversed all of the nodes in the graph: if not, it
      --  means there is a cycle.

      if not G_Copy.Nodes.Is_Empty then
         raise Program_Error;
      end if;
   end Traverse;

   ----------
   -- Sort --
   ----------

   function Sort (Types : Typ_Sets.Set) return Typ_List
   is
      G : Graph_Type;
      Sorted_Types : Typ_List;

      procedure Append (T : SP.Ref);

      ------------
      -- Append --
      ------------

      procedure Append (T : SP.Ref) is
      begin
         Sorted_Types.Append (T);
      end Append;

   begin
      --  Create the nodes of the graph

      for T of Types loop
         Create_Node (G, T);
      end loop;

      --  Create the edges

      for T of Types loop
         for Dep of Type_Dependencies (T) loop

            --  Filter out type dependencies that don't belong to this
            --  package

            if Types.Contains (Dep) then
               Create_Edge (G, Dep, T);
            end if;
         end loop;
      end loop;

         --  Sort the types

      Traverse (G, Append'Access);
      return Sorted_Types;
   end Sort;

end TGen.Dependency_Graph;
