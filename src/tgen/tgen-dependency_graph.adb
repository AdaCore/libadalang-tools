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

package body TGen.Dependency_Graph is

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

end TGen.Dependency_Graph;
