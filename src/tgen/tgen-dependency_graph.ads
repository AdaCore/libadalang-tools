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

with Ada.Containers.Ordered_Maps;

with TGen.Context; use TGen.Context;
with TGen.Types;   use TGen.Types;

package TGen.Dependency_Graph is

   package Typ_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => SP.Ref,
        Element_Type => Typ_Set,
        "="          => Typ_Sets."=");
   subtype Typ_Map is Typ_Maps.Map;

   function Type_Dependencies
     (T : SP.Ref; Transitive : Boolean := False) return Typ_Set;
   --  Return all the types that T needs to have visibility on (i.e. index/
   --  component / discriminant types). If transitive is True, this returns
   --  the transitive closure of types on which self depends.

   type Graph_Type is record
      Nodes      : Typ_Set;
      Succ, Pred : Typ_Map;
   end record;

   procedure Create_Node (G : in out Graph_Type; New_Node : SP.Ref);

   procedure Create_Edge (G : in out Graph_Type; From, To : SP.Ref)
   with Pre => G.Nodes.Contains (From) and then G.Nodes.Contains (To);

   procedure Traverse
     (G : Graph_Type; Callback : access procedure (N : SP.Ref));
   --  Traverse the graph in the topological order and call callback on every
   --  node.

   function Sort (Types : Typ_Sets.Set) return Typ_List;
   --  Sort the types in their dependency order

end TGen.Dependency_Graph;
