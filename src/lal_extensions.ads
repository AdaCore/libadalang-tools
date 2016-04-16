with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang; use Libadalang;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.AST; use Libadalang.AST;
with Libadalang.AST.Types; use Libadalang.AST.Types;

package LAL_Extensions is

   --  ???Perhaps we should move (some of?) this stuff into package
   --  Libadalang.AST.

   package Ada_Node_Vecs renames AST.Ada_Node_Vectors;

   function Childx
     (Node  : access Ada_Node_Type'Class;
      Index : Natural) return Ada_Node;
   --  Should this replace AST.Child???

   procedure Find_Iter
     (Node      : Ada_Node;
      Predicate : not null access function (Node : Ada_Node) return Boolean;
      Visit     : not null access procedure (Node : Ada_Node));
   --  Iterate over the tree in depth-first search order, calling Visit for
   --  each node that satisfies the Predicate.

   procedure Find_Iter
     (Node      : Ada_Node;
      Node_Kind : Ada_Node_Type_Kind;
      Visit     : not null access procedure (Node : Ada_Node));
   --  Iterate over the tree in depth-first search order, calling Visit for
   --  each node whose kind is Node_Kind.

   function Find_All
     (Node      : Ada_Node;
      Predicate : not null access function (Node : Ada_Node) return Boolean)
      return Ada_Node_Vecs.Elements_Array;
   --  Same as above, except return an array of the relevant nodes

   function Find_All
     (Node      : Ada_Node;
      Node_Kind : Ada_Node_Type_Kind) return Ada_Node_Vecs.Elements_Array;
   --  Same as above, except return an array of the relevant nodes

   function Full_Name (Nm : Name; Unit : Analysis_Unit) return Text_Type;
   --  Returns the full expanded name

   function Get_Name (Decl : Ada_Node) return Name;
   --  Returns the defining name of a declaration or body

   function Get_Aspects (Decl : Basic_Decl) return Aspect_Specification;
   --  Wrapper for F_Aspects functions

end LAL_Extensions;
