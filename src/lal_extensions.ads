with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang; use Libadalang;
with Libadalang.AST; use Libadalang.AST;
with Libadalang.AST.Types; use Libadalang.AST.Types;

with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;

package LAL_Extensions is

   --  ???Perhaps we should move (some of?) this stuff into package
   --  Libadalang.AST.

   package Ada_Node_Vecs renames AST.Ada_Node_Vectors;

   function Childx
     (Node  : access Ada_Node_Type'Class;
      Index : Positive) return Ada_Node;
   --  Should this replace AST.Child???

   procedure Find_Iter
     (Node      : access Ada_Node_Type'Class;
      Predicate : not null access function
        (Node : access Ada_Node_Type'Class) return Boolean;
      Visit     : not null access procedure
        (Node : access Ada_Node_Type'Class));
   --  Iterate over the tree in depth-first search order, calling Visit for
   --  each node that satisfies the Predicate.

   procedure Find_Iter
     (Node      : access Ada_Node_Type'Class;
      Node_Kind : Ada_Node_Kind_Type;
      Visit     : not null access procedure
        (Node : access Ada_Node_Type'Class));
   --  Iterate over the tree in depth-first search order, calling Visit for
   --  each node whose kind is Node_Kind.

   function Find_All
     (Node      : access Ada_Node_Type'Class;
      Predicate : not null access function
        (Node : access Ada_Node_Type'Class) return Boolean)
      return Ada_Node_Vecs.Elements_Array;
   --  Same as above, except return an array of the relevant nodes

   function Find_All
     (Node      : access Ada_Node_Type'Class;
      Node_Kind : Ada_Node_Kind_Type) return Ada_Node_Vecs.Elements_Array;
   --  Same as above, except return an array of the relevant nodes

   function Id_Name
     (Nm : access Ada_Node_Type'Class)
     return W_Str with
     Pre => Kind (Nm) in Ada_Identifier | Ada_String_Literal;
   function L_Name
     (Nm : access Ada_Node_Type'Class)
     return W_Str with
     Pre => Kind (Nm) = Ada_Identifier;
   --  Text name of an identifier. The L_Name is converted to lower
   --  case.

   function Full_Name (Nm : Name) return W_Str;
   --  Returns the full expanded name

   function Get_Def_Name (Decl : Ada_Node) return Name;
   --  Returns the defining name of a declaration or body

   function Get_Aspects (Decl : Basic_Decl) return Aspect_Specification;
   --  Wrapper for F_Aspects functions

   function Vis_Part
     (Node : access Ada_Node_Type'Class) return Public_Part;
   --  Return the visible part of a package, generic package, task decl, or
   --  protected decl.

   function Priv_Part
     (Node : access Ada_Node_Type'Class) return Private_Part;
   --  Return the private part of a package, generic package, task decl, or
   --  protected decl.

   function Body_Decls
     (Node : access Ada_Node_Type'Class) return Declarative_Part;
   --  Return the declarative part of a body

   function Text_To_W_Str (X : Text_Type) return W_Str;
   --  Libadalang deals with Wide_Wide_Strings, whereas ASIS deals with
   --  Wide_Strings. We want to share code with ASIS tools at least for a
   --  while, so we convert Wide_Wide_Strings to Wide_Strings, and use
   --  Wide_Strings in libadalang-tools. This function does the conversion.

   function Short_Image (Node : Ada_Node) return String is
     (ASIS_UL.String_Utilities.To_UTF8
       (Text_To_W_Str
         (Text_Type'(Short_Image (Node)))));

end LAL_Extensions;
