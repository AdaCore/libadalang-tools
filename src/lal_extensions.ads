with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang; use Libadalang;
with Libadalang.Analysis; use Libadalang.Analysis;

with Utils.String_Utilities; use Utils.String_Utilities;
with Utils.Vectors;

package LAL_Extensions is

   --  ???Perhaps we should move (some of?) this stuff into package
   --  Libadalang.Analysis.

   package Ada_Node_Vectors is new Utils.Vectors
     (Index_Type => Positive,
      Element_Type => Ada_Node,
      Elements_Array => Ada_Node_Array);
   subtype Ada_Node_Vector is Ada_Node_Vectors.Vector;

   function Childx
     (Node  : access Ada_Node_Type'Class;
      Index : Positive) return Ada_Node;
   --  Should this replace Analysis.Child???

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
      return Ada_Node_Array;
   --  Same as above, except return an array of the relevant nodes

   function Find_All
     (Node      : access Ada_Node_Type'Class;
      Node_Kind : Ada_Node_Kind_Type) return Ada_Node_Array;
   --  Same as above, except return an array of the relevant nodes

   function Token_Text (Tok : Token_Type) return W_Str;
   function L_Token_Text (Tok : Token_Type) return W_Str;
   --  Text of a token. The L_Token_Text is converted to lower case.

   function Id_Name
     (Nm : access Ada_Node_Type'Class)
     return W_Str with
     Pre => Kind (Nm) in Ada_Identifier |
       Ada_Int_Literal | Ada_Real_Literal |
       Ada_String_Literal | Ada_Char_Literal;
   function L_Name
     (Nm : access Ada_Node_Type'Class)
     return W_Str with
     Pre => Kind (Nm) in Ada_Identifier | Ada_String_Literal;
   --  Text name of an identifier. The L_Name is converted to lower
   --  case.

   function Label_Name
     (L : access Ada_Node_Type'Class)
     return W_Str with
     Pre => Kind (L) = Ada_Label;

   function Full_Name (Nm : Name) return W_Str;
   function L_Full_Name (Nm : Name) return W_Str;
   --  Returns the full expanded name. The L_Full_Name is converted to lower
   --  case.  ????????????????That comment is wrong.  So is the name.

   function Get_Def_Name (Decl : Ada_Node) return Name;
   --  Returns the defining name of a declaration or body

   function Get_Aspects (Decl : Basic_Decl) return Aspect_Spec;
   --  Wrapper for F_Aspects functions

   function G_Formal_Part
     (Node : access Ada_Node_Type'Class) return Generic_Formal_Part;
   --  Return the generic formal part of a generic unit.

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

   function Is_Program_Unit (Node : Ada_Node) return Boolean;
   --  True if Node is a program unit as defined in the Ada RM

   function Get_Subp_Spec (Node : Ada_Node) return Subp_Spec;
   --  Return F_Subp_Spec

   function Adds_New_Nesting_Level (Node : Ada_Node) return Boolean;
   --  True if Node should be counted as a nesting level for the purposes of
   --  the Construct_Nesting metric in gnatmetric.

   function Short_Image (Node : Ada_Node) return String is
     (Utils.String_Utilities.To_UTF8
       (Text_To_W_Str
         (Text_Type'(Short_Image (Node)))));

   function P_Defining_Name
     (Node : access Basic_Decl_Type'Class) return Ada_Node is
      (Ada_Node (Name'(P_Defining_Name (Node))));

end LAL_Extensions;
