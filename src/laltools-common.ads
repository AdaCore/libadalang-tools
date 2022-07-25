------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021-2022, AdaCore                   --
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
--  This package contains LAL_Tools common utilities to be used by other
--  packages

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

with Ada.Strings.Unbounded;

with GNATCOLL.Traces;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;
with Libadalang.Helpers;

package Laltools.Common is

   function "<" (Left, Right : Defining_Name) return Boolean is
     (Left.Text < Right.Text
      or else (Left.Text = Right.Text
        and then Left.Full_Sloc_Image < Right.Full_Sloc_Image));
   --  The Ordered_Maps is using the "<" in its Equivalent_Keys function:
   --  this is too basic and it will assume that Left.Text = Right.Text implies
   --  Left = Right which is wrong.
   --  If Left.Text = Right.Text then Full_Sloc_Image will sort first by
   --  file and then by Sloc (first by line and then by column).

   function "<"
     (Left, Right : Source_Location_Range)
      return Boolean;
   --  Checks if L is < than R, first based on the line number and then on
   --  the column number

   function "<" (Left, Right : Base_Id) return Boolean is
     (Left.Text < Right.Text
      or else Left.Sloc_Range < Right.Sloc_Range);
   --  Use the Sloc to compare two Base_Id nodes when their text is equal.

   function Node_Equal
     (Left, Right : Libadalang.Analysis.Ada_Node)
      return Boolean is (Libadalang.Analysis.Full_Sloc_Image (Left)
                         = Libadalang.Analysis.Full_Sloc_Image (Right));
   --  Libadalang.Analysis."=" is not enough to fully detect duplicates

   type Analysis_Unit_Array_Access is access Analysis_Unit_Array;

   package Ada_List_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Natural,
      Element_Type => Ada_List'Class,
      "="          => "=");

   subtype Ada_List_Vector is Ada_List_Vectors.Vector;

   procedure Append_If_Not_Null
     (Vector : in out Ada_List_Vector;
      List   : Ada_List'Class);
   --  Checks if Ada_List.Is_Null and if not, appends it to Vector

   function Hash (Node : Ada_List'Class) return Ada.Containers.Hash_Type is
     (Hash (Node.As_Ada_Node));

   package Ada_List_Hashed_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type => Ada_List'Class,
      Hash => Hash,
      Equivalent_Elements => "=",
      "=" => "=");

   subtype Ada_List_Hashed_Set is Ada_List_Hashed_Sets.Set;

   package Base_Id_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Base_Id,
      "="          => "=");

   package Basic_Decl_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Basic_Decl,
      "="          => "=");

   subtype Basic_Decl_Vector is Basic_Decl_Vectors.Vector;

   package Bodies_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Defining_Name,
      "="          => "=");

   package Declarative_Part_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Natural,
      Element_Type => Declarative_Part'Class,
      "="          => "=");

   subtype Declarative_Part_Vector is Declarative_Part_Vectors.Vector;

   package Node_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Ada_Node,
      "="          => "=");

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Ada_Node,
      Hash                => Hash,
      Equivalent_Elements => Node_Equal,
      "="                 => Node_Equal);

   package References_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Base_Id,
      "<"          => "<",
      "="          => "=");

   package References_By_Subprogram is new Ada.Containers.Ordered_Maps
     (Key_Type     => Defining_Name,
      Element_Type => References_Sets.Set,
      "<"          => "<",
      "="          => References_Sets."=");

   package Source_Location_Range_Sets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Source_Location_Range,
        "<"          => "<",
        "="          => "=");

   subtype Source_Location_Range_Set is
     Source_Location_Range_Sets.Set;

   package Source_Location_Range_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Source_Location_Range_Sets.Set,
        "<"          => "<",
        "="          => Source_Location_Range_Sets."=");

   subtype Source_Location_Range_Map is
     Source_Location_Range_Maps.Map;

   function Compilation_Unit_Hash (Comp_Unit : Compilation_Unit)
                                   return Ada.Containers.Hash_Type;
   --  Casts Comp_Unit as Ada_Node and uses Hash from Libadalang.Analysis.
   --  This is convenient for containers with Compilation_Unit elements.

   function Contains
     (Token   : Token_Reference;
      Pattern : Wide_Wide_String;
      As_Word : Boolean;
      Span    : out Source_Location_Range)
      return Boolean;
   --  Return True if the Token text contains Pattern and set position in Span.
   --  Checks whether the Token's Pattern is delimited by word delimiters
   --  if As_Word is True.

   function Count_Param_Spec_Parameters
     (Param_Spec : Libadalang.Analysis.Param_Spec'Class)
      return Natural;
   --  Returns the amount of parameters Param_Spec has

   function Count_Subp_Param_Specs (Subp_Params : Params'Class) return Natural;
   --  Returns the amount of Param_Spec nodes 'Subp_Params' has.

   function Count_Subp_Parameters (Subp_Params : Params'Class) return Natural;
   --  Returns the amount of parameters 'Subp_Params' has.

   function Expand_SLOC_Range
     (Node : Ada_Node'Class)
      return Source_Location_Range;
   --  Expand Node's Source_Location_Range by including leading and trailing
   --  whitespaces, and empty lines immediately after.

   function Expand_SLOC_Range
     (Unit       : Analysis_Unit;
      SLOC_Range : Source_Location_Range)
      return Source_Location_Range;
   --  Expand SLOC_Range it by including leading and trailing whitespaces,
   --  and empty lines immediately after.

   package Source_Location_Range_Ordered_Sets is
     new Ada.Containers.Ordered_Sets
       (Element_Type => Source_Location_Range);

   subtype Source_Location_Range_Ordered_Set is
     Source_Location_Range_Ordered_Sets.Set;

   function Expand_SLOC_Ranges
     (Unit        : Analysis_Unit;
      SLOC_Ranges : Source_Location_Range_Ordered_Set)
      return Source_Location_Range_Ordered_Set;
   --  For each Source_Location_Range of SLOC_Range, expands it by including
   --  leading and trailing whitespaces, and empty lines immediately after.

   procedure Find_All_References
     (Node     : Defining_Name'Class;
      Units    : Analysis_Unit_Array;
      Callback : not null access procedure
        (Reference : Ref_Result;
         Stop      : in out Boolean));
   --  TODO: Imprecise fallback makes sense here?
   --  Wrapper around Libadalang.Analysis.P_Find_All_References.
   --  Calls Callback for each reference found or until Stop is True.
   --  Callback is responsible for defining the stop criteria.
   --  Does nothing if Node is a No_Defining_Name.

   function Find_All_References_For_Renaming
     (Definition : Defining_Name;
      Units      : Analysis_Unit_Array)
      return Base_Id_Vectors.Vector
     with Pre => not Definition.Is_Null;
   --  Returns a vector with all references of Definition.
   --  Depending if Definition is associated to a parameter spec or to a
   --  subprogram, the return vector will include returns of
   --  Find_All_Param_References_In_Subp_Hierarchy and
   --  Find_All_Subp_References_In_Subp_Hierarchy.

   function Find_All_Param_References_In_Subp_Hierarchy
     (Param_Definition : Defining_Name;
      Units            : Analysis_Unit_Array)
      return Base_Id_Vectors.Vector
     with Pre => not Param_Definition.Is_Null
     and then Param_Definition.Parent.Parent.Kind in Ada_Param_Spec_Range;
   --  Retruns a vector with all references of Param_Definition.
   --  If Param_Definition is associated to a Param_Spec in a subprogram
   --  'Foo' that is a primitive of a type, then the vector includes refereces
   --  to the associated parameter spec of supbprograms that 'Foo' is
   --  overriding or that is being overridden by.

   function Find_All_Subp_References_In_Subp_Hierarchy
     (Subp  : Basic_Decl;
      Units : Analysis_Unit_Array)
      return Base_Id_Vectors.Vector
     with Pre => Is_Subprogram (Subp);
   --  Retruns a vector with all references of Subp, and if Subp is
   --  a primitive subrogram of a type, then the vector includes references of
   --  supbprograms that Definition is overriding or that is being overridden
   --  by.

   function Find_Canonical_Part
     (Definition         : Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise_Fallback : Boolean := False)
      return Defining_Name;
   --  Wrapper around P_Canonical_Part that returns null if canonical part
   --  is name itself. It also catches Property_Error and reports it in traces.

   procedure Find_Matching_Parents
     (Node     : Ada_Node'Class;
      Match    : not null access function
        (Node : Ada_Node'Class) return Boolean;
      Callback : not null access procedure
        (Parent : Ada_Node;
         Stop   : in out Boolean));
   --  Iterates through the parents of Node and calls Callback on the parents
   --  where Match returns True. This iterative process stops if Callback sets
   --  Stop to True.

   function Is_Scopes_Owner
     (Node : Ada_Node'Class)
      return Boolean;
   --  Checks if Candidate is a Declarative_Part owner, an Expr_Function that
   --  constains a Decl_Expr, or a declaration that can have a Params node.

   function Get_Params (Node : Ada_Node'Class) return Params;
   --  If Node is a Params owner, returns it. Otherwise, returns null.
   --  In this context, a Params owner is a subprogram, Entry_Decl, Entry_Body
   --  or Accept_Stmt whose spec has a Params node.

   function Find_Other_Part
     (List : Param_Spec_List'Class)
      return Param_Spec_List;
   --  If List's parent declaration has another part, finds its
   --  Param_Spec_List.

   function Find_First_Common_Parent
     (Start_Node : Ada_Node'Class;
      End_Node   : Ada_Node'Class;
      With_Self  : Boolean := True)
      return Ada_Node
     with Post => (if Start_Node.Unit = End_Node.Unit then
                     not Find_First_Common_Parent'Result.Is_Null);
   --  Finds the first common parent of Start_Node and End_Node.
   --  Returns No_Ada_Node if Start_Node and End_Node don't have common
   --  parents.

   function Find_Enclosing_Declarative_Parts
     (Node : Ada_Node'Class)
      return Ada_List_Hashed_Set
     with Post => (for all List of Find_Enclosing_Declarative_Parts'Result =>
                     List.Kind in Ada_Ada_Node_List_Range
                                     | Ada_Basic_Decl_List_Range);
   --  Finds the Declarative_Parts or Decl_Expr of the first scope visible by
   --  Node. In this context, scope is the nearest union of Ada_List'Class
   --  nodes where declarations can be done.
   --  So a scope can be the union of a Declarative_Part, a Basic_Decl_List of
   --  a Decl_Expr (in Ada 2022, Expr_Function can have a Decl_Expr node) or a
   --  Param_Spec_List.
   --  Returns an empty vector if Node.Is_Null.
   --
   --  Example:
   --
   --     procedure Foo (B : Bar);
   --     procedure Foo (B : Bar) is
   --        C : Corge;
   --     begin
   --        null;
   --        declare
   --           D : Delta;
   --        begin
   --           null;
   --        end;
   --     end Foo;
   --
   --     When called on C, B or or the first null, it returns Foo's
   --     Declarative_Part.
   --
   --     procedure Foo (A : Bar);
   --     procedure Foo (A : Bar) is
   --        function Qux (B, C : Integer) return Integer (B + C);
   --     begin
   --        null;
   --     end Foo;
   --
   --     When called on B returns empty since the first scope only contains
   --     Qux's Param_Spec_List.

   function Find_Enclosing_Param_Spec_Lists
     (Node : Ada_Node'Class)
      return Ada_List_Hashed_Set
     with Post => (for all List of Find_Enclosing_Param_Spec_Lists'Result =>
                     List.Kind in Ada_Param_Spec_List_Range);
   --  Finds the Param_Spec_List associated to the first scope visible by Node.
   --  In this context, scope is the nearest union of Ada_List'Class nodes
   --  where declarations can be done. So a scope can be the union of a
   --  Declarative_Part, a Basic_Decl_List of a Decl_Expr (in Ada 2022,
   --  Expr_Function can have a Decl_Expr node) or a Param_Spec_List.
   --  Returns an empty vector if Node.Is_Null.
   --
   --  Example:
   --
   --     procedure Foo (B : Bar);
   --     procedure Foo (B : Bar) is
   --        C : Corge;
   --     begin
   --        null;
   --        declare
   --           D : Delta;
   --        begin
   --           null;
   --        end;
   --     end Foo;
   --
   --     When called on C, B or or the first null, it returns Foo's
   --     Param_Spec_List.
   --     When called on D or the second null, returns an empty set since
   --     the declare block (first visible scope) does not have an associated
   --     Param_Spec_List.

   function Find_Enclosing_Scopes
     (Node : Ada_Node'Class)
      return Ada_List_Hashed_Set
     with Post => (for all List of Find_Enclosing_Scopes'Result =>
                     List.Kind in Ada_Ada_Node_List_Range
                                    | Ada_Basic_Decl_List_Range
                                    | Ada_Param_Spec_List_Range);
   --  Union between Find_Enclosing_Declarative_Parts and
   --  Find_Enclosing_Param_Spec_Lists.

   function Find_Visible_Declarative_Parts
     (Node : Ada_Node'Class)
      return Ada_List_Hashed_Set
     with Post => (for all List of Find_Visible_Declarative_Parts'Result =>
                     List.Kind in Ada_Ada_Node_List_Range
                                   | Ada_Basic_Decl_List_Range);
   --  Find all Declarative_Part and Decl_Expr nodes visible by Node
   --  (excluding the ones visible by a use clause).
   --  Returns an empty vector if Node.Is_Null.

   function Find_Visible_Param_Spec_Lists
     (Node : Ada_Node'Class)
      return Ada_List_Hashed_Set
     with Post => (for all List of Find_Visible_Param_Spec_Lists'Result =>
                     List.Kind in Ada_Param_Spec_List_Range);
   --  Find all Param_Spec_List visible by Node

   function Find_Visible_Scopes
     (Node : Ada_Node'Class)
      return Ada_List_Hashed_Set
     with Post => (for all List of Find_Visible_Scopes'Result =>
                     List.Kind in Ada_Ada_Node_List_Range
                                    | Ada_Basic_Decl_List_Range
                                    | Ada_Param_Spec_List_Range);
   --  Union between Find_Visible_Declarative_Parts and
   --  Find_Visible_Param_Spec_Lists.

   function Find_Nested_Scopes (Node : Ada_Node'Class)
                                return Declarative_Part_Vectors.Vector;
   --  Finds all scopes that have visibility if Node and that are nested
   --  in Node's own scope.

   function Find_Next_Part
     (Definition         : Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise_Fallback : Boolean := False)
      return Defining_Name;
   --  Wrapper around P_Next_Part that returns No_Defining_Name if next part
   --  is name itself. It also catches Property_Error and reports it in traces.

   function Find_Previous_Part
     (Definition         : Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise_Fallback : Boolean := False)
      return Defining_Name;
   --  Wrapper around P_Previous_Part that returns No_Defining_Name if previous
   --  part is name itself. It also catches Property_Error and reports it
   --  in traces.

   function Find_Other_Part_Fallback
     (Definition : Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle)
      return Defining_Name;
   --  Attempt to find the other part of a definition manually, with
   --  simple heuristics that look at the available entities with matching
   --- names and profiles.
   --  This should be called only if straightforward Libadalang calls
   --  have failed.

   function Find_Next_Part_For_Decl
     (Decl               : Basic_Decl;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise_Fallback : Boolean := False)
      return Basic_Decl;
   --  Wrapper around P_Next_Part_For_Decl that returns No_Basic_Decl if
   --  next part is name itself. It also catches Property_Error and reports
   --  it in traces.

   function Find_Subp_Body (Subp : Basic_Decl'Class) return Base_Subp_Body
     with Pre => Is_Subprogram (Subp);
   --  If Subp is of kind Ada_Subp_Decl or Ada_Generic_Subp_Decl then
   --  returns its body part, if is exists. Otherwise return No_Base_Subp_Body.

   function Get_Basic_Decl_Header_SLOC_Range
     (Decl : Basic_Decl'Class)
      return Source_Location_Range;
   --  Returns the Source_Location_Range of Decl's header or
   --  No_Source_Location_Range if it does not exist.

   function Get_Compilation_Unit
     (Node : Ada_Node'Class)
      return Compilation_Unit;
   --  Returns the Compilation_Unit associated to Node

   package Compilation_Unit_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Compilation_Unit,
      "="          => "=");

   subtype Compilation_Unit_Vector is Compilation_Unit_Vectors.Vector;

   function Get_Compilation_Units
     (Analysis_Unit : Libadalang.Analysis.Analysis_Unit)
      return Compilation_Unit_Vector;
   --  Returns a vector with all Compilation_Unit nodes of Analysis_Unit

   function Get_Insert_With_Location
     (Node      : Compilation_Unit'Class;
      Pack_Name : Text_Type;
      Last      : out Boolean)
      return Source_Location;
   --  Returns the Source_Location where to insert a with clause for Pack_Name
   --  or No_Source_Location if Pack_Name is already withed.
   --  Last indicates if the location is after the last with/use clause.

   function Get_Decl_Block_Declarative_Part (Decl_B : Decl_Block)
                                             return Declarative_Part;
   --  Gets the Declarative_Part of a Decl_Block

   function Get_Decl_Block_Decls (Decl_B : Decl_Block) return Ada_Node_List;
   --  Gets the Ada_Node_List of a Declarative_Part associated to a Decl_Block

   function Get_Dotted_Name_First_Name
     (Dotted_Name : Libadalang.Analysis.Dotted_Name'Class)
      return Name;
   pragma Unreferenced (Get_Dotted_Name_First_Name);
   --  Gets Dotted_Name first name.
   --  Example: For a Dotted_Name Foo.Bar, return the name Foo.

   function Get_Dotted_Name_Definitions
     (Dotted_Name : Libadalang.Analysis.Dotted_Name'Class)
      return Defining_Name_Array;
   --  Gets a Defining_Name_Array with the Defining_Name of each name of
   --  Dotted_Name, in the reverse order.
   --  Example: For a Dotted_Name Foo.Bar.Baz, returns a Defining_Name_Array
   --  with Baz's, Bar's and Foo's Defining_Name.

   function Is_Declarative_Part_Owner
     (Node : Ada_Node'Class)
      return Boolean;
   --  Checks if Node can have a Declarative_Part child

   function Is_Decl_Expr_Owner
     (Node : Ada_Node'Class)
      return Boolean
     with Post => (if Is_Decl_Expr_Owner'Result then
                     Node.Kind in Ada_Expr_Function_Range);
   --  Checks if Node is an Expr_Function with a Decl_Expr child

   function Is_Params_Owner
     (Node : Ada_Node'Class)
      return Boolean;
   --  Checks if Node can have a Params child

   function Is_Whole_Line_Comment
     (Token : Token_Reference)
      return Boolean;
   --  Checks if Token is a whole line Ada_Comment

   function Get_Ada_Analysis_Units
     (Source_Provider  : Libadalang.Helpers.Source_Provider;
      Analysis_Context : Libadalang.Analysis.Analysis_Context)
      return Analysis_Unit_Array;
   --  Get all Ada Analysis Units from Source_Provider using Analysis_Context

   function Get_Declarative_Part
     (Node         : Ada_Node'Class;
      Private_Part : Boolean := False)
      return Declarative_Part
     with Pre => not Node.Is_Null
                 and then (Is_Declarative_Part_Owner (Node)
                           or else Node.Kind in Ada_Handled_Stmts_Range);
   --  Returns the Declarative_Part of Node. If Node.Kind is
   --  Ada_Base_Package_Decl | Ada_Protected_Def_Range | Ada_Task_Def_Range,
   --  it might have a Private_Part node. If Private_Part is True, then
   --  the Private_Part is returned, which can be null is it does not exist.

   function Get_Declarative_Parts
     (Node : Ada_Node'Class)
      return Declarative_Part_Vector
     with Pre  => not Node.Is_Null and then Is_Declarative_Part_Owner (Node),
          Post => not Declarative_Part_Vectors.Is_Empty
                        (Get_Declarative_Parts'Result)
                  and then (for all Decl_Part of
                              Get_Declarative_Parts'Result =>
                                not Decl_Part.Is_Null);
   --  Returns a vector with the Declarative_Part, Public_Part and Private_Part
   --  of Owner, if they exist.

   function Get_Defining_Name_Id (Definition : Defining_Name)
                                  return Identifier;
   --  Gets the Identifier of Definition. If Definition is associated to a
   --  Dotted_Name them return the suffix.

   function Get_First_Identifier_From_Declaration (Decl : Basic_Decl'Class)
                                                   return Identifier;
   --  Return the first identifier found in a basic declaration.

   function Get_Last_Name (Name_Node : Name)
                           return Unbounded_Text_Type;
   --  Return the last name, for example if name is A.B.C then return C.

   function Get_Name_As_Defining (Name_Node : Name)
                                  return Defining_Name;
   --  Wrapper around P_Enclosing_Defining_Name that returns No_Defining_Name
   --  if Name_Node is No_Name or not a Defining_Name.

   function Get_Enclosing_Declarative_Part
     (Node : Ada_Node'Class)
      return Declarative_Part;
   --  Finds Node's nearest visible declarative part

   function Get_Node_As_Name (Node : Ada_Node) return Name;
   --  Wrapper around As_Name that returns No_Name if Node is not a Name.

   function Get_Package_Body_Declative_Part (Pkg_Body : Package_Body)
                                             return Declarative_Part;
   --  Gets the Declarative_Part associated to a Package_Body.

   function Get_Package_Body_Decls (Pkg_Body : Package_Body)
                                    return Ada_Node_List;
   --  Gets the Ada_Node_List of a Declarative_Part associated to a
   --  Package_Body.

   function Get_Package_Declarative_Parts
     (Pkg_Decl : Base_Package_Decl'Class)
      return Declarative_Part_Vectors.Vector;
   --  Gets all the Declarative_Parts associated to a Base_Package_Decl
   --  (public, private and body declarative parts).

   function Get_Package_Declarative_Parts
     (Pkg_Body : Package_Body)
      return Declarative_Part_Vectors.Vector
   is (Get_Package_Declarative_Parts
       (Pkg_Body.P_Canonical_Part.As_Base_Package_Decl));
   --  Gets all the Declarative_Parts associated to a Package_Body
   --  (public, private and body declarative parts).

   function Get_Package_Decls
     (Pkg_Decl : Base_Package_Decl'Class)
      return Ada_List_Vector;
   --  Gets all the Ada_Node_Lists of the Declarative_Parts associated to a
   --  Base_Package_Decl (public, private and body declarative parts).

   function Get_Package_Decls
     (Pkg_Body : Package_Body)
      return Ada_List_Vector
   is (Get_Package_Decls (Pkg_Body.P_Canonical_Part.As_Base_Package_Decl));
   --  Gets all the Ada_Node_Lists of the Declarative_Parts associated to a
   --- Package_Body (public, private and body declarative parts).

   function Get_Package_Decl_Private_Declarative_Part
     (Pkg_Decl : Base_Package_Decl'Class)
      return Declarative_Part;
   --  Gets the private Declarative_Part associated to a Package_Body

   function Get_Package_Decl_Private_Decls
     (Pkg_Decl : Base_Package_Decl'Class)
      return Ada_Node_List;
   --  Gets the Ada_Node_List of the private Declarative_Part associated to a
   --  Package_Body, if it exists.

   function Get_Package_Decl_Public_Declarative_Part
     (Pkg_Decl : Base_Package_Decl'Class)
      return Declarative_Part;
   --  Gets the public Declarative_Part associated to a Base_Package_Decl

   function Get_Package_Decl_Public_Decls
     (Pkg_Decl : Base_Package_Decl'Class)
      return Ada_Node_List;
   --  Gets the Ada_Node_List of the the public Declarative_Part associated to
   --  a Package_Decl.

   function Get_Param_Spec_Index (Target : Param_Spec) return Positive
     with Pre => not Target.Is_Null;
   --  Returns the index of 'Target' regardind its parent Param_Spec_List

   function Get_Parameter_Absolute_Index
     (Target : Defining_Name)
      return Natural
     with Pre => Target.Parent.Parent.Kind = Ada_Param_Spec;
   --  Returns the index of 'Target' regarding all parameters os its parent
   --  subprogram.

   function Get_Parameter_Name
     (Parameters      : Params'Class;
      Parameter_Index : Positive)
      return Text_Type;
   --  TODO

   function Get_Parameter_Name
     (Subp            : Basic_Decl'Class;
      Parameter_Index : Positive)
      return Text_Type
     with Pre => Is_Subprogram (Subp);
   --  Returns the name of the parameters associated to 'Parameter_Index'.
   --  Is 'Parameter_Index' is > than the amount of parameters 'Subp' has, then
   --  return an empty Text_Type.

   function Get_Subp_Body_Declarative_Part (Subp_B : Subp_Body)
                                            return Declarative_Part;
   --  Gets the Declarative_Part associated to a Subp_Body.

   function Get_Subp_Body_Decls (Subp_B : Subp_Body)
                                 return Ada_Node_List;
   --  Gets the Ada_Node_List of a Declarative_Part associated to a Subp_Body

   function Get_Subp_Params (Subp : Basic_Decl'Class) return Params
     with Inline,
          Pre => Is_Subprogram (Subp)
                 or else (not Subp.Is_Null
                          and then Subp.Kind in
                            Ada_Generic_Subp_Instantiation);
   --  Gets the Params node associated to Subp, if it exists.
   --  If it doesn't exist returns No_Params.

   function Get_Subp_Spec (Subp : Basic_Decl'Class) return Base_Subp_Spec
     with Inline,
          Pre => Is_Subprogram (Subp)
                 or else (not Subp.Is_Null
                          and then Subp.Kind in
                            Ada_Generic_Subp_Instantiation);
   --  Gets the Subp_Spec node associated to Subp

   function Get_Subp_Spec_Params
     (Subp_Spec : Base_Subp_Spec'Class)
      return Params;
   --  Gets the Params node associated to Subp_Spec, if it exists.
   --  If it doesn't exist returns No_Params.

   function Get_Task_Body_Declarative_Part (Task_B : Task_Body)
                                            return Declarative_Part;
   --  Gets the Declarative_Part associated to a Task_Body.

   function Get_Task_Body_Decls (Task_B : Task_Body) return Ada_Node_List;
   --  Gets the Ada_Node_List of a Declarative_Part associated to a Task_Body.

   function Get_Use_Units_Public_Parts
     (Node : Ada_Node'Class)
      return Declarative_Part_Vectors.Vector;
   --  Gets all public Declarative_Parts of the units used by Node's unit

   function Get_Used_Units
     (Node : Compilation_Unit'Class)
      return Compilation_Unit_Array;
   --  Returns a Compilation_Unit_Array with all the Compilation_Unit
   --  whose Node has a use clause for. If Node is null, then returns an empty
   --  Compilation_Unit_Array. The return array does not contain null
   --  Compilation_Units.

   procedure Insert
     (Map     : in out Source_Location_Range_Map;
      Key     : String;
      Element : Source_Location_Range);
   --  Safely inserts a new 'Key : Element' in 'Map'. If Element =
   --  No_Source_Location_Range then it is NOT inserted.

   function Is_Access_Ref (Node : Ada_Node) return Boolean;
   --  Return True if the node or the dotted name is an access reference.

   function Is_Call
     (Node      : Ada_Node'Class;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : in out Boolean) return Boolean;
   --  Check if a node is a call and an identifier. Enum literals
   --  in DottedName are excluded.

   function Is_Constant (Node : Basic_Decl) return Boolean;
   --  Return True if the decl contains the constant keyword.

   function Is_Definition_Without_Separate_Implementation
     (Definition : Defining_Name)
      return Boolean;
   --  Return True if the definition given is a subprogram that does not call
   --  for a body, ie a "is null" procedure, an expression function, or an
   --  abstract subprogram.

   function Is_End_Label (Node : Ada_Node) return Boolean
   is
     (not Node.Parent.Is_Null
      and then (Node.Parent.Kind in Ada_End_Name
        or else (Node.Parent.Kind in Ada_Dotted_Name
          and then not Node.Parent.Parent.Is_Null
          and then Node.Parent.Parent.Kind in
            Ada_End_Name)));
   --  Return True if the node belongs to an end label node.
   --  Used to filter out end label references.

   function Is_Enum_Literal
     (Node      : Ada_Node'Class;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : in out Boolean) return Boolean;
   --  Check if a node is an enum literal.

   function Is_Renamable (Node : Ada_Node'Class) return Boolean;
   --  A node is renamable only if a precise definition is found.

   function Is_Structure (Node : Basic_Decl) return Boolean;
   --  Return True if the type contains a record part.

   function Is_Subprogram (Decl : Basic_Decl'Class) return Boolean is
     (not Decl.Is_Null
      and then (Decl.P_Is_Subprogram
                or else Decl.Kind in Ada_Generic_Subp_Decl_Range)
      and then not (Decl.Kind in Ada_Enum_Literal_Decl_Range));
   --  Checks if Decl is a subprogram excluding enum literals

   function Is_Type_Derivation (Node : Ada_Node) return Boolean
   is
     (not Node.Parent.Is_Null
      and then
        (Node.Parent.Kind in Ada_Subtype_Indication_Range
         and then not Node.Parent.Parent.Is_Null
         and then Node.Parent.Parent.Kind in
           Ada_Derived_Type_Def_Range));
   --  Return True if the node belongs to derived type declaration.

   function Length (List : Assoc_List) return Natural;
   --  Returns how many Basic_Assoc nodes L has.

   function Length (List : Compilation_Unit_List) return Natural;
   --  Returns how many Compilation_Unit nodes List has.

   function Length (List : Defining_Name_List) return Natural;
   --  Returns how many Defining_Name nodes L has.

   function Length (List : Param_Spec_List) return Natural;
   --  Returns how many Param_Spec nodes L has.

   function List_Bodies_Of
     (Definition         : Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise          : in out Boolean)
      return Bodies_List.List;
   --  List all the bodies of Definition. This does not list the bodies of the
   --  parent. It sets Imprecise to True if any request returns
   --  imprecise results.

   procedure Merge
     (Left  : in out Source_Location_Range_Map;
      Right : Source_Location_Range_Map);
   --  Safely merges 'Right' into 'Left'.

   function Resolve_Name
     (Name_Node : Name;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : out Boolean)
      return Defining_Name;
   --  Return the definition node (canonical part) of the given name.
   --  Imprecise is set to True if LAL has marked the result as imprecise or
   --  if an error happened.

   function Resolve_Name_Precisely (Name_Node : Name) return Defining_Name;
   --  Return the definition node (canonical part) of the given name.

   function Validate_Syntax
     (Source : Ada.Strings.Unbounded.Unbounded_String;
      Rule   : Grammar_Rule)
      return Boolean;
   --  True if Source has a valid syntax according to Rule

   package Grammar_Rule_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Grammar_Rule);

   subtype Grammar_Rule_Vector is Grammar_Rule_Vectors.Vector;

   function Validate_Syntax
     (Source : Ada.Strings.Unbounded.Unbounded_String;
      Rules  : Grammar_Rule_Vector)
      return Boolean;
   --  True if Source has a valid syntax according to any rule of Rules

end Laltools.Common;
