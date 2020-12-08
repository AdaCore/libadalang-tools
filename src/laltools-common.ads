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
--
--  This package contains LAL_Tools common utilities to be used by other
--  packages

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with GNATCOLL.Traces;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;

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

   package Ada_Node_List_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Ada_Node_List,
      "="          => "=");

   package Base_Id_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Base_Id,
      "="          => "=");

   package Basic_Decl_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Basic_Decl,
      "="          => "=");

   package Bodies_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Defining_Name,
      "="          => "=");

   package Declarative_Part_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Declarative_Part,
      "="          => "=");

   type Param_Data is record
      Param_Mode : Mode;
      Param_Type : Basic_Decl;
   end record;

   function "=" (Left, Right : Param_Data) return Boolean;
   --  Returns True is the type of both parameters is the same, and then if
   --  it's mode is also the same or equivalent. In and default modes are
   --  equivalent.

   package Param_Data_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Param_Data,
      "="          => "=");

   package References_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Base_Id,
      "="          => "=");

   package References_By_Subprogram is new Ada.Containers.Ordered_Maps
     (Key_Type     => Defining_Name,
      Element_Type => References_List.List,
      "<"          => "<",
      "="          => References_List."=");

   function Are_Subprograms_Type_Conformant
     (Subp_A      : Subp_Spec;
      Subp_B      : Subp_Spec;
      Check_Modes : Boolean := False)
      return Boolean;
   --  Checks if Subp_A and Subp_B are type conformant. Check_Modes is a flag
   --  that defines in the mode of each parameter is also checked. In such
   --  check, In and default mode are considered the same.

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

   function Create_Param_Data_Vector
     (Parameters : Params)
      return Param_Data_Vectors.Vector;
   --  Creates a vector of Param_Data (mode and type) of each parameter of
   --  Params.

   function Create_Param_Type_Vector
     (Parameters : Params)
      return Basic_Decl_Vectors.Vector;
   --  Creates a vector of the associated Basic_Decl of each parameter of
   --  Params.

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

   function Is_Type_Derivation (Node : Ada_Node) return Boolean
   is
     (not Node.Parent.Is_Null
      and then
        (Node.Parent.Kind in Ada_Subtype_Indication_Range
         and then not Node.Parent.Parent.Is_Null
         and then Node.Parent.Parent.Kind in
           Ada_Derived_Type_Def_Range));
   --  Return True if the node belongs to derived type declaration.

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

   function Find_All_Base_Subp_Declarations
     (Subp  : Basic_Decl)
      return Basic_Decl_Array
     with Pre => not Subp.Is_Null
     and then Subp.Kind in Ada_Subp_Body_Range | Ada_Subp_Decl_Range;
   --  If Subp is a subprogram, then this function returns an array with all
   --  declarations that Subp overrides or that is being overridden by.

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
     with Pre => not Subp.Is_Null
     and then Subp.Kind in Ada_Subp_Body_Range | Ada_Subp_Decl_Range;
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

   function Find_Local_Scopes (Node : Ada_Node'Class)
                               return Ada_Node_List_Vectors.Vector;
   --  Find all scopes in Node's compilation unit visible by Node.

   function Find_Local_Scopes (Node : Ada_Node'Class)
                               return Declarative_Part_Vectors.Vector;
   --  Find all scopes in Node's compilation unit visible by Node.

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

   function Find_Other_Part_Fallback
     (Definition : Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle)
      return Defining_Name;
   --  Attempt to find the other part of a definition manually, with
   --  simple heuristics that look at the available entities with matching
   --- names and profiles.
   --  This should be called only if straightforward Libadalang calls
   --  have failed.

   function Get_CU_Visible_Declarative_Parts
     (Node : Ada_Node'Class;
      Skip_First : Boolean := False)
      return Declarative_Part_Vectors.Vector;
   --  Find all Declarative_Parts in Node's compilation unit visible by Node.

   function Get_Decl_Block_Declarative_Part (Decl_B : Decl_Block)
                                             return Declarative_Part;
   --  Gets the Declarative_Part of a Decl_Block.

   function Get_Decl_Block_Decls (Decl_B : Decl_Block) return Ada_Node_List;
   --  Gets the Ada_Node_List of a Declarative_Part associated to a Decl_Block.

   function Get_Declarative_Part (Stmts : Handled_Stmts)
                                  return Declarative_Part;
   --  Finds the Handled_Stmts's respective Declarative_Part, if it exists.
   --  ??? Possibly move this function to Libadalang.

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
     (Pkg_Decl : Package_Decl)
      return Declarative_Part_Vectors.Vector;
   --  Gets all the Declarative_Parts associated to a Package_Decl
   --  (public, private and body declarative parts).

   function Get_Package_Declarative_Parts
     (Pkg_Body : Package_Body)
      return Declarative_Part_Vectors.Vector
   is (Get_Package_Declarative_Parts
       (Pkg_Body.P_Canonical_Part.As_Package_Decl));
   --  Gets all the Declarative_Parts associated to a Package_Body
   --  (public, private and body declarative parts).

   function Get_Package_Decls (Pkg_Decl : Package_Decl)
                               return Ada_Node_List_Vectors.Vector;
   --  Gets all the Ada_Node_Lists of the Declarative_Parts associated to a
   --- Package_Decl (public, private and body declarative parts).

   function Get_Package_Decls
     (Pkg_Body : Package_Body)
      return Ada_Node_List_Vectors.Vector
   is (Get_Package_Decls (Pkg_Body.P_Canonical_Part.As_Package_Decl));
   --  Gets all the Ada_Node_Lists of the Declarative_Parts associated to a
   --- Package_Body (public, private and body declarative parts).

   function Get_Package_Decl_Private_Declarative_Part (Pkg_Decl : Package_Decl)
                                                       return Declarative_Part;
   --  Gets the private Declarative_Part associated to a Package_Body.

   function Get_Package_Decl_Private_Decls (Pkg_Decl : Package_Decl)
                                            return Ada_Node_List;
   --  Gets the Ada_Node_List of the private Declarative_Part associated to a
   --  Package_Body, if it exists.

   function Get_Package_Decl_Public_Declarative_Part (Pkg_Decl : Package_Decl)
                                                      return Declarative_Part;
   --  Gets the public Declarative_Part associated to a Package_Decl.

   function Get_Package_Decl_Public_Decls (Pkg_Decl : Package_Decl)
                                           return Ada_Node_List;
   --  Gets the Ada_Node_List of the the public Declarative_Part associated to
   --  a Package_Decl.

   function Get_Subp_Body_Declarative_Part (Subp_B : Subp_Body)
                                            return Declarative_Part;
   --  Gets the Declarative_Part associated to a Subp_Body.

   function Get_Subp_Body_Decls (Subp_B : Subp_Body)
                                 return Ada_Node_List;
   --  Gets the Ada_Node_List of a Declarative_Part associated to a Subp_Body.

   function Get_Subp_Hierarchy
     (Subp  : Basic_Decl;
      Units : Analysis_Unit_Array)
      return Basic_Decl_Array
     with Pre => not Subp.Is_Null
     and then Subp.Kind in Ada_Subp_Body_Range | Ada_Subp_Decl_Range;
   --  If Subp is a primitive subrogram of a type, then returns an array
   --  with all declaration overriding or that are being overridden by Subp.

   function Get_Subp_Spec
     (Subp : Basic_Decl'Class)
      return Subp_Spec;
   --  Gets the Subp_Spec associated to a subprogram. If Subp is not a
   --  subprogram, returns No_Subp_Spec.

   function Get_Task_Body_Declarative_Part (Task_B : Task_Body)
                                            return Declarative_Part;
   --  Gets the Declarative_Part associated to a Task_Body.

   function Get_Task_Body_Decls (Task_B : Task_Body) return Ada_Node_List;
   --  Gets the Ada_Node_List of a Declarative_Part associated to a Task_Body.

   function Get_Use_Units_Declarative_Parts
     (Node : Ada_Node'Class)
      return Declarative_Part_Vectors.Vector;
   --  Gets all public Declarative_Parts of the units used by Node's unit.

   function List_Bodies_Of
     (Definition         : Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise          : in out Boolean)
      return Bodies_List.List;
   --  List all the bodies of Definition. This does not list the bodies of the
   --  parent. It sets Imprecise to True if any request returns
   --  imprecise results.

   function Resolve_Name
     (Name_Node : Name;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : out Boolean)
      return Defining_Name;
   --  Return the definition node (canonical part) of the given name.
   --  Imprecise is set to True if LAL's imprecise fallback mechanism has been
   --  used to compute the cross reference.

   function Resolve_Name_Precisely (Name_Node : Name) return Defining_Name;
   --  Return the definition node (canonical part) of the given name.

   function Subprograms_Have_Same_Signature
     (Subp_A      : Subp_Decl;
      Subp_B      : Subp_Decl;
      Check_Modes : Boolean := False)
      return Boolean;
   --  Checks if Subp_A and Subp_B have the same name and are type
   --  conformant. Check_Modes is a flag that defines in the mode of
   --  each parameter is also checked.
end Laltools.Common;
