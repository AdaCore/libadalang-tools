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

with GNATCOLL.Traces;

with Libadalang.Analysis;
with Libadalang.Common;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

package Laltools.Common is

   package LALAnalysis renames Libadalang.Analysis;
   package LALCommon renames Libadalang.Common;
   package LKSSlocs renames Langkit_Support.Slocs;
   package LKSText renames Langkit_Support.Text;

   function "<" (Left, Right : LALAnalysis.Defining_Name) return Boolean is
     (Left.Text < Right.Text
      or else (Left.Text = Right.Text
               and then Left.Full_Sloc_Image < Right.Full_Sloc_Image));
   --  The Ordered_Maps is using the "<" in its Equivalent_Keys function:
   --  this is too basic and it will assume that Left.Text = Right.Text implies
   --  Left = Right which is wrong.
   --  If Left.Text = Right.Text then Full_Sloc_Image will sort first by
   --  file and then by Sloc (first by line and then by column).

   package Bodies_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => LALAnalysis.Defining_Name,
      "="          => LALAnalysis."=");

   package References_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => LALAnalysis.Base_Id,
      "="          => LALAnalysis."=");

   package References_By_Subprogram is new Ada.Containers.Ordered_Maps
     (Key_Type     => LALAnalysis.Defining_Name,
      Element_Type => References_List.List,
      "<"          => "<",
      "="          => References_List."=");

   function Contains
     (Token   : LALCommon.Token_Reference;
      Pattern : Wide_Wide_String;
      As_Word : Boolean;
      Span    : out LKSSlocs.Source_Location_Range)
      return Boolean;
   --  Return True if the Token text contains Pattern and set position in Span.
   --  Checks whether the Token's Pattern is delimited by word delimiters
   --  if As_Word is True.

   function Is_Access_Ref (Node : LALAnalysis.Ada_Node) return Boolean;
   --  Return True if the node or the dotted name is an access reference.

   function Is_Call
     (Node      : LALAnalysis.Ada_Node'Class;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : in out Boolean) return Boolean;
   --  Check if a node is a call and an identifier. Enum literals
   --  in DottedName are excluded.

   function Is_Constant
     (Node : LALAnalysis.Basic_Decl) return Boolean;
   --  Return True if the decl contains the constant keyword.

   function Is_Definition_Without_Separate_Implementation
     (Definition : LALAnalysis.Defining_Name) return Boolean;
   --  Return True if the definition given is a subprogram that does not call
   --  for a body, ie a "is null" procedure, an expression function, or an
   --  abstract subprogram.

   function Is_End_Label (Node : LALAnalysis.Ada_Node) return Boolean
   is
     (not Node.Parent.Is_Null
      and then (Node.Parent.Kind in LALCommon.Ada_End_Name
                or else (Node.Parent.Kind in LALCommon.Ada_Dotted_Name
                         and then not Node.Parent.Parent.Is_Null
                         and then Node.Parent.Parent.Kind in
                           LALCommon.Ada_End_Name)));
   --  Return True if the node belongs to an end label node.
   --  Used to filter out end label references.

   function Is_Enum_Literal
     (Node      : LALAnalysis.Ada_Node'Class;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : in out Boolean) return Boolean;
   --  Check if a node is an enum literal.

   function Is_Structure
     (Node : LALAnalysis.Basic_Decl) return Boolean;
   --  Return True if the type contains a record part.

   function Is_Type_Derivation (Node : LALAnalysis.Ada_Node) return Boolean
   is
     (not Node.Parent.Is_Null
      and then
        (Node.Parent.Kind in LALCommon.Ada_Subtype_Indication_Range
         and then not Node.Parent.Parent.Is_Null
         and then Node.Parent.Parent.Kind in
           LALCommon.Ada_Derived_Type_Def_Range));
   --  Return True if the node belongs to derived type declaration.

   function Find_Canonical_Part
     (Definition : LALAnalysis.Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle)
      return LALAnalysis.Defining_Name;
   --  Wrapper around P_Canonical_Part that returns null if canonical part
   --  is name itself. It also catches Property_Error and reports it in traces.

   function Find_Next_Part
     (Definition : LALAnalysis.Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle)
      return LALAnalysis.Defining_Name;
   --  Wrapper around P_Next_Part that returns No_Defining_Name if next part
   --  is name itself. It also catches Property_Error and reports it in traces.

   function Find_Other_Part_Fallback
     (Definition : LALAnalysis.Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle)
      return LALAnalysis.Defining_Name;
   --  Attempt to find the other part of a definition manually, with
   --  simple heuristics that look at the available entities with matching
   --- names and profiles.
   --  This should be called only if straightforward Libadalang calls
   --  have failed.

   function Get_Last_Name (Name_Node : LALAnalysis.Name)
                           return LKSText.Unbounded_Text_Type;
   --  Return the last name, for example if name is A.B.C then return C.

   function Get_Name_As_Defining (Name_Node : LALAnalysis.Name)
                                  return LALAnalysis.Defining_Name;
   --  Wrapper around P_Enclosing_Defining_Name that returns No_Defining_Name
   --  if Name_Node is No_Name or not a Defining_Name.

   function Get_Node_As_Name (Node : LALAnalysis.Ada_Node)
                              return LALAnalysis.Name;
   --  Wrapper around As_Name that returns No_Name if Node is not a Name.

   function List_Bodies_Of
     (Definition         : LALAnalysis.Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise          : in out Boolean)
      return Bodies_List.List;
   --  List all the bodies of Definition. This does not list the bodies of the
   --  parent. It sets Imprecise to True if any request returns
   --  imprecise results.

   function Resolve_Name
     (Name_Node : LALAnalysis.Name;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : out Boolean) return LALAnalysis.Defining_Name;
   --  Return the definition node (canonical part) of the given name.
   --  Imprecise is set to True if LAL's imprecise fallback mechanism has been
   --  used to compute the cross reference.

end Laltools.Common;
