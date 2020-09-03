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

with Ada.Strings.Wide_Wide_Fixed;

with Ada.Exceptions;

with Ada.Wide_Wide_Characters.Handling;

with GNAT.Traceback.Symbolic;

package body Laltools.Common is

   procedure Log
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "");
   --  Log an exception in the given traces, with an optional message.

   --------------
   -- Contains --
   --------------

   function Contains
     (Token   : LALCommon.Token_Reference;
      Pattern : Wide_Wide_String;
      As_Word : Boolean;
      Span    : out LKSSlocs.Source_Location_Range)
      return Boolean
   is
      T    : constant LKSText.Text_Type :=
        Ada.Wide_Wide_Characters.Handling.To_Lower (LALCommon.Text (Token));
      Idx  : constant Integer := Ada.Strings.Wide_Wide_Fixed.Index
        (T, Pattern);
      Last : Integer;

      function Is_Word_Delimiter (C : Wide_Wide_Character) return Boolean;

      -----------------------
      -- Is_Word_Delimiter --
      -----------------------

      function Is_Word_Delimiter (C : Wide_Wide_Character) return Boolean is
      begin
         return not Ada.Wide_Wide_Characters.Handling.Is_Alphanumeric (C)
           and then C /= '_';
      end Is_Word_Delimiter;

      use type LKSSlocs.Column_Number;
   begin
      if Idx < T'First then
         return False;
      end if;

      --  Treat the Pattern as a word
      if As_Word then
         if Idx > T'First
           and then not Is_Word_Delimiter (T (Idx - 1))
         then
            return False;
         end if;

         Last := Idx + Pattern'Length;
         if Last <= T'Last
           and then not Is_Word_Delimiter (T (Last))
         then
            return False;
         end if;
      end if;

      Span := LALCommon.Sloc_Range (LALCommon.Data (Token));
      Span.Start_Column :=
        Span.Start_Column + LKSSlocs.Column_Number (Idx - T'First);
      Span.End_Column :=
        Span.Start_Column + LKSSlocs.Column_Number (Pattern'Length);
      return True;
   end Contains;

   -------------------
   -- Is_Access_Ref --
   -------------------

   function Is_Access_Ref (Node : LALAnalysis.Ada_Node) return Boolean is
      use type LALCommon.Ada_Node_Kind_Type;
   begin
      if Node.Parent.Is_Null then
         return False;
      end if;

      if Node.Parent.Kind = LALCommon.Ada_Dotted_Name then
         return Is_Access_Ref (Node.Parent);
      end if;

      if Node.Parent.Kind in LALCommon.Ada_Name then
         declare
            Sibling : constant LALAnalysis.Ada_Node := Node.Next_Sibling;
            Text    : constant Wide_Wide_String :=
              (if Sibling.Is_Null
               then ""
               else Ada.Wide_Wide_Characters.Handling.To_Lower
                 (Sibling.Text));
         begin
            return
              Text = "access"
              or else Text = "unrestricted_access"
              or else Text = "unchecked_access"
              or else Text = "address";
         end;
      end if;
      return False;
   end Is_Access_Ref;

   -------------
   -- Is_Call --
   -------------

   function Is_Call
     (Node      : LALAnalysis.Ada_Node'Class;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : in out Boolean) return Boolean
   is
      use type LALAnalysis.Ada_Node;
      use type LALCommon.Ada_Node_Kind_Type;
   begin
      return Node.As_Ada_Node /= LALAnalysis.No_Ada_Node
        and then Node.Kind in LALCommon.Ada_Name
        and then Node.As_Name.P_Is_Call
        and then Node.Kind = LALCommon.Ada_Identifier
        and then not Is_Enum_Literal (Node, Trace, Imprecise);
   end Is_Call;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant
     (Node : LALAnalysis.Basic_Decl) return Boolean is
      use type LALAnalysis.Ada_Node;
      use type LALCommon.Ada_Node_Kind_Type;
   begin
      for Child of Node.Children loop
         if Child /= LALAnalysis.No_Ada_Node
           and then Child.Kind = LALCommon.Ada_Constant_Present
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Constant;

   ---------------------------------------------------
   -- Is_Definition_Without_Separate_Implementation --
   ---------------------------------------------------

   function Is_Definition_Without_Separate_Implementation
     (Definition : LALAnalysis.Defining_Name) return Boolean
   is
      Parents : constant LALAnalysis.Ada_Node_Array := Definition.Parents;
   begin
      return Parents'Length > 2 and then Parents (Parents'First + 2).Kind in
        LALCommon.Ada_Abstract_Subp_Decl
      --  This is as abstract subprogram
        | LALCommon.Ada_Null_Subp_Decl
      --  This is an "is null" procedure
        | LALCommon.Ada_Expr_Function;
      --  This is an expression function
   end Is_Definition_Without_Separate_Implementation;

   ---------------------
   -- Is_Enum_Literal --
   ---------------------

   function Is_Enum_Literal
     (Node      : LALAnalysis.Ada_Node'Class;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : in out Boolean) return Boolean
   is
      Definition     : LALAnalysis.Defining_Name;
      This_Imprecise : Boolean := False;
      use type LALAnalysis.Ada_Node;
      use type LALAnalysis.Defining_Name;
      use type LALCommon.Ada_Node_Kind_Type;
   begin
      if Node.As_Ada_Node /= LALAnalysis.No_Ada_Node
        and then Node.Kind in LALCommon.Ada_Name
      then
         Definition := Laltools.Common.Resolve_Name
           (Node.As_Name, Trace, This_Imprecise);
         Imprecise := Imprecise or This_Imprecise;
         return Definition /= LALAnalysis.No_Defining_Name
           and then Definition.P_Basic_Decl.Kind =
             LALCommon.Ada_Enum_Literal_Decl;
      end if;

      return False;
   end Is_Enum_Literal;

   ------------------
   -- Is_Structure --
   ------------------

   function Is_Structure
     (Node : LALAnalysis.Basic_Decl) return Boolean is
      use type LALAnalysis.Ada_Node;
      use type LALCommon.Ada_Node_Kind_Type;
   begin
      for Child of Node.Children loop
         if Child /= LALAnalysis.No_Ada_Node
           and then Child.Kind = LALCommon.Ada_Record_Type_Def
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Structure;

   -------------------------
   -- Find_Canonical_Part --
   -------------------------

   function Find_Canonical_Part
     (Definition : LALAnalysis.Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle)
      return LALAnalysis.Defining_Name
   is
      Canonical : LALAnalysis.Defining_Name;
      use type LALAnalysis.Defining_Name;
   begin
      Canonical := Definition.P_Canonical_Part;

      if Canonical = Definition then
         return LALAnalysis.No_Defining_Name;
      else
         return Canonical;
      end if;

   exception
      when E :  LALCommon.Property_Error =>
         Log (Trace, E);
         return LALAnalysis.No_Defining_Name;
   end Find_Canonical_Part;

   --------------------
   -- Find_Next_Part --
   --------------------

   function Find_Next_Part
     (Definition : LALAnalysis.Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle)
      return LALAnalysis.Defining_Name
   is
      Next   : LALAnalysis.Defining_Name;
      use type LALAnalysis.Defining_Name;
   begin
      Next := Definition.P_Next_Part;

      if Next = Definition then
         return LALAnalysis.No_Defining_Name;
      else
         return Next;
      end if;
   exception
      when E :  LALCommon.Property_Error =>
         Log (Trace, E);
         return LALAnalysis.No_Defining_Name;
   end Find_Next_Part;

   ------------------------------
   -- Find_Other_Part_Fallback --
   ------------------------------

   function Find_Other_Part_Fallback
     (Definition : LALAnalysis.Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle)
      return LALAnalysis.Defining_Name
   is
      use type LALAnalysis.Defining_Name;

      Qualified_Name : constant Langkit_Support.Text.Text_Type :=
        Definition.P_Basic_Decl.P_Fully_Qualified_Name;
      --  The name that we'll try to match

      Found : LALAnalysis.Defining_Name := LALAnalysis.No_Defining_Name;
      --  The result that has been found

      function Matches
        (Node : LALAnalysis.Ada_Node'Class) return LALCommon.Visit_Status;
      --  Return True if the name of Node matches Qualified_Name

      -------------
      -- Matches --
      -------------

      function Matches
        (Node : LALAnalysis.Ada_Node'Class) return LALCommon.Visit_Status is
      begin
         if Node.Is_Null
           or else Node.Kind not in Libadalang.Common.Ada_Basic_Decl
         then
            return Libadalang.Common.Into;
         end if;

         --  Note: in this function, we are simply looking at the first
         --  result that matches.
         --  TODO: improve this by find all entities that match, and
         --  finding the best through a distance/scoring heuristics.

         declare
            Decl : constant LALAnalysis.Basic_Decl := Node.As_Basic_Decl;
            Def  : constant LALAnalysis.Defining_Name := Decl.P_Defining_Name;
         begin
            if Def /= Definition
              and then Decl.P_Fully_Qualified_Name = Qualified_Name
            then
               Found := Def;
               return Libadalang.Common.Stop;
            end if;
         end;

         return Libadalang.Common.Into;
      end Matches;

      Parent_Node : LALAnalysis.Ada_Node;
      Parent_Spec : LALAnalysis.Defining_Name;
      Parent_Body : LALAnalysis.Defining_Name;
   begin
      --  The heuristics implemented is the following: we're looking at the
      --  spec and body of the enclosing entity, to find an entity that
      --  could correspond to Definition.
      --
      --  For instance, if Definition points to a function Foo that is defined
      --  in a package P, we're going to look in the spec and body of P for
      --  any items named Foo, excluding Definition itself.

      --  Eliminate some cases. The subprogram does not have an other part if
      --  it is an expression function, or an abstract subprogram declaration,
      --  or a null procedure.

      if Laltools.Common.Is_Definition_Without_Separate_Implementation
        (Definition)
      then
         return LALAnalysis.No_Defining_Name;
      end if;

      --  First obtain the spec.
      --  Note: we could refine the number of calls to P_Semantic_Parent.
      --  Two calls to P_Semantic_Parents are needed in the case of a
      --  subprogram: the first jumps to the SubpDecl, the second to the
      --  PackageDecl.

      Parent_Node := Definition.P_Semantic_Parent.P_Semantic_Parent;

      if Parent_Node.Is_Null
        or else Parent_Node.Kind not in LALCommon.Ada_Basic_Decl
      then
         return LALAnalysis.No_Defining_Name;
      end if;

      Parent_Spec := Parent_Node.As_Basic_Decl.
        P_Canonical_Part.P_Defining_Name;

      --  Traverse the spec. The visiting function assigns the matching
      --  result, if any, to Found.
      Parent_Spec.Parent.Traverse (Matches'Unrestricted_Access);

      --  If we didn't find a result when traversing the spec, traverse the
      --  body of the containing entity.
      if Found = LALAnalysis.No_Defining_Name then
         Parent_Body := Laltools.Common.Find_Next_Part (Parent_Spec, Trace);
         if Parent_Body = LALAnalysis.No_Defining_Name then
            Parent_Body := Laltools.Common.Find_Next_Part (Parent_Spec, Trace);
         end if;
         if Parent_Body /= LALAnalysis.No_Defining_Name then
            Parent_Body.Parent.Traverse (Matches'Unrestricted_Access);
         end if;
      end if;

      return Found;

   exception
      when E : LALCommon.Property_Error =>
         Log (Trace, E);
         return LALAnalysis.No_Defining_Name;
   end Find_Other_Part_Fallback;

   -------------------
   -- Get_Last_Name --
   -------------------

   function Get_Last_Name (Name_Node : LALAnalysis.Name)
      return LKSText.Unbounded_Text_Type
   is
      Names : constant LALAnalysis.Unbounded_Text_Type_Array :=
        LALAnalysis.P_As_Symbol_Array (Name_Node);
   begin
      return Names (Names'Last);
   end Get_Last_Name;

   --------------------------
   -- Get_Name_As_Defining --
   --------------------------

   function Get_Name_As_Defining (Name_Node : LALAnalysis.Name)
                                  return LALAnalysis.Defining_Name is
      use type LALAnalysis.Name;
   begin
      if Name_Node = LALAnalysis.No_Name or else not Name_Node.P_Is_Defining
      then
         return LALAnalysis.No_Defining_Name;
      end if;

      return Name_Node.P_Enclosing_Defining_Name;
   end Get_Name_As_Defining;

   ----------------------
   -- Get_Node_As_Name --
   ----------------------

   function Get_Node_As_Name (Node : LALAnalysis.Ada_Node)
                              return LALAnalysis.Name is
      use type LALAnalysis.Ada_Node;
   begin

      if Node = LALAnalysis.No_Ada_Node
        or else Node.Kind not in LALCommon.Ada_Name
      then
         return LALAnalysis.No_Name;
      end if;

      return Node.As_Name;

   end Get_Node_As_Name;

   --------------------
   -- List_Bodies_Of --
   --------------------

   function List_Bodies_Of
     (Definition         : LALAnalysis.Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise          : in out Boolean)
      return Bodies_List.List
   is
      List       : Bodies_List.List;
      Next_Part  : LALAnalysis.Defining_Name;
      Loop_Count : Natural := 0;
      Parents    : constant LALAnalysis.Ada_Node_Array := Definition.Parents;

      use type LALAnalysis.Defining_Name;
   begin
      --  If this happens to be the definition of a subprogram that
      --  does not call for a body, let's consider that this *is* the
      --  implementation. Return this, and do not attempt to look
      --  for secondary implementations in this case.
      if Parents'Length > 2 and then Parents (Parents'First + 2).Kind in
        Libadalang.Common.Ada_Null_Subp_Decl     --  "is null" procedure?
          | Libadalang.Common.Ada_Expr_Function  --  expression function?
      then
         List.Append (Definition);
         return List;
      end if;

      --  If the definition that we found is a subprogram body, add this to the
      --  list
      if Parents'Length > 2 and then Parents (Parents'First + 2).Kind in
        Libadalang.Common.Ada_Subp_Body
      then
         List.Append (Definition);
      end if;

      --  TODO: Reactivate these lines when libadalang supports
      --  P_Next_Part for tasks: T716-049
      --  if Parents'Length > 1 and then Parents (Parents'First + 1).Kind in
      --    Libadalang.Common.Ada_Task_Body
      --  then
      --     List.Append (Definition);
      --  end if;

      Next_Part := Definition;

      --  Now that we have a definition, list all the implementations for
      --  this definition. We do this by iterating on Find_Next_Part
      loop
         --  Safety net, don't rely on the results making sense, since
         --  the code might be invalid.
         Next_Part := Laltools.Common.Find_Next_Part (Next_Part, Trace);

         exit when Next_Part = LALAnalysis.No_Defining_Name;

         List.Append (Next_Part);

         Loop_Count := Loop_Count + 1;
         if Loop_Count > 5 then
            Imprecise := True;
            exit;
         end if;
      end loop;
      return List;
   end List_Bodies_Of;

   ---------
   -- Log --
   ---------

   procedure Log
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "") is
   begin
      if Message /= "" then
         Trace.Trace (Message);
      end if;

      Trace.Trace (Ada.Exceptions.Exception_Name (E)
                   & ": "
                   & Ada.Exceptions.Exception_Message (E)
                   & ASCII.LF
                   & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end Log;

   ------------------
   -- Resolve_Name --
   ------------------

   function Resolve_Name
     (Name_Node : LALAnalysis.Name;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : out Boolean) return LALAnalysis.Defining_Name
   is
      Result : LALAnalysis.Defining_Name;
      use type LALAnalysis.Defining_Name;
   begin

      --  First try to resolve precisely
      begin
         if Name_Node.P_Is_Defining then
            Result := Name_Node.P_Enclosing_Defining_Name.P_Canonical_Part;
         else
            Result := Name_Node.P_Referenced_Defining_Name
              (Imprecise_Fallback => False);
            if Result /= LALAnalysis.No_Defining_Name then
               Result := Result.P_Canonical_Part;
            end if;
         end if;
      exception
         when E : LALCommon.Property_Error =>
            Log (Trace, E);
            Result := LALAnalysis.No_Defining_Name;
      end;

      --  The result was found precisely: return it
      if Result /= LALAnalysis.No_Defining_Name then
         return Result;
      end if;

      --  If we reach this, it means we've failed to get a precise result.
      --  Try again with the imprecise fallback.
      if not Name_Node.P_Is_Defining then
         Result := Name_Node.P_Referenced_Defining_Name
           (Imprecise_Fallback => True);
         if Result /= LALAnalysis.No_Defining_Name then
            Result := Result.P_Canonical_Part;
         end if;
         Imprecise := Result /= LALAnalysis.No_Defining_Name;
      end if;

      return Result;

   exception
      when E : LALCommon.Property_Error =>
         Log (Trace, E);
         return LALAnalysis.No_Defining_Name;
   end Resolve_Name;

end Laltools.Common;
