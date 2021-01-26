------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Libadalang.Iterators;

package body Laltools.Common is

   procedure Log
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "");
   --  Log an exception in the given traces, with an optional message.

   -------
   -- = --
   -------

   function "=" (Left, Right : Param_Data)
                 return Boolean
   is
      use type Basic_Decl;

      Same_Type       : constant Boolean :=
        Left.Param_Type = Right.Param_Type;
      Same_Mode       : constant Boolean :=
        Left.Param_Mode.Kind = Right.Param_Mode.Kind;
      Equivalent_Mode : constant Boolean :=
        Left.Param_Mode.Kind in Ada_Mode_Default | Ada_Mode_In
        and then Right.Param_Mode.Kind in Ada_Mode_Default | Ada_Mode_In;
   begin
      return Same_Type and then (Same_Mode or else Equivalent_Mode);
   end "=";

   -------------------------------------
   -- Are_Subprograms_Type_Conformant --
   -------------------------------------

   function Are_Subprograms_Type_Conformant
     (Subp_A      : Subp_Spec;
      Subp_B      : Subp_Spec;
      Check_Modes : Boolean := False)
      return Boolean
   is
      use type Ada_Node_Kind_Type;
      use type Param_Data_Vectors.Vector;
      use type Basic_Decl_Vectors.Vector;
   begin
      if Subp_A.F_Subp_Kind /= Subp_A.F_Subp_Kind
      then
         return False;
      end if;

      case Check_Modes is
         when True =>
            if Create_Param_Data_Vector (Subp_A.F_Subp_Params) /=
              Create_Param_Data_Vector (Subp_B.F_Subp_Params)
            then
               return False;
            end if;
         when False =>
            if Create_Param_Type_Vector (Subp_A.F_Subp_Params) /=
              Create_Param_Type_Vector (Subp_B.F_Subp_Params)
            then
               return False;
            end if;
      end case;

      return Subp_A.F_Subp_Kind = Ada_Subp_Kind_Function
        and then Subp_A.P_Return_Type =
          Subp_B.P_Return_Type;
   end Are_Subprograms_Type_Conformant;

   ---------------------------
   -- Compilation_Unit_Hash --
   ---------------------------

   function Compilation_Unit_Hash (Comp_Unit : Compilation_Unit)
                                   return Ada.Containers.Hash_Type is
   begin
      return Hash (Comp_Unit.As_Ada_Node);
   end Compilation_Unit_Hash;

   --------------
   -- Contains --
   --------------

   function Contains
     (Token   : Token_Reference;
      Pattern : Wide_Wide_String;
      As_Word : Boolean;
      Span    : out Source_Location_Range)
      return Boolean
   is
      T    : constant Text_Type :=
        Ada.Wide_Wide_Characters.Handling.To_Lower (Text (Token));
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

      use type Column_Number;
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

      Span := Sloc_Range (Data (Token));
      Span.Start_Column :=
        Span.Start_Column + Column_Number (Idx - T'First);
      Span.End_Column :=
        Span.Start_Column + Column_Number (Pattern'Length);
      return True;
   end Contains;

   ------------------------------
   -- Create_Param_Data_Vector --
   ------------------------------

   function Create_Param_Data_Vector
     (Parameters : Params)
      return Param_Data_Vectors.Vector
   is
      Param_Vector : Param_Data_Vectors.Vector;
      use type Ada_Node;
   begin
      if Parameters = No_Params or else
        Parameters.F_Params = No_Param_Spec_List
      then
         return Param_Vector;
      end if;
      for Param of Parameters.F_Params loop
         declare
            Data : constant Param_Data :=
              (Param_Mode => Param.F_Mode,
               Param_Type => Param.F_Type_Expr.P_Type_Name.
                 P_Referenced_Decl (False).P_Canonical_Part);
         begin
            for Dummy of Param.F_Ids loop
               Param_Vector.Append (New_Item => Data);
            end loop;
         end;
      end loop;
      return Param_Vector;
   end Create_Param_Data_Vector;

   ------------------------------
   -- Create_Param_Type_Vector --
   ------------------------------

   function Create_Param_Type_Vector
     (Parameters : Params)
      return Basic_Decl_Vectors.Vector
   is
      Param_Types : Basic_Decl_Vectors.Vector;
   begin
      if Parameters = No_Params or else
        Parameters.F_Params = No_Param_Spec_List
      then
         return Param_Types;
      end if;
      for P of Parameters.F_Params loop
         for Dummy of P.F_Ids loop
            Param_Types.Append
              (P.F_Type_Expr.P_Type_Name.P_Referenced_Decl.P_Canonical_Part);
         end loop;
      end loop;
      return Param_Types;
   end Create_Param_Type_Vector;

   -------------------
   -- Is_Access_Ref --
   -------------------

   function Is_Access_Ref (Node : Ada_Node) return Boolean is
      use type Ada_Node_Kind_Type;
   begin
      if Node.Parent.Is_Null then
         return False;
      end if;

      if Node.Parent.Kind = Ada_Dotted_Name then
         return Is_Access_Ref (Node.Parent);
      end if;

      if Node.Parent.Kind in Ada_Name then
         declare
            Sibling : constant Ada_Node := Node.Next_Sibling;
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
     (Node      : Ada_Node'Class;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : in out Boolean) return Boolean
   is
      use type Ada_Node;
      use type Ada_Node_Kind_Type;
   begin
      return Node.As_Ada_Node /= No_Ada_Node
        and then Node.Kind in Ada_Name
        and then Node.As_Name.P_Is_Call
        and then Node.Kind = Ada_Identifier
        and then not Is_Enum_Literal (Node, Trace, Imprecise);
   end Is_Call;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant
     (Node : Basic_Decl) return Boolean is
      use type Ada_Node;
      use type Ada_Node_Kind_Type;
   begin
      for Child of Node.Children loop
         if Child /= No_Ada_Node
           and then Child.Kind = Ada_Constant_Present
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
     (Definition : Defining_Name) return Boolean
   is
      Parents : constant Ada_Node_Array := Definition.Parents;
   begin
      return Parents'Length > 2 and then Parents (Parents'First + 2).Kind in
        Ada_Abstract_Subp_Decl
      --  This is as abstract subprogram
        | Ada_Null_Subp_Decl
      --  This is an "is null" procedure
        | Ada_Expr_Function;
      --  This is an expression function
   end Is_Definition_Without_Separate_Implementation;

   ---------------------
   -- Is_Enum_Literal --
   ---------------------

   function Is_Enum_Literal
     (Node      : Ada_Node'Class;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : in out Boolean) return Boolean
   is
      Definition     : Defining_Name;
      This_Imprecise : Boolean := False;
      use type Ada_Node;
      use type Defining_Name;
      use type Ada_Node_Kind_Type;
   begin
      if Node.As_Ada_Node /= No_Ada_Node
        and then Node.Kind in Ada_Name
      then
         Definition := Laltools.Common.Resolve_Name
           (Node.As_Name, Trace, This_Imprecise);
         Imprecise := Imprecise or This_Imprecise;
         return Definition /= No_Defining_Name
           and then Definition.P_Basic_Decl.Kind =
             Ada_Enum_Literal_Decl;
      end if;

      return False;
   end Is_Enum_Literal;

   ------------------
   -- Is_Renamable --
   ------------------

   function Is_Renamable (Node : Ada_Node'Class) return Boolean is
      Node_Name : constant Libadalang.Analysis.Name
        := Get_Node_As_Name (Node.As_Ada_Node);
      use type Libadalang.Analysis.Name;
      use type Defining_Name;
   begin
      --  Only consider renamable if a precise definition is found
      return Node_Name /= No_Name and then
        (Node_Name.P_Is_Defining or else
         Node.As_Name.P_Referenced_Defining_Name (Imprecise_Fallback => False)
         /= No_Defining_Name);
   end Is_Renamable;

   ------------------
   -- Is_Structure --
   ------------------

   function Is_Structure
     (Node : Basic_Decl) return Boolean is
      use type Ada_Node;
      use type Ada_Node_Kind_Type;
   begin
      for Child of Node.Children loop
         if Child /= No_Ada_Node
           and then Child.Kind = Ada_Record_Type_Def
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Structure;

   -------------------------------------
   -- Find_All_Base_Subp_Declarations --
   -------------------------------------

   function Find_All_Base_Subp_Declarations (Subp  : Basic_Decl)
                                             return Basic_Decl_Array
   is
      Lal_Result : constant Basic_Decl_Array := Subp.P_Base_Subp_Declarations;
      Our_Result : Basic_Decl_Array (1 .. Lal_Result'Length - 1);
      Index      : Positive := 1;
   begin
      --  Libadalang returns an empty array if this is not a subprogram
      --  that's a primitive of a tagged type.

      if Lal_Result'Length = 0 then
         return (1 .. 0 => <>);
      end if;

      --  The result returned by Libadalang includes Subp; we want to remove
      --  this from the list.

      for J of Lal_Result loop
         if J /= Subp then
            Our_Result (Index) := J;
            Index := Index + 1;
         end if;
      end loop;

      return Our_Result;
   end Find_All_Base_Subp_Declarations;

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Node     : Defining_Name'Class;
      Units    : Analysis_Unit_Array;
      Callback : not null access procedure
        (Reference : Ref_Result;
         Stop      : in out Boolean))
   is
      Stop : Boolean := False;
      use type Defining_Name;
   begin
      if Node.As_Defining_Name = No_Defining_Name then
         return;
      end if;

      for Ref of Node.P_Find_All_References (Units, False)
      loop
         Callback (Ref, Stop);
         exit when Stop;
      end loop;
   end Find_All_References;

   --------------------------------------
   -- Find_All_References_For_Renaming --
   --------------------------------------

   function Find_All_References_For_Renaming
     (Definition : Defining_Name;
      Units      : Analysis_Unit_Array)
      return Base_Id_Vectors.Vector
   is
      All_References : Base_Id_Vectors.Vector;
      Is_Param : constant Boolean :=
        Definition.P_Basic_Decl.Kind in Ada_Param_Spec_Range;
      Is_Subp  : constant Boolean :=
        Definition.P_Basic_Decl.Kind in
          Ada_Subp_Body_Range | Ada_Subp_Decl_Range;
   begin
      if Definition.P_Canonical_Part.F_Name.Kind = Ada_Dotted_Name then
         All_References.Append
           (Definition.P_Canonical_Part.F_Name.As_Dotted_Name.
              F_Suffix.As_Base_Id);
      else
         All_References.Append
           (Definition.P_Canonical_Part.F_Name.As_Base_Id);
      end if;

      for Reference of Definition.P_Find_All_References (Units) loop
         All_References.Append (Ref (Reference).As_Base_Id);
      end loop;

      if Is_Param then
         All_References.Append
           (Find_All_Param_References_In_Subp_Hierarchy
              (Definition.P_Canonical_Part, Units));
      elsif Is_Subp then
         All_References.Append
           (Find_All_Subp_References_In_Subp_Hierarchy
              (Definition.P_Canonical_Part.P_Basic_Decl, Units));
      end if;

      return All_References;
   end Find_All_References_For_Renaming;

   --------------------------------------------
   -- Find_All_Param_References_In_Hierarchy --
   --------------------------------------------

   function Find_All_Param_References_In_Subp_Hierarchy
     (Param_Definition : Defining_Name;
      Units            : Analysis_Unit_Array)
      return Base_Id_Vectors.Vector
   is
      Parents   : constant Ada_Node_Array := Param_Definition.Parents;

      Subp : constant Basic_Decl :=
        Parents (Parents'First + 6).As_Basic_Decl;

      Hierarchy : constant Basic_Decl_Array :=
        Get_Subp_Hierarchy (Subp, Units);

      Param_References : Base_Id_Vectors.Vector;
   begin
      for Decl of Hierarchy loop
         for Param_Spec of
           Decl.As_Basic_Subp_Decl.P_Subp_Decl_Spec.P_Params
         loop
            for Param of Param_Spec.F_Ids loop
               if Param_Definition.Text = Param.Text then
                  Param_References.Append
                    (Param.P_Canonical_Part.F_Name.As_Base_Id);
                  for Reference of
                    Param.P_Canonical_Part.P_Find_All_References (Units)
                  loop
                     Param_References.Append (Ref (Reference).As_Base_Id);
                  end loop;
               end if;
            end loop;
         end loop;
      end loop;
      return Param_References;
   end Find_All_Param_References_In_Subp_Hierarchy;

   ------------------------------------------------
   -- Find_All_Subp_References_In_Subp_Hierarchy --
   ------------------------------------------------

   function Find_All_Subp_References_In_Subp_Hierarchy
     (Subp  : Basic_Decl;
      Units : Analysis_Unit_Array)
      return Base_Id_Vectors.Vector
   is
      Hierarchy : constant Basic_Decl_Array :=
        Get_Subp_Hierarchy (Subp, Units);

      Param_References : Base_Id_Vectors.Vector;
   begin
      for Decl of Hierarchy loop
         Param_References.Append (Decl.P_Defining_Name.F_Name.As_Base_Id);
         for Reference of
           Decl.P_Defining_Name.P_Find_All_References (Units)
         loop
            Param_References.Append (Ref (Reference).As_Base_Id);
         end loop;
      end loop;
      return Param_References;
   end Find_All_Subp_References_In_Subp_Hierarchy;

   -------------------------
   -- Find_Canonical_Part --
   -------------------------

   function Find_Canonical_Part
     (Definition         : Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise_Fallback : Boolean := False)
      return Defining_Name
   is
      Canonical : Defining_Name;
      use type Defining_Name;
   begin
      Canonical :=
        Definition.P_Canonical_Part (Imprecise_Fallback => Imprecise_Fallback);

      if Canonical = Definition then
         return No_Defining_Name;
      else
         return Canonical;
      end if;

   exception
      when E :  Libadalang.Common.Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Canonical_Part;

   -----------------------
   -- Find_Local_Scopes --
   -----------------------

   function Find_Local_Scopes (Node : Ada_Node'Class)
                               return Ada_Node_List_Vectors.Vector
   is
      use type Ada_Node;
      use type Ada_Node_Kind_Type;
   begin
      if Node.Parent = No_Ada_Node
      then
         return Local_Scope : Ada_Node_List_Vectors.Vector;
      end if;

      for Parent of Node.Parent.Parents loop
         case Parent.Kind is
            when Ada_Package_Body =>
               --  Local_Scope will have at most three Ada_Node_Array nodes:
               --  Package body, public part of the package spec and private
               --  part of the package spec.
               return Get_Package_Decls (Parent.As_Package_Body);

            when Ada_Package_Decl =>
               --  Local_Scope will have at most three Ada_Node_Array nodes:
               --  Package body, public part of the package spec and private
               --  part of the package spec.
               return Get_Package_Decls (Parent.As_Package_Decl);

            when Ada_Subp_Decl =>
               return Local_Scope : Ada_Node_List_Vectors.Vector do
                  Local_Scope.Append
                    (Get_Subp_Body_Decls
                       (Parent.As_Subp_Decl.P_Body_Part.As_Subp_Body));
               end return;

            when Ada_Subp_Body =>
               return Local_Scope : Ada_Node_List_Vectors.Vector do
                  Local_Scope.Append
                    (Get_Subp_Body_Decls (Parent.As_Subp_Body));
               end return;

            when Ada_Task_Body =>
               return Local_Scope : Ada_Node_List_Vectors.Vector do
                  Local_Scope.Append
                    (Get_Task_Body_Decls (Parent.As_Task_Body));
               end return;

            when Ada_Decl_Block =>
               return Local_Scope : Ada_Node_List_Vectors.Vector do
                  Local_Scope.Append
                    (Get_Decl_Block_Decls (Parent.As_Decl_Block));
               end return;

            when others =>
               null;
         end case;
      end loop;

      return Local_Scope : Ada_Node_List_Vectors.Vector;
   end Find_Local_Scopes;

   -----------------------
   -- Find_Local_Scopes --
   -----------------------

   function Find_Local_Scopes (Node : Ada_Node'Class)
                               return Declarative_Part_Vectors.Vector
   is
      use type Ada_Node;
      use type Ada_Node_Kind_Type;
   begin
      if Node.Parent = No_Ada_Node
      then
         return Local_Scope : Declarative_Part_Vectors.Vector;
      end if;

      for Parent of Node.Parent.Parents loop
         case Parent.Kind is
            when Ada_Package_Body =>
               --  Local_Scope will have at most three Ada_Node_Array nodes:
               --  Package body, public part of the package spec and private
               --  part of the package spec.
               return Get_Package_Declarative_Parts (Parent.As_Package_Body);

            when Ada_Package_Decl =>
               --  Local_Scope will have at most three Ada_Node_Array nodes:
               --  Package body, public part of the package spec and private
               --  part of the package spec.
               return Get_Package_Declarative_Parts (Parent.As_Package_Decl);

            when Ada_Subp_Body =>
               return Local_Scope : Declarative_Part_Vectors.Vector do
                  Local_Scope.Append
                    (Get_Subp_Body_Declarative_Part (Parent.As_Subp_Body));
               end return;

            when Ada_Task_Body =>
               return Local_Scope : Declarative_Part_Vectors.Vector do
                  Local_Scope.Append
                    (Get_Task_Body_Declarative_Part (Parent.As_Task_Body));
               end return;

            when Ada_Decl_Block =>
               return Local_Scope : Declarative_Part_Vectors.Vector do
                  Local_Scope.Append
                    (Get_Decl_Block_Declarative_Part (Parent.As_Decl_Block));
               end return;

            when others =>
               null;
         end case;
      end loop;

      return Local_Scope : Declarative_Part_Vectors.Vector;
   end Find_Local_Scopes;

   ------------------------
   -- Find_Nested_Scopes --
   ------------------------

   function Find_Nested_Scopes (Node : Ada_Node'Class)
                                return Declarative_Part_Vectors.Vector
   is
      Nested_Declarative_Parts : Declarative_Part_Vectors.Vector;

      function Find_Nested_Declarative_Parts (This_Node : Ada_Node'Class)
                                              return Visit_Status;

      function Find_Nested_Declarative_Parts (This_Node : Ada_Node'Class)
                                              return Visit_Status is
      begin
         if This_Node.Kind in Ada_Declarative_Part_Range then
            Nested_Declarative_Parts.Append (This_Node.As_Declarative_Part);
         end if;

         return Into;
      end Find_Nested_Declarative_Parts;

   begin
      Find_Declarative_Part_Owner :
      for N of Node.Parents loop
         case N.Kind is
            when Ada_Decl_Block =>
               Traverse
                 (N.As_Decl_Block.F_Decls.F_Decls,
                  Find_Nested_Declarative_Parts'Access);
               Traverse
                 (N.As_Decl_Block.F_Stmts,
                  Find_Nested_Declarative_Parts'Access);
               exit Find_Declarative_Part_Owner;

            when Ada_Package_Body =>
               Traverse
                 (N.As_Package_Body.F_Decls.F_Decls,
                  Find_Nested_Declarative_Parts'Access);
               exit Find_Declarative_Part_Owner;

            when Ada_Package_Decl =>
               if not N.As_Package_Decl.P_Body_Part.Is_Null then
                  Traverse
                    (N.As_Package_Decl.P_Body_Part.F_Decls.F_Decls,
                     Find_Nested_Declarative_Parts'Access);
               end if;

               Traverse
                 (N.As_Package_Decl.F_Public_Part.F_Decls,
                  Find_Nested_Declarative_Parts'Access);

               if not N.As_Package_Decl.F_Private_Part.Is_Null then
                  Traverse
                    (N.As_Package_Decl.F_Private_Part.F_Decls,
                     Find_Nested_Declarative_Parts'Access);
               end if;

               exit Find_Declarative_Part_Owner;

            when Ada_Subp_Body =>
               Traverse
                 (N.As_Subp_Body.F_Decls.F_Decls,
                  Find_Nested_Declarative_Parts'Access);
               Traverse
                 (N.As_Subp_Body.F_Stmts,
                  Find_Nested_Declarative_Parts'Access);
               exit Find_Declarative_Part_Owner;

            when Ada_Subp_Decl =>
               Traverse
                 (N.As_Subp_Decl.P_Body_Part.As_Subp_Body.F_Decls.F_Decls,
                  Find_Nested_Declarative_Parts'Access);
               Traverse
                 (N.As_Subp_Decl.P_Body_Part.As_Subp_Body.F_Stmts,
                  Find_Nested_Declarative_Parts'Access);
               exit Find_Declarative_Part_Owner;

            when Ada_Task_Body =>
               Traverse
                 (N.As_Task_Body.F_Decls.F_Decls,
                  Find_Nested_Declarative_Parts'Access);
               Traverse
                 (N.As_Task_Body.F_Stmts,
                  Find_Nested_Declarative_Parts'Access);
               exit Find_Declarative_Part_Owner;

            when others =>
               null;
         end case;
      end loop Find_Declarative_Part_Owner;
      return Nested_Declarative_Parts;
   end Find_Nested_Scopes;

   --------------------
   -- Find_Next_Part --
   --------------------

   function Find_Next_Part
     (Definition         : Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise_Fallback : Boolean := False)
      return Defining_Name
   is
      Next   : Defining_Name;
      use type Defining_Name;
   begin
      Next :=
        Definition.P_Next_Part (Imprecise_Fallback => Imprecise_Fallback);

      if Next = Definition then
         return No_Defining_Name;
      else
         return Next;
      end if;
   exception
      when E :  Libadalang.Common.Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Next_Part;

   ------------------------------
   -- Find_Other_Part_Fallback --
   ------------------------------

   function Find_Other_Part_Fallback
     (Definition : Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle)
      return Defining_Name
   is
      use type Defining_Name;

      Qualified_Name : constant Langkit_Support.Text.Text_Type :=
        Definition.P_Basic_Decl.P_Fully_Qualified_Name;
      --  The name that we'll try to match

      Found : Defining_Name := No_Defining_Name;
      --  The result that has been found

      function Matches
        (Node : Ada_Node'Class) return Visit_Status;
      --  Return True if the name of Node matches Qualified_Name

      -------------
      -- Matches --
      -------------

      function Matches
        (Node : Ada_Node'Class) return Visit_Status is
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
            Decl : constant Basic_Decl := Node.As_Basic_Decl;
            Def  : constant Defining_Name := Decl.P_Defining_Name;
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

      Parent_Node : Ada_Node;
      Parent_Spec : Defining_Name;
      Parent_Body : Defining_Name;
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
         return No_Defining_Name;
      end if;

      --  First obtain the spec.
      --  Note: we could refine the number of calls to P_Semantic_Parent.
      --  Two calls to P_Semantic_Parents are needed in the case of a
      --  subprogram: the first jumps to the SubpDecl, the second to the
      --  PackageDecl.

      Parent_Node := Definition.P_Semantic_Parent.P_Semantic_Parent;

      if Parent_Node.Is_Null
        or else Parent_Node.Kind not in Ada_Basic_Decl
      then
         return No_Defining_Name;
      end if;

      Parent_Spec := Parent_Node.As_Basic_Decl.
        P_Canonical_Part.P_Defining_Name;

      --  Traverse the spec. The visiting function assigns the matching
      --  result, if any, to Found.
      Parent_Spec.Parent.Traverse (Matches'Unrestricted_Access);

      --  If we didn't find a result when traversing the spec, traverse the
      --  body of the containing entity.
      if Found = No_Defining_Name then
         Parent_Body := Laltools.Common.Find_Next_Part (Parent_Spec, Trace);
         if Parent_Body = No_Defining_Name then
            Parent_Body := Laltools.Common.Find_Next_Part (Parent_Spec, Trace);
         end if;
         if Parent_Body /= No_Defining_Name then
            Parent_Body.Parent.Traverse (Matches'Unrestricted_Access);
         end if;
      end if;

      return Found;

   exception
      when E : Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Other_Part_Fallback;

   --------------------------------------
   -- Get_CU_Visible_Declarative_Parts --
   --------------------------------------

   function Get_CU_Visible_Declarative_Parts
     (Node       : Ada_Node'Class;
      Skip_First : Boolean := False)
      return Declarative_Part_Vectors.Vector
   is
      Declarative_Parts : Declarative_Part_Vectors.Vector;
      Last_Parent       : Ada_Node := Node.Parent;
   begin
      if Skip_First then
         Skip_First_Parent :
         for Parent of Node.Parent.Parents loop
            if Parent.Kind in
              Ada_Package_Body | Ada_Package_Decl | Ada_Subp_Body
                | Ada_Task_Body | Ada_Decl_Block
            then
               Last_Parent := Parent;
               exit Skip_First_Parent;
            end if;
         end loop Skip_First_Parent;
      end if;

      if Last_Parent.Parent = No_Ada_Node then
         return Declarative_Parts;
      end if;

      for Parent of Last_Parent.Parent.Parents loop
         case Parent.Kind is
            when Ada_Package_Body =>
               for Declarative_Part of
                 Get_Package_Declarative_Parts (Parent.As_Package_Body)
               loop
                  Declarative_Parts.Append (Declarative_Part);
               end loop;

            when Ada_Package_Decl =>
               for Declarative_Part of
                 Get_Package_Declarative_Parts (Parent.As_Package_Decl)
               loop
                  Declarative_Parts.Append (Declarative_Part);
               end loop;

            when Ada_Subp_Body =>
               Declarative_Parts.Append
                 (Get_Subp_Body_Declarative_Part (Parent.As_Subp_Body));

            when Ada_Task_Body =>
               Declarative_Parts.Append
                 (Get_Task_Body_Declarative_Part (Parent.As_Task_Body));

            when Ada_Decl_Block =>
               Declarative_Parts.Append
                 (Get_Decl_Block_Declarative_Part (Parent.As_Decl_Block));

            when others =>
               null;
         end case;
      end loop;

      return Declarative_Parts;
   end Get_CU_Visible_Declarative_Parts;

   -------------------------------------
   -- Get_Decl_Block_Declarative_Part --
   -------------------------------------

   function Get_Decl_Block_Declarative_Part
     (Decl_B : Decl_Block)
      return Declarative_Part
   is
      use type Decl_Block;
   begin
      if Decl_B = No_Decl_Block then
         return No_Declarative_Part;
      end if;

      return Decl_B.F_Decls;
   end Get_Decl_Block_Declarative_Part;

   --------------------------
   -- Get_Decl_Block_Decls --
   --------------------------

   function Get_Decl_Block_Decls
     (Decl_B : Decl_Block)
      return Ada_Node_List
   is
      use type Decl_Block;
   begin
      if Decl_B = No_Decl_Block then
         return No_Ada_Node_List;
      end if;

      return Decl_B.F_Decls.F_Decls;
   end Get_Decl_Block_Decls;

   --------------------------
   -- Get_Declarative_Part --
   --------------------------

   function Get_Declarative_Part
     (Stmts : Handled_Stmts) return Declarative_Part
   is
      use type Handled_Stmts;
      use type Ada_Node;
   begin
      if Stmts = No_Handled_Stmts
        or else Stmts.Parent = No_Ada_Node
      then
         return No_Declarative_Part;
      end if;

      case Stmts.Parent.Kind is
         when Ada_Decl_Block =>
            return Stmts.Parent.As_Decl_Block.F_Decls;

         when Ada_Entry_Body =>
            return Stmts.Parent.As_Entry_Body.F_Decls;

         when Ada_Package_Body =>
            return Stmts.Parent.As_Package_Body.F_Decls;

         when Ada_Subp_Body =>
            return Stmts.Parent.As_Subp_Body.F_Decls;

         when Ada_Task_Body =>
            return Stmts.Parent.As_Task_Body.F_Decls;

         when others =>
            return No_Declarative_Part;
      end case;
   end Get_Declarative_Part;

   --------------------------
   -- Get_Defining_Name_Id --
   --------------------------

   function Get_Defining_Name_Id (Definition : Defining_Name)
                                  return Identifier
   is
      use type Ada_Node_Kind_Type;
   begin
      case Definition.F_Name.Kind is
         when Ada_Identifier =>
            return Definition.F_Name.As_Identifier;

         when Ada_Dotted_Name =>
            return Definition.F_Name.As_Dotted_Name.F_Suffix.As_Identifier;

         when others =>
            raise Program_Error;
      end case;
   end Get_Defining_Name_Id;

   --------------------------------------------
   --  Get_First_Identifier_From_Declaration --
   --------------------------------------------

   function Get_First_Identifier_From_Declaration
     (Decl : Basic_Decl'Class) return Identifier
   is
      Node : constant Ada_Node :=
        Libadalang.Iterators.Find_First
          (Decl, Libadalang.Iterators.Kind_Is (Ada_Identifier));
      use type Ada_Node;
   begin
      if Node /= No_Ada_Node then
         return Node.As_Identifier;
      else
         return No_Identifier;
      end if;
   end Get_First_Identifier_From_Declaration;

   -------------------
   -- Get_Last_Name --
   -------------------

   function Get_Last_Name (Name_Node : Name)
                           return Unbounded_Text_Type
   is
      Names : constant Unbounded_Text_Type_Array :=
        P_As_Symbol_Array (Name_Node);
   begin
      return Names (Names'Last);
   end Get_Last_Name;

   --------------------------
   -- Get_Name_As_Defining --
   --------------------------

   function Get_Name_As_Defining (Name_Node : Name)
                                  return Defining_Name is
      use type Name;
   begin
      if Name_Node = No_Name or else not Name_Node.P_Is_Defining
      then
         return No_Defining_Name;
      end if;

      return Name_Node.P_Enclosing_Defining_Name;
   end Get_Name_As_Defining;

   ----------------------
   -- Get_Node_As_Name --
   ----------------------

   function Get_Node_As_Name (Node : Ada_Node)
                              return Name is
      use type Ada_Node;
   begin
      if Node = No_Ada_Node
        or else Node.Kind not in Ada_Name
      then
         return No_Name;
      end if;

      return Node.As_Name;
   end Get_Node_As_Name;

   -------------------------------------
   -- Get_Package_Body_Declative_Part --
   -------------------------------------

   function Get_Package_Body_Declative_Part
     (Pkg_Body : Package_Body)
      return Declarative_Part
   is
      use type Package_Body;
   begin
      if Pkg_Body = No_Package_Body then
         return No_Declarative_Part;
      end if;

      return Pkg_Body.F_Decls;
   end Get_Package_Body_Declative_Part;

   ----------------------------
   -- Get_Package_Body_Decls --
   ----------------------------

   function Get_Package_Body_Decls
     (Pkg_Body : Package_Body)
      return Ada_Node_List
   is
      use type Package_Body;
   begin
      if Pkg_Body = No_Package_Body then
         return No_Ada_Node_List;
      end if;

      return Pkg_Body.F_Decls.F_Decls;
   end Get_Package_Body_Decls;

   ----------------------------------------
   -- Get_Package_Decl_Declarative_Parts --
   ----------------------------------------

   function Get_Package_Declarative_Parts
     (Pkg_Decl : Package_Decl)
      return Declarative_Part_Vectors.Vector
   is
      Decls : Declarative_Part_Vectors.Vector;

      --  A Package_Decl always has a Public_Part but might not have a
      --  Private_Part or an associated Package_Body with a Declarative_Part.

      Private_Part : Declarative_Part;
      Body_Part    : Declarative_Part;
   begin
      if Pkg_Decl = No_Package_Decl then
         return Decls;
      end if;

      Decls.Append (Get_Package_Decl_Public_Declarative_Part (Pkg_Decl));

      Private_Part := Get_Package_Decl_Private_Declarative_Part (Pkg_Decl);

      if Private_Part /= No_Declarative_Part then
         Decls.Append (Private_Part);
      end if;

      if Pkg_Decl.P_Body_Part /= No_Package_Body then
         Body_Part := Get_Package_Body_Declative_Part (Pkg_Decl.P_Body_Part);

         if Body_Part /= No_Declarative_Part then
            Decls.Append (Body_Part);
         end if;
      end if;

      return Decls;
   end Get_Package_Declarative_Parts;

   -----------------------
   -- Get_Package_Decls --
   -----------------------

   function Get_Package_Decls
     (Pkg_Decl : Package_Decl)
      return Ada_Node_List_Vectors.Vector
   is
      Decls : Ada_Node_List_Vectors.Vector;
   begin
      Decls.Append (Get_Package_Decl_Public_Decls (Pkg_Decl));
      declare
         Private_Decls : constant Ada_Node_List :=
           Get_Package_Decl_Private_Decls (Pkg_Decl);
      begin
         case Private_Decls /= No_Ada_Node_List is
            when True =>
               Decls.Append (Private_Decls);
            when False =>
               null;
         end case;
      end;
      Decls.Append (Get_Package_Body_Decls (Pkg_Decl.P_Body_Part));
      return Decls;
   end Get_Package_Decls;

   -----------------------------------------------
   -- Get_Package_Decl_Private_Declarative_Part --
   -----------------------------------------------

   function Get_Package_Decl_Private_Declarative_Part
     (Pkg_Decl : Package_Decl)
      return Declarative_Part
   is
      use type Package_Decl;
   begin
      if Pkg_Decl = No_Package_Decl
        or else Pkg_Decl.F_Private_Part = No_Private_Part
      then
         return No_Declarative_Part;
      end if;

      return Pkg_Decl.F_Private_Part.As_Declarative_Part;
   end Get_Package_Decl_Private_Declarative_Part;

   ------------------------------------
   -- Get_Package_Decl_Private_Decls --
   ------------------------------------

   function Get_Package_Decl_Private_Decls
     (Pkg_Decl : Package_Decl)
      return Ada_Node_List
   is
      use type Package_Decl;
      --  use type Private_Part;
      use type Ada_Node;
   begin
      if Pkg_Decl = No_Package_Decl
      --  or else Pkg_Decl.F_Private_Part = No_Private_Part
        or else Pkg_Decl.F_Private_Part.As_Ada_Node = No_Ada_Node
      then
         return No_Ada_Node_List;
      end if;

      return Pkg_Decl.F_Private_Part.F_Decls;
   end Get_Package_Decl_Private_Decls;

   ----------------------------------------------
   -- Get_Package_Decl_Public_Declarative_Part --
   ----------------------------------------------

   function Get_Package_Decl_Public_Declarative_Part
     (Pkg_Decl : Package_Decl)
      return Declarative_Part
   is
      use type Package_Decl;
   begin
      if Pkg_Decl = No_Package_Decl then
         return No_Declarative_Part;
      end if;

      return Pkg_Decl.F_Public_Part.As_Declarative_Part;
   end Get_Package_Decl_Public_Declarative_Part;

   -----------------------------------
   -- Get_Package_Decl_Public_Decls --
   -----------------------------------

   function Get_Package_Decl_Public_Decls
     (Pkg_Decl : Package_Decl)
      return Ada_Node_List
   is
      use type Package_Decl;
   begin
      if Pkg_Decl = No_Package_Decl then
         return No_Ada_Node_List;
      end if;

      return Pkg_Decl.F_Public_Part.F_Decls;
   end Get_Package_Decl_Public_Decls;

   ------------------------------------
   -- Get_Subp_Body_Declarative_Part --
   ------------------------------------

   function Get_Subp_Body_Declarative_Part
     (Subp_B : Subp_Body)
      return Declarative_Part
   is
      use type Subp_Body;
   begin
      if Subp_B = No_Subp_Body then
         return No_Declarative_Part;
      end if;

      return Subp_B.F_Decls;
   end Get_Subp_Body_Declarative_Part;

   -------------------------
   -- Get_Subp_Body_Decls --
   -------------------------

   function Get_Subp_Body_Decls
     (Subp_B : Subp_Body)
      return Ada_Node_List
   is
      use type Subp_Body;
   begin
      if Subp_B = No_Subp_Body then
         return No_Ada_Node_List;
      end if;

      return Subp_B.F_Decls.F_Decls;
   end Get_Subp_Body_Decls;

   ------------------------
   -- Get_Subp_Hierarchy --
   ------------------------

   function Get_Subp_Hierarchy
     (Subp  : Basic_Decl;
      Units : Analysis_Unit_Array)
      return Basic_Decl_Array
   is
      Overriding_Decls : constant Basic_Decl_Array :=
        Subp.P_Find_All_Overrides (Units);

      Base_Subp_Decls : constant Basic_Decl_Array :=
        Find_All_Base_Subp_Declarations (Subp);
   begin
      return Overriding_Decls & Base_Subp_Decls;
   end Get_Subp_Hierarchy;

   -------------------
   -- Get_Subp_Spec --
   -------------------

   function Get_Subp_Spec
     (Subp : Basic_Decl'Class)
      return Subp_Spec is

   begin
      case Subp.Kind is
         when Ada_Subp_Decl =>
            return Subp.As_Subp_Decl.F_Subp_Spec;

         when Ada_Subp_Body =>
            return Subp.As_Subp_Body.F_Subp_Spec;

         when Ada_Null_Subp_Decl =>
            return Subp.As_Null_Subp_Decl.F_Subp_Spec;

         when Ada_Subp_Renaming_Decl =>
            declare
               Original_Subp : constant Basic_Decl'Class :=
                 Resolve_Name_Precisely
                   (Subp.As_Subp_Renaming_Decl.F_Renames.F_Renamed_Object).
                     P_Basic_Decl;
            begin
               return Get_Subp_Spec (Original_Subp);
            end;

         when others =>
            return No_Subp_Spec;
      end case;
   end Get_Subp_Spec;

   ------------------------------------
   -- Get_Task_Body_Declarative_Part --
   ------------------------------------

   function Get_Task_Body_Declarative_Part
     (Task_B : Task_Body)
      return Declarative_Part
   is
      use type Task_Body;
   begin
      if Task_B = No_Task_Body then
         return No_Declarative_Part;
      end if;

      return Task_B.F_Decls;
   end Get_Task_Body_Declarative_Part;

   -------------------------
   -- Get_Task_Body_Decls --
   -------------------------

   function Get_Task_Body_Decls
     (Task_B : Task_Body)
      return Ada_Node_List
   is
      use type Task_Body;
   begin
      if Task_B = No_Task_Body then
         return No_Ada_Node_List;
      end if;

      return Task_B.F_Decls.F_Decls;
   end Get_Task_Body_Decls;

   --------------------------------------
   --  Get_Use_Units_Declarative_Parts --
   --------------------------------------

   function Get_Use_Units_Declarative_Parts
     (Node : Ada_Node'Class)
      return Declarative_Part_Vectors.Vector
   is
      This_Unit : Compilation_Unit renames Node.Unit.Root.As_Compilation_Unit;
      This_Prelude : constant Ada_Node_List := This_Unit.F_Prelude;

      Declarative_Parts : Declarative_Part_Vectors.Vector;

      use type Ada_Node_Kind_Type;
   begin
      for Node of This_Prelude loop
         if Node.Kind = Ada_Use_Package_Clause then
            for P of Node.As_Use_Package_Clause.F_Packages loop
               declare
                  Package_Name : constant Name :=
                    Get_Node_As_Name (P.As_Ada_Node);
               begin
                  Declarative_Parts.Append
                    (Get_Package_Decl_Public_Declarative_Part
                       (Resolve_Name_Precisely (Package_Name).
                            P_Basic_Decl.As_Package_Decl));
               end;
            end loop;
         end if;
      end loop;
      return Declarative_Parts;
   end Get_Use_Units_Declarative_Parts;

   ------------
   -- Length --
   ------------

   function Length (List : Compilation_Unit_List) return Natural
   is
      L : Natural := 0;
   begin
      for Unit of List loop
         L := L + 1;
      end loop;
      return L;
   end Length;

   --------------------
   -- List_Bodies_Of --
   --------------------

   function List_Bodies_Of
     (Definition         : Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise          : in out Boolean)
      return Bodies_List.List
   is
      List       : Bodies_List.List;
      Next_Part  : Defining_Name;
      Loop_Count : Natural := 0;
      Parents    : constant Ada_Node_Array := Definition.Parents;

      use type Defining_Name;
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

         exit when Next_Part = No_Defining_Name;

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
     (Name_Node : Name;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : out Boolean) return Defining_Name
   is
      Result : Defining_Name;
      use type Defining_Name;
   begin
      Imprecise := False;

      if Name_Node.Is_Null then
         return No_Defining_Name;
      end if;

      --  First try to resolve precisely
      begin
         if Name_Node.P_Is_Defining then
            Result := Name_Node.P_Enclosing_Defining_Name.P_Canonical_Part;
         else
            Result := Name_Node.P_Referenced_Defining_Name
              (Imprecise_Fallback => False);
            if Result /= No_Defining_Name then
               Result := Result.P_Canonical_Part;
            end if;
         end if;
      exception
         when E : Property_Error =>
            Log (Trace, E);
            Result := No_Defining_Name;
      end;

      --  The result was found precisely: return it
      if Result /= No_Defining_Name then
         return Result;
      end if;

      --  If we reach this, it means we've failed to get a precise result.
      --  Try again with the imprecise fallback.
      if not Name_Node.P_Is_Defining then
         Result := Name_Node.P_Referenced_Defining_Name
           (Imprecise_Fallback => True);
         if Result /= No_Defining_Name then
            Result := Result.P_Canonical_Part;
         end if;
         Imprecise := Result /= No_Defining_Name;
      end if;

      return Result;

   exception
      when E : Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Resolve_Name;

   ----------------------------
   -- Resolve_Name_Precisely --
   ----------------------------

   function Resolve_Name_Precisely
     (Name_Node : Name)
      return Defining_Name
   is
      use type Name;
      use type Defining_Name;
   begin
      if Name_Node = No_Name then
         return No_Defining_Name;
      end if;

      if Name_Node.P_Is_Defining then
         return Name_Node.P_Enclosing_Defining_Name.P_Canonical_Part;
      else
         return Result : Defining_Name :=
           Name_Node.P_Referenced_Defining_Name (Imprecise_Fallback => False)
         do
            if Result /= No_Defining_Name then
               Result := Result.P_Canonical_Part;
            end if;
         end return;
      end if;
   end Resolve_Name_Precisely;

   -------------------------------------
   -- Subprograms_Have_Same_Signature --
   -------------------------------------

   function Subprograms_Have_Same_Signature
     (Subp_A      : Subp_Decl;
      Subp_B      : Subp_Decl;
      Check_Modes : Boolean := False)
      return Boolean is
   begin
      return Subp_A.P_Defining_Name.F_Name.As_Identifier.Text =
        Subp_B.P_Defining_Name.F_Name.As_Identifier.Text and then
        Are_Subprograms_Type_Conformant
          (Subp_A.F_Subp_Spec, Subp_B.F_Subp_Spec, Check_Modes);
   end Subprograms_Have_Same_Signature;

end Laltools.Common;
