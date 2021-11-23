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

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Wide_Wide_Characters.Handling;

with GNAT.Traceback.Symbolic;

with Libadalang.Iterators;

with Laltools.Subprogram_Hierarchy; use Laltools.Subprogram_Hierarchy;

package body Laltools.Common is

   procedure Log
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "");
   --  Log an exception in the given traces, with an optional message.

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Source_Location_Range) return Boolean is
   begin
      if Left.Start_Line = Right.Start_Line then
         return Left.Start_Column < Right.Start_Column;
      end if;

      return Left.Start_Line < Right.Start_Line;
   end "<";

   ------------------------
   -- Append_If_Not_Null --
   ------------------------

   procedure Append_If_Not_Null
     (Vector : in out Ada_List_Vector;
      List   : Ada_List'Class) is
   begin
      if not List.Is_Null then
         Vector.Append (List);
      end if;
   end Append_If_Not_Null;

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

   ---------------------------------
   -- Count_Param_Spec_Parameters --
   ---------------------------------

   function Count_Param_Spec_Parameters
     (Param_Spec : Libadalang.Analysis.Param_Spec'Class)
      return Natural is
   begin
      return Count : Natural := 0 do
         if not Param_Spec.Is_Null then
            for Parameter of Param_Spec.F_Ids loop
               Count := Count + 1;
            end loop;
         end if;
      end return;
   end Count_Param_Spec_Parameters;

   ----------------------------
   -- Count_Subp_Param_Specs --
   ----------------------------

   function Count_Subp_Param_Specs
     (Subp_Params : Params'Class)
      return Natural is
   begin
      return Count : Natural := 0 do
         if not Subp_Params.Is_Null then
            for Param_Spec of Subp_Params.F_Params loop
               Count := Count + 1;
            end loop;
         end if;
      end return;
   end Count_Subp_Param_Specs;

   ---------------------------
   -- Count_Subp_Parameters --
   ---------------------------

   function Count_Subp_Parameters
     (Subp_Params : Params'Class)
      return Natural is
   begin
      return Count : Natural := 0 do
         if not Subp_Params.Is_Null then
            for Param_Spec of Subp_Params.F_Params loop
               Count := Count + Count_Param_Spec_Parameters (Param_Spec);
            end loop;
         end if;
      end return;
   end Count_Subp_Parameters;

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
        Is_Subprogram (Definition.P_Basic_Decl);
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

      declare
         Vector : constant Base_Id_Vectors.Vector :=
           (if Is_Param then Find_All_Param_References_In_Subp_Hierarchy
              (Definition.P_Canonical_Part, Units)
            elsif Is_Subp then Find_All_Subp_References_In_Subp_Hierarchy
              (Definition.P_Canonical_Part.P_Basic_Decl, Units)
            else
               Base_Id_Vectors.Empty_Vector);
      begin
         for X of Vector loop
            All_References.Append (X);
         end loop;
      end;

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
      --  The semantic parent of this parameter can either be a subprogram
      --  declaration or an access to a subprogram definition.
      --  In the latter case, there is no need to rename the parameter in the
      --  entire hierarchy since Ada allows different names.
      --
      --  Example:
      --
      --     procedure Bar (Self : A; Baz : access procedure (P : Boolean))
      --     is abstract;
      --
      --     overriding
      --     procedure Bar (Self : B; Baz : access procedure (Q : Boolean));
      --
      --     The parameters of Baz have different names (P and Q) and this is
      --     allowed. Therefore, only references need to be renamed.

      Semantic_Parent         : constant Ada_Node :=
        Param_Definition.P_Semantic_Parent;
      Is_Semantic_Parent_Subp : constant Boolean :=
        not Semantic_Parent.Is_Null
        and then Semantic_Parent.Kind in Ada_Basic_Decl
        and then Is_Subprogram (Semantic_Parent.As_Basic_Decl);
      Semantic_Parent_Subp    : constant Basic_Decl :=
        (if Is_Semantic_Parent_Subp then Semantic_Parent.As_Basic_Decl
         else No_Basic_Decl);

      Hierarchy : constant Basic_Decl_Array :=
        (if Is_Semantic_Parent_Subp then
            Get_Subp_Hierarchy (Semantic_Parent_Subp, Units)
         else (1 .. 0 => <>));

      Param_References : Base_Id_Vectors.Vector;

   begin
      if Is_Semantic_Parent_Subp then
         --  This is a parameter of a non-anonymous subprogram declaration so
         --  rename it in whole hierarchy.

         for Decl of Hierarchy loop
            for Param_Spec of Decl.P_Subp_Spec_Or_Null.P_Params loop
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

      else
         --  This is a parameter of an access to a subprogram definition, so
         --  only replace its references.

         for Reference of
           Param_Definition.P_Canonical_Part.P_Find_All_References (Units)
         loop
            Param_References.Append (Ref (Reference).As_Base_Id);
         end loop;
      end if;

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
         --  If Decl is a top level declaration then it can be a dotted name
         --  In that case, consider the suffix as the reference.

         if Decl.P_Defining_Name.F_Name.Kind in Ada_Dotted_Name then
            Param_References.Append
              (Decl.P_Defining_Name.F_Name.As_Dotted_Name.F_Suffix);
         else
            Param_References.Append (Decl.P_Defining_Name.F_Name.As_Base_Id);
         end if;

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

   function Find_Local_Scopes
     (Node : Ada_Node'Class)
      return Ada_List_Vector
   is
      Local_Scopes_Owner : Ada_Node;

      procedure Set_Local_Scopes_Owner
        (Owner : Ada_Node;
         Stop  : in out Boolean);
      --  Callback for Find_Matching_Parents that sets
      --  Parent_Declarative_Part_Or_Decl_Owner and stops the iteration.

      function Is_Local_Scopes_Owner
        (Node : Ada_Node'Class)
         return Boolean
        with Post => (if Is_Local_Scopes_Owner'Result
                      then Is_Declarative_Part_Owner (Node)
                           xor Is_Decl_Expr_Owner (Node)
                           xor Node.Kind in Ada_Classic_Subp_Decl
                           xor Node.Kind in Ada_Generic_Subp_Decl_Range);
      --  Return True if Is_Declarative_Part_Owner (Node) or else
      --  Is_Decl_Expr_Owner (Node) or else Node.Kin in Ada_Subp_Decl_Range.

      function Process_Declarative_Part_Owner
        (Declarative_Part_Owner : Ada_Node'Class)
         return Ada_List_Vector;
      --  Gets all Declarative_Part'Class nodes of Declarative_Part_Owner and
      --  return a vector with their Ada_Node_List nodes.

      function Process_Decl_Expr_Owner
        (Decl_Expr_Owner : Expr_Function)
         return Ada_List_Vector;
      --  Returns a vector with the Basic_Decl_List of Decl_Expr_Owner

      function Process_Body_Part
        (Body_Part : Base_Subp_Body)
         return Ada_List_Vector;
      --  Checks if Is_Declarative_Part_Owner (Body_Part) or else
      --  Is_Decl_Expr_Owner (Body_Part), and if so, processes Body_Part
      --  with Process_Declarative_Part_Owner and Process_Decl_Expr_Owner
      --  respectively.

      ----------------------------
      -- Set_Local_Scopes_Owner --
      ----------------------------

      procedure Set_Local_Scopes_Owner
        (Owner : Ada_Node;
         Stop  : in out Boolean) is
      begin
         Local_Scopes_Owner := Owner;
         Stop := True;
      end Set_Local_Scopes_Owner;

      ---------------------------
      -- Is_Local_Scopes_Owner --
      ---------------------------

      function Is_Local_Scopes_Owner
        (Node : Ada_Node'Class)
         return Boolean
      is (not Node.Is_Null
          and then (Is_Declarative_Part_Owner (Node)
                    or else Is_Decl_Expr_Owner (Node)
                    or else Node.Kind in Ada_Classic_Subp_Decl
            or else Node.Kind in Ada_Generic_Subp_Decl_Range));

      ------------------------------------
      -- Process_Declarative_Part_Owner --
      ------------------------------------

      function Process_Declarative_Part_Owner
        (Declarative_Part_Owner : Ada_Node'Class)
         return Ada_List_Vector
      is
         Declarative_Parts : constant Declarative_Part_Vector :=
           Get_Declarative_Parts (Declarative_Part_Owner);

      begin
         return Result : Ada_List_Vector do
            for Declarative_Part of Declarative_Parts loop
               Result.Append (Declarative_Part.F_Decls);
            end loop;
         end return;
      end Process_Declarative_Part_Owner;

      -----------------------------
      -- Process_Decl_Expr_Owner --
      -----------------------------

      function Process_Decl_Expr_Owner
        (Decl_Expr_Owner : Expr_Function)
         return Ada_List_Vector
      is
         Decl_Expr : constant Libadalang.Analysis.Decl_Expr :=
           Decl_Expr_Owner.F_Expr.As_Paren_Expr.
             F_Expr.As_Decl_Expr;

      begin
         return Result : Ada_List_Vector do
            Result.Append (Decl_Expr.F_Decls);
         end return;
      end Process_Decl_Expr_Owner;

      -----------------------
      -- Process_Body_Part --
      -----------------------

      function Process_Body_Part
        (Body_Part : Base_Subp_Body)
         return Ada_List_Vector is
      begin
         if Is_Declarative_Part_Owner (Body_Part) then
            return Process_Declarative_Part_Owner (Body_Part);
         elsif Is_Decl_Expr_Owner (Body_Part) then
            return Process_Decl_Expr_Owner (Body_Part.As_Expr_Function);
         end if;

         return Ada_List_Vectors.Empty;
      end Process_Body_Part;

   begin
      Find_Matching_Parents
        (Node     => Node,
         Match    => Is_Local_Scopes_Owner'Access,
         Callback => Set_Local_Scopes_Owner'Access);

      if Local_Scopes_Owner.Is_Null then
         return Ada_List_Vectors.Empty;
      end if;

      if Is_Declarative_Part_Owner (Local_Scopes_Owner) then
         return Process_Declarative_Part_Owner (Local_Scopes_Owner);

      elsif Is_Decl_Expr_Owner (Local_Scopes_Owner) then
         return Process_Decl_Expr_Owner (Local_Scopes_Owner.As_Expr_Function);

      elsif Local_Scopes_Owner.Kind in Ada_Classic_Subp_Decl then
         declare
            Body_Part : constant Base_Subp_Body :=
              Local_Scopes_Owner.As_Classic_Subp_Decl.P_Body_Part;

         begin
            if not Body_Part.Is_Null then
               return Process_Body_Part (Body_Part);
            end if;
         end;

      else
         Assert (Local_Scopes_Owner.Kind in Ada_Generic_Subp_Decl_Range);

         declare
            Body_Part : constant Base_Subp_Body :=
              Local_Scopes_Owner.As_Generic_Subp_Decl.P_Body_Part;

         begin
            if not Body_Part.Is_Null then
               return Process_Body_Part (Body_Part);
            end if;
         end;
      end if;

      return Ada_List_Vectors.Empty;
   end Find_Local_Scopes;

   ------------------------
   -- Find_Nested_Scopes --
   ------------------------

   function Find_Nested_Scopes
     (Node : Ada_Node'Class)
      return Declarative_Part_Vector
   is
      Nested_Declarative_Parts : Declarative_Part_Vectors.Vector;

      Parent_Declarative_Part_Owner : Ada_Node;

      procedure Set_Parent_Declarative_Part_Owner
        (Owner : Ada_Node;
         Stop  : in out Boolean);
      --  Callback of Find_Matching_Parents that sets
      --  Parent_Declarative_Part_Owner and stops the iteration.

      function Append_Nested_Declarative_Parts
        (This_Node : Ada_Node'Class)
         return Visit_Status;
      --  Callback of Find_Matching_Parents that checks if This_Node is a
      --  Declarative_Part, and if so, appends it to Nested_Declarative_Parts.

      -------------------------------------
      -- Append_Nested_Declarative_Parts --
      -------------------------------------

      function Append_Nested_Declarative_Parts
        (This_Node : Ada_Node'Class)
         return Visit_Status is
      begin
         if This_Node.Kind in Ada_Declarative_Part_Range then
            Nested_Declarative_Parts.Append (This_Node.As_Declarative_Part);
         end if;

         return Into;
      end Append_Nested_Declarative_Parts;

      ---------------------------------------
      -- Set_Parent_Declarative_Part_Owner --
      ---------------------------------------

      procedure Set_Parent_Declarative_Part_Owner
        (Owner : Ada_Node;
         Stop  : in out Boolean) is
      begin
         Parent_Declarative_Part_Owner := Owner;
         Stop := True;
      end Set_Parent_Declarative_Part_Owner;

      Body_Decl_Part    : Declarative_Part := No_Declarative_Part;
      Public_Decl_Part  : Public_Part := No_Public_Part;
      Private_Decl_Part : Private_Part := No_Private_Part;
      Stmts             : Handled_Stmts := No_Handled_Stmts;

   begin
      if Node.Is_Null then
         return Nested_Declarative_Parts;
      end if;

      if Is_Declarative_Part_Owner (Node) then
         Parent_Declarative_Part_Owner := Node.As_Ada_Node;

      else
         Find_Matching_Parents
           (Node     => Node,
            Match    => Is_Declarative_Part_Owner'Access,
            Callback => Set_Parent_Declarative_Part_Owner'Access);
      end if;

      if Parent_Declarative_Part_Owner.Is_Null then
         return Nested_Declarative_Parts;
      end if;

      case Parent_Declarative_Part_Owner.Kind is
         when Ada_Decl_Block_Range =>
            Body_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Decl_Block.F_Decls;
            Stmts := Parent_Declarative_Part_Owner.As_Decl_Block.F_Stmts;

         when Ada_Entry_Body_Range =>
            Body_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Entry_Body.F_Decls;
            Stmts := Parent_Declarative_Part_Owner.As_Entry_Body.F_Stmts;

         when Ada_Package_Body_Range =>
            Body_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Package_Body.F_Decls;

            declare
               Pkg_Decl : constant Basic_Decl :=
                 Parent_Declarative_Part_Owner.As_Package_Body.
                   P_Canonical_Part;

            begin
               if not Pkg_Decl.Is_Null then
                  Public_Decl_Part :=
                    Pkg_Decl.As_Base_Package_Decl.F_Public_Part;
                  Private_Decl_Part :=
                    Pkg_Decl.As_Base_Package_Decl.F_Private_Part;
               end if;
            end;

         when Ada_Protected_Body_Range =>
            Body_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Entry_Body.F_Decls;
            Stmts := Parent_Declarative_Part_Owner.As_Entry_Body.F_Stmts;

         when Ada_Subp_Body_Range =>
            Body_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Subp_Body.F_Decls;
            Stmts := Parent_Declarative_Part_Owner.As_Subp_Body.F_Stmts;

         when Ada_Task_Body_Range =>
            Body_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Task_Body.F_Decls;
            Stmts := Parent_Declarative_Part_Owner.As_Task_Body.F_Stmts;

         when Ada_Base_Package_Decl =>
            Public_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Base_Package_Decl.F_Public_Part;
            Private_Decl_Part :=
              Parent_Declarative_Part_Owner.
                As_Base_Package_Decl.F_Private_Part;

            declare
               Pkg_Body : constant Package_Body :=
                 Parent_Declarative_Part_Owner.As_Base_Package_Decl.
                   P_Body_Part;

            begin
               if not Pkg_Body.Is_Null then
                  Body_Decl_Part := Pkg_Body.F_Decls;
               end if;
            end;

         when Ada_Protected_Def_Range =>
            Public_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Protected_Def.F_Public_Part;
            Private_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Protected_Def.F_Private_Part;

         when Ada_Task_Def_Range =>
            Public_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Task_Def.F_Public_Part;
            Private_Decl_Part :=
              Parent_Declarative_Part_Owner.As_Task_Def.F_Private_Part;

         when others =>
            raise Assertion_Error;
      end case;

      if not Body_Decl_Part.Is_Null then
         Traverse
           (Body_Decl_Part.F_Decls,
            Append_Nested_Declarative_Parts'Access);
      end if;

      if not Public_Decl_Part.Is_Null then
         Traverse
           (Public_Decl_Part.F_Decls,
            Append_Nested_Declarative_Parts'Access);
      end if;

      if not Private_Decl_Part.Is_Null then
         Traverse
           (Private_Decl_Part.F_Decls,
            Append_Nested_Declarative_Parts'Access);
      end if;

      if not Stmts.Is_Null then
         Traverse (Stmts, Append_Nested_Declarative_Parts'Access);
      end if;

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

   ------------------------
   -- Find_Previous_Part --
   ------------------------

   function Find_Previous_Part
     (Definition         : Defining_Name;
      Trace              : GNATCOLL.Traces.Trace_Handle;
      Imprecise_Fallback : Boolean := False)
      return Defining_Name
   is
      Next   : Defining_Name;
      use type Defining_Name;
   begin
      Next :=
        Definition.P_Previous_Part (Imprecise_Fallback => Imprecise_Fallback);

      if Next = Definition then
         return No_Defining_Name;
      else
         return Next;
      end if;
   exception
      when E :  Libadalang.Common.Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Previous_Part;

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
         if Node.Is_Null then
            return Libadalang.Common.Into;
         end if;

         --  Note: in this function, we are simply looking at the first
         --  result that matches.
         --  TODO: improve this by find all entities that match, and
         --  finding the best through a distance/scoring heuristics.
         if Node.Kind in Libadalang.Common.Ada_Basic_Decl then
            declare
               Decl     : constant Basic_Decl := Node.As_Basic_Decl;
               Def      : constant Defining_Name := Decl.P_Defining_Name;
               Def_Name : constant Langkit_Support.Text.Text_Type :=
                 Decl.P_Fully_Qualified_Name;
            begin
               --  Search a declaration with the same qualified_name which is
               --  not Definition itself.
               if Def /= Definition
                 and then Def_Name = Qualified_Name
               then
                  Found := Def;
                  return Libadalang.Common.Stop;
               end if;
            end;
         end if;

         return Libadalang.Common.Into;

      exception
         when Property_Error =>
            return Libadalang.Common.Into;
      end Matches;

      Parent_Node  : Ada_Node := No_Ada_Node;
      Current_Root : Defining_Name;
      Other_Root   : Defining_Name;
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

      --  First obtain the highest level declaration of the current tree
      declare
         All_Parents : constant Ada_Node_Array := Definition.Parents;
      begin
         for P of reverse All_Parents loop
            if P.Kind in Ada_Basic_Decl then
               Parent_Node := P;
               exit;
            end if;
         end loop;
      end;

      if Parent_Node = No_Ada_Node then
         return No_Defining_Name;
      end if;

      --  Traverse the current tree. The visiting function assigns the matching
      --  result, if any, to Found.
      Parent_Node.Traverse (Matches'Unrestricted_Access);

      if Found = No_Defining_Name then
         Current_Root := Parent_Node.As_Basic_Decl.P_Defining_Name;
         --  Try Next_Part and then Previous_Part to find the body/spec of
         --  the current root.
         Other_Root := Laltools.Common.Find_Next_Part (Current_Root, Trace);
         if Other_Root = No_Defining_Name then
            Other_Root := Laltools.Common.Find_Previous_Part
              (Current_Root, Trace);
         end if;
         --  Traverse the other root
         if Other_Root /= No_Defining_Name then
            Other_Root.Parent.Traverse (Matches'Unrestricted_Access);
         end if;
      end if;

      return Found;

   exception
      when E : Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Other_Part_Fallback;

   --------------------
   -- Find_Subp_Body --
   --------------------

   function Find_Subp_Body (Subp : Basic_Decl'Class) return Base_Subp_Body is
   begin
      case Subp.Kind is
         when Ada_Subp_Decl_Range =>
            return Subp.As_Subp_Decl.P_Body_Part;

         when Ada_Generic_Subp_Decl_Range =>
            return Subp.As_Generic_Subp_Decl.P_Body_Part;

         when others =>
            return No_Base_Subp_Body;
      end case;
   end Find_Subp_Body;

   --------------------------
   -- Get_Compilation_Unit --
   --------------------------

   function Get_Compilation_Unit
     (Node : Ada_Node'Class)
         return Compilation_Unit
   is
      C_Unit : Ada_Node :=
        (if Node.Is_Null then No_Ada_Node else Node.As_Ada_Node);

   begin
      --  Iterate throught the parents until a Compilation_Unit node is
      --  found
      while not C_Unit.Is_Null
        and then not (C_Unit.Kind in Ada_Compilation_Unit_Range)
      loop
         C_Unit := C_Unit.Parent;
      end loop;

      if C_Unit.Is_Null
        or else not (C_Unit.Kind in Ada_Compilation_Unit_Range)
      then
         return No_Compilation_Unit;
      end if;

      return C_Unit.As_Compilation_Unit;
   end Get_Compilation_Unit;

   ------------------------------
   -- Get_Insert_With_Location --
   ------------------------------

   function Get_Insert_With_Location
     (Node      : Compilation_Unit'Class;
      Pack_Name : Text_Type;
      Last      : out Boolean)
      return Source_Location
   is
      --  Cover the no with clause case
      Res                  : Source_Location := Start_Sloc (Node.Sloc_Range);
      Searching_Insert_Loc : Boolean := True;
   begin
      for N of Node.F_Prelude loop
         if N.Kind in Ada_With_Clause_Range then
            --  Handle list of packages: "with A, B, C;"
            for P of N.As_With_Clause.F_Packages loop
               if Pack_Name = P.Text then
                  --  We are already withed
                  return No_Source_Location;
               elsif Searching_Insert_Loc
                 and then Pack_Name < P.Text
               then
                  --  Assuming the with clauses are sorted alphabetically,
                  --  the insert location is before the first clause higher
                  --  than us. (Attention we must insert before N and not P)
                  Last := False;
                  Res := Start_Sloc (N.Sloc_Range);
                  Searching_Insert_Loc := False;
               end if;
            end loop;
         end if;

         if Searching_Insert_Loc
           and then N.Kind in Ada_With_Clause_Range | Ada_Use_Package_Clause
         then
            --  If the highest alphabetically, insert it after the last
            --  with clause. To not split a pair also keep track of the last
            --  use clause.
            Last := True;
            Res := End_Sloc (N.Sloc_Range);
         end if;
      end loop;

      return Res;
   end Get_Insert_With_Location;

   ---------------------------
   -- Find_Matching_Parents --
   ---------------------------

   procedure Find_Matching_Parents
     (Node     : Ada_Node'Class;
      Match    : not null access function
        (Node : Ada_Node'Class) return Boolean;
      Callback : not null access procedure
        (Parent : Ada_Node;
         Stop   : in out Boolean))
   is
      Stop   : Boolean := False;
      Parent : Ada_Node :=
        (if Node.Is_Null then No_Ada_Node else Node.As_Ada_Node);

   begin
      while not Stop loop
         Parent := Parent.Parent;

         exit when Parent.Is_Null;

         if Match (Parent) then
            Callback (Parent, Stop);
         end if;
      end loop;
   end Find_Matching_Parents;

   --------------------------------------
   -- Get_CU_Visible_Declarative_Parts --
   --------------------------------------

   function Get_CU_Visible_Declarative_Parts
     (Node       : Ada_Node'Class;
      Skip_First : Boolean := False)
      return Declarative_Part_Vectors.Vector
   is
      Decl_Parts : Declarative_Part_Vector;

      Parent : Ada_Node := No_Ada_Node;

      procedure Ignore_First (Owner : Ada_Node; Stop : in out Boolean);
      --  This procedure is used as a Callback of Find_Matching_Parents.
      --  It simply sets Parent to the first matching parent and stops the
      --  iterative process.

      procedure Append_Declarative_Parts
        (Owner : Ada_Node;
         Stop : in out Boolean);
      --  This procedure is used as a Callback of Find_Matching_Parents and it
      --  will add every Declarative_Part of Owner to Decl_Parts, including
      --  the Public_Part and Private_Part (if existent) if Owner kind is in
      --  Ada_Package_Body_Range, Ada_Base_Package_Decl,
      --  Ada_Protected_Def_Range, Ada_Protected_Def_Range or
      --  Ada_Task_Def_Range.

      ------------------
      -- Ignore_First --
      ------------------

      procedure Ignore_First (Owner : Ada_Node; Stop : in out Boolean) is
      begin
         Stop := True;
         Parent := Owner;
      end Ignore_First;

      ------------------------------
      -- Append_Declarative_Parts --
      ------------------------------

      procedure Append_Declarative_Parts
        (Owner : Ada_Node;
         Stop  : in out Boolean)
      is
         pragma Unreferenced (Stop);

      begin
         case Owner.Kind is
            when Ada_Decl_Block_Range =>
               Decl_Parts.Append (Owner.As_Decl_Block.F_Decls);

            when Ada_Entry_Body_Range =>
               Decl_Parts.Append (Owner.As_Entry_Body.F_Decls);

            when Ada_Package_Body_Range =>
               declare
                  Pkg_Decl          : constant Base_Package_Decl :=
                    Owner.As_Package_Body.P_Canonical_Part.
                      As_Base_Package_Decl;
                  Private_Decl_Part : constant Private_Part :=
                    Pkg_Decl.F_Private_Part;

               begin
                  Decl_Parts.Append (Pkg_Decl.F_Public_Part);

                  if not Private_Decl_Part.Is_Null then
                     Decl_Parts.Append (Private_Decl_Part);
                  end if;

                  Decl_Parts.Append (Owner.As_Package_Body.F_Decls);
               end;

            when Ada_Protected_Body_Range =>
               Decl_Parts.Append (Owner.As_Protected_Body.F_Decls);

            when Ada_Subp_Body_Range =>
               Decl_Parts.Append (Owner.As_Subp_Body.F_Decls);

            when Ada_Task_Body_Range =>
               Decl_Parts.Append (Owner.As_Task_Body.F_Decls);

            when Ada_Base_Package_Decl =>
               declare
                  Pkg_Body          : constant Package_Body :=
                    Owner.As_Base_Package_Decl.P_Body_Part;
                  Private_Decl_Part : constant Private_Part :=
                    Owner.As_Base_Package_Decl.F_Private_Part;

               begin
                  Decl_Parts.Append (Owner.As_Base_Package_Decl.F_Public_Part);

                  if not Private_Decl_Part.Is_Null then
                     Decl_Parts.Append (Private_Decl_Part);
                  end if;

                  if not Pkg_Body.Is_Null then
                     Decl_Parts.Append (Pkg_Body.F_Decls);
                  end if;
               end;

            when Ada_Protected_Def_Range =>
               declare
                  Private_Decl_Part : constant Private_Part :=
                    Owner.As_Protected_Def.F_Private_Part;
               begin
                  Decl_Parts.Append (Owner.As_Protected_Def.F_Public_Part);

                  if not Private_Decl_Part.Is_Null then
                     Decl_Parts.Append (Private_Decl_Part);
                  end if;
               end;

            when Ada_Task_Def_Range =>
               declare
                  Private_Decl_Part : constant Private_Part :=
                    Owner.As_Task_Def.F_Private_Part;
               begin
                  Decl_Parts.Append (Owner.As_Task_Def.F_Public_Part);

                  if not Private_Decl_Part.Is_Null then
                     Decl_Parts.Append (Private_Decl_Part);
                  end if;
               end;

            when others =>
               raise Assertion_Error;
         end case;
      end Append_Declarative_Parts;

   begin
      if Node.Is_Null then
         return Decl_Parts;
      end if;

      --  For detecting if a declaration hides another one, we are not
      --  interested in the first declarative part since any conflicts there
      --  would be considered name collisions.

      if Skip_First then
         Find_Matching_Parents
           (Node     => Node,
            Match    => Is_Declarative_Part_Owner'Access,
            Callback => Ignore_First'Access);

         if Parent.Is_Null then
            return Decl_Parts;
         end if;

      else
         Parent := Node.As_Ada_Node;
      end if;

      Find_Matching_Parents
        (Node     => Parent,
         Match    => Is_Declarative_Part_Owner'Access,
         Callback => Append_Declarative_Parts'Access);

      return Decl_Parts;
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
     (Node         : Ada_Node'Class;
      Private_Part : Boolean := False)
      return Declarative_Part
   is
      function Get_Declarative_Part_From_Owner
        (This_Node : Ada_Node'Class)
         return Declarative_Part;
      --  Gets the Declarative_Part node of This_Node

      -------------------------------------
      -- Get_Declarative_Part_From_Owner --
      -------------------------------------

      function Get_Declarative_Part_From_Owner
        (This_Node : Ada_Node'Class)
         return Declarative_Part is
      begin
         case This_Node.Kind is
            when Ada_Decl_Block_Range =>
               return This_Node.As_Decl_Block.F_Decls;

            when Ada_Entry_Body_Range =>
               return This_Node.As_Entry_Body.F_Decls;

            when Ada_Package_Body_Range =>
               return This_Node.As_Package_Body.F_Decls;

            when Ada_Protected_Body_Range =>
               return This_Node.As_Entry_Body.F_Decls;

            when Ada_Subp_Body_Range =>
               return This_Node.As_Subp_Body.F_Decls;

            when Ada_Task_Body_Range =>
               return This_Node.As_Task_Body.F_Decls;

            when Ada_Base_Package_Decl =>
               return (if Private_Part then
                          This_Node.As_Base_Package_Decl.F_Private_Part.
                            As_Declarative_Part
                       else
                          This_Node.As_Base_Package_Decl.F_Public_Part.
                            As_Declarative_Part);

            when Ada_Protected_Def_Range =>
               return (if Private_Part then
                          This_Node.As_Protected_Def.F_Private_Part.
                            As_Declarative_Part
                       else
                          This_Node.As_Protected_Def.F_Public_Part.
                            As_Declarative_Part);

            when Ada_Task_Def_Range =>
               return (if Private_Part then
                          This_Node.As_Task_Def.F_Private_Part.
                            As_Declarative_Part
                       else
                          This_Node.As_Task_Def.F_Public_Part.
                            As_Declarative_Part);

            when others =>
               raise Assertion_Error;
         end case;
      end Get_Declarative_Part_From_Owner;

   begin
      if Node.Kind in Ada_Handled_Stmts_Range
        and then Is_Declarative_Part_Owner (Node.Parent)
      then
         return Get_Declarative_Part_From_Owner (Node.Parent);

      elsif Is_Declarative_Part_Owner (Node) then
         return Get_Declarative_Part_From_Owner (Node);

      else
         raise Assertion_Error;
      end if;
   end Get_Declarative_Part;

   ---------------------------
   -- Get_Declarative_Parts --
   ---------------------------

   function Get_Declarative_Parts
     (Node : Ada_Node'Class)
      return Declarative_Part_Vector
   is
      Declarative_Parts : Declarative_Part_Vector;

      Body_Decl_Part    : Declarative_Part := No_Declarative_Part;
      Public_Decl_Part  : Public_Part := No_Public_Part;
      Private_Decl_Part : Private_Part := No_Private_Part;

      procedure Safe_Append (Decl_Part : Declarative_Part'Class);

      procedure Safe_Append (Decl_Part : Declarative_Part'Class) is
      begin
         if not Decl_Part.Is_Null then
            Declarative_Parts.Append (Decl_Part);
         end if;
      end Safe_Append;

   begin
      case Node.Kind is
         when Ada_Decl_Block_Range =>
            Body_Decl_Part := Node.As_Decl_Block.F_Decls;

         when Ada_Entry_Body_Range =>
            Body_Decl_Part := Node.As_Entry_Body.F_Decls;

         when Ada_Package_Body_Range =>
            Body_Decl_Part := Node.As_Package_Body.F_Decls;

            declare
               Pkg_Decl : constant Basic_Decl :=
                 Node.As_Package_Body.P_Canonical_Part;

            begin
               if not Pkg_Decl.Is_Null then
                  Public_Decl_Part :=
                    Pkg_Decl.As_Base_Package_Decl.F_Public_Part;
                  Private_Decl_Part :=
                    Pkg_Decl.As_Base_Package_Decl.F_Private_Part;
               end if;
            end;

         when Ada_Protected_Body_Range =>
            Body_Decl_Part := Node.As_Entry_Body.F_Decls;

         when Ada_Subp_Body_Range =>
            Body_Decl_Part := Node.As_Subp_Body.F_Decls;

         when Ada_Task_Body_Range =>
            Body_Decl_Part := Node.As_Task_Body.F_Decls;

         when Ada_Base_Package_Decl =>
            Public_Decl_Part := Node.As_Base_Package_Decl.F_Public_Part;
            Private_Decl_Part := Node.As_Base_Package_Decl.F_Private_Part;

            declare
               Pkg_Body : constant Package_Body :=
                 Node.As_Base_Package_Decl.P_Body_Part;

            begin
               if not Pkg_Body.Is_Null then
                  Body_Decl_Part := Pkg_Body.F_Decls;
               end if;
            end;

         when Ada_Protected_Def_Range =>
            Public_Decl_Part := Node.As_Protected_Def.F_Public_Part;
            Private_Decl_Part := Node.As_Protected_Def.F_Private_Part;

         when Ada_Task_Def_Range =>
            Public_Decl_Part := Node.As_Task_Def.F_Public_Part;
            Private_Decl_Part := Node.As_Task_Def.F_Private_Part;

         when others =>
            raise Assertion_Error;
      end case;

      Safe_Append (Body_Decl_Part);
      Safe_Append (Public_Decl_Part);
      Safe_Append (Private_Decl_Part);

      return Declarative_Parts;
   end Get_Declarative_Parts;

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
     (Pkg_Decl : Base_Package_Decl'Class)
      return Declarative_Part_Vectors.Vector
   is
      Decls : Declarative_Part_Vectors.Vector;

      --  A Base_Package_Decl always has a Public_Part but might not have a
      --  Private_Part or an associated Package_Body with a Declarative_Part.

      Private_Part : Declarative_Part;
      Body_Part    : Declarative_Part;
   begin
      if Pkg_Decl.Is_Null then
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
     (Pkg_Decl : Base_Package_Decl'Class)
      return Ada_List_Vector is
   begin
      return Result : Ada_List_Vector do
         Append_If_Not_Null
           (Result, Get_Package_Decl_Public_Decls (Pkg_Decl));
         Append_If_Not_Null
           (Result, Get_Package_Decl_Private_Decls (Pkg_Decl));
         Append_If_Not_Null
           (Result, Get_Package_Body_Decls (Pkg_Decl.P_Body_Part));
      end return;
   end Get_Package_Decls;

   -----------------------------------------------
   -- Get_Package_Decl_Private_Declarative_Part --
   -----------------------------------------------

   function Get_Package_Decl_Private_Declarative_Part
     (Pkg_Decl : Base_Package_Decl'Class)
      return Declarative_Part is
   begin
      if Pkg_Decl.Is_Null or else Pkg_Decl.F_Private_Part.Is_Null then
         return No_Declarative_Part;
      end if;

      return Pkg_Decl.F_Private_Part.As_Declarative_Part;
   end Get_Package_Decl_Private_Declarative_Part;

   ------------------------------------
   -- Get_Package_Decl_Private_Decls --
   ------------------------------------

   function Get_Package_Decl_Private_Decls
     (Pkg_Decl : Base_Package_Decl'Class)
      return Ada_Node_List is
   begin
      if Pkg_Decl.Is_Null or else Pkg_Decl.F_Private_Part.Is_Null then
         return No_Ada_Node_List;
      end if;

      return Pkg_Decl.F_Private_Part.F_Decls;
   end Get_Package_Decl_Private_Decls;

   ----------------------------------------------
   -- Get_Package_Decl_Public_Declarative_Part --
   ----------------------------------------------

   function Get_Package_Decl_Public_Declarative_Part
     (Pkg_Decl : Base_Package_Decl'Class)
      return Declarative_Part is
   begin
      if Pkg_Decl.Is_Null then
         return No_Declarative_Part;
      end if;

      return Pkg_Decl.F_Public_Part.As_Declarative_Part;
   end Get_Package_Decl_Public_Declarative_Part;

   -----------------------------------
   -- Get_Package_Decl_Public_Decls --
   -----------------------------------

   function Get_Package_Decl_Public_Decls
     (Pkg_Decl : Base_Package_Decl'Class)
      return Ada_Node_List is
   begin
      if Pkg_Decl.Is_Null then
         return No_Ada_Node_List;
      end if;

      return Pkg_Decl.F_Public_Part.F_Decls;
   end Get_Package_Decl_Public_Decls;

   --------------------------
   -- Get_Param_Spec_Index --
   --------------------------

   function Get_Param_Spec_Index (Target : Param_Spec) return Positive
   is
      Index : Positive := 1;

   begin
      for Param_Spec of Target.Parent.As_Param_Spec_List loop
         if Param_Spec = Target then
            return Index;
         end if;

         Index := Index + 1;
      end loop;

      raise Program_Error with "Bug detected";
   end Get_Param_Spec_Index;

   ----------------------------------
   -- Get_Parameter_Absolute_Index --
   ----------------------------------

   function Get_Parameter_Absolute_Index
     (Target : Defining_Name)
      return Natural
   is
      Index : Positive := 1;

   begin
      for Param_Spec of
        Get_Subp_Params (Target.P_Parent_Basic_Decl).F_Params
      loop
         for Parameter of Param_Spec.F_Ids loop
            if Target = Parameter then
               return Index;
            end if;

            Index := Index + 1;
         end loop;
      end loop;

      raise Program_Error with "Bug detected";
   end Get_Parameter_Absolute_Index;

   ------------------------
   -- Get_Parameter_Name --
   ------------------------

   function Get_Parameter_Name
     (Parameters      : Params'Class;
      Parameter_Index : Positive)
      return Text_Type
   is
      Index  : Positive := 1;

   begin
      for Param_Spec of Parameters.F_Params loop
         for Parameter of Param_Spec.F_Ids loop
            if Index = Parameter_Index then
               return Parameter.As_Defining_Name.F_Name.Text;
            end if;

            Index := Index + 1;
         end loop;
      end loop;

      raise Assertion_Error;
   end Get_Parameter_Name;

   ------------------------
   -- Get_Parameter_Name --
   ------------------------

   function Get_Parameter_Name
     (Subp            : Basic_Decl'Class;
      Parameter_Index : Positive)
      return Text_Type
   is (Get_Parameter_Name (Get_Subp_Params (Subp), Parameter_Index));

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
   is (if Subp_B.Is_Null then No_Ada_Node_List else Subp_B.F_Decls.F_Decls);

   ---------------------
   -- Get_Subp_Params --
   ---------------------

   function Get_Subp_Params
     (Subp : Basic_Decl'Class)
      return Params is
     (Get_Subp_Spec_Params (Get_Subp_Spec (Subp)));

   -------------------
   -- Get_Subp_Spec --
   -------------------

   function Get_Subp_Spec (Subp : Basic_Decl'Class) return Base_Subp_Spec is
     (if Subp.Is_Null then No_Base_Subp_Spec
      else Subp.P_Subp_Spec_Or_Null (True));

   --------------------------
   -- Get_Subp_Spec_Params --
   --------------------------

   function Get_Subp_Spec_Params
     (Subp_Spec : Base_Subp_Spec'Class)
      return Params is
   begin
      if Subp_Spec.Is_Null then
         return No_Params;
      end if;

      case Ada_Base_Subp_Spec (Subp_Spec.Kind) is
         when Ada_Entry_Spec_Range
            => return Subp_Spec.As_Entry_Spec.F_Entry_Params;
         when Ada_Enum_Subp_Spec_Range
            => return No_Params;
         when Ada_Subp_Spec_Range
            => return Subp_Spec.As_Subp_Spec.F_Subp_Params;
         when Ada_Predefined_Bin_Op_Spec
            => return No_Params;
         when Ada_Predefined_Un_Op_Spec
            => return No_Params;
      end case;
   end Get_Subp_Spec_Params;

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

   --------------------------------
   -- Get_Use_Units_Public_Parts --
   --------------------------------

   function Get_Use_Units_Public_Parts
     (Node : Ada_Node'Class)
      return Declarative_Part_Vector
   is
      Public_Parts : Declarative_Part_Vector;

      Node_Unit  : constant Compilation_Unit := Get_Compilation_Unit (Node);
      Used_Units : constant Compilation_Unit_Array :=
        Get_Used_Units (Node_Unit);

      procedure Process_Top_Level_Decl (TLD : Basic_Decl);
      --  Processes the top level declaration of a unit if it is a
      --  Base_Package_Decl, Generic_Package_Instantiation or a
      --  Package_Rename_Decl.
      --  Processes by getting the public part of the package, casting it
      --  as Declarative_Part and adding it to Declarative_Parts.
      --  This package can be recursive up to one time, i.e., it can call
      --  itself if TLD is a Package_Rename_Decl, but then it won't call
      --  itself again.

      ----------------------------
      -- Process_Top_Level_Decl --
      ----------------------------

      procedure Process_Top_Level_Decl (TLD : Basic_Decl) is
         --  Designated_Generic_Decl
         DGD : Basic_Decl;

      begin
         if not TLD.Is_Null then
            case TLD.Kind is
               when Ada_Base_Package_Decl =>
                  Public_Parts.Append (TLD.As_Base_Package_Decl.F_Public_Part);

               when Ada_Generic_Package_Instantiation_Range =>
                  --  If TLD is a Generic_Package_Instantiation then we need to
                  --  get its designated generic declaration, which can be
                  --  null.

                  DGD :=
                    TLD.As_Generic_Instantiation.P_Designated_Generic_Decl;

                  if not DGD.Is_Null
                    and then DGD.Kind in Ada_Generic_Package_Decl_Range
                  then
                     Public_Parts.Append
                       (DGD.As_Generic_Package_Decl.F_Package_Decl.
                          F_Public_Part.As_Declarative_Part);
                  end if;

               when Ada_Package_Renaming_Decl_Range =>
                  --  If TLD is a Package_Renaming_Decl, unwind the renames
                  --  the final declaration if reached. This will be a package
                  --  Decl, which can be considered as a TLD. Therefore,
                  --  call recursively call Process_Top_Level_Decl with the
                  --  final TLD.

                  Process_Top_Level_Decl
                    (TLD.As_Package_Renaming_Decl.P_Final_Renamed_Package);

               when others =>
                  raise Assertion_Error;
            end case;
         end if;
      end Process_Top_Level_Decl;

   begin
      for Used_Unit of Used_Units loop
         --  The array returned by Get_Used_Units does not contain null
         --  Compilation_Units, so it safe to try to get the top level
         --  declaration and process it.

         Process_Top_Level_Decl
           (Used_Unit.P_Top_Level_Decl (Used_Unit.Unit));
      end loop;

      return Public_Parts;
   end Get_Use_Units_Public_Parts;

   --------------------
   -- Get_Used_Units --
   --------------------

   function Get_Used_Units
     (Node : Compilation_Unit'Class)
         return Compilation_Unit_Array
   is
      package Compilation_Unit_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Compilation_Unit,
         "="          => "=");

      Used_Units : Compilation_Unit_Vectors.Vector;

   begin
      if Node.Is_Null then
         return R : constant Compilation_Unit_Array (1 .. 0) := (others => <>);
      end if;

      for Clause of Node.F_Prelude loop
         if Clause.Kind in Ada_Use_Package_Clause_Range then
            for Use_Clause of Clause.As_Use_Package_Clause.F_Packages loop
               declare
                  C_Unit : constant Compilation_Unit :=
                    Get_Compilation_Unit (Use_Clause.P_Referenced_Decl);
               begin
                  if not C_Unit.Is_Null then
                     Used_Units.Append (C_Unit);
                  end if;
               end;
            end loop;
         end if;
      end loop;

      --  Copy the Used_Units elements to an array

      return R : Compilation_Unit_Array
        (1 .. Integer (Used_Units.Length))
      do
         declare
            Idx : Positive := 1;

         begin
            for U of Used_Units loop
               R (Idx) := U;
               Idx := Idx + 1;
            end loop;
         end;
      end return;
   end Get_Used_Units;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Map     : in out Source_Location_Range_Map;
      Key     : String;
      Element : Source_Location_Range) is
   begin
      if Element = No_Source_Location_Range then
         return;
      end if;

      if Map.Contains (Key)
      then
         if not Map.Reference (Key).Contains (Element)
         then
            Map.Reference (Key).Insert (Element);
         end if;
      else
         declare
            S : Source_Location_Range_Set;
         begin
            S.Insert (Element);
            Map.Insert (Key, S);
         end;
      end if;
   end Insert;

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

   -------------------------------
   -- Is_Declarative_Part_Owner --
   -------------------------------

   function Is_Declarative_Part_Owner
     (Node : Ada_Node'Class)
      return Boolean
   is (not Node.Is_Null
       and then Node.Kind in
         Ada_Decl_Block_Range
           | Ada_Entry_Body_Range
             | Ada_Package_Body_Range
               | Ada_Protected_Body_Range
                 | Ada_Subp_Body_Range
                   | Ada_Task_Body_Range
                     | Ada_Base_Package_Decl
                       | Ada_Protected_Def_Range
                         | Ada_Task_Def_Range);

   ------------------------
   -- Is_Decl_Expr_Owner --
   ------------------------
   function Is_Decl_Expr_Owner
     (Node : Ada_Node'Class)
      return Boolean
   is (not Node.Is_Null
       and then Node.Kind in Ada_Expr_Function
       and then Node.As_Expr_Function.F_Expr.Kind in
         Ada_Paren_Expr_Range
       and then Node.As_Expr_Function.F_Expr.As_Paren_Expr.F_Expr.Kind in
         Ada_Decl_Expr_Range);

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

   ------------
   -- Length --
   ------------

   function Length (List : Assoc_List) return Natural
   is
      L : Natural := 0;
   begin
      for Node of List loop
         L := L + 1;
      end loop;

      return L;
   end Length;

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

   ------------
   -- Length --
   ------------

   function Length (List : Defining_Name_List) return Natural
   is
      L : Natural := 0;
   begin
      for Node of List loop
         L := L + 1;
      end loop;

      return L;
   end Length;

   ------------
   -- Length --
   ------------

   function Length (List : Param_Spec_List) return Natural
   is
      L : Natural := 0;
   begin
      for Node of List loop
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

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Left  : in out Source_Location_Range_Map;
      Right : Source_Location_Range_Map)
   is
      Map_Cursor : Source_Location_Range_Maps.Cursor := Right.First;

   begin
      while Source_Location_Range_Maps.Has_Element (Map_Cursor) loop
         if Left.Contains (Source_Location_Range_Maps.Key (Map_Cursor)) then
            declare
               Set_Cursor : Source_Location_Range_Sets.Cursor :=
                 Right.Constant_Reference (Map_Cursor).First;

            begin
               while Source_Location_Range_Sets.Has_Element (Set_Cursor) loop
                  Left.Reference (Source_Location_Range_Maps.Key (Map_Cursor))
                    .Insert (Source_Location_Range_Sets.Element (Set_Cursor));
                  Source_Location_Range_Sets.Next (Set_Cursor);
               end loop;
            end;

         else
            Left.Insert
              (Source_Location_Range_Maps.Key (Map_Cursor),
               Source_Location_Range_Maps.Element (Map_Cursor));
         end if;

         Source_Location_Range_Maps.Next (Map_Cursor);
      end loop;
   end Merge;

   ------------------
   -- Resolve_Name --
   ------------------

   function Resolve_Name
     (Name_Node : Name;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : out Boolean) return Defining_Name
   is
      Result : Defining_Name;
      Failsafe_Result : Refd_Def;
      use type Defining_Name;
   begin
      Imprecise := False;

      if Name_Node.Is_Null then
         return No_Defining_Name;
      --  P_Failsafe_Referenced_Def_Name doesn't work on the decl itself
      elsif Name_Node.P_Is_Defining then
         Result := Name_Node.P_Enclosing_Defining_Name.P_Canonical_Part;
      else
         Failsafe_Result := Name_Node.P_Failsafe_Referenced_Def_Name
           (Imprecise_Fallback => True);
         case Kind (Failsafe_Result) is
            when Precise =>
            --  Nothing extra to do here
               null;
            when Libadalang.Common.Imprecise =>
               Imprecise := True;
            when Error =>
               Imprecise := True;
               return No_Defining_Name;
            when Noref =>
               return No_Defining_Name;
         end case;

         Result := Defining_Name (Def_Name (Failsafe_Result));

         if Result /= No_Defining_Name then
            Result := Result.P_Canonical_Part;
         end if;
      end if;

      return Result;

   exception
      when E : Property_Error =>
         Log (Trace, E);
         Imprecise := True;
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

      --  P_Referenced_Defining_Name doesn't work on the decl itself
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

   ---------------------
   -- Validate_Syntax --
   ---------------------

   function Validate_Syntax
     (Value : Ada.Strings.Unbounded.Unbounded_String;
      Rule  : Grammar_Rule)
      return Boolean
   is
      Unit : constant Analysis_Unit :=
        Create_Context.Get_From_Buffer
          (Filename => "", Buffer => Value, Rule => Rule);

   begin
      if Unit.Has_Diagnostics then
         return False;
      else

         return True;
      end if;

   exception
      when others =>
         return False;
   end Validate_Syntax;

end Laltools.Common;
