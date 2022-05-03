------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
with Libadalang.Common; use Libadalang.Common;
with Laltools.Subprogram_Hierarchy; use Laltools.Subprogram_Hierarchy;

package body Laltools.Refactor.Subprogram_Signature.Remove_Parameter is

   function Unique is new Generic_Array_Unique
     (Index_Type   => Positive,
      Element_Type => Positive,
      Array_Type   => Parameter_Indices_Type,
      "<"          => "<");
   --  Sorts and removes duplicates of a Parameter_Indices_Type

   function Remove_All_Parameters
     (Subp                    : Basic_Decl'Class;
      Units                   : Analysis_Unit_Array)
      return Text_Edit_Map
     with Pre => Is_Subprogram (Subp);
   --  Removes all parameters os 'Subp'. The parameters are removed in the
   --  entire subprogram hierarchy, as well as, all renames hierarchy.

   function Remove_Parameter
     (Subp            : Basic_Decl;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Text_Edit_Map
     with Pre => Is_Subprogram (Subp);
   --  Removes the parameter defined by Parameter_Index. The parameter is
   --  removed in the entire subprogram hierarchy, as well as, all renames
   --  hierarchy.

   function Remove_Parameters
     (Subp              : Basic_Decl;
      Parameter_Indices : Parameter_Indices_Type;
      Units             : Analysis_Unit_Array)
      return Text_Edit_Map;
   --  Removes the parameters defined by 'Parameter_Indices'. The parameter is
   --  removed in the entire subprogram hierarchy, as well as, all renames
   --  hierarchy.

   function Remove_Parameters
     (Subp                     : Basic_Decl;
      Parameter_Indices_Ranges : Parameter_Indices_Ranges_Type;
      Units                    : Analysis_Unit_Array)
      return Text_Edit_Map
     with Pre => Is_Subprogram (Subp);
   --  Removes the parameters defined by 'Parameter_Indices_Ranges'. The
   --  parameter is removed in the entire subprogram hierarchy, as well as, all
   --  renames hierarchy.

   -----------------------------------
   -- Is_Remove_Parameter_Available --
   -----------------------------------

   function Is_Remove_Parameter_Available
     (Node                    : Ada_Node'Class;
      Subp                    : out Basic_Decl;
      Parameter_Indices_Range : out Parameter_Indices_Range_Type)
      return Boolean
   is
      procedure Initialize_Out_Parameters;
      --  Initiazes the out parameters of this function with values as if
      --  the refactoring is not available.

      -------------------------------
      -- Initialize_Out_Parameters --
      -------------------------------

      procedure Initialize_Out_Parameters is
      begin
         Subp := No_Basic_Decl;
         Parameter_Indices_Range := (1, 1);
      end Initialize_Out_Parameters;

      Parent_Identifier          : Identifier := No_Identifier;
      Parent_Defining_Name       : Defining_Name := No_Defining_Name;
      Parent_Subptype_Indication : Subtype_Indication := No_Subtype_Indication;
      Parent_Defining_Name_List  : Defining_Name_List := No_Defining_Name_List;
      Parent_Param_Spec          : Param_Spec := No_Param_Spec;
      Parent_Param_Spec_List     : Param_Spec_List := No_Param_Spec_List;
      Parent_Subp_Decl           : Basic_Decl := No_Basic_Decl;

      Parameter_Absolute_Index : Positive;

      Total_Parameters : Natural;

      Aux_Node : Ada_Node := Node.As_Ada_Node;

   begin
      Initialize_Out_Parameters;

      Find_Subp :
      while not Aux_Node.Is_Null loop
         exit Find_Subp when Aux_Node.Kind in Ada_Subp_Spec_Range;

         case Aux_Node.Kind is
            when Ada_Identifier =>
               Parent_Identifier := Aux_Node.As_Identifier;

            when Ada_Defining_Name =>
               Parent_Defining_Name := Aux_Node.As_Defining_Name;

            when Ada_Subtype_Indication =>
               Parent_Subptype_Indication := Aux_Node.As_Subtype_Indication;

            when Ada_Defining_Name_List =>
               Parent_Defining_Name_List := Aux_Node.As_Defining_Name_List;

            when Ada_Param_Spec =>
               Parent_Param_Spec := Aux_Node.As_Param_Spec;

            when Ada_Param_Spec_List =>
               Parent_Param_Spec_List := Aux_Node.As_Param_Spec_List;

            when others => null;
         end case;

         Aux_Node := Aux_Node.Parent;
      end loop Find_Subp;

      if Aux_Node.Is_Null then
         return False;
      end if;

      Parent_Subp_Decl := Aux_Node.As_Subp_Spec.P_Parent_Basic_Decl;

      if not Is_Subprogram (Parent_Subp_Decl) then
         return False;
      end if;

      if Parent_Defining_Name_List /= No_Defining_Name_List
        and then Parent_Defining_Name /= No_Defining_Name
      then
         Parameter_Absolute_Index :=
           Get_Parameter_Absolute_Index (Parent_Defining_Name);

         Subp := Parent_Subp_Decl;
         Parameter_Indices_Range.First := Parameter_Absolute_Index;
         Parameter_Indices_Range.Last  := Parameter_Absolute_Index;

         return True;

      elsif Parent_Subptype_Indication /= No_Subtype_Indication then
         Subp := Parent_Subp_Decl;

         Parameter_Absolute_Index := 1;

         for Param_Spec of Parent_Param_Spec_List loop
            if Param_Spec = Parent_Param_Spec then
               Parameter_Indices_Range.First := Parameter_Absolute_Index;
               Parameter_Indices_Range.Last :=
                 Parameter_Absolute_Index + Length (Param_Spec.F_Ids) - 1;

            else
               Parameter_Absolute_Index :=
                 Parameter_Absolute_Index + Length (Param_Spec.F_Ids);
            end if;
         end loop;

         return True;

      else
         if Parent_Identifier = No_Identifier
           or else Get_Subp_Params (Parent_Subp_Decl).Is_Null
         then
            return False;
         end if;

         Total_Parameters :=
           Count_Subp_Parameters (Get_Subp_Params (Parent_Subp_Decl));

         if Total_Parameters = 0 then
            return False;

         else
            Subp := Parent_Subp_Decl;
            Parameter_Indices_Range.First := 1;
            Parameter_Indices_Range.Last := Total_Parameters;

            return True;
         end if;
      end if;
   exception
      when Precondition_Failure =>
         --  Assume that Precondition_Failure is raised due to invalid code and
         --  set again the out parameters since they might have been changed
         --  since initialized.
         Initialize_Out_Parameters;
         return False;
   end Is_Remove_Parameter_Available;

   ---------------------------
   -- Remove_All_Parameters --
   ---------------------------

   function Remove_All_Parameters
     (Subp  : Basic_Decl'Class;
      Units : Analysis_Unit_Array)
      return Text_Edit_Map
   is
      Edits : Text_Edit_Map;

      procedure Decl_Callback (Relative_Subp : Basic_Decl'Class);
      --  Gets the parameters slocs in 'Relative_Subp' and its body and merges
      --  those with 'Slocs'.

      procedure Calls_Callback
        (Call_Identifier : Base_Id'Class;
         Kind            : Ref_Result_Kind;
         Cancel          : in out Boolean);
      --  Gets the arguments slocs in 'Call' and merges those with 'Slocs'

      -------------------
      -- Decl_Callback --
      -------------------

      procedure Decl_Callback (Relative_Subp : Basic_Decl'Class) is
      begin
         if Is_Subprogram (Relative_Subp) then
            for Relative_Subp_Part of Relative_Subp.P_All_Parts loop
               Safe_Insert
                 (Edits     => Edits,
                  File_Name => Relative_Subp_Part.Unit.Get_Filename,
                  Edit      => Text_Edit'
                    (Location => Params_SLOC (Relative_Subp_Part),
                     Text     => Null_Unbounded_String));
            end loop;
         end if;
      end Decl_Callback;

      --------------------
      -- Calls_Callback --
      --------------------

      procedure Calls_Callback
        (Call_Identifier : Base_Id'Class;
         Kind            : Ref_Result_Kind;
         Cancel          : in out Boolean)
      is
         pragma Unreferenced (Kind);
         Call_Expression : Call_Expr;

         Call_Name   : Name := No_Name;
         Prefix_Name : Name := No_Name;
         Suffix_Name : Name := No_Name;

         --  Example: A.B.C.Call_Name (Arg1, ...);
         --  Call_Name = "A.B.C.Call_Name"
         --  Prefix_Name = "C"
         --  Suffix_Name = "Primitive_Name"

         Prefix_Definition : Defining_Name := No_Defining_Name;
      begin
         Cancel := False;

         if Call_Identifier.Is_Null
           or else Call_Identifier.Parent.Is_Null
           or else Call_Identifier.Parent.Parent.Is_Null
         then
            return;
         end if;

         if Call_Identifier.Parent.Kind in Ada_Dotted_Name then
            if Call_Identifier.Parent.Parent.Kind in Ada_Call_Expr_Range then
               Call_Expression := Call_Identifier.Parent.Parent.As_Call_Expr;
            else
               return;
            end if;
         else
            if Call_Identifier.Parent.Kind in Ada_Call_Expr_Range then
               Call_Expression := Call_Identifier.Parent.As_Call_Expr;
            else
               return;
            end if;
         end if;

         Safe_Insert
           (Edits     => Edits,
            File_Name => Call_Identifier.Unit.Get_Filename,
            Edit      => Text_Edit'
              (Location => Source_Location_Range'
                (Start_Line   => Call_Identifier.Sloc_Range.End_Line,
                 End_Line     => Call_Expression.Sloc_Range.End_Line,
                 Start_Column => Call_Identifier.Sloc_Range.End_Column,
                 End_Column   => Call_Expression.Sloc_Range.End_Column),
               Text     => Null_Unbounded_String));

         Call_Name := Call_Expression.F_Name.As_Name;

         --  Subprogram_Name (Args ...);
         --  Primitive_Name (Object_Name, Args ...);
         --  Package_Name.Subprogram_Name (Args ...);
         --  Package_Name.Primitive_Name (Object_Name, Args ...);

         if Call_Name.Kind in Ada_Dotted_Name_Range then
            Prefix_Name := Call_Name.As_Dotted_Name.F_Prefix;
            Suffix_Name := Call_Name.As_Dotted_Name.F_Suffix.As_Name;

            if Prefix_Name.Kind in Ada_Dotted_Name_Range then
               Prefix_Name := Prefix_Name.As_Dotted_Name.F_Suffix.As_Name;
            end if;

            Prefix_Definition := Resolve_Name_Precisely (Prefix_Name);

            if not Prefix_Definition.Is_Null
              and then Prefix_Definition.P_Basic_Decl.Kind in
                Ada_Object_Decl_Range
            then
               --  Object_Name.Primitive_Name (Object_Name, Args ...);
               --  Package_Name.Object_Name.Primitive_Name (Args ...);
               Safe_Insert
                 (Edits     => Edits,
                  File_Name => Call_Identifier.Unit.Get_Filename,
                  Edit      => Text_Edit'
                    (Location => Source_Location_Range'
                      (Start_Line   => Call_Expression.Sloc_Range.Start_Line,
                       End_Line     => Suffix_Name.Sloc_Range.Start_Line,
                       Start_Column => Call_Expression.Sloc_Range.Start_Column,
                       End_Column   => Suffix_Name.Sloc_Range.Start_Column),
                     Text     => Null_Unbounded_String));
            end if;
         end if;
      end Calls_Callback;

   begin
      Find_Subp_Relatives
        (Subp           => Subp,
         Units          => Units,
         Decls_Callback => Decl_Callback'Access,
         Find_Calls     => True,
         Calls_Callback => Calls_Callback'Access);

      return Edits;
   end Remove_All_Parameters;

   ----------------------
   -- Remove_Parameter --
   ----------------------

   function Remove_Parameter
     (Subp            : Basic_Decl;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Text_Edit_Map
   is (Remove_Parameters
       (Subp              => Subp,
        Parameter_Indices => [1 => Parameter_Index],
        Units             => Units));

   -----------------------
   -- Remove_Parameters --
   -----------------------

   function Remove_Parameters
     (Subp              : Basic_Decl;
      Parameter_Indices : Parameter_Indices_Type;
      Units             : Analysis_Unit_Array)
      return Text_Edit_Map
   is
      Unique_Ranges : constant Parameter_Indices_Ranges_Type :=
        To_Unique_Ranges (Parameter_Indices);

   begin
      return Remove_Parameters (Subp, Unique_Ranges, Units);
   end Remove_Parameters;

   -----------------------
   -- Remove_Parameters --
   -----------------------

   function Remove_Parameters
     (Subp                     : Basic_Decl;
      Parameter_Indices_Ranges : Parameter_Indices_Ranges_Type;
      Units                    : Analysis_Unit_Array)
      return Text_Edit_Map
   is
      Edits : Text_Edit_Map;
      Parameters       : constant Params := Get_Subp_Params (Subp);

      procedure Calls_Callback
        (Call_Identifier : Base_Id'Class;
         Kind            : Ref_Result_Kind;
         Cancel          : in out Boolean);
      --  Callback to remove the actual parameters of a subprogram call

      procedure Decl_Callback
        (Relative_Subp : Basic_Decl'Class);
      --  Callback to remove the parameters of a subprogram signature

      --------------------
      -- Calls_Callback --
      --------------------

      procedure Calls_Callback
        (Call_Identifier : Base_Id'Class;
         Kind            : Ref_Result_Kind;
         Cancel          : in out Boolean)
      is
         pragma Unreferenced (Kind, Cancel);

         Arguments : constant Assoc_List :=
           (if Call_Identifier.Parent.Kind /= Ada_Call_Expr then
               No_Assoc_List
            else
               Call_Identifier.Parent.As_Call_Expr.F_Suffix.As_Assoc_List);
      begin
         if Arguments.Is_Null then
            return;
         end if;

         declare
            Aux_Argument_Indices : constant Extended_Argument_Indicies_Type :=
              Map_Parameters_To_Arguments
                (Parameters, Call_Identifier.Parent.As_Call_Expr);
            Argument_Indices : Parameter_Indices_Type
              (Aux_Argument_Indices'First .. Aux_Argument_Indices'Last);
            Index : Positive := Aux_Argument_Indices'First;

         begin
            for Indices_Range of Parameter_Indices_Ranges loop
               for J in Indices_Range.First .. Indices_Range.Last loop
                  Argument_Indices (Index) := Aux_Argument_Indices (J);
                  Index := Index + 1;
               end loop;
            end loop;

            for SLOC of
              Arguments_SLOC
                (Call_Identifier.Parent.As_Call_Expr,
                 Unique
                   (Argument_Indices
                      (Aux_Argument_Indices'First .. Index - 1)))
            loop
               Safe_Insert
                 (Edits     => Edits,
                  File_Name => Call_Identifier.Unit.Get_Filename,
                  Edit      => Text_Edit'(SLOC, Null_Unbounded_String));
            end loop;
         end;

      end Calls_Callback;

      -------------------
      -- Decl_Callback --
      -------------------

      procedure Decl_Callback (Relative_Subp : Basic_Decl'Class) is
      begin
         if Is_Subprogram (Relative_Subp) then
            for Relative_Subp_Part of Relative_Subp.P_All_Parts loop
               for Sloc of
                 Parameters_SLOC (Relative_Subp_Part, Parameter_Indices_Ranges)
               loop
                  Safe_Insert
                    (Edits,
                     Relative_Subp_Part.Unit.Get_Filename,
                     (Sloc, Ada.Strings.Unbounded.Null_Unbounded_String));
               end loop;
            end loop;
         end if;
      end Decl_Callback;

   begin

      --  Check if Parameter_Indices refers to all parameters of Subp.
      --  If so, remove all, otherwise, remove

      if Parameter_Indices_Ranges'Length = 1
        and then Parameter_Indices_Ranges
          (Parameter_Indices_Ranges'First).First = 1
        and then (Parameter_Indices_Ranges
                  (Parameter_Indices_Ranges'First).Last =
                      Count_Subp_Parameters (Get_Subp_Params (Subp)))
      then
         Edits := Remove_All_Parameters (Subp, Units);
      end if;

      Find_Subp_Relatives
        (Subp           => Subp,
         Units          => Units,
         Decls_Callback => Decl_Callback'Access,
         Find_Calls     => True,
         Calls_Callback => Calls_Callback'Access);

      return Edits;
   end Remove_Parameters;

   ------------
   -- Create --
   ------------

   function Create
     (Target                  : Basic_Decl;
      Parameter_Indices_Range : Parameter_Indices_Range_Type;
      Configuration           : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Parameter_Remover
   is (Parameter_Remover'(Target, Parameter_Indices_Range, Configuration));

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self : Parameter_Remover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits is
   begin
      return Refactoring_Edits'
        (Text_Edits =>
           Remove_Parameters
             (Subp                     => Self.Subp,
              Parameter_Indices_Ranges => [Self.Parameter_Indices_Range],
              Units                    => Analysis_Units.all),
         others     => <>);
   end Refactor;

end Laltools.Refactor.Subprogram_Signature.Remove_Parameter;
