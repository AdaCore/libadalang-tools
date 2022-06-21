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

with Langkit_Support.Errors;
with Libadalang.Common;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Ada.Containers.Vectors;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Langkit_Support.Text;
with Laltools.Refactor.Subprogram_Signature;
with Laltools.Refactor.Subprogram_Signature.Remove_Parameter;
with Output;

package body Tools.Suppress_Dead_Params_Tool is
   package LALCO renames Libadalang.Common;
   package Text renames Langkit_Support.Text;

   package Range_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => ReFac.Subprogram_Signature.Parameter_Indices_Range_Type,
      "="          => ReFac.Subprogram_Signature."="
     );

   ---------
   -- "<" --
   ---------

   function "<" (L, R : LAL.Defining_Name) return Boolean is
   begin
      if L.Unit.Get_Filename < R.Unit.Get_Filename then
         return True;
      else
         if L.Unit.Get_Filename > R.Unit.Get_Filename then
            return False;
         else
            return (Start_Sloc (LAL.Sloc_Range (L)) <
                      Start_Sloc (LAL.Sloc_Range (R)));
         end if;
      end if;
   end "<";

   ------------------------
   -- Find_Useless_Param --
   ------------------------

   function Find_Dead_Param (Unit_Array : LAL.Analysis_Unit_Array)
                             return Edit_Infos is
      Any_Value : constant Value := (Fixed => False);
      Vals : Values.Map;
      Edit_Infos : Subp_Spec_To_Edit_Text.Map;
      Subpspec_To_Param : Subp_Spec_To_Defining_Names_Set.Map;

      procedure Add_Value
        (S : LAL.Subp_Spec;
         N : LAL.Defining_Name;
         E : LAL.Enum_Literal_Decl);
      --  Add the called value to the relative subprogram

      procedure Add_Any_Value
        (S : LAL.Subp_Spec;
         N : LAL.Defining_Name);
      --  Erase any fixed enumeration value associated to N

      procedure Safe_Insert
        (Params : in out Defining_Name_Ordered_Sets.Set;
         Param  : LAL.Defining_Name);
      --  Insert Removable Parameter's name to the Set

      procedure Safe_Insert
        (Edits     : in out Subp_Spec_To_Defining_Names_Set.Map;
         Func_Name : LAL.Subp_Spec;
         Param     : LAL.Defining_Name);
      --  Insert Removable Parameter's name to the Map

      procedure Visit_Subp_Spec (Node : LAL.Subp_Spec);
      --  Handle a subprogram spec for the canonical declaration of a
      --  subprogram, which can be its spec or its body. For every default
      --  value of a parameter of an enumeration type, consider that there is
      --  a call with that default value, and treat it similarly.

      function Find_Subprogram (Node : LAL.Ada_Node'Class)
                                return LALCO.Visit_Status;
      --  travers all the AST and find out all the Supb_Spec

      function Get_Enumeration_Literal (Node : LAL.Expr'Class)
                                        return LAL.Enum_Literal_Decl;
      --  if the subprogram is called by a enumerate type, return it.

      procedure Generate_Edits (Func_Name : LAL.Subp_Spec);

      --  Value E was observed for parameter N

      ---------------
      -- Add_Value --
      ---------------

      procedure Add_Value
        (S : LAL.Subp_Spec;
         N : LAL.Defining_Name;
         E : LAL.Enum_Literal_Decl) is
         V : constant Value := (True, E);
         C : Values.Cursor;
         R : Boolean;
      begin
         Vals.Insert (Key => N, New_Item => V, Position => C, Inserted => R);
         Safe_Insert (Subpspec_To_Param, S, N);
         if not R
           and then Vals (C) /= V
         then
            Vals.Include (Key => N, New_Item => Any_Value);
            if Subpspec_To_Param.Contains (S) then
               Subpspec_To_Param (S).Exclude (N);
            end if;
         end if;
      end Add_Value;

      -------------------
      -- Add_Any_Value --
      -------------------

      procedure Add_Any_Value
        (S : LAL.Subp_Spec;
         N : LAL.Defining_Name)
      is
      begin
         Vals.Include (Key => N, New_Item => Any_Value);
         if Subpspec_To_Param.Contains (S) then
            Subpspec_To_Param (S).Exclude (N);
         end if;
      end Add_Any_Value;

      -----------------
      -- Safe_Insert --
      -----------------

      procedure Safe_Insert
        (Params : in out Defining_Name_Ordered_Sets.Set;
         Param  : LAL.Defining_Name) is
      begin
         if not Params.Contains (Param) then
            Params.Insert (Param);
         end if;
      end Safe_Insert;

      -----------------
      -- Safe_Insert --
      -----------------

      procedure Safe_Insert
        (Edits     : in out Subp_Spec_To_Defining_Names_Set.Map;
         Func_Name : LAL.Subp_Spec;
         Param     : LAL.Defining_Name) is
         Edits_Set : Defining_Name_Ordered_Sets.Set;
      begin
         if Edits.Contains (Func_Name) then
            Safe_Insert (Edits.Reference (Func_Name), Param);

         else
            Edits_Set.Insert (Param);
            Edits.Insert (Func_Name, Edits_Set);
         end if;
      end Safe_Insert;

      -----------------------------
      -- Get_Enumeration_Literal --
      -----------------------------

      function Get_Enumeration_Literal (Node : LAL.Expr'Class)
                                        return LAL.Enum_Literal_Decl is
      begin
         if Node.Kind in LALCO.Ada_Identifier_Range
         and then not Node.As_Identifier.P_Referenced_Decl.Is_Null
         and then Node.As_Identifier.P_Referenced_Decl.Kind
                   in LALCO.Ada_Enum_Literal_Decl_Range
         then
            return Node.As_Identifier.P_Referenced_Decl.As_Enum_Literal_Decl;
         else
            return LAL.No_Enum_Literal_Decl;
         end if;
      end Get_Enumeration_Literal;

      ---------------------
      -- Visit_Subp_Spec --
      ---------------------

      procedure Visit_Subp_Spec (Node : LAL.Subp_Spec) is
      begin
         if not Node.As_Subp_Spec.F_Subp_Params.Is_Null then
            declare
               Params : constant LAL.Param_Spec_List :=
                 Node.As_Subp_Spec.F_Subp_Params.F_Params;
            begin
               for Param of Params loop
                  declare
                     Default : constant LAL.Expr'Class :=
                       Param.As_Param_Spec.F_Default_Expr;
                  begin
                     if not Default.Is_Null then
                        if not Get_Enumeration_Literal (Default).Is_Null then
                           for P of Param.As_Param_Spec.F_Ids loop
                              Add_Value (Node,
                                         P.As_Defining_Name,
                                         Get_Enumeration_Literal (Default));
                           end loop;
                        elsif not Default.P_Expression_Type.Is_Null
                          and then Default.P_Expression_Type.P_Is_Enum_Type
                        then
                           for P of Param.As_Param_Spec.F_Ids loop
                              Add_Any_Value (Node, P.As_Defining_Name);
                           end loop;
                        end if;
                     end if;
                  end;
               end loop;
            end;
         end if;
      end Visit_Subp_Spec;

      -----------
      -- Visit --
      -----------

      function Find_Subprogram  (Node : LAL.Ada_Node'Class)
                                 return LALCO.Visit_Status is
      begin
         case Node.Kind is
         when LALCO.Ada_Subp_Spec_Range =>
            if Node.Parent.Kind not in LALCO.Ada_Subp_Body_Range then
               Visit_Subp_Spec (Node.As_Subp_Spec);
            end if;
         when LALCO.Ada_Call_Expr =>
            declare
               Call : constant LAL.Call_Expr'Class := Node.As_Call_Expr;
            begin
               if Call.F_Suffix.Kind in LALCO.Ada_Assoc_List_Range then
                  declare
                     Params : constant LAL.Param_Actual_Array :=
                       Call.F_Suffix.As_Assoc_List.P_Zip_With_Params;
                  begin
                     for Pair of Params loop
                        declare
                           P : constant LAL.Defining_Name'Class :=
                             LAL.Param (Pair).P_Canonical_Part;
                           A : constant LAL.Expr'Class := LAL.Actual (Pair);
                        begin
                           if not Get_Enumeration_Literal (A).Is_Null then
                              Add_Value (Call.P_Called_Subp_Spec.As_Subp_Spec,
                                         P.As_Defining_Name
                                         , Get_Enumeration_Literal (A));
                           elsif P.F_Name.P_Is_Constant
                           then
                              Add_Value (Call.P_Called_Subp_Spec.As_Subp_Spec,
                                         P.As_Defining_Name,
                                         P.As_Defining_Name.F_Name
                                         .P_Referenced_Decl
                                         .As_Object_Decl.F_Default_Expr
                                         .As_Enum_Literal_Decl);
                           elsif not A.P_Expression_Type.Is_Null
                             and then A.P_Expression_Type.P_Is_Enum_Type
                           then
                              Add_Any_Value (Call.P_Called_Subp_Spec
                                             .As_Subp_Spec,
                                             P.As_Defining_Name);
                           end if;
                        end;
                     end loop;
                  end;
               end if;
            exception
                  --  due to null dereference during P_Zip_With_Params
                  --  as well as timeout
               when Langkit_Support.Errors.Property_Error => null;
            end;

         when others =>
            null;
         end case;
         return LALCO.Into;
      end Find_Subprogram;

      --------------------
      -- Generate_Edits --
      --------------------

      procedure Generate_Edits (Func_Name : LAL.Subp_Spec) is
         Param_Removable : constant Defining_Name_Ordered_Sets.Set :=
           Subpspec_To_Param (Func_Name);
         Param_Indices   : Range_Vectors.Vector;
         Params          : constant LAL.Param_Spec_Array :=
           Func_Name.P_Params;
         Flag            : Boolean := False;
         First, Last     : Positive := 1;
         Indice_Range    : ReFac.Subprogram_Signature.
           Parameter_Indices_Range_Type;
         Father          : constant LAL.Ada_Node := Func_Name.Parent;

         function Generate_Constant return ReFac.Text_Edit;
         --  generate the text_edit for each dead parameters

         -----------------------
         -- Generate_Constant --
         -----------------------

         function Generate_Constant return ReFac.Text_Edit is
            Define_Text : Ada.Strings.Unbounded.Unbounded_String :=
              Ada.Strings.Unbounded.Null_Unbounded_String;
            Text_Edit : ReFac.Text_Edit;
            Position : constant Source_Location_Range := Func_Name.Sloc_Range;

            procedure Generate_Text;
            --  generate the declaration of the dead parameters

            -------------------
            -- Generate_Text --
            -------------------

            procedure Generate_Text is
            begin
               for Param of Param_Removable loop
                  Define_Text := Define_Text
                    & Text.Image (Param.P_Basic_Decl.Text);
                  if Param.P_Basic_Decl.As_Param_Spec.F_Default_Expr.Is_Null
                  then
                     Define_Text := Define_Text & " := "
                       & Text.Image (Vals (Param).V.Text);
                  end if;
                  Define_Text := Define_Text & ";";
               end loop;
               Define_Text := Define_Text & Ada.Characters.Latin_1.LF;
            end Generate_Text;
         begin
            if Father.Kind in LALCO.Ada_Subp_Body_Range then
               Text_Edit.Location.Start_Column := Position.Start_Column + 3;
               Text_Edit.Location.End_Column := Position.Start_Column + 3;
               Text_Edit.Location.Start_Line := Father.As_Subp_Body.F_Decls
                 .Sloc_Range.Start_Line + 1;
               Text_Edit.Location.End_Line := Father.As_Subp_Body.F_Decls
                 .Sloc_Range.Start_Line + 1;
               Generate_Text;
               Text_Edit.Text := Define_Text;
            else
               Text_Edit.Location := Father.As_Expr_Function
                 .F_Expr.Children (1).Sloc_Range;
               Generate_Text;
               Text_Edit.Text := "(declare " & Define_Text & "begin "
                 & Text.Image
                 (Father.As_Expr_Function.F_Expr.Children (1).Text)
                 & ")";
            end if;
            return Text_Edit;
         end Generate_Constant;
      begin
         for I_Param in Params'Range loop
            if Param_Removable.Contains (Params (I_Param).P_Defining_Name) then
               Last := I_Param;
               Flag := True;
            else
               if Flag then
                  Indice_Range := (First, Last);
                  Param_Indices.Append (Indice_Range);
                  Flag := False;
               end if;
               First := I_Param + 1;
            end if;
         end loop;
         if Flag then
            Indice_Range := (First, Last);
            Param_Indices.Append (Indice_Range);
         end if;
         if not Param_Indices.Is_Empty then
            declare
               Indice_Array : ReFac.Subprogram_Signature
                 .Parameter_Indices_Ranges_Type
                   (1 .. Param_Indices.Last_Index);
               Edit_Info : ReFac.Text_Edit_Map;
            begin
               for I in 1 .. Param_Indices.Last_Index loop
                  Indice_Array (I) := Param_Indices (I);
               end loop;
               Edit_Info := ReFac.Subprogram_Signature.Remove_Parameter
                 .Remove_Parameters (Func_Name.P_Parent_Basic_Decl,
                                     Indice_Array,
                                     Unit_Array);
               if Father.Kind not in LALCO.Ada_Subp_Decl_Range then
                  ReFac.Safe_Insert (Edit_Info, Func_Name.Unit.Get_Filename,
                                  Generate_Constant);
               end if;
               Edit_Infos.Insert (Func_Name, Edit_Info);
            end;
         end if;
      end Generate_Edits;
   begin
      for Unit of Unit_Array loop
         Unit.Root.Traverse (Find_Subprogram'Access);
      end loop;
      for Subp_Name in Subpspec_To_Param.Iterate loop
         --  Put_Line (Text.Image (Subp_Name.Key.Text));
         --  for Param of Subpspec_To_Param (Subp_Name) loop
         --     Put_Line (Text.Image (Param.Text));
         --  end loop;
         Generate_Edits (Subp_Name.Key);
      end loop;
      return (Edit_Infos, Subpspec_To_Param);
   end Find_Dead_Param;

   ---------
   -- Run --
   ---------

   procedure Run
     (Unit_Array : LAL.Analysis_Unit_Array;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class)
   is
      Edit_Info : Edit_Infos;

   begin
      Edit_Info := Find_Dead_Param (Unit_Array);
      Output.JSON_Serialize (Edit_Info, Stream);
   end Run;

end Tools.Suppress_Dead_Params_Tool;
