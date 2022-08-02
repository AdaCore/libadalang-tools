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

with Ada.Strings.Equal_Case_Insensitive;

with Langkit_Support.Text; use Langkit_Support.Text;

with Laltools.Subprogram_Hierarchy; use Laltools.Subprogram_Hierarchy;

package body Laltools.Refactor.Subprogram_Signature.
               Change_Parameters_Default_Value is

   Tool_Name : constant String := "Change Parameters Default Value";

   procedure Change_Parameter_Default_Value
     (Subprogram                  : Basic_Decl'Class;
      Parameters_Indices          : Parameter_Indices_Range_Type;
      New_Parameter_Default_Value : Unbounded_String;
      Edits                       : in out Refactoring_Edits);
   --  Changes the type of parameters defined by Parameters_Indices
   --  in Subprogram, and adds the Text_Edits to Edits.

   function Get_Canonical_Target_Subprogram
     (Unit                             : Analysis_Unit;
      Parameters_Source_Location_Range : Source_Location_Range)
      return Basic_Decl
   is (Unit.Root.Lookup (Start_Sloc (Parameters_Source_Location_Range)).
         P_Parent_Basic_Decl);
   --  Returns the canonical parent subprogram of the parameters defined by
   --  Parameters_Source_Location_Range.

   function Get_Parameters_Indices
     (Unit                             : Analysis_Unit;
      Parameters_Source_Location_Range : Source_Location_Range)
      return Parameter_Indices_Range_Type;
   --  Returns the indices of the parameters defined by
   --  Parameters_Source_Location_Range.

   ------------------------------------
   -- Change_Parameter_Default_Value --
   ------------------------------------

   procedure Change_Parameter_Default_Value
     (Subprogram                  : Basic_Decl'Class;
      Parameters_Indices          : Parameter_Indices_Range_Type;
      New_Parameter_Default_Value : Unbounded_String;
      Edits                       : in out Refactoring_Edits)
   is
      Next_Parameter_Index  : Positive := Parameters_Indices.First;
      End_Parameter_Index   : constant Positive := Parameters_Indices.Last;
      N_Of_Parameters_Left  : Natural :=
        End_Parameter_Index - Next_Parameter_Index + 1;

      Param_Spec_Length                : Positive;
      Param_Spec_Start_Parameter_Index : Positive := 1;
      Param_Spec_End_Parameter_Index   : Natural := 0;

      Param_Spec_Mode : Mode;

      Param_Spec_Type_Expr            : Type_Expr;
      Param_Spec_Type_Expr_SLOC_Range : Source_Location_Range;

      Param_Spec_Default_Expr            : Expr;
      Param_Spec_Default_Expr_SLOC_Range : Source_Location_Range;

      Param_Spec_It                   : Positive;
      Param_Spec_Slice_Start_Param    : Defining_Name;
      Param_Spec_Slice_End_Param      : Defining_Name;

   begin
      for Param_Spec of Get_Subp_Params (Subprogram).F_Params loop
         exit when N_Of_Parameters_Left = 0;

         Param_Spec_Length := Length (Param_Spec.F_Ids);
         Param_Spec_End_Parameter_Index :=
           Param_Spec_Start_Parameter_Index + Param_Spec_Length - 1;

         if Next_Parameter_Index = Param_Spec_Start_Parameter_Index
           and then End_Parameter_Index >= Param_Spec_End_Parameter_Index
         then
            --  All parameters in this Param_Spec are affected.
            --  Simply change the default Expr of this Param_Spec.
            --
            --  Examples:
            --  A : Integer -> A : Integer := 2
            --  A : Integer := 1 -> A : Integer := 2
            --  A : Integer := 1 -> A : Integer

            Param_Spec_Default_Expr := Param_Spec.F_Default_Expr;

            if New_Parameter_Default_Value = "" then
               --  Do nothing if there is not a default value.
               --  Otherwise, remove it.

               if Param_Spec_Default_Expr.Is_Null then
                  null;

               elsif not Ada.Strings.Equal_Case_Insensitive
                           (To_String (New_Parameter_Default_Value),
                            To_UTF8 (Param_Spec_Default_Expr.Text))
               then
                  Param_Spec_Default_Expr_SLOC_Range :=
                    Param_Spec_Default_Expr.Sloc_Range;
                  Param_Spec_Type_Expr := Param_Spec.F_Type_Expr;
                  Param_Spec_Type_Expr_SLOC_Range :=
                    Param_Spec_Type_Expr.Sloc_Range;

                  Safe_Insert
                    (Edits     => Edits.Text_Edits,
                     File_Name => Subprogram.Unit.Get_Filename,
                     Edit      =>
                       Text_Edit'
                         (Location =>
                            Make_Range
                              (End_Sloc (Param_Spec_Type_Expr_SLOC_Range),
                               End_Sloc (Param_Spec_Default_Expr_SLOC_Range)),
                          Text     => New_Parameter_Default_Value));
               end if;

            else
               --  Add the default value, if it does not exists.
               --  Otherwise, change it.

               if Param_Spec_Default_Expr.Is_Null then
                  Param_Spec_Type_Expr := Param_Spec.F_Type_Expr;
                  Param_Spec_Type_Expr_SLOC_Range :=
                    Param_Spec_Type_Expr.Sloc_Range;

                  Safe_Insert
                    (Edits     => Edits.Text_Edits,
                     File_Name => Subprogram.Unit.Get_Filename,
                     Edit      =>
                       Text_Edit'
                         (Location   =>
                            Make_Range
                              (End_Sloc (Param_Spec_Type_Expr_SLOC_Range),
                               End_Sloc (Param_Spec_Type_Expr_SLOC_Range)),
                          Text       => " := " & New_Parameter_Default_Value));

               elsif not Ada.Strings.Equal_Case_Insensitive
                           (To_String (New_Parameter_Default_Value),
                            To_UTF8 (Param_Spec_Default_Expr.Text))
               then
                  Param_Spec_Default_Expr_SLOC_Range :=
                    Param_Spec_Default_Expr.Sloc_Range;

                  Safe_Insert
                    (Edits     => Edits.Text_Edits,
                     File_Name => Subprogram.Unit.Get_Filename,
                     Edit      =>
                       Text_Edit'
                         (Location => Param_Spec_Default_Expr_SLOC_Range,
                          Text     => New_Parameter_Default_Value));
               end if;
            end if;

            Next_Parameter_Index :=
              Param_Spec_End_Parameter_Index + 1;
            N_Of_Parameters_Left :=
              @ - (Param_Spec_End_Parameter_Index - Next_Parameter_Index + 1);

         elsif Next_Parameter_Index = Param_Spec_Start_Parameter_Index
           and then End_Parameter_Index < Param_Spec_End_Parameter_Index
         then
            Param_Spec_Default_Expr := Param_Spec.F_Default_Expr;

            if (New_Parameter_Default_Value /= ""
                or else not Param_Spec_Default_Expr.Is_Null)
              and then not (not Param_Spec_Default_Expr.Is_Null
                            and then Ada.Strings.Equal_Case_Insensitive
                                       (To_String
                                          (New_Parameter_Default_Value),
                                        To_UTF8
                                          (Param_Spec_Default_Expr.Text)))
            then
               --  The first parameter(s) (but not all!) in this Param_Spec are
               --  affected.
               --
               --  Examples:
               --  A, B : Integer -> A : Integer := 1; B : Integer
               --  A, B : Integer := 1 -> A : Integer := 2; B : Integer := 1
               --  A, B : Integer := 1 -> A : Integer; B : Integer := 1

               Param_Spec_Mode := Param_Spec.F_Mode;
               Param_Spec_Type_Expr := Param_Spec.F_Type_Expr;

               Param_Spec_It := Param_Spec_Start_Parameter_Index;
               for Parameter of Param_Spec.F_Ids loop
                  if Param_Spec_It = End_Parameter_Index then
                     Param_Spec_Slice_Start_Param :=
                       Parameter.As_Defining_Name;
                  elsif Param_Spec_It = End_Parameter_Index + 1 then
                     Param_Spec_Slice_End_Param := Parameter.As_Defining_Name;
                     exit;
                  end if;
                  Param_Spec_It := Param_Spec_It + 1;
               end loop;

               Safe_Insert
                 (Edits     => Edits.Text_Edits,
                  File_Name => Subprogram.Unit.Get_Filename,
                  Edit      =>
                    Text_Edit'
                      (Location =>
                         Source_Location_Range'
                           (Start_Line   =>
                              Param_Spec_Slice_Start_Param.Sloc_Range.End_Line,
                            End_Line     =>
                              Param_Spec_Slice_End_Param.Sloc_Range.Start_Line,
                            Start_Column =>
                              Param_Spec_Slice_Start_Param.Sloc_Range.
                                End_Column,
                            End_Column   =>
                              Param_Spec_Slice_End_Param.Sloc_Range.
                                Start_Column),
                       Text     =>
                         " : "
                         & (if Param_Spec_Mode.Kind not in
                              Ada_Mode_Default_Range
                            then
                              To_UTF8 (Param_Spec_Mode.Text) & " "
                            else "")
                         & To_UTF8 (Param_Spec_Type_Expr.Text)
                         & (if New_Parameter_Default_Value = "" then
                              New_Parameter_Default_Value
                            else
                              " := " & New_Parameter_Default_Value)
                         & "; "));

               N_Of_Parameters_Left := 0;
            end if;

         elsif Next_Parameter_Index > Param_Spec_Start_Parameter_Index
           and then Next_Parameter_Index <= Param_Spec_End_Parameter_Index
           and then End_Parameter_Index >= Param_Spec_End_Parameter_Index
         then
            Param_Spec_Default_Expr := Param_Spec.F_Default_Expr;

            if (New_Parameter_Default_Value /= ""
                or else not Param_Spec_Default_Expr.Is_Null)
              and then not (not Param_Spec_Default_Expr.Is_Null
                            and then Ada.Strings.Equal_Case_Insensitive
                                       (To_String
                                          (New_Parameter_Default_Value),
                                        To_UTF8
                                          (Param_Spec_Default_Expr.Text)))
            then
               --  The last parameter(s) (but not all!), in this Param_Spec are
               --  affected.
               --
               --  Examples:
               --  A, B : Integer -> A : Integer; B : Integer := 1
               --  A, B : Integer := 1 -> A : Integer := 1; B : Integer := 2
               --  A, B : Integer := 1 -> A : Integer := 1; B : Integer

               Param_Spec_Mode := Param_Spec.F_Mode;
               Param_Spec_Type_Expr := Param_Spec.F_Type_Expr;

               Param_Spec_It := Param_Spec_Start_Parameter_Index;
               for Parameter of Param_Spec.F_Ids loop
                  if Param_Spec_It = Next_Parameter_Index - 1 then
                     Param_Spec_Slice_Start_Param :=
                       Parameter.As_Defining_Name;
                  elsif Param_Spec_It = Next_Parameter_Index then
                     Param_Spec_Slice_End_Param := Parameter.As_Defining_Name;
                     exit;
                  end if;
                  Param_Spec_It := Param_Spec_It + 1;
               end loop;

               Safe_Insert
                 (Edits     => Edits.Text_Edits,
                  File_Name => Subprogram.Unit.Get_Filename,
                  Edit      =>
                    Text_Edit'
                      (Location =>
                         Source_Location_Range'
                           (Start_Line   =>
                              Param_Spec_Slice_Start_Param.Sloc_Range.End_Line,
                            End_Line     =>
                              Param_Spec_Slice_End_Param.Sloc_Range.Start_Line,
                            Start_Column =>
                              Param_Spec_Slice_Start_Param.Sloc_Range.
                                End_Column,
                            End_Column   =>
                              Param_Spec_Slice_End_Param.Sloc_Range.
                                Start_Column),
                       Text     =>
                         To_Unbounded_String (" : ")
                         & (if Param_Spec_Mode.Kind not in
                              Ada_Mode_Default_Range
                           then
                              To_UTF8 (Param_Spec_Mode.Text) & " "
                           else "")
                         & To_UTF8 (Param_Spec_Type_Expr.Text)
                         & (if Param_Spec_Default_Expr.Is_Null then
                              ""
                            else
                              " := " & To_UTF8 (Param_Spec_Default_Expr.Text))
                         & "; "));

               if New_Parameter_Default_Value = "" then
                  --  Do nothing if there is not a default value.
                  --  Otherwise, remove it.

                  if Param_Spec_Default_Expr.Is_Null then
                     null;

                  else
                     Param_Spec_Default_Expr_SLOC_Range :=
                       Param_Spec_Default_Expr.Sloc_Range;
                     Param_Spec_Type_Expr_SLOC_Range :=
                       Param_Spec_Type_Expr.Sloc_Range;

                     Safe_Insert
                       (Edits     => Edits.Text_Edits,
                        File_Name => Subprogram.Unit.Get_Filename,
                        Edit      =>
                          Text_Edit'
                            (Location =>
                               Make_Range
                                 (End_Sloc (Param_Spec_Type_Expr_SLOC_Range),
                                  End_Sloc
                                    (Param_Spec_Default_Expr_SLOC_Range)),
                             Text     => New_Parameter_Default_Value));
                  end if;

               else
                  --  Add the default value, if it does not exists.
                  --  Otherwise, change it.

                  if Param_Spec_Default_Expr.Is_Null then
                     Param_Spec_Type_Expr := Param_Spec.F_Type_Expr;
                     Param_Spec_Type_Expr_SLOC_Range :=
                       Param_Spec_Type_Expr.Sloc_Range;

                     Safe_Insert
                       (Edits     => Edits.Text_Edits,
                        File_Name => Subprogram.Unit.Get_Filename,
                        Edit      =>
                          Text_Edit'
                            (Location   =>
                               Make_Range
                                 (End_Sloc (Param_Spec_Type_Expr_SLOC_Range),
                                  End_Sloc (Param_Spec_Type_Expr_SLOC_Range)),
                             Text       =>
                               " := " & New_Parameter_Default_Value));

                  else
                     Param_Spec_Default_Expr_SLOC_Range :=
                       Param_Spec_Default_Expr.Sloc_Range;

                     Safe_Insert
                       (Edits     => Edits.Text_Edits,
                        File_Name => Subprogram.Unit.Get_Filename,
                        Edit      =>
                          Text_Edit'
                            (Location => Param_Spec_Default_Expr_SLOC_Range,
                             Text     => New_Parameter_Default_Value));
                  end if;
               end if;

               Next_Parameter_Index :=
                 Param_Spec_End_Parameter_Index + 1;
               N_Of_Parameters_Left :=
                 @ - (Param_Spec_End_Parameter_Index
                      - Next_Parameter_Index + 1);

            end if;

         elsif Next_Parameter_Index > Param_Spec_Start_Parameter_Index
           and then End_Parameter_Index < Param_Spec_End_Parameter_Index
         then
            Param_Spec_Default_Expr := Param_Spec.F_Default_Expr;

            if (New_Parameter_Default_Value /= ""
                or else not Param_Spec_Default_Expr.Is_Null)
              and then not (not Param_Spec_Default_Expr.Is_Null
                            and then Ada.Strings.Equal_Case_Insensitive
                                       (To_String
                                          (New_Parameter_Default_Value),
                                        To_UTF8
                                          (Param_Spec_Default_Expr.Text)))
            then
               --  The middle parameter(s) (but not the first nor the last!),
               --  in this Param_Spec are affected.
               --
               --  Examples:
               --  A, B, C : Integer ->
               --    A : Integer; B : Integer := 1; C : Integer
               --  A, B, C : Integer := 1 ->
               --    A : Integer := 1; B : Integer := 2; C : Integer := 1
               --  A, B, C : Integer := 1 ->
               --    A : Integer := 1; B : Integer; C : Integer := 1

               Param_Spec_Mode := Param_Spec.F_Mode;
               Param_Spec_Type_Expr := Param_Spec.F_Type_Expr;
               Param_Spec_Default_Expr := Param_Spec.F_Default_Expr;

               Param_Spec_It := Param_Spec_Start_Parameter_Index;
               for Parameter of Param_Spec.F_Ids loop
                  if Param_Spec_It = Next_Parameter_Index - 1 then
                     Param_Spec_Slice_Start_Param :=
                       Parameter.As_Defining_Name;
                  elsif Param_Spec_It = Next_Parameter_Index then
                     Param_Spec_Slice_End_Param := Parameter.As_Defining_Name;
                     exit;
                  end if;
                  Param_Spec_It := Param_Spec_It + 1;
               end loop;

               Safe_Insert
                 (Edits     => Edits.Text_Edits,
                  File_Name => Subprogram.Unit.Get_Filename,
                  Edit      =>
                    Text_Edit'
                      (Location =>
                         Source_Location_Range'
                           (Start_Line   =>
                              Param_Spec_Slice_Start_Param.Sloc_Range.End_Line,
                            End_Line     =>
                              Param_Spec_Slice_End_Param.Sloc_Range.Start_Line,
                            Start_Column =>
                              Param_Spec_Slice_Start_Param.Sloc_Range.
                                End_Column,
                            End_Column   =>
                              Param_Spec_Slice_End_Param.Sloc_Range.
                                Start_Column),
                       Text     =>
                         To_Unbounded_String
                           (" : "
                            & (if Param_Spec_Mode.Kind not in
                                    Ada_Mode_Default_Range
                              then
                                 To_UTF8 (Param_Spec_Mode.Text) & " "
                              else "")
                            & To_UTF8 (Param_Spec_Type_Expr.Text)
                            & (if not Param_Spec_Default_Expr.Is_Null then
                                 " := "
                                 & To_UTF8 (Param_Spec_Default_Expr.Text)
                               else "")
                            & "; ")));

               Param_Spec_It := Param_Spec_Start_Parameter_Index;
               for Parameter of Param_Spec.F_Ids loop
                  if Param_Spec_It = End_Parameter_Index then
                     Param_Spec_Slice_Start_Param :=
                       Parameter.As_Defining_Name;
                  elsif Param_Spec_It = End_Parameter_Index + 1 then
                     Param_Spec_Slice_End_Param := Parameter.As_Defining_Name;
                     exit;
                  end if;
                  Param_Spec_It := Param_Spec_It + 1;
               end loop;

               Safe_Insert
                 (Edits     => Edits.Text_Edits,
                  File_Name => Subprogram.Unit.Get_Filename,
                  Edit      =>
                    Text_Edit'
                      (Location =>
                         Source_Location_Range'
                           (Start_Line   =>
                              Param_Spec_Slice_Start_Param.Sloc_Range.End_Line,
                            End_Line     =>
                              Param_Spec_Slice_End_Param.Sloc_Range.Start_Line,
                            Start_Column =>
                              Param_Spec_Slice_Start_Param.Sloc_Range.
                                End_Column,
                            End_Column   =>
                              Param_Spec_Slice_End_Param.Sloc_Range.
                                Start_Column),
                       Text     =>
                         To_Unbounded_String (" : ")
                         & (if Param_Spec_Mode.Kind not in
                              Ada_Mode_Default_Range
                            then
                              To_UTF8 (Param_Spec_Mode.Text) & " "
                            else "")
                         & To_UTF8 (Param_Spec_Type_Expr.Text)
                         & (if New_Parameter_Default_Value = "" then
                              New_Parameter_Default_Value
                            else
                              " := " & New_Parameter_Default_Value)
                         & "; "));

               N_Of_Parameters_Left := 0;
            end if;
         end if;

         Param_Spec_Start_Parameter_Index :=
           Param_Spec_End_Parameter_Index + 1;
      end loop;
   end Change_Parameter_Default_Value;

   ---------------------------
   -- Get_Parameter_Indices --
   ---------------------------

   function Get_Parameters_Indices
     (Unit                             : Analysis_Unit;
      Parameters_Source_Location_Range : Source_Location_Range)
         return Parameter_Indices_Range_Type
   is
      Start_Node_SLOC   : constant Source_Location :=
        Start_Sloc (Parameters_Source_Location_Range);
      Aux_End_Node_SLOC : constant Source_Location :=
        End_Sloc (Parameters_Source_Location_Range);
      End_Node_SLOC     : constant Source_Location :=
        (Aux_End_Node_SLOC.Line,
         (if Start_Node_SLOC = Aux_End_Node_SLOC then
            Aux_End_Node_SLOC.Column
          else
            (if Aux_End_Node_SLOC.Column = 0 then 0
             else Aux_End_Node_SLOC.Column - 1)));

      Start_Parameter : constant Defining_Name :=
        Unit.Root.Lookup (Start_Node_SLOC).As_Name.P_Enclosing_Defining_Name;
      End_Parameter   : constant Defining_Name :=
        Unit.Root.Lookup (End_Node_SLOC).As_Name.P_Enclosing_Defining_Name;
      Param_Specs     : constant Param_Spec_List :=
        Start_Parameter.Parent.As_Defining_Name_List.Parent.Parent.
          As_Param_Spec_List;

      Index : Positive := 1;

      Result : Parameter_Indices_Range_Type;

   begin
      for Param_Spec of Param_Specs loop
         for Param of Param_Spec.F_Ids loop
            if Param = Start_Parameter then
               Result.First := Index;
            end if;
            if Param = End_Parameter then
               Result.Last := Index;
               exit;
            end if;
            Index := @ + 1;
         end loop;
      end loop;

      return Result;
   end Get_Parameters_Indices;

   --------------------------------------------------
   -- Is_Change_Parameters_Default_Value_Available --
   --------------------------------------------------

   function Is_Change_Parameters_Default_Value_Available
     (Unit                             : Analysis_Unit;
      Parameters_Source_Location_Range : Source_Location_Range)
      return Boolean
   is
      Start_Node_SLOC   : constant Source_Location :=
        Start_Sloc (Parameters_Source_Location_Range);
      Start_Node        : constant Ada_Node :=
        Unit.Root.Lookup (Start_Node_SLOC);
      Aux_End_Node_SLOC : constant Source_Location :=
        End_Sloc (Parameters_Source_Location_Range);
      End_Node_SLOC     : constant Source_Location :=
        (Aux_End_Node_SLOC.Line,
         (if Start_Node_SLOC = Aux_End_Node_SLOC then
            Aux_End_Node_SLOC.Column
          else
            (if Aux_End_Node_SLOC.Column = 0 then 0
             else Aux_End_Node_SLOC.Column - 1)));
      End_Node          : constant Ada_Node :=
        Unit.Root.Lookup (End_Node_SLOC);

      Start_Defining_Name  : Defining_Name;
      End_Defining_Name    : Defining_Name;
      Start_Param_Spec     : Param_Spec;
      End_Param_Spec       : Param_Spec;
      Param_Spec_It        : Param_Spec;
      Has_Mode             : Boolean := False;

   begin
      if (Start_Node.Is_Null or else End_Node.Is_Null)
        or else Start_Node.Kind not in Ada_Name
        or else End_Node.Kind not in Ada_Name
      then
         return False;
      end if;

      Start_Defining_Name := Start_Node.As_Name.P_Enclosing_Defining_Name;
      End_Defining_Name := End_Node.As_Name.P_Enclosing_Defining_Name;

      if Start_Defining_Name.Is_Null
          or else End_Defining_Name.Is_Null
      then
         return False;
      end if;

      if Start_Defining_Name.Parent.Parent.Kind not in Ada_Param_Spec
        or else End_Defining_Name.Parent.Parent.Kind not in Ada_Param_Spec
      then
         return False;
      end if;

      Start_Param_Spec := Start_Defining_Name.Parent.Parent.As_Param_Spec;
      End_Param_Spec := End_Defining_Name.Parent.Parent.As_Param_Spec;

      Param_Spec_It := Start_Param_Spec;
      loop
         Has_Mode :=
           @ or Param_Spec_It.F_Mode.Kind not in Ada_Mode_Default_Range;
         exit when Has_Mode
           or else Param_Spec_It = End_Param_Spec
           or else Param_Spec_It.Next_Sibling.Is_Null;
         Param_Spec_It := Param_Spec_It.Next_Sibling.As_Param_Spec;
      end loop;

      return True;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Is_Refactoring_Tool_Available_Default_Error_Message (Tool_Name));
         return False;
   end Is_Change_Parameters_Default_Value_Available;

   ---------------------------------------------
   -- Create_Parameters_Default_Value_Changer --
   ---------------------------------------------

   function Create_Parameters_Default_Value_Changer
     (Unit                             : Analysis_Unit;
      Parameters_Source_Location_Range : Source_Location_Range;
      New_Parameters_Default_Value     : Unbounded_String;
      Configuration                    :
        Signature_Changer_Configuration_Type := Default_Configuration)
      return Parameters_Default_Value_Changer
   is (Parameters_Default_Value_Changer'
         (Unit                             => Unit,
          Parameters_Source_Location_Range => Parameters_Source_Location_Range,
          New_Parameters_Default_Value     => New_Parameters_Default_Value,
          Configuration                    => Configuration));

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Parameters_Default_Value_Changer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      Edits : Refactoring_Edits;

      Subprogram         : constant Basic_Decl :=
        Get_Canonical_Target_Subprogram
          (Self.Unit, Self.Parameters_Source_Location_Range);
      Parameters_Indices : constant Parameter_Indices_Range_Type :=
        Get_Parameters_Indices
          (Self.Unit, Self.Parameters_Source_Location_Range);

      procedure Change_Parameter_Default_Values_Callback
        (Relative_Subp : Basic_Decl'Class);
      --  Determines the necessary changes to 'Relative_Subp' specification,
      --  and adds them to 'Edits'.

      ----------------------------------------------
      -- Change_Parameter_Default_Values_Callback --
      ----------------------------------------------

      procedure Change_Parameter_Default_Values_Callback
        (Relative_Subp : Basic_Decl'Class)
      is
         Relative_Subp_Body : constant Base_Subp_Body :=
           Find_Subp_Body (Relative_Subp);

      begin
         if Is_Subprogram (Relative_Subp) then
            Change_Parameter_Default_Value
              (Relative_Subp,
               Parameters_Indices,
               Self.New_Parameters_Default_Value,
               Edits);

            if not Relative_Subp_Body.Is_Null then
               Change_Parameter_Default_Value
                 (Relative_Subp_Body,
                  Parameters_Indices,
                  Self.New_Parameters_Default_Value,
                  Edits);
            end if;
         end if;
      end Change_Parameter_Default_Values_Callback;

   begin
      Find_Subp_Relatives
        (Subp           => Subprogram,
         Units          => Analysis_Units.all,
         Decls_Callback => Change_Parameter_Default_Values_Callback'Access);

      return Edits;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));
         return No_Refactoring_Edits;
   end Refactor;

end Laltools.Refactor.Subprogram_Signature.Change_Parameters_Default_Value;
