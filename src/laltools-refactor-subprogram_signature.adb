------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;

with Laltools.Subprogram_Hierarchy; use Laltools.Subprogram_Hierarchy;

package body Laltools.Refactor.Subprogram_Signature is

   --------------------------
   -- Generic_Array_Unique --
   --------------------------

   function Generic_Array_Unique (Container : Array_Type) return Array_Type
   is
      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Index_Type   => Index_Type,
         Element_Type => Element_Type,
         Array_Type   => Array_Type,
         "<"          => "<");

      Unique_Container : Array_Type := Container;
      Cursor           : Index_Type := Container'First;
      Last_New_Element : Element_Type;

   begin
      --  Empty arrays or arrays with only one element do not need to be
      --  processed.

      if Unique_Container'Length < 2 then
         return Unique_Container;
      end if;

      Sort (Unique_Container);

      Last_New_Element := Unique_Container (Unique_Container'First);

      for J in
        Index_Type'Succ (Unique_Container'First) .. Unique_Container'Last
      loop
         if Unique_Container (J) /= Last_New_Element then
            Cursor := Index_Type'Succ (Cursor);
            Unique_Container (Cursor) := Unique_Container (J);
            Last_New_Element := Unique_Container (J);
         end if;
      end loop;

      return Unique_Container (Unique_Container'First .. Cursor);
   end Generic_Array_Unique;

   function Unique is new Generic_Array_Unique
     (Index_Type   => Positive,
      Element_Type => Positive,
      Array_Type   => Parameter_Indices_Type,
      "<"          => "<");
   --  Sorts and removes duplicates of a Parameter_Indices_Type

   type Param_Spec_Indices_Range_Type is
      record
         First, Last : Positive;
      end record;

   type Param_Spec_Indices_Ranges_Type is
     array (Positive range <>) of Param_Spec_Indices_Range_Type;

   type Optional_Designator_Index (Exists : Boolean) is
      record
         case Exists is
            when True =>
               Value : Positive;
            when False =>
               null;
         end case;
      end record;

   function Argument_SLOC
     (Call            : Call_Expr;
      Parameter_Index : Positive)
      return Source_Location_Range
     with Pre => not Call.Is_Null;
   --  Returns the source location range of the argument associated to
   --  'Parameter_Index'.
   --  An Assertion_Error exception is raised if 'Parameter_Index' is greater
   --  than the number of arguments 'Call' has.

   function Arguments_SLOC
     (Call                    : Call_Expr;
      Parameter_Indices_Range : Parameter_Indices_Range_Type)
      return Source_Location_Range
     with Pre => not Call.Is_Null;
   --  Returns a set of source location ranges of the arguments associated to
   --  'Parameter_Indices_Range'.
   --  An Assertion_Error exception is raised if 'Parameter_Indices_Range'
   --  contains an element that is greater than the number of arguments
   --  'Call' has.

   function Arguments_SLOC
     (Call                     : Call_Expr;
      Parameter_Indices_Ranges : Parameter_Indices_Ranges_Type)
      return Source_Location_Range_Set
     with Pre => not Call.Is_Null and then Parameter_Indices_Ranges'Length > 0;
   --  Returns a set of source location ranges of the arguments associated to
   --  'Parameter_Indices_Ranges'.
   --  An Assertion_Error exception is raised if 'Parameter_Indices_Ranges'
   --  contains an element that is greater than the number of arguments
   --  'Call' has, if it is not sorted or if it has overlapping ranges.

   function Arguments_SLOC (Call : Call_Expr) return Source_Location_Range
     with Pre => not Call.Is_Null;
   --  Returns the source location range of Call's arguments

   procedure Change_Mode
     (Subp               : Basic_Decl'Class;
      Parameters_Indices : Parameter_Indices_Range_Type;
      New_Mode           : Ada_Mode;
      Edits              : in out Text_Edit_Map);
   --  Changes the parameter mode of the parameters with indices defined by
   --  'Parameter_Indices_Range' to 'New_Mode'.

   function First_Designator_Index
     (Arguments : Assoc_List)
      return Optional_Designator_Index;
   --  Finds the index of the first parameter association that has a designator

   procedure Move_Backward
     (Subp            : Basic_Decl'Class;
      Parameter_Index : Positive;
      Edits           : in out Text_Edit_Map);
   --  Moves the parameter defined by 'Parameter_Index' backward

   function Param_Specs_SLOC
     (Subp                      : Basic_Decl'Class;
      Param_Spec_Indices_Ranges : Param_Spec_Indices_Ranges_Type)
      return Source_Location_Range_Set
     with Pre => Is_Subprogram (Subp)
                 and then Param_Spec_Indices_Ranges'Length > 0;
   --  Returns a map with the source location range of the Param_Spec nodes
   --  with indices given by Param_Spec_Indices_Ranges. If maximum value of
   --  Param_Spec_Indices_Ranges is > than the amout of Param_Spec nodes 'Subp'
   --  has, then a 'Program_Error' exception is raised.

   -------------------
   -- Argument_SLOC --
   -------------------

   function Argument_SLOC
     (Call            : Call_Expr;
      Parameter_Index : Positive)
      return Source_Location_Range
   is (Arguments_SLOC
       (Call,
        Parameter_Indices_Range_Type'(Parameter_Index, Parameter_Index)));

   --------------------
   -- Arguments_SLOC --
   --------------------

   function Arguments_SLOC
     (Call              : Call_Expr;
      Parameter_Indices : Parameter_Indices_Type)
      return Source_Location_Range_Set
   is
      Max_Length     : constant Positive := Parameter_Indices'Length;
      Indices_Ranges : Parameter_Indices_Ranges_Type (1 .. Max_Length);
      Real_Length    : Positive := 1;
      Last_Index     : Positive;
      Sorted_Indices : constant Parameter_Indices_Type :=
        Unique (Parameter_Indices);

   begin
      --  Start by transforming 'Sorted_Indices' in 'Indices_Ranges'.
      --
      --  Example 1: If 'Sorted_Indices' is [1, 3, 5] then Max_Length = 3,
      --  Indices_Ranges = [{1, 1}, {3, 3}, {5, 5}] and Real_Length = 3.
      --
      --  Example 2: If 'Sorted_Indices' is [1, 2, 3] then Max_Length = 3,
      --  Indices_Ranges = [{1, 3}] and Real_Length = 1.
      --
      --  Then dispatch to an 'Arguments_SLOC' function that receives a
      --  'Parameter_Indices_Ranges_Type'.

      if Max_Length = 1 then
         Real_Length := 1;
         Indices_Ranges (1).First := Sorted_Indices (1);
         Indices_Ranges (1).Last := Sorted_Indices (1);

      else
         Indices_Ranges (Real_Length).First := Sorted_Indices (1);
         Last_Index := Sorted_Indices (1);

         for Index of Sorted_Indices (2 .. Max_Length) loop
            if Index > Last_Index + 1 then
               Indices_Ranges (Real_Length).Last := Last_Index;
               Real_Length := Real_Length + 1;
               Indices_Ranges (Real_Length).First := Index;
            end if;

            Last_Index := Index;
         end loop;

         Indices_Ranges (Real_Length).Last := Last_Index;
      end if;

      return Arguments_SLOC (Call, Indices_Ranges (1 .. Real_Length));
   end Arguments_SLOC;

   --------------------
   -- Arguments_SLOC --
   --------------------

   function Arguments_SLOC
     (Call                    : Call_Expr;
      Parameter_Indices_Range : Parameter_Indices_Range_Type)
      return Source_Location_Range
   is
      Sloc : Source_Location_Range := No_Source_Location_Range;

      Args         : constant Assoc_List := Call.F_Suffix.As_Assoc_List;
      Args_Length  : constant Positive := Length (Args);
      Arg_Index    : Positive := 1;

      First_Arg    : Param_Assoc := No_Param_Assoc;
      Last_Arg     : Param_Assoc := No_Param_Assoc;
      Previous_Arg : Param_Assoc := No_Param_Assoc;

      Remove_Leading_Comma : Boolean := False;

   begin
      if Parameter_Indices_Range.Last > Args_Length then
         raise Assertion_Error;
      end if;

      --  First check if 'Parameter_Indices_Range' refers to all arguments

      if Parameter_Indices_Range.First = 1
        and then Parameter_Indices_Range.Last = Args_Length
      then
         return Arguments_SLOC (Call);
      end if;

      --  The following loop makes sure that two corner cases are dealt with:
      --
      --  Example 1: Remove A from 'My_Call (A, B, C)'
      --
      --  Example 2: Remove B or C from 'My_Call (A, B, C)'
      --
      --  In Example 1, it's 'A' and the leading comma that must be remove.
      --  However, in example 2, it's B and the trailing comma that must be
      --  removed. The leading comma must only be removed if the first argument
      --  we want to remove is also the first argument of the call.

      for Arg of Args loop
         if Remove_Leading_Comma then
            Last_Arg := Arg.As_Param_Assoc;
            Sloc.Start_Line := First_Arg.Sloc_Range.Start_Line;
            Sloc.Start_Column := First_Arg.Sloc_Range.Start_Column;
            Sloc.End_Line := Last_Arg.Sloc_Range.Start_Line;
            Sloc.End_Column := Last_Arg.Sloc_Range.Start_Column;
            Remove_Leading_Comma := False;
         end if;

         exit when Args_Length < Arg_Index;

         if Arg_Index = Parameter_Indices_Range.First then
            if Arg_Index = 1 then
               First_Arg := Arg.As_Param_Assoc;

            else
               First_Arg := Previous_Arg;
            end if;
         end if;

         if Arg_Index = Parameter_Indices_Range.Last then
            Last_Arg := Arg.As_Param_Assoc;

            if Parameter_Indices_Range.First = 1 then
               Remove_Leading_Comma := True;

            else
               Sloc.Start_Line := First_Arg.Sloc_Range.End_Line;
               Sloc.Start_Column := First_Arg.Sloc_Range.End_Column;
               Sloc.End_Line := Last_Arg.Sloc_Range.End_Line;
               Sloc.End_Column := Last_Arg.Sloc_Range.End_Column;
            end if;
         end if;

         Previous_Arg := Arg.As_Param_Assoc;
         Arg_Index := Arg_Index + 1;
      end loop;

      --  'Parameter_Indices_Range' might have been computed using the amount
      --  of parameters a subprogram has. If at least one of those parameters
      --  has a default value, then we might not have found the last arguments
      --  or even any arguments on this 'Call'.

      if Last_Arg = No_Param_Assoc then
         if First_Arg = No_Param_Assoc then
            --  No arguments were found.

            return No_Source_Location_Range;

         else
            --  The last arguments were not found.

            if Parameter_Indices_Range.First = 1 then
               --  The first argument we want to remove is also the first
               --  argument on this 'Call', therefore, remove all arguments.

               return Arguments_SLOC (Call);

            else
               Sloc.Start_Line := First_Arg.Sloc_Range.End_Line;
               Sloc.Start_Column := First_Arg.Sloc_Range.End_Column;
               Sloc.End_Line := Previous_Arg.Sloc_Range.End_Line;
               Sloc.End_Column := Previous_Arg.Sloc_Range.End_Column;
            end if;
         end if;
      end if;

      return Sloc;
   end Arguments_SLOC;

   --------------------
   -- Arguments_SLOC --
   --------------------

   function Arguments_SLOC
     (Call                     : Call_Expr;
      Parameter_Indices_Ranges : Parameter_Indices_Ranges_Type)
      return Source_Location_Range_Set
   is
      Previous_Last_Index : Natural := 0;

   begin
      return Slocs : Source_Location_Range_Set do
         for Parameter_Indices_Range of Parameter_Indices_Ranges loop
            if Parameter_Indices_Range.First <= Previous_Last_Index then
               raise Assertion_Error;
            end if;

            Previous_Last_Index := Parameter_Indices_Range.Last;

            Slocs.Insert (Arguments_SLOC (Call, Parameter_Indices_Range));
         end loop;
      end return;
   end Arguments_SLOC;

   ---------------------
   -- Arguments_SLOC --
   ---------------------

   function Arguments_SLOC (Call : Call_Expr) return Source_Location_Range
   is
      Call_Expr_SLOC_Range : constant Source_Location_Range :=
        Call.Sloc_Range;
      Call_Name_SLOC_Range : constant Source_Location_Range :=
        Call.F_Name.Sloc_Range;

   begin
      return Args_Sloc_Range : Source_Location_Range do
         Args_Sloc_Range.Start_Line := Call_Name_SLOC_Range.End_Line;
         Args_Sloc_Range.Start_Column := Call_Name_SLOC_Range.End_Column;
         Args_Sloc_Range.End_Line   := Call_Expr_SLOC_Range.End_Line;
         Args_Sloc_Range.End_Column := Call_Expr_SLOC_Range.End_Column;
      end return;
   end Arguments_SLOC;

   ----------------------
   -- Param_Specs_SLOC --
   ----------------------

   function Param_Specs_SLOC
     (Subp                      : Basic_Decl'Class;
      Param_Spec_Indices_Ranges : Param_Spec_Indices_Ranges_Type)
      return Source_Location_Range_Set
   is
      Slocs : Source_Location_Range_Set;

      Subp_Params : constant Params := Get_Subp_Params (Subp);

      procedure Add_To_Slocs (Sloc_Range : Source_Location_Range);
      --  Adds Sloc_Range to Sloc. Does not add No_Source_Location_Range.

      -------------------
      -- Add_To_Result --
      -------------------

      procedure Add_To_Slocs (Sloc_Range : Source_Location_Range) is
      begin
         if Sloc_Range = No_Source_Location_Range then
            return;
         end if;

         Slocs.Insert (Sloc_Range);
      end Add_To_Slocs;

      Indices_Ranges_Index : Positive := 1;
      Param_Spec_Index : Positive := 1;
      Param_Spec_List_Length : Positive;

      First_Param_Spec : Param_Spec := No_Param_Spec;
      Last_Param_Spec : Param_Spec := No_Param_Spec;
      Before_First_Param_Spec : Param_Spec := No_Param_Spec;
      Previous_Param_Spec : Param_Spec := No_Param_Spec;

      Next_Is_Last : Boolean := False;

      Param_Specs_Range : Source_Location_Range := No_Source_Location_Range;

   begin
      if Subp_Params.Is_Null or else Subp_Params = No_Params then
         raise Program_Error
           with Subp.Image & " does not have any params";
      end if;

      if Param_Spec_Indices_Ranges'Length = 0 then
         return Slocs;
      end if;

      Param_Spec_List_Length := Length (Subp_Params.F_Params);

      if Param_Spec_Indices_Ranges (1).First = 1 and then
        Param_Spec_Indices_Ranges (1).Last = Param_Spec_List_Length
      then
         if Param_Spec_Indices_Ranges'Length /= 1 then
            raise Program_Error
              with "Param_Spec_Indices_Ranges is invalid";
         end if;

         Add_To_Slocs (Params_SLOC (Subp));
      else
         for Param_Spec of Subp_Params.F_Params loop
            if Next_Is_Last then
               Last_Param_Spec := Param_Spec.As_Param_Spec;

               Param_Specs_Range.Start_Line :=
                 First_Param_Spec.Sloc_Range.Start_Line;
               Param_Specs_Range.Start_Column :=
                 First_Param_Spec.Sloc_Range.Start_Column;
               Param_Specs_Range.End_Line :=
                 Last_Param_Spec.Sloc_Range.Start_Line;
               Param_Specs_Range.End_Column :=
                 Last_Param_Spec.Sloc_Range.Start_Column;

               Add_To_Slocs (Param_Specs_Range);

               Param_Specs_Range := No_Source_Location_Range;

               Next_Is_Last := False;
            end if;

            exit when Indices_Ranges_Index > Param_Spec_Indices_Ranges'Length;

            if Param_Spec_Index =
              Param_Spec_Indices_Ranges (Indices_Ranges_Index).First
            then
               First_Param_Spec := Param_Spec.As_Param_Spec;
               Before_First_Param_Spec := Previous_Param_Spec.As_Param_Spec;
            end if;

            if Param_Spec_Index =
              Param_Spec_Indices_Ranges (Indices_Ranges_Index).Last
            then
               if Param_Spec_Index = Param_Spec_List_Length then
                  Last_Param_Spec := Param_Spec.As_Param_Spec;

                  Param_Specs_Range.Start_Line :=
                    Before_First_Param_Spec.Sloc_Range.End_Line;
                  Param_Specs_Range.Start_Column :=
                    Before_First_Param_Spec.Sloc_Range.End_Column;
                  Param_Specs_Range.End_Line :=
                    Last_Param_Spec.Sloc_Range.End_Line;
                  Param_Specs_Range.End_Column :=
                    Last_Param_Spec.Sloc_Range.End_Column;

                  Add_To_Slocs (Param_Specs_Range);

                  Param_Specs_Range := No_Source_Location_Range;
               else
                  Next_Is_Last := True;
               end if;

               Indices_Ranges_Index := Indices_Ranges_Index + 1;
            end if;

            Previous_Param_Spec := Param_Spec.As_Param_Spec;
            Param_Spec_Index := Param_Spec_Index + 1;
         end loop;
      end if;

      return Slocs;
   end Param_Specs_SLOC;

   ----------------------
   -- Parameters_SLOC --
   ----------------------

   function Parameters_SLOC
     (Subp                     : Basic_Decl'Class;
      Parameter_Indices_Ranges : Parameter_Indices_Ranges_Type)
      return Source_Location_Range_Set
   is
      Slocs : Source_Location_Range_Set;

      Subp_Params : constant  Params := Get_Subp_Params (Subp);

      procedure Add_To_Slocs (Sloc_Range : Source_Location_Range);
      --  Adds Sloc_Range to Sloc. Does not add No_Source_Location_Range.

      procedure Process_Subp_Params (Subp_Params : Params);
      --  Finds the source location range of the parameters with index given
      --  by 'Parameter_Indices_Ranges', and adds them to 'Slocs'.

      ------------------
      -- Add_To_Slocs --
      ------------------

      procedure Add_To_Slocs (Sloc_Range : Source_Location_Range) is
      begin
         if Sloc_Range = No_Source_Location_Range then
            return;
         end if;

         Slocs.Insert (Sloc_Range);
      end Add_To_Slocs;

      ------------------
      -- Process_Subp --
      ------------------

      procedure Process_Subp_Params (Subp_Params : Params) is
         Indices_Index        : Positive := 1;
         Param_Spec_Index     : Positive := 1;
         Param_Spec_Length    : Positive;
         Relative_Index       : Positive;
         Absolute_Index       : Positive;
         First_Relative_Index : Natural := 0;
         First_Param          : Defining_Name := No_Defining_Name;
         Before_First_Param   : Defining_Name := No_Defining_Name;
         Update_First_Param   : Boolean := False;
         Last_Relative_Index  : Natural := 0;
         Last_Param           : Defining_Name := No_Defining_Name;
         Last_Param_Aux       : Defining_Name := No_Defining_Name;
         Next_Is_Last         : Boolean := False;
         Params_Sloc_Range    : Source_Location_Range :=
           No_Source_Location_Range;

         package Positive_Vectors is new Ada.Containers.Vectors
           (Index_Type   => Positive,
            Element_Type => Positive,
            "="          => "=");

         use Positive_Vectors;

         Param_Spec_Indices : Vector;

         use type Ada.Containers.Count_Type;

      begin
         Absolute_Index := 1;
         for Param_Spec of Subp_Params.F_Params loop
            Param_Spec_Length := Length (Param_Spec.F_Ids);
            Relative_Index := 1;

            for Param of Param_Spec.F_Ids loop
               if Next_Is_Last then
                  Last_Param := Param.As_Defining_Name;
                  Params_Sloc_Range.Start_Line :=
                    First_Param.Sloc_Range.Start_Line;
                  Params_Sloc_Range.Start_Column :=
                    First_Param.Sloc_Range.Start_Column;
                  Params_Sloc_Range.End_Line :=
                    Last_Param.Sloc_Range.Start_Line;
                  Params_Sloc_Range.End_Column :=
                    Last_Param.Sloc_Range.Start_Column;
                  Add_To_Slocs (Params_Sloc_Range);
                  Params_Sloc_Range := No_Source_Location_Range;
                  First_Param := No_Defining_Name;
                  Last_Param := No_Defining_Name;
                  Next_Is_Last := False;
               end if;

               if Indices_Index > Parameter_Indices_Ranges'Length then
                  exit;
               end if;

               if Update_First_Param
                 or else Absolute_Index =
                   Parameter_Indices_Ranges (Indices_Index).First
               then
                  First_Relative_Index := Relative_Index;
                  First_Param := Param.As_Defining_Name;

                  if Relative_Index /= 1 then
                     Before_First_Param := Last_Param_Aux.As_Defining_Name;
                  end if;

                  if Update_First_Param then
                     Update_First_Param := False;
                  end if;
               end if;

               if Absolute_Index =
                 Parameter_Indices_Ranges (Indices_Index).Last
               then
                  Last_Relative_Index := Relative_Index;
                  Last_Param := Param.As_Defining_Name;

                  if First_Relative_Index = 1 then
                     if Last_Relative_Index = Param_Spec_Length then
                        Param_Spec_Indices.Append (Param_Spec_Index);

                     else
                        Next_Is_Last := True;
                     end if;

                  else
                     Params_Sloc_Range.Start_Line :=
                       Before_First_Param.Sloc_Range.End_Line;
                     Params_Sloc_Range.Start_Column :=
                       Before_First_Param.Sloc_Range.End_Column;
                     Params_Sloc_Range.End_Line :=
                       Last_Param.Sloc_Range.End_Line;
                     Params_Sloc_Range.End_Column :=
                       Last_Param.Sloc_Range.End_Column;
                     Add_To_Slocs (Params_Sloc_Range);
                     Params_Sloc_Range := No_Source_Location_Range;
                  end if;

                  Indices_Index := Indices_Index + 1;
                  First_Relative_Index := 0;
                  Last_Relative_Index := 0;
                  if not Next_Is_Last then
                     First_Param := No_Defining_Name;
                     Last_Param := No_Defining_Name;
                  end if;
               end if;

               Relative_Index := Relative_Index + 1;
               Absolute_Index := Absolute_Index + 1;

               Last_Param_Aux := Param.As_Defining_Name;
            end loop;

            if First_Relative_Index /= 0 then
               Update_First_Param := True;

               if First_Relative_Index = 1 then
                  Param_Spec_Indices.Append (Param_Spec_Index);

               else
                  Params_Sloc_Range.Start_Line :=
                    Before_First_Param.Sloc_Range.End_Line;
                  Params_Sloc_Range.Start_Column :=
                    Before_First_Param.Sloc_Range.End_Column;
                  Params_Sloc_Range.End_Line :=
                    Last_Param_Aux.Sloc_Range.End_Line;
                  Params_Sloc_Range.End_Column :=
                    Last_Param_Aux.Sloc_Range.End_Column;
                  Add_To_Slocs (Params_Sloc_Range);
                  Params_Sloc_Range := No_Source_Location_Range;
               end if;
            end if;

            Param_Spec_Index := Param_Spec_Index + 1;
         end loop;

         if Param_Spec_Indices.Length /= 0 then
            declare
               Param_Spec_Indices_Ranges : Param_Spec_Indices_Ranges_Type
                 (1 .. Positive (Param_Spec_Indices.Length));
               This_Indices_Range : Param_Spec_Indices_Range_Type;
               Real_Length : Positive := 1;
               Update_First_Index : Boolean := True;
               Last_Index : Positive := 1;  -- Default value does not matter

            begin
               for Index of Param_Spec_Indices loop
                  if Update_First_Index then
                     This_Indices_Range.First := Index;
                     Update_First_Index := False;

                  else
                     if Index /= Last_Index + 1 then
                        This_Indices_Range.Last := Last_Index;
                        Param_Spec_Indices_Ranges (Real_Length) :=
                          This_Indices_Range;
                        Real_Length := Real_Length + 1;
                        This_Indices_Range.First := Index;
                     end if;
                  end if;

                  Last_Index := Index;
               end loop;

               This_Indices_Range.Last := Last_Index;
               Param_Spec_Indices_Ranges (Real_Length) := This_Indices_Range;

               for Sloc of
                 Param_Specs_SLOC
                   (Subp_Params.P_Semantic_Parent.As_Basic_Decl,
                    Param_Spec_Indices_Ranges (1 .. Real_Length))
               loop
                  Slocs.Insert (Sloc);
               end loop;
            end;
         end if;
      end Process_Subp_Params;

   begin
      Process_Subp_Params (Subp_Params);

      return Slocs;
   end Parameters_SLOC;

   ----------------------
   -- To_Unique_Ranges --
   ----------------------

   function To_Unique_Ranges
     (Parameter_Indices : Parameter_Indices_Type)
         return Parameter_Indices_Ranges_Type
   is
      Unique_Indices : constant Parameter_Indices_Type :=
        Unique (Parameter_Indices);
      Max_Length     : constant Natural := Unique_Indices'Length;
      Unique_Ranges  : Parameter_Indices_Ranges_Type (1 .. Max_Length);
      Real_Length    : Positive := 1;
      Last_Index     : Positive;

   begin
      if Max_Length = 0 then
         return [];
      end if;

      --  Start by transforming 'Sorted_Indices' in 'Indices_Ranges'.
      --
      --  Example 1: If 'Sorted_Indices' is [1, 3, 5] then Max_Length = 3,
      --  Indices_Ranges = [{1, 1}, {3, 3}, {5, 5}] and Real_Length = 3.
      --
      --  Example 2: If 'Sorted_Indices' is [1, 2, 3] then Max_Length = 3,
      --  Indices_Ranges = [{1, 3}] and Real_Length = 1.
      --
      --  Then dispatch to an 'Arguments_SLOC' function that receives a
      --  'Parameter_Indices_Ranges_Type'.

      if Max_Length = 1 then
         Real_Length := 1;
         Unique_Ranges (1).First := Unique_Indices (Unique_Indices'First);
         Unique_Ranges (1).Last := Unique_Indices (Unique_Indices'First);

      else
         Unique_Ranges (Real_Length).First := Unique_Indices (1);
         Last_Index := Unique_Indices (1);

         for Index of Unique_Indices (2 .. Max_Length) loop
            if Index > Last_Index + 1 then
               Unique_Ranges (Real_Length).Last := Last_Index;
               Real_Length := Real_Length + 1;
               Unique_Ranges (Real_Length).First := Index;
            end if;

            Last_Index := Index;
         end loop;

         Unique_Ranges (Real_Length).Last := Last_Index;
      end if;

      return Unique_Ranges (1 .. Real_Length);
   end To_Unique_Ranges;

   --------------------------------
   -- Is_Add_Parameter_Available --
   --------------------------------

   function Is_Add_Parameter_Available
     (Unit                        : Analysis_Unit;
      Location                    : Source_Location;
      Requires_Full_Specification : out Boolean)
      return Boolean
   is
      Aux_Node : Ada_Node :=
        (if Unit /= No_Analysis_Unit
           and then Location /= No_Source_Location
         then
            Unit.Root.Lookup (Location)
         else
            No_Ada_Node);

   begin
      if Aux_Node.Is_Null then
         return False;
      end if;

      if Aux_Node.Kind in Ada_Subp_Decl_Range
        and then Is_Subprogram (Aux_Node.As_Basic_Decl)
      then
         Aux_Node := Get_Subp_Spec (Aux_Node.As_Basic_Decl).As_Ada_Node;

      else
         while not Aux_Node.Is_Null loop
            exit when Aux_Node.Kind in Ada_Subp_Spec_Range;

            Aux_Node := Aux_Node.Parent;
         end loop;
      end if;

      if Aux_Node.Is_Null then
         return False;
      end if;

      Requires_Full_Specification :=
        Aux_Node.As_Subp_Spec.F_Subp_Params.Is_Null;

      return True;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Is_Refactoring_Tool_Available_Default_Error_Message
              ("Add Parameter"));
         return False;
   end Is_Add_Parameter_Available;

   ------------------------------
   -- Is_Change_Mode_Available --
   ------------------------------

   function Is_Change_Mode_Available
     (Node                    : Ada_Node'Class;
      Subp                    : out Basic_Decl;
      Parameter_Indices_Range : out Parameter_Indices_Range_Type;
      Mode_Alternatives       : out Mode_Alternatives_Type)
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
         Mode_Alternatives :=
           [Ada_Mode_Default, Ada_Mode_Default, Ada_Mode_Default];
      end Initialize_Out_Parameters;

      Parent_Defining_Name       : Defining_Name := No_Defining_Name;
      Parent_Subptype_Indication : Subtype_Indication := No_Subtype_Indication;
      Parent_Defining_Name_List  : Defining_Name_List := No_Defining_Name_List;
      Parent_Param_Spec          : Param_Spec := No_Param_Spec;
      Parent_Param_Spec_List     : Param_Spec_List := No_Param_Spec_List;
      Parent_Subp_Decl           : Basic_Decl := No_Basic_Decl;

      Parameter_Absolute_Index : Positive;

      Aux_Node : Ada_Node := Node.As_Ada_Node;

   begin
      Initialize_Out_Parameters;

      Find_Subp :
      while not Aux_Node.Is_Null loop
         exit Find_Subp when Aux_Node.Kind in Ada_Subp_Spec_Range;

         case Aux_Node.Kind is
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
         Parameter_Indices_Range.Last := Parameter_Absolute_Index;
         Mode_Alternatives :=
           Mode_Alternatives_Map (Parent_Param_Spec.F_Mode.Kind);

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

         Mode_Alternatives :=
           Mode_Alternatives_Map (Parent_Param_Spec.F_Mode.Kind);

         return True;

      else
         return False;
      end if;

   exception
      when E : others =>
         --  Assume that exceptions can be raised due to invalid code and
         --  set again the out parameters since they might have been changed
         --  since initialized.
         Initialize_Out_Parameters;
         Refactor_Trace.Trace
           (E,
            Is_Refactoring_Tool_Available_Default_Error_Message
              ("Change Parameter Mode"));
         return False;
   end Is_Change_Mode_Available;

   ---------------------------------
   -- Is_Move_Parameter_Available --
   ---------------------------------

   function Is_Move_Parameter_Available
     (Node            : Ada_Node'Class;
      Subp            : out Basic_Decl;
      Parameter_Index : out Positive;
      Move_Directions : out Move_Direction_Availability_Type)
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
         Parameter_Index := 1;
         Move_Directions := [False, False];
      end Initialize_Out_Parameters;

      --  Aux_Node must have the following parent nodes in order to guarantee
      --  that Aux_Node is related to a parameter.
      Parent_Defining_Name       : Defining_Name := No_Defining_Name;
      Parent_Defining_Name_List  : Defining_Name_List := No_Defining_Name_List;
      Parent_Param_Spec          : Param_Spec := No_Param_Spec;

      --  Number of parameters the target subprogram has
      Total_Parameters         : Positive;

      --  Auxiliary node to climb the tree
      Aux_Node : Ada_Node := Node.As_Ada_Node;

   begin
      Initialize_Out_Parameters;

      Find_Subp :
      while not Aux_Node.Is_Null loop
         exit Find_Subp when Aux_Node.Kind in Ada_Subp_Spec_Range;

         case Aux_Node.Kind is
            when Ada_Defining_Name =>
               Parent_Defining_Name := Aux_Node.As_Defining_Name;

            when Ada_Defining_Name_List =>
               Parent_Defining_Name_List := Aux_Node.As_Defining_Name_List;

            when Ada_Param_Spec =>
               Parent_Param_Spec := Aux_Node.As_Param_Spec;

            when others => null;
         end case;

         Aux_Node := Aux_Node.Parent;
      end loop Find_Subp;

      if Aux_Node.Is_Null
        or else Parent_Defining_Name = No_Defining_Name
        or else Parent_Defining_Name_List = No_Defining_Name_List
        or else Parent_Param_Spec = No_Param_Spec
      then
         return False;
      end if;

      Subp := Aux_Node.As_Subp_Spec.P_Parent_Basic_Decl;

      Parameter_Index :=
        Get_Parameter_Absolute_Index (Parent_Defining_Name);

      Total_Parameters :=
        Count_Subp_Parameters (Get_Subp_Params (Subp));

      --  If the target subprogram only has one parameter, then it can't be
      --  moved.

      if Total_Parameters = 1 then
         return False;
      end if;

      Assert (Total_Parameters >= Parameter_Index);

      --  If the target parameter is the first one, then it can only be moved
      --  forward. If it is the last one, then it can only be moved backward.
      --  Otherwise, both directions are valid.

      if Parameter_Index = 1 then
         Move_Directions := Only_Forward;

      elsif Parameter_Index = Total_Parameters then
         Move_Directions := Only_Backward;

      else
         Move_Directions := Both_Directions;
      end if;

      return True;

   exception
      when E : others =>
         --  Assume that exceptions can be raised due to invalid code and
         --  set again the out parameters since they might have been changed
         --  since initialized.
         Initialize_Out_Parameters;
         Refactor_Trace.Trace
           (E,
            Is_Refactoring_Tool_Available_Default_Error_Message
              ("Move Parameter"));
         return False;
   end Is_Move_Parameter_Available;

   -----------------
   -- Change_Mode --
   -----------------

   function Change_Mode
     (Subp                    : Basic_Decl;
      Parameter_Indices_Range : Parameter_Indices_Range_Type;
      New_Mode                : Ada_Mode;
      Units                   : Analysis_Unit_Array)
      return Text_Edit_Map
   is
      Edits : Text_Edit_Map;

      procedure Change_Mode_Callback (Relative_Subp : Basic_Decl'Class);
      --  Determines the necessary changes to 'Relative_Subp' specification,
      --  and adds them to 'Edits'.

      --------------------------
      -- Change_Mode_Callback --
      --------------------------

      procedure Change_Mode_Callback (Relative_Subp : Basic_Decl'Class) is
         Relative_Subp_Body : constant Base_Subp_Body :=
           Find_Subp_Body (Relative_Subp);

      begin
         if Is_Subprogram (Relative_Subp) then
            Change_Mode
              (Relative_Subp,
               Parameter_Indices_Range,
               New_Mode,
               Edits);

            if not Relative_Subp_Body.Is_Null then
               Change_Mode
                 (Relative_Subp_Body,
                  Parameter_Indices_Range,
                  New_Mode,
                  Edits);
            end if;
         end if;
      end Change_Mode_Callback;

   begin
      Find_Subp_Relatives
        (Subp           => Subp,
         Units          => Units,
         Decls_Callback => Change_Mode_Callback'Access);

      return Edits;
   end Change_Mode;

   -------------------
   -- Move_Backward --
   -------------------

   function Move_Backward
     (Subp            : Basic_Decl;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Text_Edit_Map
   is
      Edits : Text_Edit_Map;

      procedure Move_Parameter_Callback (Relative_Subp : Basic_Decl'Class);
      --  Callback that adds to Edits the necessary edits to 'Relative_Subp'
      --  specification.

      procedure Move_Arguments_Callback
        (Call_Identifier : Base_Id'Class;
         Kind            : Ref_Result_Kind;
         Cancel          : in out Boolean);
      --  Callback that adds to Edits the necessary edits to 'Call'

      -----------------------------
      -- Move_Parameter_Callback --
      -----------------------------

      procedure Move_Parameter_Callback (Relative_Subp : Basic_Decl'Class) is
         Relative_Subp_Body : constant Base_Subp_Body :=
           Find_Subp_Body (Relative_Subp);

      begin
         if Is_Subprogram (Relative_Subp) then
            Move_Backward
              (Relative_Subp, Parameter_Index, Edits);

            if not Relative_Subp_Body.Is_Null then
               Move_Backward
                 (Relative_Subp_Body, Parameter_Index, Edits);
            end if;
         end if;
      end Move_Parameter_Callback;

      -----------------------------
      -- Move_Arguments_Callback --
      -----------------------------

      procedure Move_Arguments_Callback
        (Call_Identifier : Base_Id'Class;
         Kind            : Ref_Result_Kind;
         Cancel          : in out Boolean)
      is
         pragma Unreferenced (Kind);

         Call_Expression : Call_Expr;

         function Has_Parameter_Association return Boolean;
         --  Returns True if 'Call_Expression' has a parameter association

         procedure Process_Dot_Call;
         --  Processes calls with the dot notation

         procedure Process_Non_Dot_Call (Target_Index : Positive);
         --  PRocesses calls without the dot notation

         -------------------------------
         -- Has_Parameter_Association --
         -------------------------------

         function Has_Parameter_Association return Boolean
         is
            Has_Designator : Boolean;

         begin
            for Param_Assoc of Call_Expression.F_Suffix.As_Assoc_List loop
               Has_Designator :=
                 not Param_Assoc.As_Param_Assoc.F_Designator.Is_Null;

               --  We just need to check the first argument. If it has a
               --  designator, then all the other ones must also have.

               exit;
            end loop;

            return Has_Designator;
         end Has_Parameter_Association;

         ----------------------
         -- Process_Dot_Call --
         ----------------------

         procedure Process_Dot_Call is
            Edits_Set : Text_Edit_Ordered_Set;

            Object_Name : Base_Id := No_Base_Id;
            Call_Name   : Base_Id := No_Base_Id;

            Object_Name_Sloc : Source_Location_Range :=
              No_Source_Location_Range;
            Call_Name_Sloc : Source_Location_Range :=
              No_Source_Location_Range;

            Object_Name_Text : Unbounded_String := Null_Unbounded_String;

            Delition_Sloc : Source_Location_Range :=
              No_Source_Location_Range;

            Insertion_Sloc : Source_Location_Range :=
              No_Source_Location_Range;

            First_Parameter_Name : Unbounded_String := Null_Unbounded_String;

            New_Text : Unbounded_String := Null_Unbounded_String;

         begin
            if Parameter_Index = 2 then
               Call_Name :=
                 Call_Expression.F_Name.As_Dotted_Name.F_Suffix;

               if Call_Expression.F_Name.As_Dotted_Name.F_Prefix.Kind =
                 Ada_Dotted_Name
               then
                  Object_Name :=
                    Call_Expression.F_Name.
                      As_Dotted_Name.F_Prefix.As_Dotted_Name.F_Suffix;

               else
                  Object_Name :=
                    Call_Expression.F_Name.
                      As_Dotted_Name.F_Prefix.As_Base_Id;
               end if;

               Object_Name_Sloc := Object_Name.Sloc_Range;
               Call_Name_Sloc := Call_Name.Sloc_Range;

               Object_Name_Text := To_Unbounded_String
                 (Ada.Characters.Conversions.To_String
                    (Object_Name.Text));

               for Param_Assoc of
                 Call_Expression.F_Suffix.As_Assoc_List
               loop
                  Insertion_Sloc := Source_Location_Range'
                    (Param_Assoc.Sloc_Range.End_Line,
                     Param_Assoc.Sloc_Range.End_Line,
                     Param_Assoc.Sloc_Range.End_Column,
                     Param_Assoc.Sloc_Range.End_Column);

                  exit;
               end loop;

               if Has_Parameter_Association then
                  First_Parameter_Name := To_Unbounded_String
                    (Ada.Characters.Conversions.To_String
                       (Get_Parameter_Name (Subp, 1)));

                  New_Text := ", "
                    & First_Parameter_Name
                    & " => "
                    & Object_Name_Text;
               else
                  New_Text := ", " & Object_Name_Text;
               end if;

               Delition_Sloc := Source_Location_Range'
                 (Object_Name_Sloc.Start_Line,
                  Call_Name_Sloc.Start_Line,
                  Object_Name_Sloc.Start_Column,
                  Call_Name_Sloc.Start_Column);

               Edits_Set.Insert
                 ((Delition_Sloc, Null_Unbounded_String));
               Edits_Set.Insert
                 ((Insertion_Sloc, New_Text));

            elsif not Has_Parameter_Association then
               Process_Non_Dot_Call (Parameter_Index - 1);
            end if;
         end Process_Dot_Call;

         --------------------------
         -- Process_Non_Dot_Call --
         --------------------------

         procedure Process_Non_Dot_Call (Target_Index : Positive) is
            Arg_A : Param_Assoc := No_Param_Assoc;
            Arg_B : Param_Assoc := No_Param_Assoc;

            Arg_A_Sloc : Source_Location_Range := No_Source_Location_Range;
            Arg_B_Sloc : Source_Location_Range := No_Source_Location_Range;

            Arg_A_Text : Unbounded_String := Null_Unbounded_String;
            Arg_B_Text : Unbounded_String := Null_Unbounded_String;

            Param_Assoc_Index : Positive := 1;

            Edits_Set : Text_Edit_Ordered_Set;

         begin
            for Param_Assoc of Call_Expression.F_Suffix.As_Assoc_List loop
               if Param_Assoc_Index = Target_Index - 1 then
                  Arg_A := Param_Assoc.As_Param_Assoc;

               elsif Param_Assoc_Index = Target_Index then
                  Arg_B := Param_Assoc.As_Param_Assoc;

                  exit;
               end if;

               Param_Assoc_Index := Param_Assoc_Index + 1;
            end loop;

            --  Assert that Arg_A and Arg_B have been found and have changed
            Assert (Arg_A /= No_Param_Assoc and Arg_B /= No_Param_Assoc);

            Arg_A_Sloc := Arg_A.Sloc_Range;
            Arg_B_Sloc := Arg_B.Sloc_Range;

            Arg_A_Text := To_Unbounded_String
              (Ada.Characters.Conversions.To_String (Arg_A.Text));
            Arg_B_Text := To_Unbounded_String
              (Ada.Characters.Conversions.To_String (Arg_B.Text));

            Edits_Set.Insert ((Arg_B_Sloc, Arg_A_Text));
            Edits_Set.Insert ((Arg_A_Sloc, Arg_B_Text));

            for Edit of Edits_Set loop
               Safe_Insert
                 (Edits,
                  Call_Expression.Unit.Get_Filename,
                  (Edit.Location, Edit.Text));
            end loop;
         end Process_Non_Dot_Call;

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

         case Call_Identifier.P_Is_Dot_Call is
            when True =>
               Process_Dot_Call;

            when False =>
               if not Has_Parameter_Association then
                  Process_Non_Dot_Call (Parameter_Index);
               end if;

         end case;
      end Move_Arguments_Callback;

   begin
      Find_Subp_Relatives
        (Subp           => Subp,
         Units          => Units,
         Decls_Callback => Move_Parameter_Callback'Access,
         Find_Calls     => True,
         Calls_Callback => Move_Arguments_Callback'Access);

      return Edits;
   end Move_Backward;

   ----------------------------
   -- First_Designator_Index --
   ----------------------------

   function First_Designator_Index
     (Arguments : Assoc_List)
      return Optional_Designator_Index
   is
      Index : Positive := 1;

   begin
      if Arguments.Is_Null then
         return Optional_Designator_Index'(Exists => False);
      end if;

      for Argument of Arguments loop
         if not Argument.As_Param_Assoc.F_Designator.Is_Null then
            return Optional_Designator_Index'
              (Exists => True,
               Value  => Index);
         end if;

         Index := Index + 1;
      end loop;

      return Optional_Designator_Index'(Exists => False);
   end First_Designator_Index;

   ---------------------------------
   -- Map_Parameters_To_Arguments --
   ---------------------------------

   function Map_Parameters_To_Arguments
     (Parameters : Params'Class;
      Call       : Call_Expr'Class)
      return Extended_Argument_Indicies_Type
   is
      Parameters_Count : constant Natural :=
        Count_Subp_Parameters (Parameters.As_Params);
      Arguments        : constant Assoc_List := Call.F_Suffix.As_Assoc_List;
      Arguments_Count  : constant Natural := Length (Arguments);
      First_Designator : constant Optional_Designator_Index :=
        First_Designator_Index (Arguments);

      function Map_Parameter_To_Named_Argument
        (Parameter_Index : Positive)
         return Natural;
      --  Searches for a actual parameter on 'Call' that has the same name as
      --  the parameter with index 'Parameter_Index'. Returns the index of the
      --  actual parameter if found, otherwise, returns 0.

      function Map_Parameter_To_Named_Argument
        (Parameter_Index : Positive)
         return Natural
      is
         Parameter_Name : constant Text_Type :=
           Get_Parameter_Name (Parameters, Parameter_Index);
         Argument_Index : Positive := 1;

      begin
         for Argument of Arguments loop
            --  FIXME: Do a case insensitive comparison
            if not Argument.As_Param_Assoc.F_Designator.Is_Null
              and then Argument.As_Param_Assoc.F_Designator.Text
                = Parameter_Name
            then
               return Argument_Index;
            end if;

            Argument_Index := Argument_Index + 1;
         end loop;

         return 0;
      end Map_Parameter_To_Named_Argument;

      Map : Extended_Argument_Indicies_Type (1 .. Parameters_Count) :=
        [others => 0];

   begin
      Assert
        (Parameters_Count /= 0
         and then Arguments_Count /= 0
         and then Parameters_Count >= Arguments_Count);

      if First_Designator.Exists then
         Assert (Arguments_Count >= First_Designator.Value);

         for J in 1 .. First_Designator.Value - 1 loop
            Map (J) := J;
         end loop;

         for J in First_Designator.Value .. Parameters_Count loop
            Map (J) := Map_Parameter_To_Named_Argument (J);
         end loop;

      else
         for J in 1 .. Arguments_Count loop
            Map (J) := J;
         end loop;
      end if;

      return Map;
   end Map_Parameters_To_Arguments;

   function Get_Parameter_Relative_Position
     (Subp_Spec : Libadalang.Analysis.Subp_Spec;
      Location  : Source_Location)
      return Parameter_Relative_Position_Type;
   --  Returns the relative position of Location in relation to a parameter in
   --  Subp_Spec.
   --
   --  Example:
   --
   --  procedure Foo (A, B : Bar; C : Baz);
   --                ||  |       |
   --                ||  |       `-> Location_0
   --                ||  `-> Location_1
   --                |`-> Location_2
   --                `->Location_3
   --  Location 0 = Before C
   --  Location 1 = After B
   --  Location 2 = Before B
   --  Location 3 = Before A
   --
   --  Note that the distintic between "Before C" and "After B" is important
   --  when adding a parameter without fully specifying it. In that case, the
   --  new parameter will have the same Param_Spec as the already existing one.
   --  Therefore, "After B" means that the new parameter will be on the
   --  "A, B : Bar" Param_Spec, after parameter B. "After C" means that the
   --  new parameter will be on the "C : Baz" Param_Spec, before parameter C.

   -------------------------------------
   -- Get_Parameter_Relative_Position --
   -------------------------------------

   function Get_Parameter_Relative_Position
     (Subp_Spec : Libadalang.Analysis.Subp_Spec;
      Location  : Source_Location)
      return Parameter_Relative_Position_Type
   is
      Subp_Params : constant Params := Subp_Spec.F_Subp_Params;

      Total_Param_Specs           : constant Natural :=
        Count_Subp_Param_Specs (Subp_Params);
      Total_Param_Spec_Parameters : Natural;

      Is_First_Param_Spec : Boolean := True;
      Is_Last_Param_Spec  : Boolean;

      Is_First_Parameter : Boolean := True;
      Is_Last_Parameter  : Boolean;

      Previous_Param_Spec : Param_Spec := No_Param_Spec;
      Previous_Parameter  : Defining_Name := No_Defining_Name;

      Param_Spec_Idx           : Positive := 1;
      Param_Spec_Parameter_Idx : Positive := 1;
      Parameter_Idx            : Positive := 1;

   begin
      if Total_Param_Specs = 0 then
         return (Before, 1);
      end if;

      Foo :
      for Param_Spec of Subp_Params.F_Params loop
         Is_Last_Param_Spec := Param_Spec_Idx = Total_Param_Specs;
         Is_First_Parameter := True;
         Total_Param_Spec_Parameters :=
           Count_Param_Spec_Parameters (Param_Spec.As_Param_Spec);
         Param_Spec_Parameter_Idx := 1;

         for Parameter of Param_Spec.F_Ids loop
            Is_Last_Parameter :=
              Param_Spec_Parameter_Idx = Total_Param_Spec_Parameters;

            if Is_First_Parameter
              and then Is_Last_Parameter
              and then Is_First_Param_Spec
              and then Is_Last_Param_Spec
            then
               declare
                  Tail   : constant Source_Location :=
                    Start_Sloc (Subp_Spec.Sloc_Range);
                  Middle : constant Source_Location :=
                    Start_Sloc (Parameter.Sloc_Range);
                  Trail  : constant Source_Location :=
                    End_Sloc (Subp_Spec.Sloc_Range);
               begin
                  Assert
                    (Parameter_Idx = 1,
                     "First parameter does not have index 1");
                  if Location < Middle then
                     Assert (Location >= Tail);
                     return (Before, Parameter_Idx);
                  else
                     Assert (Location <= Trail);
                     return (After, Parameter_Idx);
                  end if;
               end;

            elsif Is_First_Parameter
              and then Is_Last_Parameter
              and then Is_First_Param_Spec
              and then not Is_Last_Param_Spec
            then
               declare
                  Tail   : constant Source_Location :=
                    Start_Sloc (Subp_Spec.Sloc_Range);
                  Middle : constant Source_Location :=
                    Start_Sloc (Parameter.Sloc_Range);
                  Trail  : constant Source_Location :=
                    End_Sloc (Param_Spec.Sloc_Range);

               begin
                  Assert (Parameter_Idx = 1);

                  if Location >= Tail and then Location < Middle then
                     return (Before, Parameter_Idx);
                  elsif Location >= Middle and Location <= Trail then
                     return (After, Parameter_Idx);
                  end if;
               end;

            elsif Is_First_Parameter
              and then Is_Last_Parameter
              and then not Is_First_Param_Spec
              and then not Is_Last_Param_Spec
            then
               Assert (not Previous_Param_Spec.Is_Null);

               declare
                  Tail   : constant Source_Location :=
                    End_Sloc (Previous_Param_Spec.Sloc_Range);
                  Middle : constant Source_Location :=
                    Start_Sloc (Parameter.Sloc_Range);
                  Trail  : constant Source_Location :=
                    End_Sloc (Param_Spec.Sloc_Range);

               begin
                  Assert (Parameter_Idx > 1);

                  if Location >= Tail and then Location < Middle then
                     return (Before, Parameter_Idx);
                  elsif Location >= Middle and Location <= Trail then
                     return (After, Parameter_Idx);
                  end if;
               end;

            elsif Is_First_Parameter
              and then Is_Last_Parameter
              and then not Is_First_Param_Spec
              and then Is_Last_Param_Spec
            then
               Assert (not Previous_Param_Spec.Is_Null);

               declare
                  Tail   : constant Source_Location :=
                    End_Sloc (Previous_Param_Spec.Sloc_Range);
                  Middle : constant Source_Location :=
                    Start_Sloc (Parameter.Sloc_Range);
                  Trail  : constant Source_Location :=
                    End_Sloc (Subp_Spec.Sloc_Range);

               begin
                  Assert (Parameter_Idx > 1);

                  if Location >= Tail and then Location < Middle then
                     return (Before, Parameter_Idx);
                  elsif Location >= Middle and Location <= Trail then
                     return (After, Parameter_Idx);
                  end if;
               end;

            elsif Is_First_Parameter
              and then not Is_Last_Parameter
              and then Is_First_Param_Spec
            then
               Assert
                 (Parameter_Idx = 1,
                  "First parameter does not have index 1");

               declare
                  Tail   : constant Source_Location :=
                    Start_Sloc (Subp_Spec.Sloc_Range);
                  Trail  : constant Source_Location :=
                    Start_Sloc (Parameter.Sloc_Range);

               begin
                  Assert (Location >= Tail);
                  if Location < Trail then
                     return (Before, Parameter_Idx);
                  end if;
               end;

            elsif not Is_First_Parameter
              and then Is_Last_Parameter
              and then not Is_Last_Param_Spec
            then
               Assert (not Previous_Parameter.Is_Null);
               Assert (Parameter_Idx > 1);

               declare
                  Tail   : constant Source_Location :=
                    Start_Sloc (Previous_Parameter.Sloc_Range);
                  Middle : constant Source_Location :=
                    Start_Sloc (Parameter.Sloc_Range);
                  Trail  : constant Source_Location :=
                    End_Sloc (Param_Spec.Sloc_Range);

               begin
                  if Location >= Tail and then Location < Middle then
                     return (Before, Parameter_Idx);
                  elsif Location >= Middle and Location <= Trail then
                     return (After, Parameter_Idx);
                  end if;
               end;

            elsif Is_First_Parameter
              and then not Is_Last_Parameter
              and then not Is_First_Param_Spec
            then
               Assert (not Previous_Param_Spec.Is_Null);

               declare
                  Tail  : constant Source_Location :=
                    End_Sloc (Previous_Param_Spec.Sloc_Range);
                  Trail : constant Source_Location :=
                    Start_Sloc (Param_Spec.Sloc_Range);

               begin
                  Assert (Parameter_Idx > 1);

                  if Location >= Tail and then Location < Trail then
                     return (Before, Parameter_Idx);
                  end if;
               end;

            elsif not Is_First_Parameter
              and then not Is_Last_Parameter
            then
               Assert (not Previous_Parameter.Is_Null);

               declare
                  Tail  : constant Source_Location :=
                    Start_Sloc (Previous_Parameter.Sloc_Range);
                  Trail : constant Source_Location :=
                    Start_Sloc (Parameter.Sloc_Range);

               begin
                  if Location >= Tail and then Location < Trail then
                     return (Before, Parameter_Idx);
                  end if;
               end;

            elsif not Is_First_Parameter
              and then Is_Last_Parameter
              and then Is_Last_Param_Spec
            then
               Assert (not Previous_Parameter.Is_Null);

               declare
                  Tail   : constant Source_Location :=
                    Start_Sloc (Previous_Parameter.Sloc_Range);
                  Middle : constant Source_Location :=
                    Start_Sloc (Parameter.Sloc_Range);
                  Trail  : constant Source_Location :=
                    End_Sloc (Subp_Spec.Sloc_Range);

               begin
                  Assert (Parameter_Idx > 1);

                  if Location >= Tail and then Location < Middle then
                     return (Before, Parameter_Idx);
                  elsif Location >= Middle and Location <= Trail then
                     return (After, Parameter_Idx);
                  end if;
               end;

            end if;

            Previous_Parameter := Parameter.As_Defining_Name;
            Param_Spec_Parameter_Idx := Param_Spec_Parameter_Idx + 1;
            Parameter_Idx := Parameter_Idx + 1;
            Is_First_Parameter := False;
         end loop;

         Previous_Param_Spec := Param_Spec.As_Param_Spec;
         Param_Spec_Idx := Param_Spec_Idx + 1;
         Is_First_Param_Spec := False;
      end loop Foo;

      raise Program_Error;
   end Get_Parameter_Relative_Position;

   ------------
   -- Create --
   ------------

   function Create
     (Unit          : Analysis_Unit;
      Location      : Source_Location;
      New_Parameter : Unbounded_String)
      return Parameter_Adder
   is
      Is_Defining_Id      : constant Boolean :=
        Validate_Syntax (New_Parameter, Defining_Id_Rule);
      Is_Defining_Id_List : constant Boolean :=
        Validate_Syntax (New_Parameter, Defining_Id_List_Rule);
      Is_Param_Spec       : constant Boolean :=
        Validate_Syntax (New_Parameter, Param_Spec_Rule);

      Is_Valid : constant Boolean :=
        Is_Defining_Id or else Is_Defining_Id_List or else Is_Param_Spec;

      Aux_Node : Ada_Node :=
        (if Location /= No_Source_Location
         and then Unit /= No_Analysis_Unit
         then
            Unit.Root.Lookup (Location)
         else
            No_Ada_Node);

      Subp_Spec : Libadalang.Analysis.Subp_Spec := No_Subp_Spec;

   begin
      if Aux_Node.Is_Null or not Is_Valid then
         raise Program_Error;
      end if;

      if not (Aux_Node.Kind in Ada_Basic_Decl
              and then (Aux_Node.Kind in Ada_Generic_Subp_Decl_Range
                        or else Aux_Node.As_Basic_Decl.P_Is_Subprogram))
      then
         while not Aux_Node.Is_Null loop

            exit when Aux_Node.Kind in Ada_Subp_Spec_Range;

            Aux_Node := Aux_Node.Parent;
         end loop;
      end if;

      if Aux_Node.Is_Null then
         raise Program_Error;
      end if;

      Subp_Spec := (if Aux_Node.Kind in Ada_Subp_Spec_Range then
                       Aux_Node.As_Subp_Spec
                    else
                       Get_Subp_Spec (Aux_Node.As_Basic_Decl).As_Subp_Spec);

      return Parameter_Adder'
        (Spec               => Subp_Spec,
         New_Parameter      => New_Parameter,
         Relative_Position  =>
           Get_Parameter_Relative_Position (Subp_Spec, Location),
         Full_Specification => Is_Param_Spec);
   end Create;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Parameter_Adder;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      Edits : Text_Edit_Map;

      generic
         with procedure Add_Parameter
           (Self   : Parameter_Adder;
            Target : Basic_Decl'Class;
            Edits  : in out Text_Edit_Map);
      procedure Add_Parameter_Callback (Relative_Subp : Basic_Decl'Class);
      --  Generic callback to add a parameter.
      --  Add_Parameter can be adjusted depending on where/how we're adding
      --  a parameter, for instance, if we're adding an identifier or a full
      --  parameter specification

      ----------------------------
      -- Add_Parameter_Callback --
      ----------------------------

      procedure Add_Parameter_Callback (Relative_Subp : Basic_Decl'Class)
      is
         Relative_Subp_Body : constant Base_Subp_Body :=
           Find_Subp_Body (Relative_Subp);

      begin
         if Relative_Subp.P_Is_Subprogram
           or else Relative_Subp.Kind in Ada_Generic_Subp_Decl_Range
         then
            Add_Parameter (Self, Relative_Subp, Edits);

            if not Relative_Subp_Body.Is_Null then
               Add_Parameter (Self, Relative_Subp_Body, Edits);
            end if;
         end if;
      end Add_Parameter_Callback;

      procedure Add_Full_Parameter_Specification_Callback is
        new Add_Parameter_Callback (Add_Full_Parameter_Specification);
      --  Callback to add a parameter fully specified

      procedure Add_Parameter_Defining_Id_Or_Ids_Callback is
        new Add_Parameter_Callback (Add_Parameter_Defining_Id_Or_Ids);
      --  Callback to add a parameter identifier or a list of identifiers

   begin
      if Self.Full_Specification then
         Find_Subp_Relatives
           (Subp           => Self.Spec.P_Parent_Basic_Decl,
            Units          => Analysis_Units.all,
            Decls_Callback =>
              Add_Full_Parameter_Specification_Callback'Access);
      else
         Find_Subp_Relatives
           (Subp           => Self.Spec.P_Parent_Basic_Decl,
            Units          => Analysis_Units.all,
            Decls_Callback =>
              Add_Parameter_Defining_Id_Or_Ids_Callback'Access);
      end if;

      return Refactoring_Edits'(Text_Edits => Edits, others => <>);

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message ("Add Parameter"));
         return No_Refactoring_Edits;
   end Refactor;

   --------------------------------------
   -- Add_Full_Parameter_Specification --
   --------------------------------------

   procedure Add_Full_Parameter_Specification
     (Self   : Parameter_Adder;
      Target : Basic_Decl'Class;
      Edits  : in out Text_Edit_Map)
   is
      function "+" (T : Text_Type) return Unbounded_String is
         (To_Unbounded_String (To_UTF8 (T)));

      Subp_Spec   : constant Libadalang.Analysis.Subp_Spec :=
        Get_Subp_Spec (Target).As_Subp_Spec;
      Subp_Params : constant Params :=
        (if Subp_Spec.Is_Null then No_Params else Subp_Spec.F_Subp_Params);

      Total_Param_Specs           : constant Natural :=
        Count_Subp_Param_Specs (Subp_Params);
      Total_Param_Spec_Parameters : Natural;

      Is_First_Param_Spec : Boolean := True;
      Is_Last_Param_Spec  : Boolean;

      Is_First_Parameter : Boolean := True;
      Is_Last_Parameter  : Boolean;

      Previous_Parameter  : Defining_Name := No_Defining_Name;

      Param_Spec_Idx           : Positive := 1;
      Param_Spec_Parameter_Idx : Positive := 1;
      Parameter_Idx  : Positive := 1;

      Aliased_Text   : Unbounded_String;
      Mode_Text      : Unbounded_String;
      Type_Expr_Text : Unbounded_String;
      Default_Text   : Unbounded_String;

   begin
      if Subp_Params.Is_Null then
         Safe_Insert
           (Edits     => Edits,
            File_Name => Target.Unit.Get_Filename,
            Edit      => Text_Edit'
              (Location =>
                 Make_Range
                   (End_Sloc (Subp_Spec.Sloc_Range),
                    End_Sloc (Subp_Spec.Sloc_Range)),
               Text     => " (" & Self.New_Parameter & ")"));
         return;
      end if;

      for Param_Spec of Subp_Params.F_Params loop

         Is_First_Parameter := True;
         Total_Param_Spec_Parameters :=
           Count_Param_Spec_Parameters (Param_Spec.As_Param_Spec);
         Param_Spec_Parameter_Idx := 1;
         Is_Last_Param_Spec :=
           Param_Spec_Parameter_Idx = Total_Param_Specs;

         for Parameter of Param_Spec.F_Ids loop
            Is_Last_Parameter :=
              Param_Spec_Parameter_Idx = Total_Param_Spec_Parameters;

            if Parameter_Idx = Self.Relative_Position.Index then
               if Is_First_Parameter
                 and then Is_Last_Parameter
                 and then Is_First_Param_Spec
                 and then Is_Last_Param_Spec
               then
                  case Self.Relative_Position.Side is
                     when Before =>
                        Safe_Insert
                          (Edits     => Edits,
                           File_Name => Target.Unit.Get_Filename,
                           Edit      => Text_Edit'
                             (Location =>
                                Make_Range
                                  (Start_Sloc (Parameter.Sloc_Range),
                                   Start_Sloc (Parameter.Sloc_Range)),
                              Text     => Self.New_Parameter & "; "));

                     when After =>
                        Safe_Insert
                          (Edits     => Edits,
                           File_Name => Target.Unit.Get_Filename,
                           Edit      => Text_Edit'
                             (Location =>
                                Make_Range
                                  (End_Sloc (Param_Spec.Sloc_Range),
                                   End_Sloc (Param_Spec.Sloc_Range)),
                              Text     => "; " & Self.New_Parameter));
                  end case;

                  return;

               elsif (Is_First_Parameter
                      and then Is_Last_Parameter
                      and then Is_First_Param_Spec
                      and then not Is_Last_Param_Spec)
                 or else (Is_First_Parameter
                          and then Is_Last_Parameter
                          and then not Is_First_Param_Spec
                          and then not Is_Last_Param_Spec)
                 or else (Is_First_Parameter
                          and then Is_Last_Parameter
                          and then not Is_First_Param_Spec
                          and then Is_Last_Param_Spec)
               then
                  case Self.Relative_Position.Side is
                     when Before =>
                        Safe_Insert
                          (Edits     => Edits,
                           File_Name => Target.Unit.Get_Filename,
                           Edit      => Text_Edit'
                             (Location =>
                                Make_Range
                                  (Start_Sloc (Parameter.Sloc_Range),
                                   Start_Sloc (Parameter.Sloc_Range)),
                              Text     => Self.New_Parameter & "; "));

                     when After =>
                        Safe_Insert
                          (Edits     => Edits,
                           File_Name => Target.Unit.Get_Filename,
                           Edit      => Text_Edit'
                             (Location =>
                                Make_Range
                                  (End_Sloc (Param_Spec.Sloc_Range),
                                   End_Sloc (Param_Spec.Sloc_Range)),
                              Text     => "; " & Self.New_Parameter));
                  end case;

                  return;

               elsif Is_First_Parameter
                 and then not Is_Last_Parameter
                 and then Is_First_Param_Spec
               then
                  Assert (Self.Relative_Position.Side = Before);

                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Target.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location =>
                          Make_Range
                            (Start_Sloc (Parameter.Sloc_Range),
                             Start_Sloc (Parameter.Sloc_Range)),
                        Text     => Self.New_Parameter & "; "));

                  return;

               elsif not Is_First_Parameter
                 and then Is_Last_Parameter
                 and then not Is_Last_Param_Spec
               then
                  case Self.Relative_Position.Side is
                     when Before =>
                        Aliased_Text :=
                          (if Param_Spec.F_Has_Aliased.Kind
                             in Ada_Aliased_Absent_Range
                           then Null_Unbounded_String
                           else +Param_Spec.F_Has_Aliased.Text & " ");
                        Mode_Text :=
                          (if Param_Spec.F_Mode.Kind
                             in Ada_Mode_Default_Range
                           then Null_Unbounded_String
                           else +Param_Spec.F_Mode.Text & " ");
                        Type_Expr_Text := +Param_Spec.F_Type_Expr.Text;
                        Default_Text :=
                          (if Param_Spec.F_Default_Expr.Is_Null
                           then Null_Unbounded_String
                           else " := " & (+Param_Spec.F_Default_Expr.Text));

                        Safe_Insert
                          (Edits     => Edits,
                           File_Name => Target.Unit.Get_Filename,
                           Edit      => Text_Edit'
                             (Location =>
                                Make_Range
                                  (End_Sloc (Previous_Parameter.Sloc_Range),
                                   Start_Sloc (Parameter.Sloc_Range)),
                              Text     =>
                                 " : " & Aliased_Text & Mode_Text
                                 & Type_Expr_Text & Default_Text & "; "));
                        Safe_Insert
                          (Edits     => Edits,
                           File_Name => Target.Unit.Get_Filename,
                           Edit      => Text_Edit'
                             (Location =>
                                Make_Range
                                  (Start_Sloc (Parameter.Sloc_Range),
                                   Start_Sloc (Parameter.Sloc_Range)),
                              Text     => Self.New_Parameter & "; "));

                     when After =>
                        Safe_Insert
                          (Edits     => Edits,
                           File_Name => Target.Unit.Get_Filename,
                           Edit      => Text_Edit'
                             (Location =>
                                Make_Range
                                  (End_Sloc (Param_Spec.Sloc_Range),
                                   End_Sloc (Param_Spec.Sloc_Range)),
                              Text     => "; " & Self.New_Parameter));
                  end case;

                  return;

               elsif Is_First_Parameter
                 and then not Is_Last_Parameter
                 and then not Is_First_Param_Spec
               then
                  Assert (Self.Relative_Position.Side = Before);

                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Target.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location =>
                          Make_Range
                            (Start_Sloc (Parameter.Sloc_Range),
                             Start_Sloc (Parameter.Sloc_Range)),
                        Text     => Self.New_Parameter & "; "));

                  return;

               elsif not Is_First_Parameter
                 and then not Is_Last_Parameter
               then
                  Assert (Self.Relative_Position.Side = Before);

                  Aliased_Text :=
                    (if Param_Spec.F_Has_Aliased.Kind
                       in Ada_Aliased_Absent_Range
                     then Null_Unbounded_String
                     else +Param_Spec.F_Has_Aliased.Text & " ");
                  Mode_Text :=
                    (if Param_Spec.F_Mode.Kind in Ada_Mode_Default_Range
                     then Null_Unbounded_String
                     else +Param_Spec.F_Mode.Text & " ");
                  Type_Expr_Text := +Param_Spec.F_Type_Expr.Text;
                  Default_Text :=
                    (if Param_Spec.F_Default_Expr.Is_Null
                     then Null_Unbounded_String
                     else " := " & (+Param_Spec.F_Default_Expr.Text));

                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Target.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location =>
                          Make_Range
                            (End_Sloc (Previous_Parameter.Sloc_Range),
                             Start_Sloc (Parameter.Sloc_Range)),
                        Text     => " : " & Aliased_Text & Mode_Text
                                    & Type_Expr_Text & Default_Text  & "; "));
                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Target.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location =>
                          Make_Range
                            (Start_Sloc (Parameter.Sloc_Range),
                             Start_Sloc (Parameter.Sloc_Range)),
                        Text     => Self.New_Parameter & "; "));

                  return;

               elsif not Is_First_Parameter
                 and then Is_Last_Parameter
                 and then Is_Last_Param_Spec
               then
                  case Self.Relative_Position.Side is
                     when Before =>
                        Aliased_Text :=
                          (if Param_Spec.F_Has_Aliased.Kind
                             in Ada_Aliased_Absent_Range
                           then Null_Unbounded_String
                           else +Param_Spec.F_Has_Aliased.Text & " ");
                        Mode_Text :=
                          (if Param_Spec.F_Mode.Kind
                             in Ada_Mode_Default_Range
                           then Null_Unbounded_String
                           else +Param_Spec.F_Mode.Text & " ");
                        Type_Expr_Text := +Param_Spec.F_Type_Expr.Text;
                        Default_Text :=
                          (if Param_Spec.F_Default_Expr.Is_Null then
                              Null_Unbounded_String
                           else " := " & (+Param_Spec.F_Default_Expr.Text));

                        Safe_Insert
                          (Edits     => Edits,
                           File_Name => Target.Unit.Get_Filename,
                           Edit      => Text_Edit'
                             (Location =>
                                Make_Range
                                  (End_Sloc (Previous_Parameter.Sloc_Range),
                                   Start_Sloc (Parameter.Sloc_Range)),
                              Text     => " : " & Aliased_Text & Mode_Text
                                 & Type_Expr_Text & Default_Text  & "; "));
                        Safe_Insert
                          (Edits     => Edits,
                           File_Name => Target.Unit.Get_Filename,
                           Edit      => Text_Edit'
                             (Location =>
                                Make_Range
                                  (Start_Sloc (Parameter.Sloc_Range),
                                   Start_Sloc (Parameter.Sloc_Range)),
                              Text     => Self.New_Parameter & "; "));

                     when After =>
                        Safe_Insert
                          (Edits     => Edits,
                           File_Name => Target.Unit.Get_Filename,
                           Edit      => Text_Edit'
                             (Location =>
                                Make_Range
                                  (End_Sloc (Param_Spec.Sloc_Range),
                                   End_Sloc (Param_Spec.Sloc_Range)),
                              Text     => "; " & Self.New_Parameter));
                  end case;

                  return;
               end if;

               raise Program_Error;
            end if;

            Previous_Parameter := Parameter.As_Defining_Name;
            Param_Spec_Parameter_Idx := Param_Spec_Parameter_Idx + 1;
            Parameter_Idx := Parameter_Idx + 1;
            Is_First_Parameter := False;
         end loop;

         Param_Spec_Idx := Param_Spec_Idx + 1;
         Is_First_Param_Spec := False;
         Is_Last_Param_Spec := Total_Param_Specs = Param_Spec_Idx;
      end loop;
   end Add_Full_Parameter_Specification;

   ------------------------------
   -- Add_Parameter_Identifier --
   ------------------------------

   procedure Add_Parameter_Defining_Id_Or_Ids
     (Self   : Parameter_Adder;
      Target : Basic_Decl'Class;
      Edits  : in out Text_Edit_Map)
   is
      Subp_Spec   : constant Libadalang.Analysis.Subp_Spec :=
        Get_Subp_Spec (Target).As_Subp_Spec;
      Subp_Params : constant Params :=
        (if Subp_Spec.Is_Null then No_Params else Subp_Spec.F_Subp_Params);

      Parameter_Idx  : Positive := 1;

   begin
      if Subp_Params.Is_Null then
         raise Assertion_Error with
           "Cannot add an identifier as a parameter of a subprogram "
           & "without parameters";
      end if;

      for Param_Spec of Subp_Params.F_Params loop
         for Parameter of Param_Spec.F_Ids loop
            if Parameter_Idx = Self.Relative_Position.Index then
               case Self.Relative_Position.Side is
                  when Before =>
                     Safe_Insert
                       (Edits     => Edits,
                        File_Name => Target.Unit.Get_Filename,
                        Edit      => Text_Edit'
                          (Location =>
                             Make_Range
                               (Start_Sloc (Parameter.Sloc_Range),
                                Start_Sloc (Parameter.Sloc_Range)),
                           Text     => Self.New_Parameter & ", "));

                  when After =>
                     Safe_Insert
                       (Edits     => Edits,
                        File_Name => Target.Unit.Get_Filename,
                        Edit      => Text_Edit'
                          (Location =>
                             Make_Range
                               (End_Sloc (Parameter.Sloc_Range),
                                End_Sloc (Parameter.Sloc_Range)),
                           Text     => ", " & Self.New_Parameter));
               end case;

               return;
            end if;

            Parameter_Idx := Parameter_Idx + 1;
         end loop;
      end loop;

      raise Assertion_Error with
        "Bug detected: Unreachable code. Failed to add an Identifier as a new "
        & "parameter due to not finding the target parameter index.";
   end Add_Parameter_Defining_Id_Or_Ids;

   ------------
   -- Create --
   ------------

   function Create
     (Target          : Basic_Decl;
      Parameter_Index : Natural;
      New_Mode        : Ada_Mode;
      Configuration   : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Mode_Changer
   is
      Parameter_Indices_Range : constant Parameter_Indices_Range_Type :=
        (Parameter_Index, Parameter_Index);
   begin
      return Mode_Changer'
        (Subp                    => Target,
         Parameter_Indices_Range => Parameter_Indices_Range,
         New_Mode                => New_Mode,
         Configuration           => Configuration);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Target                  : Basic_Decl;
      Parameter_Indices_Range : Parameter_Indices_Range_Type;
      New_Mode                : Ada_Mode;
      Configuration           : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Mode_Changer is
   begin
      return Mode_Changer'
        (Subp                    => Target,
         Parameter_Indices_Range => Parameter_Indices_Range,
         New_Mode                => New_Mode,
         Configuration           => Configuration);
   end Create;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self : Mode_Changer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits is
   begin
      return Refactoring_Edits'
        (Text_Edits =>
           Change_Mode
             (Self.Subp,
              Self.Parameter_Indices_Range,
              Self.New_Mode,
              Analysis_Units.all),
         others     => <>);

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message
              ("Change Parameter Mode"));
         return No_Refactoring_Edits;
   end Refactor;

   -----------------
   -- Change_Mode --
   -----------------

   procedure Change_Mode
     (Subp               : Basic_Decl'Class;
      Parameters_Indices : Parameter_Indices_Range_Type;
      New_Mode           : Ada_Mode;
      Edits              : in out Text_Edit_Map)
   is
      Current_Parameter_Index  : Positive := 1;
      First_Parameter_Index    : Positive :=
        Parameters_Indices.First;
      Last_Parameter_Index     : constant Positive :=
        Parameters_Indices.Last;
      N_Of_Parameters_Left     : Natural :=
        Last_Parameter_Index - First_Parameter_Index + 1;

      Param_Spec_Length               : Positive;
      Param_Spec_Last_Parameter_Index : Positive;

      New_Mode_Text : Unbounded_String;

   begin
      case New_Mode is
         when Ada_Mode_Default =>
            New_Mode_Text := To_Unbounded_String ("");

         when Ada_Mode_In =>
            New_Mode_Text := To_Unbounded_String ("in");

         when Ada_Mode_Out =>
            New_Mode_Text := To_Unbounded_String ("out");

         when Ada_Mode_In_Out =>
            New_Mode_Text := To_Unbounded_String ("in out");
      end case;

      for Param_Spec of Get_Subp_Params (Subp).F_Params loop

         exit when N_Of_Parameters_Left = 0;

         Param_Spec_Length := Length (Param_Spec.F_Ids);
         Param_Spec_Last_Parameter_Index :=
           Current_Parameter_Index + Param_Spec_Length - 1;

         if First_Parameter_Index = Current_Parameter_Index then
            if Param_Spec_Last_Parameter_Index < Last_Parameter_Index then
               --  Case 1: Checked

               Safe_Insert
                 (Edits    => Edits,
                  File_Name => Subp.Unit.Get_Filename,
                  Edit => Text_Edit'
                    (Location => Source_Location_Range'
                      (Start_Line   => Param_Spec.F_Mode.Sloc_Range.Start_Line,
                       End_Line     => Param_Spec.F_Mode.Sloc_Range.End_Line,
                       Start_Column =>
                         Param_Spec.F_Mode.Sloc_Range.Start_Column,
                       End_Column   =>
                         Param_Spec.F_Mode.Sloc_Range.End_Column),
                     Text     => New_Mode_Text));

               First_Parameter_Index := Param_Spec_Last_Parameter_Index + 1;
               N_Of_Parameters_Left :=
                 N_Of_Parameters_Left - Param_Spec_Length;

            elsif Param_Spec_Last_Parameter_Index = Last_Parameter_Index then
               --  Case 2: Checked

               if To_String (Param_Spec.F_Mode.Text) /= New_Mode_Text then
                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location   => Source_Location_Range'
                         (Start_Line   =>
                            Param_Spec.F_Mode.Sloc_Range.Start_Line,
                          End_Line     =>
                            Param_Spec.F_Mode.Sloc_Range.End_Line,
                          Start_Column =>
                            Param_Spec.F_Mode.Sloc_Range.Start_Column,
                          End_Column   =>
                            Param_Spec.F_Mode.Sloc_Range.End_Column),
                        Text       => (if New_Mode_Text = "" then
                                          Null_Unbounded_String
                                       else
                                          " " & New_Mode_Text)));
               end if;

               N_Of_Parameters_Left := 0;

               exit;

            else
               --  Case 3: Checked

               declare
                  Parameter_A  : Defining_Name := No_Defining_Name;
                  Parameter_B  : Defining_Name := No_Defining_Name;
                  Subtype_Text : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index = Last_Parameter_Index then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index =
                       Last_Parameter_Index + 1
                     then
                        Parameter_B := Parameter.As_Defining_Name;
                        exit;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location => Source_Location_Range'
                         (Start_Line   => Parameter_A.Sloc_Range.End_Line,
                          End_Line     => Parameter_B.Sloc_Range.Start_Line,
                          Start_Column => Parameter_A.Sloc_Range.End_Column,
                          End_Column   => Parameter_B.Sloc_Range.Start_Column),
                        Text     =>
                          " : " & New_Mode_Text & " " & Subtype_Text & ";"));
               end;

               N_Of_Parameters_Left := 0;

               exit;

            end if;

         elsif First_Parameter_Index in
           Current_Parameter_Index + 1 .. Param_Spec_Last_Parameter_Index
         then
            if First_Parameter_Index = Param_Spec_Last_Parameter_Index then
               --  Case 4: Checked

               declare
                  Parameter_A  : Defining_Name := No_Defining_Name;
                  Parameter_B  : Defining_Name := No_Defining_Name;

                  Parameter_B_Text : Unbounded_String;
                  Subtype_Text     : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index =
                       First_Parameter_Index - 1
                     then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index = First_Parameter_Index then
                        Parameter_B := Parameter.As_Defining_Name;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      => Text_Edit'(
                     Location  => Source_Location_Range'
                       (Start_Line   => Parameter_A.Sloc_Range.End_Line,
                        End_Line     => Parameter_B.Sloc_Range.End_Line,
                        Start_Column => Parameter_A.Sloc_Range.End_Column,
                        End_Column   => Parameter_B.Sloc_Range.End_Column),
                     Text     => To_Unbounded_String ("")));

                  Parameter_B_Text := To_Unbounded_String
                    (To_String (Parameter_B.Text));
                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location => Source_Location_Range'
                         (Start_Line   => Param_Spec.Sloc_Range.End_Line,
                          End_Line     => Param_Spec.Sloc_Range.End_Line,
                          Start_Column => Param_Spec.Sloc_Range.End_Column,
                          End_Column   => Param_Spec.Sloc_Range.End_Column),
                        Text     =>
                          "; " & Parameter_B_Text & " : " & New_Mode_Text
                        & " " & Subtype_Text));
               end;

               First_Parameter_Index := Current_Parameter_Index;
               N_Of_Parameters_Left := N_Of_Parameters_Left - 1;

            elsif Last_Parameter_Index > Param_Spec_Last_Parameter_Index then
               --  Case 5: Checked

               declare
                  Parameter_A  : Defining_Name := No_Defining_Name;
                  Parameter_B  : Defining_Name := No_Defining_Name;

                  Mode_Text       : constant String :=
                    To_String (Param_Spec.F_Mode.Text);
                  Subtype_Text     : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index =
                       First_Parameter_Index - 1
                     then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index = First_Parameter_Index then
                        Parameter_B := Parameter.As_Defining_Name;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  if To_String (Param_Spec.F_Mode.Text) /= New_Mode_Text then
                     Safe_Insert
                       (Edits     => Edits,
                        File_Name => Subp.Unit.Get_Filename,
                        Edit      =>
                          (Location => Param_Spec.F_Mode.Sloc_Range,
                           Text     => (if New_Mode_Text = "" then
                                          Null_Unbounded_String
                                       else
                                          " " & New_Mode_Text)));
                  end if;

                  Safe_Insert
                    (Edits    => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      =>
                       (Location => Source_Location_Range'
                         (Start_Line   => Parameter_A.Sloc_Range.End_Line,
                          End_Line     => Parameter_B.Sloc_Range.Start_Line,
                          Start_Column => Parameter_A.Sloc_Range.End_Column,
                          End_Column   => Parameter_B.Sloc_Range.Start_Column),
                        Text     =>
                          To_Unbounded_String
                            (" : " & Mode_Text & " " & Subtype_Text & "; ")));
               end;

               N_Of_Parameters_Left :=
                 N_Of_Parameters_Left -
                   ((Current_Parameter_Index - 1) - First_Parameter_Index);
               First_Parameter_Index := Current_Parameter_Index;

            elsif Last_Parameter_Index = Param_Spec_Last_Parameter_Index then
               --  Case 6: Checked

               declare
                  Parameter_A  : Defining_Name := No_Defining_Name;
                  Parameter_B  : Defining_Name := No_Defining_Name;

                  Mode_Text    : constant String :=
                    To_String (Param_Spec.F_Mode.Text);
                  Subtype_Text : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index =
                       First_Parameter_Index - 1
                     then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index = First_Parameter_Index then
                        Parameter_B := Parameter.As_Defining_Name;
                        exit;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  if To_String (Param_Spec.F_Mode.Text) /= New_Mode_Text then
                     Safe_Insert
                       (Edits     => Edits,
                        File_Name => Subp.Unit.Get_Filename,
                        Edit      => Text_Edit'
                          (Location => Param_Spec.F_Mode.Sloc_Range,
                           Text     => " " & New_Mode_Text));
                  end if;

                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      =>
                       Text_Edit'
                         (Location => Source_Location_Range'
                           (Start_Line   => Parameter_A.Sloc_Range.End_Line,
                            End_Line     => Parameter_B.Sloc_Range.Start_Line,
                            Start_Column => Parameter_A.Sloc_Range.End_Column,
                            End_Column   =>
                              Parameter_B.Sloc_Range.Start_Column),
                          Text     => To_Unbounded_String
                            (" : " & Mode_Text & " " & Subtype_Text & "; ")));
               end;

               N_Of_Parameters_Left := 0;

               exit;

            else
               --  Case 7: Checked

               Assert (Last_Parameter_Index < Param_Spec_Last_Parameter_Index);

               declare
                  Parameter_A  : Defining_Name := No_Defining_Name;
                  Parameter_B  : Defining_Name := No_Defining_Name;
                  Parameter_C  : Defining_Name := No_Defining_Name;
                  Parameter_D  : Defining_Name := No_Defining_Name;

                  Mode_Text    : constant String :=
                    To_String  (Param_Spec.F_Mode.Text);
                  Subtype_Text : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index =
                       First_Parameter_Index - 1
                     then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index = First_Parameter_Index then
                        Parameter_B := Parameter.As_Defining_Name;
                     end if;

                     if Current_Parameter_Index = Last_Parameter_Index
                     then
                        Parameter_C := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index =
                       Last_Parameter_Index + 1
                     then
                        Parameter_D := Parameter.As_Defining_Name;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  Safe_Insert
                    (Edits    => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit => (Location => Source_Location_Range'
                                  (Start_Line   =>
                                     Parameter_A.Sloc_Range.End_Line,
                                   End_Line     =>
                                     Parameter_B.Sloc_Range.Start_Line,
                                   Start_Column =>
                                     Parameter_A.Sloc_Range.End_Column,
                                   End_Column   =>
                                     Parameter_B.Sloc_Range.Start_Column),
                              Text     =>
                                To_Unbounded_String
                                  (" : " & Mode_Text & " " & Subtype_Text
                                   & "; ")));

                  Safe_Insert
                    (Edits    => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit => (Location => Source_Location_Range'
                                  (Start_Line   =>
                                     Parameter_C.Sloc_Range.End_Line,
                                   End_Line     =>
                                     Parameter_D.Sloc_Range.Start_Line,
                                   Start_Column =>
                                     Parameter_C.Sloc_Range.End_Column,
                                   End_Column   =>
                                     Parameter_D.Sloc_Range.Start_Column),
                              Text     =>
                                " : " & New_Mode_Text & " " & Subtype_Text
                              & "; "));
               end;

               N_Of_Parameters_Left := 0;

               exit;

            end if;
         end if;

         Current_Parameter_Index := Param_Spec_Last_Parameter_Index + 1;
      end loop;
   end Change_Mode;

   ------------
   -- Create --
   ------------

   function Create
     (Target          : Basic_Decl;
      Parameter_Index : Natural;
      Configuration   : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Backward_Mover
   is
   begin
      return Backward_Mover'
        (Subp            => Target,
         Parameter_Index => Parameter_Index,
         Configuration   => Configuration);
   end Create;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Backward_Mover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits is
   begin
      return Refactoring_Edits'
        (Text_Edits =>
           Move_Backward (Self.Subp, Self.Parameter_Index, Analysis_Units.all),
         others     => <>);

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message
              ("Move Parameter Backward"));
         return No_Refactoring_Edits;
   end Refactor;

   -------------------
   -- Move_Backward --
   -------------------

   procedure Move_Backward
     (Subp            : Basic_Decl'Class;
      Parameter_Index : Positive;
      Edits           : in out Text_Edit_Map)
   is
      Param_Spec_Length          : Positive;
      Current_Parameter_Index    : Positive := 1;
      Previous_Param_Spec        : Param_Spec := No_Param_Spec;
      Previous_Param_Spec_Length : Positive := 1;

   begin
      for Param_Spec of Get_Subp_Params (Subp).F_Params loop
         Param_Spec_Length := Length (Param_Spec.F_Ids);

         if Parameter_Index = Current_Parameter_Index
           and then Param_Spec_Length = 1
         then
            if Previous_Param_Spec_Length = 1 then
               --  Case 1:
               --
               --  Parameter B is the only one of the Param_Spec, and the
               --  previous parameter is also the only one of the
               --  Previous_Param_Spec.
               --
               --  A : in Integer; B : out Float
               --  --------------  -------------
               --
               --  B : out Float; A : in Integer
               --  +++++++++++++  ++++++++++++++

               Safe_Insert
                 (Edits     => Edits,
                  File_Name => Subp.Unit.Get_Filename,
                  Edit      => Text_Edit'
                    (Location => Param_Spec.Sloc_Range,
                     Text     =>
                       To_Unbounded_String
                         ((To_String (Previous_Param_Spec.Text)))));

               Safe_Insert
                 (Edits     => Edits,
                  File_Name => Subp.Unit.Get_Filename,
                  Edit      => Text_Edit'
                    (Location => Previous_Param_Spec.Sloc_Range,
                     Text     =>
                       To_Unbounded_String ((To_String (Param_Spec.Text)))));

            else
               --  Case 2:
               --
               --  Parameter C is the only one of the Param_Spec, and the
               --  previous parameter is not the only one on the
               --  Previous_Param_Spec
               --
               --  A, B : in Integer; C : out Float
               --   ---
               --
               --  A : in Integer; C : out Float; B : in Integer
               --                               ++++++++++++++++

               declare
                  Parameter_A : Defining_Name := No_Defining_Name;
                  Parameter_B : Defining_Name := No_Defining_Name;

                  Mode_Text : constant String :=
                    To_String (Previous_Param_Spec.F_Mode.Text);
                  Type_Text : constant String :=
                    To_String (Previous_Param_Spec.F_Type_Expr.Text);

                  New_Text : Unbounded_String;

                  Relative_Index : Positive := 1;

               begin
                  for Parameter of Previous_Param_Spec.F_Ids loop
                     if Relative_Index = Previous_Param_Spec_Length - 1 then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Relative_Index = Previous_Param_Spec_Length then
                        Parameter_B := Parameter.As_Defining_Name;
                     end if;

                     Relative_Index := Relative_Index + 1;
                  end loop;

                  New_Text := To_Unbounded_String
                    ("; "
                     & To_String (Parameter_B.Text)
                     & " : "
                     & Mode_Text
                     & " "
                     & Type_Text);

                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location => Source_Location_Range'
                       (Parameter_A.Sloc_Range.End_Line,
                        Parameter_B.Sloc_Range.End_Line,
                        Parameter_A.Sloc_Range.End_Column,
                        Parameter_B.Sloc_Range.End_Column),
                        Text     => Null_Unbounded_String));
                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location => Source_Location_Range'
                       (Param_Spec.Sloc_Range.End_Line,
                        Param_Spec.Sloc_Range.End_Line,
                        Param_Spec.Sloc_Range.End_Column,
                        Param_Spec.Sloc_Range.End_Column),
                        Text     => New_Text));
               end;
            end if;

            exit;

         elsif Parameter_Index = Current_Parameter_Index
           and then Param_Spec_Length > 1
         then
            if Previous_Param_Spec_Length = 1 then
               --  Case 3 :
               --
               --  Parameter B is the first one of Param_Spec, and the
               --  previous parameter is the only one on the previous
               --  Previous_Param_Spec.
               --
               --  A : in Integer; B, C : out Float
               --                  ---
               --
               --  B : out Float; A : in Integer; C : out Float
               --  +++++++++++++++
               --

               declare
                  Parameter_B : Defining_Name := No_Defining_Name;
                  Parameter_C : Defining_Name := No_Defining_Name;

                  Mode_Text : constant String :=
                    To_String (Param_Spec.F_Mode.Text);
                  Type_Text : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

                  New_Text : Unbounded_String;

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index = Parameter_Index then
                        Parameter_B := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index =
                       Parameter_Index + 1
                     then
                        Parameter_C := Parameter.As_Defining_Name;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  --  Assert that Parameter_B and Parameter_C have been found
                  --  and have changed.
                  Assert (Parameter_B /= No_Defining_Name
                          and then Parameter_C /= No_Defining_Name);

                  New_Text := To_Unbounded_String
                    (To_String (Parameter_B.Text)
                     & " : "
                     & Mode_Text
                     & " "
                     & Type_Text
                     & "; ");

                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location => Source_Location_Range'
                            (Parameter_B.Sloc_Range.Start_Line,
                             Parameter_C.Sloc_Range.Start_Line,
                             Parameter_B.Sloc_Range.Start_Column,
                             Parameter_C.Sloc_Range.Start_Column),
                        Text     => Null_Unbounded_String));
                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location => Source_Location_Range'
                            (Previous_Param_Spec.Sloc_Range.Start_Line,
                             Previous_Param_Spec.Sloc_Range.Start_Line,
                             Previous_Param_Spec.Sloc_Range.Start_Column,
                             Previous_Param_Spec.Sloc_Range.Start_Column),
                        Text     => New_Text));
               end;

            else
               --  Case 4
               --
               --  Parameter C is the first one of Param_Spec, and the
               --  previous parameter is not the only one of the previous
               --  Previous_Param_Spec.
               --
               --  A, B : in Integer; C, D : out Float
               --   --                ---
               --
               --  A : in Integer; C : out Float; B : in Integer; D : out Float
               --   ++++++++++++++++++++++++++++++
               --

               declare
                  Parameter_A : Defining_Name := No_Defining_Name;
                  Parameter_B : Defining_Name := No_Defining_Name;
                  Parameter_C : Defining_Name := No_Defining_Name;
                  Parameter_D : Defining_Name := No_Defining_Name;

                  First_Mode_Text : constant String :=
                    To_String (Previous_Param_Spec.F_Mode.Text);
                  First_Type_Text : constant String :=
                    To_String (Previous_Param_Spec.F_Type_Expr.Text);
                  Second_Mode_Text : constant String :=
                    To_String (Param_Spec.F_Mode.Text);
                  Second_Type_Text : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

                  New_Text : Unbounded_String;

                  Relative_Index : Positive := 1;

               begin
                  for Parameter of Previous_Param_Spec.F_Ids loop
                     if Relative_Index = Previous_Param_Spec_Length - 1 then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Relative_Index = Previous_Param_Spec_Length then
                        Parameter_B := Parameter.As_Defining_Name;
                     end if;

                     Relative_Index := Relative_Index + 1;
                  end loop;

                  Relative_Index := 1;

                  for Parameter of Param_Spec.F_Ids loop
                     if Relative_Index = 1 then
                        Parameter_C := Parameter.As_Defining_Name;

                     elsif Relative_Index = 2 then
                        Parameter_D := Parameter.As_Defining_Name;
                     end if;

                     Relative_Index := Relative_Index + 1;
                  end loop;

                  New_Text := To_Unbounded_String
                    (" : "
                     & First_Mode_Text
                     & " "
                     & First_Type_Text
                     & "; "
                     & To_String (Parameter_C.Text)
                     & " : "
                     & Second_Mode_Text
                     & " "
                     & Second_Type_Text
                     & "; ");

                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location => Source_Location_Range'
                            (Parameter_C.Sloc_Range.Start_Line,
                             Parameter_D.Sloc_Range.Start_Line,
                             Parameter_C.Sloc_Range.Start_Column,
                             Parameter_D.Sloc_Range.Start_Column),
                        Text     => Null_Unbounded_String));
                  Safe_Insert
                    (Edits     => Edits,
                     File_Name => Subp.Unit.Get_Filename,
                     Edit      => Text_Edit'
                       (Location => Source_Location_Range'
                            (Parameter_A.Sloc_Range.End_Line,
                             Parameter_B.Sloc_Range.Start_Line,
                             Parameter_A.Sloc_Range.End_Column,
                             Parameter_B.Sloc_Range.Start_Column),
                        Text     => New_Text));
               end;
            end if;

            exit;

         elsif Param_Spec_Length > 1
           and then Parameter_Index in
             Current_Parameter_Index + 1 ..
               Current_Parameter_Index + Param_Spec_Length - 1
         then
            --  Case 5
            --
            --  Parameter B is not the first one of Param_Spec.
            --
            --  A, B : in Integer
            --  -  -
            --
            --  B, A : in Integer
            --  +  +

            declare
               Parameter_A : Defining_Name := No_Defining_Name;
               Parameter_B : Defining_Name := No_Defining_Name;

            begin
               for Parameter of Param_Spec.F_Ids loop
                  if Current_Parameter_Index = Parameter_Index - 1 then
                     Parameter_A := Parameter.As_Defining_Name;

                  elsif Current_Parameter_Index = Parameter_Index then
                     Parameter_B := Parameter.As_Defining_Name;
                     exit;
                  end if;

                  Current_Parameter_Index := Current_Parameter_Index + 1;
               end loop;

               --  Assert that Parameter_A and Parameter_B have been found
               --  have changed.
               Assert (Parameter_A /= No_Defining_Name
                       and then Parameter_B /= No_Defining_Name);

               Safe_Insert
                 (Edits     => Edits,
                  File_Name => Subp.Unit.Get_Filename,
                  Edit      => Text_Edit'
                    (Location => Parameter_A.Sloc_Range,
                     Text     => To_Unbounded_String
                       (To_String (Parameter_B.Text))));
               Safe_Insert
                 (Edits     => Edits,
                  File_Name => Subp.Unit.Get_Filename,
                  Edit      => Text_Edit'
                    (Location => Parameter_B.Sloc_Range,
                     Text     => To_Unbounded_String
                       (To_String (Parameter_A.Text))));
            end;

            exit;

         else
            Current_Parameter_Index :=
              Current_Parameter_Index + Param_Spec_Length;
         end if;

         Previous_Param_Spec := Param_Spec.As_Param_Spec;
         Previous_Param_Spec_Length := Param_Spec_Length;
      end loop;
   end Move_Backward;

   ------------
   -- Create --
   ------------

   function Create
     (Target          : Basic_Decl;
      Parameter_Index : Natural;
      Configuration   : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Forward_Mover
   is (Forward_Mover'
         (Mover => Create (Target, Parameter_Index + 1, Configuration)));

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Forward_Mover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits is
   begin
      return Self.Mover.Refactor (Analysis_Units);

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message
              ("Move Parameter Forward"));
         return No_Refactoring_Edits;
   end Refactor;

end Laltools.Refactor.Subprogram_Signature;
