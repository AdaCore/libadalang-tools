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
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;

with Laltools.Subprogram_Hierarchy; use Laltools.Subprogram_Hierarchy;

package body Laltools.Refactor.Subprogram_Signature is

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function Generic_Array_Unique (Container : Array_Type) return Array_Type;
   --  Returns a sorted Array_Type with the unique elements of 'Container'

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

   type Extended_Argument_Indicies_Type is
     array (Positive range <>) of Natural;

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
     (Call              : Call_Expr;
      Parameter_Indices : Parameter_Indices_Type)
      return Source_Location_Range_Set
     with Pre => not Call.Is_Null and then Parameter_Indices'Length > 0;
   --  Returns a set of source location ranges of the arguments associated to
   --  'Parameter_Indices'.
   --  Duplicate values of 'Parameter_Indices' are ignored.
   --  And Assertion_Error exception is raised if 'Parameter_Indices' contains
   --  an element that is greater than the number of arguments 'Call' has.

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

   procedure Add_As_First_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Text_Edit_Map)
     with Pre => Is_Subprogram (Subp);
   --  Adds a parameter defined by 'Data' as the first parameter

   procedure Add_As_Last_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Text_Edit_Map)
     with Pre => Is_Subprogram (Subp);
   --  Adds a parameter defined by 'Data' as the last parameter

   procedure Add_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Index : Positive;
      Edits : in out Text_Edit_Map)
     with Pre => Is_Subprogram (Subp);
   --  Adds a parameter defined by 'Data' to position defined by 'Index'

   procedure Add_To_Empty_Params
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Text_Edit_Map)
     with Pre => Is_Subprogram (Subp);
   --  Adds a parameter defined by 'Data' as the first parameter of a 'Subp'
   --  that has no parameters

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

   function Map_Parameters_To_Arguments
     (Parameters : Params'Class;
      Call       : Call_Expr'Class)
      return Extended_Argument_Indicies_Type;
   --  Maps the index of each parameter of 'Parameters' to the actual parameter
   --  on 'Call'. This function assumes that both 'Parameters' and 'Call' refer
   --  to the same subprogram.
   --  The indices of the returned array represent the parameteres, and the
   --  the values represent the index of the corresponding actual parameter on
   --  subprogram call. A value of 0 means that there is no correspondent
   --  actual parameter (for instance, the paramter has a default value).

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

   function Params_SLOC
     (Subp : Basic_Decl'Class)
      return Source_Location_Range
   is (Get_Subp_Params (Subp).Sloc_Range)
     with Pre => Is_Subprogram (Subp);
   --  If 'Subp' has a Params node, then returns its source location range.
   --  Otherwise returns No_Source_Location_Range.

   function Parameters_SLOC
     (Subp                     : Basic_Decl'Class;
      Parameter_Indices_Ranges : Parameter_Indices_Ranges_Type)
      return Source_Location_Range_Set
     with Pre => Is_Subprogram (Subp)
                 and then Parameter_Indices_Ranges'Length > 0;
   --  Returns a set with the source location range of the parameters with
   --  indices given by 'Parameter_Indices_Ranges'.

   function To_Unique_Ranges
     (Parameter_Indices : Parameter_Indices_Type)
      return Parameter_Indices_Ranges_Type;
   --  Creates an array of ranges based on 'Parameter_Indices' values.
   --  Duplicate values in 'Parameter_Indices' are ignored.
   --  Example: If 'Parameter_Indices' is [1, 3, 5, 6], the returned array is
   --  [{1, 1}, {3, 3}, {5, 6}].

   function Unique is new Generic_Array_Unique
     (Index_Type   => Positive,
      Element_Type => Positive,
      Array_Type   => Parameter_Indices_Type,
      "<"          => "<");
   --  Sorts and removes duplicates of a Parameter_Indices_Type

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

   -----------
   -- Image --
   -----------

   function Image (Data : Parameter_Data_Type) return Unbounded_String is
      S : Unbounded_String;
   begin
      Append (S, Data.Name);
      Append (S, " : ");

      if Data.Mode /= Null_Unbounded_String then
         Append (S, Data.Mode & " ");
      end if;

      Append (S, Data.Type_Indication);

      if Data.Default_Expr /= Null_Unbounded_String then
         Append (S, " := " & Data.Default_Expr);
      end if;

      return S;
   end Image;

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
         return (1 .. 0 => <>);
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
     (Node            : Ada_Node'Class;
      Subp            : out Basic_Decl;
      Parameter_Index : out Positive;
      Requires_Type   : out Boolean)
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
         Requires_Type := False;
      end Initialize_Out_Parameters;

      Parent_Defining_Name       : Defining_Name := No_Defining_Name;
      Parent_Defining_Name_List  : Defining_Name_List := No_Defining_Name_List;
      Parent_Params              : Params := No_Params;
      Parent_Subp_Decl           : Basic_Decl := No_Basic_Decl;

      Parameter_Absolute_Index : Positive;

      Total_Parameters : Natural := 0;

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

            when Ada_Params =>
               Parent_Params := Aux_Node.As_Params;

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

      if Parent_Params /= No_Params
        and then Parent_Defining_Name_List /= No_Defining_Name_List
        and then Parent_Defining_Name /= No_Defining_Name
      then
         Parameter_Absolute_Index :=
           Get_Parameter_Absolute_Index (Parent_Defining_Name);

         Subp := Parent_Subp_Decl;
         Parameter_Index := Parameter_Absolute_Index + 1;
         Requires_Type := False;

         return True;

      elsif Parent_Params = No_Params then
         Subp := Parent_Subp_Decl;
         Parameter_Index := 1;

         Parent_Params := Get_Subp_Params (Parent_Subp_Decl);

         if not Parent_Params.Is_Null then
            Total_Parameters :=
              Count_Subp_Parameters (Parent_Params);
         end if;

         if Total_Parameters = 0 then
            Requires_Type := True;
         else
            Requires_Type := False;
         end if;

         return True;
      end if;

      return False;
   exception
      when Precondition_Failure =>
         --  Assume that Precondition_Failure is raised due to invalid code and
         --  set again the out parameters since they might have been changed
         --  since initialized.
         Initialize_Out_Parameters;
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
           (Ada_Mode_Default, Ada_Mode_Default, Ada_Mode_Default);
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
      when Precondition_Failure =>
         --  Assume that Precondition_Failure is raised due to invalid code and
         --  set again the out parameters since they might have been changed
         --  since initialized.
         Initialize_Out_Parameters;
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
         Move_Directions := (False, False);
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
      when Precondition_Failure =>
         --  Assume that Precondition_Failure is raised due to invalid code and
         --  set again the out parameters since they might have been changed
         --  since initialized.
         Initialize_Out_Parameters;
         return False;
   end Is_Move_Parameter_Available;

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

   -------------------
   -- Add_Parameter --
   -------------------

   function Add_Parameter
     (Subp            : Basic_Decl;
      New_Parameter   : Parameter_Data_Type;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Text_Edit_Map
   is
      Target_Params : constant Params := Get_Subp_Params (Subp);
      Params_Length : constant Natural :=
        (if Target_Params.Is_Null or else Target_Params = No_Params
         then 0
         else Length (Target_Params.F_Params));

      Edits : Text_Edit_Map;

      procedure Add_Parameter_Callback (Relative_Subp : Basic_Decl'Class);
      --  Determines the necessary changes to 'Relative_Subp' specification,
      --  and adds them to 'Edits'.

      ----------------------------
      -- Add_Parameter_Callback --
      ----------------------------

      procedure Add_Parameter_Callback (Relative_Subp : Basic_Decl'Class) is
         Relative_Subp_Body : constant Base_Subp_Body :=
           Find_Subp_Body (Relative_Subp);

      begin
         if Params_Length = 0 then
            if Is_Subprogram (Relative_Subp) then
               Add_To_Empty_Params
                 (Relative_Subp, New_Parameter, Edits);

               if not Relative_Subp_Body.Is_Null then
                  Add_To_Empty_Params
                    (Relative_Subp_Body, New_Parameter, Edits);
               end if;
            end if;

         elsif Parameter_Index = 1 then
            if Is_Subprogram (Relative_Subp) then
               Add_As_First_Parameter
                 (Relative_Subp, New_Parameter, Edits);

               if not Relative_Subp_Body.Is_Null then
                  Add_As_First_Parameter
                    (Relative_Subp_Body, New_Parameter, Edits);
               end if;
            end if;

         elsif Parameter_Index = Params_Length + 1 then
            if Is_Subprogram (Subp) then
               Add_As_Last_Parameter
                 (Relative_Subp, New_Parameter, Edits);

               if not Relative_Subp_Body.Is_Null then
                  Add_As_Last_Parameter
                    (Relative_Subp_Body, New_Parameter, Edits);
               end if;
            end if;

         else
            Assert (Parameter_Index in 2 .. Params_Length);

            if Is_Subprogram (Relative_Subp)
            then
               Add_Parameter
                 (Relative_Subp,
                  New_Parameter,
                  Parameter_Index,
                  Edits);

               if not Relative_Subp_Body.Is_Null then
                  Add_Parameter
                    (Relative_Subp_Body,
                     New_Parameter,
                     Parameter_Index,
                     Edits);
               end if;
            end if;
         end if;
      end Add_Parameter_Callback;

   begin
      Find_Subp_Relatives
        (Subp           => Subp,
         Units          => Units,
         Decls_Callback => Add_Parameter_Callback'Access);

      return Edits;
   end Add_Parameter;

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
        Parameter_Indices => (1 => Parameter_Index),
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
        (others => 0);

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
              Parameter_Indices_Ranges => (1 => Self.Parameter_Indices_Range),
              Units                    => Analysis_Units.all),
         others     => <>);
   end Refactor;

   ------------
   -- Create --
   ------------

   function Create
     (Target         : Basic_Decl;
      New_Parameter  : Parameter_Data_Type;
      Index          : Positive;
      Configuration  : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Parameter_Adder is
   begin
      return Parameter_Adder'
        (Subp            => Target,
         New_Parameter   => New_Parameter,
         Parameter_Index => Index,
         Configuration   => Configuration);
   end Create;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Parameter_Adder;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits is
   begin
      return Refactoring_Edits'
        (Text_Edits =>
           Add_Parameter
             (Self.Subp,
              Self.New_Parameter,
              Self.Parameter_Index,
              Analysis_Units.all),
         others     => <>);
   end Refactor;

   -------------------------
   -- Add_To_Empty_Params --
   -------------------------

   procedure Add_To_Empty_Params
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Text_Edit_Map)
   is
      Definition_Sloc : constant Source_Location_Range :=
        Subp.P_Defining_Name.Sloc_Range;

   begin
      Safe_Insert
        (Edits     => Edits,
         File_Name => Subp.Unit.Get_Filename,
         Edit      =>
           (Location => Source_Location_Range'
             (Start_Line   => Definition_Sloc.End_Line,
              End_Line     => Definition_Sloc.End_Line,
              Start_Column => Definition_Sloc.End_Column,
              End_Column   => Definition_Sloc.End_Column),
            Text     => " (" & Image (Data) & ")"));
   end Add_To_Empty_Params;

   ----------------------------
   -- Add_As_First_Parameter --
   ----------------------------

   procedure Add_As_First_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Text_Edit_Map)
   is
      Target_Params_List_Sloc : constant Source_Location_Range :=
        Get_Subp_Params (Subp).F_Params.Sloc_Range;
   begin
      if Data.Type_Indication /= Null_Unbounded_String then
         Safe_Insert
           (Edits     => Edits,
            File_Name => Subp.Unit.Get_Filename,
            Edit      =>
              (Location  => Source_Location_Range'
                (Start_Line   => Target_Params_List_Sloc.Start_Line,
                 End_Line     => Target_Params_List_Sloc.Start_Line,
                 Start_Column => Target_Params_List_Sloc.Start_Column,
                 End_Column   => Target_Params_List_Sloc.Start_Column),
               Text     => Image (Data) & "; "));
      end if;
   end Add_As_First_Parameter;

   ---------------------------
   -- Add_As_Last_Parameter --
   ---------------------------

   procedure Add_As_Last_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Text_Edit_Map)
   is
      Target_Params_List_Sloc : constant Source_Location_Range :=
        Get_Subp_Params (Subp).F_Params.Sloc_Range;

   begin
      Safe_Insert
        (Edits     => Edits,
         File_Name => Subp.Unit.Get_Filename,
         Edit      => Text_Edit'
           (Location => Source_Location_Range'
             (Start_Line   => Target_Params_List_Sloc.End_Line,
              End_Line     => Target_Params_List_Sloc.End_Line,
              Start_Column => Target_Params_List_Sloc.End_Column,
              End_Column   => Target_Params_List_Sloc.End_Column),
            Text     => "; " & Image (Data)));
   end Add_As_Last_Parameter;

   -------------------
   -- Add_Parameter --
   -------------------

   procedure Add_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Index : Positive;
      Edits : in out Text_Edit_Map)
   is
      Current_Parameter_Index : Positive := 1;
      Param_Spec_Ids_Length : Positive;

   begin
      for Param_Spec of Get_Subp_Params (Subp).F_Params loop
         Param_Spec_Ids_Length := Length (Param_Spec.F_Ids);

         if Index = Current_Parameter_Index then
            Safe_Insert
              (Edits     => Edits,
               File_Name => Subp.Unit.Get_Filename,
               Edit      => Text_Edit'
                 (Location => Source_Location_Range'
                   (Start_Line   => Param_Spec.Sloc_Range.Start_Line,
                    End_Line     => Param_Spec.Sloc_Range.Start_Line,
                    Start_Column => Param_Spec.Sloc_Range.Start_Column,
                    End_Column   => Param_Spec.Sloc_Range.Start_Column),
                  Text     => Image (Data) & "; "));
            exit;

         elsif Index in
           Current_Parameter_Index + 1 ..
             Current_Parameter_Index + Param_Spec_Ids_Length - 1
         then
            declare
               Last_Parameter        : Defining_Name :=
                 No_Defining_Name;
               Before_Last_Parameter : Defining_Name :=
                 No_Defining_Name;

               Mode_Text    : constant String :=
                 To_String (Param_Spec.F_Mode.Text);
               Subtype_Text : constant String :=
                 To_String (Param_Spec.F_Type_Expr.Text);
            begin
               for Param of Param_Spec.F_Ids loop
                  if Current_Parameter_Index = Index - 1 then
                     Before_Last_Parameter := Param.As_Defining_Name;

                  elsif Current_Parameter_Index = Index then
                     Last_Parameter := Param.As_Defining_Name;
                     exit;
                  end if;

                  Current_Parameter_Index := Current_Parameter_Index + 1;
               end loop;

               Safe_Insert
                 (Edits     => Edits,
                  File_Name => Subp.Unit.Get_Filename,
                  Edit      => Text_Edit'
                    (Location =>
                      (Before_Last_Parameter.Sloc_Range.End_Line,
                       Last_Parameter.Sloc_Range.Start_Line,
                       Before_Last_Parameter.Sloc_Range.End_Column,
                       Last_Parameter.Sloc_Range.Start_Column),
                     Text     =>
                       To_Unbounded_String
                         (" : " & Mode_Text & " " & Subtype_Text & "; ")
                          & Image (Data) & "; "));
            end;
            exit;

         end if;

         Current_Parameter_Index :=
           Current_Parameter_Index + Param_Spec_Ids_Length;
      end loop;
   end Add_Parameter;

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
      return Refactoring_Edits
   is (Self.Mover.Refactor (Analysis_Units));

end Laltools.Refactor.Subprogram_Signature;
