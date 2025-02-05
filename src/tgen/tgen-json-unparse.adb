------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
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

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps;      use Ada.Strings.Maps;

package body TGen.JSON.Unparse is

   procedure Remove_Trailing_Comma_And_Spaces (Text : in out Unbounded_String);

   function Unparse_Array
     (Sizes : JSON_Array; Val : JSON_Array) return Unbounded_String;

   function Unparse_Unconstrained_Array (Val : JSON_Value) return JSON_Value;

   function Unparse_Constrained_Array (Val : JSON_Value) return JSON_Value;

   function Unparse_Record (Val : JSON_Value) return Unbounded_String;

   function Unparse_Non_Discriminated_Record
     (Val : JSON_Value) return JSON_Value;

   function Unparse_Discriminated_Record (Val : JSON_Value) return JSON_Value;

   function Unparse_Quotient (Val : JSON_Value) return JSON_Value;

   --------------------------------------
   -- Remove_Trailing_Comma_And_Spaces --
   --------------------------------------

   procedure Remove_Trailing_Comma_And_Spaces (Text : in out Unbounded_String)
   is
   begin
      Trim (Text, Right);
      Trim (Text, Null_Set, To_Set (','));
   end Remove_Trailing_Comma_And_Spaces;

   -------------------
   -- Unparse_Array --
   -------------------

   function Unparse_Array
     (Sizes : JSON_Array; Val : JSON_Array) return Unbounded_String
   is
      type Nat_Array is array (Natural range <>) of Natural;

      function Pp_Arr
        (Current_Index : in out Positive; Sizes : Nat_Array)
         return Unbounded_String;
      --  Unflatten the generated array

      ------------
      -- Pp_Arr --
      ------------

      function Pp_Arr
        (Current_Index : in out Positive; Sizes : Nat_Array)
         return Unbounded_String
      is
         Unparsed_Value   : Unbounded_String;
         Current_Arr_Size : constant Natural := Sizes (Sizes'First);
      begin
         Append (Unparsed_Value, "(");

         --  Special cases for array of size 0 and array of size 1

         if Current_Arr_Size = 0 then

            --  Print an empty aggregate

            Append (Unparsed_Value, "others => <>");

         elsif Current_Arr_Size = 1 then

            --  Print an aggregate with the others keyword as we can't
            --  have an array declared e.g. (1) but (others => 1), or
            --  (<index> => 1) is allowed. As we don't have the index
            --  here, pick the former.

            Append (Unparsed_Value, "others => ");
         end if;

         for I in 1 .. Current_Arr_Size loop

            --  We have reached the last index type: unparse the component
            --  value

            if Sizes'Length = 1 then
               --  An array component can't be of an unconstrained type.

               Append
                 (Unparsed_Value,
                  UTF8_String'
                    (Unparse (Get (Val, Current_Index)).Get ("value")));
               Current_Index := @ + 1;
            else
               --  Otherwise, generate the nested array recursively

               Append
                 (Unparsed_Value,
                  Pp_Arr
                    (Current_Index, Sizes (Sizes'First + 1 .. Sizes'Last)));
            end if;
            Append (Unparsed_Value, ", ");
         end loop;
         Trim (Unparsed_Value, Right);
         Trim (Unparsed_Value, Null_Set, To_Set (','));
         Append (Unparsed_Value, ")");
         return Unparsed_Value;
      end Pp_Arr;

      Sizes_Arr   : Nat_Array (1 .. Length (Sizes));
      Dummy_Index : Positive := 1;
   begin

      --  Start by getting all of the sizes

      for I in Sizes_Arr'Range loop
         Sizes_Arr (I) := Natural'Value (Get (Array_Element (Sizes, I)));
      end loop;

      return Pp_Arr (Dummy_Index, Sizes_Arr);
   end Unparse_Array;

   ---------------------------------
   -- Unparse_Unconstrained_Array --
   ---------------------------------

   function Unparse_Unconstrained_Array (Val : JSON_Value) return JSON_Value is
      Result      : constant JSON_Value := Create_Object;
      Constraints : Unbounded_String;
      Dimensions  : constant JSON_Array := Val.Get ("dimensions");
   begin
      for Dimension of Dimensions loop
         Append (Constraints, "(");
         Append (Constraints, UTF8_String'(Dimension.Get ("First")));
         Append (Constraints, " .. ");
         Append (Constraints, UTF8_String'(Dimension.Get ("Last")));
         Append (Constraints, ") ");
      end loop;
      Trim (Constraints, Left);
      Set_Field (Result, "constraints", Constraints);

      Set_Field
        (Result,
         "value",
         Unparse_Array (Val.Get ("sizes"), Val.Get ("array")));
      return Result;
   end Unparse_Unconstrained_Array;

   -------------------------------
   -- Unparse_Constrained_Array --
   -------------------------------

   function Unparse_Constrained_Array (Val : JSON_Value) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
   begin
      Set_Field
        (Result,
         "value",
         Unparse_Array (Val.Get ("sizes"), Val.Get ("array")));
      return Result;
   end Unparse_Constrained_Array;

   --------------------
   -- Unparse_Record --
   --------------------

   function Unparse_Record (Val : JSON_Value) return Unbounded_String is
      Unparsed_Value : Unbounded_String;

      procedure Process_Component (Name : UTF8_String; Value : JSON_Value);

      procedure Process_Component (Name : UTF8_String; Value : JSON_Value) is
      begin
         Append (Unparsed_Value, Name);
         Append (Unparsed_Value, " => ");

         --  A record component can't be of an unconstrained type

         Append (Unparsed_Value, UTF8_String'(Get (Unparse (Value), "value")));
         Append (Unparsed_Value, ", ");
      end Process_Component;

   begin
      Append (Unparsed_Value, "(");
      Map_JSON_Object (Val, Process_Component'Access);

      --  If this is a null record, explicitly generate an empty aggregate

      if Length (Unparsed_Value) = 1 then
         Append (Unparsed_Value, "others => <>");

      else
         --  Otherwise, we have to trim the resulting value

         Remove_Trailing_Comma_And_Spaces (Unparsed_Value);
      end if;
      Append (Unparsed_Value, ")");
      return Unparsed_Value;
   end Unparse_Record;

   --------------------------------------
   -- Unparse_Non_Discriminated_Record --
   --------------------------------------

   function Unparse_Non_Discriminated_Record
     (Val : JSON_Value) return JSON_Value
   is
      Result : constant JSON_Value := Create_Object;
   begin
      Set_Field (Result, "value", Unparse_Record (Get (Val, "components")));
      return Result;
   end Unparse_Non_Discriminated_Record;

   ----------------------------------
   -- Unparse_Discriminated_Record --
   ----------------------------------

   function Unparse_Discriminated_Record (Val : JSON_Value) return JSON_Value
   is
      Result      : constant JSON_Value := Create_Object;
      Constraints : Unbounded_String;
      Components  : constant Unbounded_String :=
        Unparse_Record (Get (Val, "components"));

      procedure Process_Discr (Name : UTF8_String; Value : JSON_Value);

      -------------------
      -- Process_Discr --
      -------------------

      procedure Process_Discr (Name : UTF8_String; Value : JSON_Value) is
      begin
         Append (Constraints, Name);
         Append (Constraints, " => ");

         --  A record discriminant can't be of an unconstrained type

         Append (Constraints, UTF8_String'(Unparse (Value).Get ("value")));
         Append (Constraints, ", ");
      end Process_Discr;
   begin
      --  Deal with the discriminants

      Append (Constraints, "(");
      Map_JSON_Object (Get (Val, "discriminants"), Process_Discr'Access);
      Remove_Trailing_Comma_And_Spaces (Constraints);
      Append (Constraints, ")");
      Set_Field (Result, "constraints", Constraints);

      --  Also add the discriminant values to the aggregate expression, as it
      --  is required to put the discriminant value in a discriminated record
      --  aggregate expression.

      Set_Field
        (Result,
         "value",
         Slice (Constraints, 1, Length (Constraints) - 1)
         & ", "
         & Slice (Components, 2, Length (Components)));
      return Result;
   end Unparse_Discriminated_Record;

   ----------------------
   -- Unparse_Quotient --
   ----------------------

   function Unparse_Quotient (Val : JSON_Value) return JSON_Value is
      Result          : constant JSON_Value := Create_Object;
      Quotient_String : constant Unbounded_String := Get (Val, "value");
      Div_Index       : constant Natural :=
        Index (Quotient_String, To_Set ('/'));
   begin
      --  A quotient string is e.g. "2 / 4"

      Set_Field
        (Result,
         "value",
         Slice (Quotient_String, 1, Div_Index - 2)
         & ".0 "
         & Slice (Quotient_String, Div_Index, Length (Quotient_String))
         & ".0");
      return Result;
   end Unparse_Quotient;

   -------------
   -- Unparse --
   -------------

   function Unparse (Val : JSON_Value) return JSON_Value is
      Result  : constant JSON_Value := Create_Object;
      Val_Str : constant String := Val.Write;
      pragma Unreferenced (Val_Str);
   begin
      --  Dispatch on the right unparsing function

      case Kind (Val) is

         when JSON_Object_Type =>

            if Has_Field (Val, "discriminants") then

               --  Discriminated record case

               return Unparse_Discriminated_Record (Val);

            elsif Has_Field (Val, "components") then

               --  Non discriminated record case

               return Unparse_Non_Discriminated_Record (Val);

            elsif Has_Field (Val, "dimensions") then

               --  Unconstrained array case

               return Unparse_Unconstrained_Array (Val);

            elsif Has_Field (Val, "array") then

               --  Constrained array case

               return Unparse_Constrained_Array (Val);

            elsif Has_Field (Val, "quotient") then

               --  Unparse floating point / fixed point value stored in a
               --  quotient string.

               return Unparse_Quotient (Val);

            else
               --  Defensive code

               raise Program_Error with "Unknown value representation";
            end if;

         when others =>
            Set_Field (Result, "value", Val);
            return Result;
      end case;
   end Unparse;

end TGen.JSON.Unparse;
