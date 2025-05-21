------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                    Copyright (C) 2022-2025, AdaCore                      --
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

with TGen.Big_Reals;     use TGen.Big_Reals;
with TGen.Big_Reals_Aux; use TGen.Big_Reals_Aux;

package body TGen.Marshalling_Lib.JSON is

   ------------------------------
   -- Read_Write_Discrete_JSON --
   ------------------------------

   package body Read_Write_Discrete_JSON is

      -----------
      -- Write --
      -----------

      procedure Write (JSON : in out TGen.JSON.JSON_Value; V : T) is
      begin
         JSON := Create (T'Image (V));
      end Write;

      ----------
      -- Read --
      ----------

      procedure Read (JSON : TGen.JSON.JSON_Value; V : out T) is
      begin
         V := T'Value (Get (JSON));
      end Read;

   end Read_Write_Discrete_JSON;

   -----------------------------------
   -- Read_Write_Decimal_Fixed_JSON --
   -----------------------------------

   package body Read_Write_Decimal_Fixed_JSON is

      package T_Conversions is new Decimal_Fixed_Conversions (Num => T);
      --  To avoid the loss of precision, we encode the fixed point as a
      --  Big_Real and then represent it as a fraction.

      -----------
      -- Write --
      -----------

      pragma Warnings (Off, "formal parameter * is read but never assigned");
      procedure Write (JSON : in out TGen.JSON.JSON_Value; V : T) is
         V_Big_Real : constant TGen.Big_Reals.Big_Real :=
           T_Conversions.To_Big_Real (V);
      begin
         Set_Field (JSON, "quotient", True);
         Set_Field (JSON, "value", To_Quotient_String (V_Big_Real));
      end Write;
      pragma Warnings (On, "formal parameter * is read but never assigned");

      ----------
      -- Read --
      ----------

      procedure Read (JSON : TGen.JSON.JSON_Value; V : out T) is
         Value : constant JSON_Value := Get (JSON, "value");
      begin
         --  Decode the big real from the string encoded as a quotient string.

         V :=
           T_Conversions.From_Big_Real
             (Big_Reals.From_Quotient_String (Get (Value)));
      end Read;

   end Read_Write_Decimal_Fixed_JSON;

   ------------------------------------
   -- Read_Write_Ordinary_Fixed_JSON --
   ------------------------------------

   package body Read_Write_Ordinary_Fixed_JSON is

      package T_Conversions is new TGen.Big_Reals.Fixed_Conversions (Num => T);
      --  To avoid the loss of precision, we encode the fixed point as a
      --  Big_Real and then represent it as a fraction.

      -----------
      -- Write --
      -----------

      pragma Warnings (Off, "formal parameter * is read but never assigned");
      procedure Write (JSON : in out TGen.JSON.JSON_Value; V : T) is
         V_Big_Real : constant TGen.Big_Reals.Big_Real :=
           T_Conversions.To_Big_Real (V);
      begin
         Set_Field (JSON, "quotient", True);
         Set_Field (JSON, "value", To_Quotient_String (V_Big_Real));
      end Write;
      pragma Warnings (On, "formal parameter * is read but never assigned");

      ----------
      -- Read --
      ----------

      procedure Read (JSON : TGen.JSON.JSON_Value; V : out T) is
      begin
         --  Decode the big real from the string encoded as a quotient string

         V :=
           T_Conversions.From_Big_Real
             (Big_Reals.From_Quotient_String (Get (JSON, "value")));
      end Read;

   end Read_Write_Ordinary_Fixed_JSON;

   ---------------------------
   -- Read_Write_Float_JSON --
   ---------------------------

   package body Read_Write_Float_JSON is

      package T_Conversions is new TGen.Big_Reals.Float_Conversions (Num => T);
      --  To avoid the loss of precision, we need to encode the float as a
      --  Big_Real and then represent it as a fraction.

      -----------
      -- Write --
      -----------

      pragma Warnings (Off, "formal parameter * is read but never assigned");
      procedure Write (JSON : in out TGen.JSON.JSON_Value; V : T) is
         V_Big_Real : constant TGen.Big_Reals.Big_Real :=
           T_Conversions.To_Big_Real (V);
      begin
         Set_Field (JSON, "quotient", True);
         Set_Field (JSON, "value", To_Quotient_String (V_Big_Real));
      end Write;
      pragma Warnings (On, "formal parameter * is read but never assigned");

      ----------
      -- Read --
      ----------

      procedure Read (JSON : TGen.JSON.JSON_Value; V : out T) is
         Is_Quotient : constant Boolean := Get (JSON, "quotient");
      begin
         if Is_Quotient then
            --  Decode the big real from the string encoded as a quotient
            --  string
            V :=
              T_Conversions.From_Big_Real
                (Big_Reals.From_Quotient_String (Get (JSON, "value")));
         else
            V :=
              T_Conversions.From_Big_Real
                (Big_Reals.From_String (Get (JSON, "value")));
         end if;
      end Read;

   end Read_Write_Float_JSON;

   -----------------
   -- In_Out_JSON --
   -----------------

   package body In_Out_JSON is

      -----------
      -- Input --
      -----------

      function Input (JSON : TGen.JSON.JSON_Value) return T is
      begin
         return V : T do
            Read (JSON, V);
         end return;
      end Input;

      ------------
      -- Output --
      ------------

      function Output (V : T) return TGen.JSON.JSON_Value is
         JSON : TGen.JSON.JSON_Value := Create_Object;
      begin
         Write (JSON, V);
         return JSON;
      end Output;

   end In_Out_JSON;

   -------------------------------
   -- In_Out_Unconstrained_JSON --
   -------------------------------

   package body In_Out_Unconstrained_JSON is

      -----------
      -- Input --
      -----------

      function Input (JSON : TGen.JSON.JSON_Value) return T is
         H : constant Header := Input_Header (JSON);
      begin
         return V : T := Init (H) do
            Read (JSON, V);
         end return;
      end Input;

      ------------
      -- Output --
      ------------

      function Output (V : T) return TGen.JSON.JSON_Value is
         JSON : TGen.JSON.JSON_Value := Create_Object;
      begin
         pragma Warnings (Off);
         Output_Header (JSON, V);
         pragma Warnings (On);
         Write (JSON, V);
         return JSON;
      end Output;

   end In_Out_Unconstrained_JSON;

end TGen.Marshalling_Lib.JSON;
