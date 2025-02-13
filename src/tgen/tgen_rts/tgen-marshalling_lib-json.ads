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

with TGen.JSON; use TGen.JSON;

package TGen.Marshalling_Lib.JSON is

   generic
      type T is (<>);
   package Read_Write_Discrete_JSON is

      procedure Write (JSON : in out TGen.JSON.JSON_Value; V : T);

      procedure Read (JSON : TGen.JSON.JSON_Value; V : out T);

   end Read_Write_Discrete_JSON;

   generic
      type T is delta <> digits <>;
   package Read_Write_Decimal_Fixed_JSON is

      procedure Write (JSON : in out TGen.JSON.JSON_Value; V : T);

      procedure Read (JSON : TGen.JSON.JSON_Value; V : out T);

   end Read_Write_Decimal_Fixed_JSON;

   generic
      type T is delta <>;
   package Read_Write_Ordinary_Fixed_JSON is

      procedure Write (JSON : in out TGen.JSON.JSON_Value; V : T);

      procedure Read (JSON : TGen.JSON.JSON_Value; V : out T);

   end Read_Write_Ordinary_Fixed_JSON;

   generic
      type T is digits <>;
   package Read_Write_Float_JSON is

      procedure Write (JSON : in out TGen.JSON.JSON_Value; V : T);

      procedure Read (JSON : TGen.JSON.JSON_Value; V : out T);

   end Read_Write_Float_JSON;

   generic
      type T is private;

      with procedure Write (JSON : in out TGen.JSON.JSON_Value; V : T);

      with procedure Read (JSON : TGen.JSON.JSON_Value; V : out T);

   package In_Out_JSON
   is

      function Input (JSON : TGen.JSON.JSON_Value) return T;

      function Output (V : T) return TGen.JSON.JSON_Value;

   end In_Out_JSON;

   generic
      type T (<>) is private;
      type Header is private;

      with function Init (H : Header) return T;

      with procedure Output_Header (JSON : in out TGen.JSON.JSON_Value; V : T);

      with function Input_Header (JSON : TGen.JSON.JSON_Value) return Header;

      with procedure Write (JSON : in out TGen.JSON.JSON_Value; V : T);

      with procedure Read (JSON : TGen.JSON.JSON_Value; V : out T);

   package In_Out_Unconstrained_JSON
   is

      function Input (JSON : TGen.JSON.JSON_Value) return T;

      function Output (V : T) return TGen.JSON.JSON_Value;

   end In_Out_Unconstrained_JSON;

end TGen.Marshalling_Lib.JSON;
