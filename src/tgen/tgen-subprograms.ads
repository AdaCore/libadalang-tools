------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

with TGen.Strings; use TGen.Strings;
with TGen.Templates;

package TGen.Subprograms is

   use Libadalang.Common;

   type Parameter_Mode is (In_Mode, In_Out_Mode, Out_Mode);

   type Parameter_Data is
      record
         Name                      : Unbounded_Text_Type;
         Index                     : Positive;
         Mode                      : Parameter_Mode;
         Type_Name                 : Unbounded_Text_Type;
         Type_Fully_Qualified_Name : Unbounded_Text_Type;
         Type_Parent_Package       : Unbounded_Text_Type;
      end record;

   package Parameters_Data_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Parameter_Data,
      "="          => "=");
   subtype Parameters_Data_Vector is Parameters_Data_Vectors.Vector;

   type Subprogram_Data (Kind : Ada_Subp_Kind := Ada_Subp_Kind_Procedure) is
      record
         Name                 : Unbounded_Text_Type;
         Fully_Qualified_Name : Unbounded_Text_Type;
         Parent_Package       : Unbounded_Text_Type;
         Parameters_Data      : Parameters_Data_Vector;
         Precondition         : Unbounded_Text_Type;
         Some_Param_Static    : Boolean := False;
         All_Params_Static    : Boolean := False;
         case Kind is
            when Ada_Subp_Kind_Function =>
               Return_Type_Fully_Qualified_Name : Unbounded_Text_Type;
               Return_Type_Parent_Package       : Unbounded_Text_Type;
            when Ada_Subp_Kind_Procedure =>
               null;
         end case;
      end record;

   function To_String
     (Subp : Subprogram_Data) return String
     with Pre => Subp.Kind = Ada_Subp_Kind_Function;
   --  Return a string representation of the spec of the subprogram, e.g.
   --  "function Foo (Bar : Integer) return Natural".

   package Subprograms_Data_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Subprogram_Data);
   subtype Subprograms_Data_Vector is Subprograms_Data_Vectors.Vector;

   type Package_Data;
   type Package_Data_Acc is access all Package_Data;

   package Package_Data_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Package_Data_Acc);

   type Package_Data is
      record
         Subpackages : Package_Data_Vectors.Vector;
         Subprograms : Subprograms_Data_Vectors.Vector;
         Pkg_Name    : Package_Decl;
      end record;

   function "<" (L, R : Package_Data) return Boolean is
     (L.Pkg_Name.P_Fully_Qualified_Name < R.Pkg_Name.P_Fully_Qualified_Name);

   package Package_Data_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Package_Data);
   subtype Package_Data_Set is Package_Data_Sets.Set;


end TGen.Subprograms;
