------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Containers.Vectors;

with Laltools.Common; use Laltools.Common;

with TGen.Types; use TGen.Types;

package TGen.Gen_Strategies_Utils is

   use Libadalang.Common;

   function "+" (Text : Unbounded_Text_Type) return String is
     (To_UTF8 (To_Text (Text)));

   function "+" (Text : Text_Type) return String is
     (To_UTF8 (Text));

   type Parameter_Data is
      record
         Name                      : Unbounded_Text_Type;
         Index                     : Positive;
         Type_Name                 : Unbounded_Text_Type;
         Type_Fully_Qualified_Name : Unbounded_Text_Type;
         Type_Parent_Package       : Unbounded_Text_Type;
         Type_Repr : SP.Ref;
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
         case Kind is
            when Ada_Subp_Kind_Function =>
               Return_Type_Fully_Qualified_Name : Unbounded_Text_Type;
               Return_Type_Parent_Package       : Unbounded_Text_Type;
            when Ada_Subp_Kind_Procedure =>
               null;
         end case;
      end record;

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
         Pkg_Name : Package_Decl;
      end record;

   function Extract_Package_Data
     (Pkg_Decl : Package_Decl)
      return Package_Data;

   function Extract_Subprogram_Data
     (Subp : Basic_Decl'Class)
      return Subprogram_Data
     with Pre => not Subp.Is_Null and then Is_Subprogram (Subp);
   --  Extracts a Subprogram_Data object from Subp

   function Extract_Parameters_Data
     (Subp : Basic_Decl'Class)
      return Parameters_Data_Vector
     with Pre => not Subp.Is_Null and then Is_Subprogram (Subp);
   --  Returns a vector of Parameters_Data objects extracted from each
   --  parameter of Subp.

   function Gen_Param_Function_Name
     (Subp_Data : Subprogram_Data;
      Param : Parameter_Data) return String is
     ((+Subp_Data.Parent_Package) & "_" & (+Subp_Data.Name) & "_"
      & (+Param.Name));

   function Strat_Param_Name
     (Subp_Data : Subprogram_Data; Param : Parameter_Data) return String is
     ("Strat_" & (+Subp_Data.Parent_Package) & "_" & (+Subp_Data.Name) & "_"
      & (+Param.Name));

   function Param_Strat_Package_Name (Package_Name : String) return String is
     (Package_Name & ".Param_Strategies");

   function Type_Strat_Package_Name (Package_Name : String) return String is
     (Package_Name & ".Type_Strategies");

end TGen.Gen_Strategies_Utils;
