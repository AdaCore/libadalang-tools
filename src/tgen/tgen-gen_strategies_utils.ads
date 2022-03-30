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
--
--  This unit provides utilities needed to generate testing strategies

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Wide_Wide_Unbounded;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;
with Langkit_Support.Text; use Langkit_Support.Text;

with TGen.Context; use TGen.Context;
with TGen.Strings; use TGen.Strings;
with TGen.Subprograms; use TGen.Subprograms;
with TGen.Types; use TGen.Types;

with GNATCOLL.Projects; use GNATCOLL.Projects;

package TGen.Gen_Strategies_Utils is

   use Libadalang.Common;

   function Extract_Package_Data
     (Pkg_Decl : Package_Decl)
      return Package_Data;
   --  Extract unit information from Pkg_Decl

   function Extract_Subprogram_Data
     (Subp : Basic_Decl'Class)
      return Subprogram_Data
     with Pre => not Subp.Is_Null;
   --  Extracts a Subprogram_Data object from Subp

   function Extract_Parameters_Data
     (Subp : Basic_Decl'Class)
      return Parameters_Data_Vector
     with Pre => not Subp.Is_Null;
   --  Returns a vector of Parameters_Data objects extracted from each
   --  parameter of Subp.

   function Gen_Random_Function_Name
     (T : Typ'Class) return String is
      ("Gen_" & T.Type_Name);
   --  Return the name of the function generating random values for type T

   function Strat_Param_Name (Param : Parameter_Data) return String is
     ("Strat_" & (+Param.Name));
   --  Return the name of the strategy generating values for Param

   function Param_Strat_Package_Name (Package_Name : String) return String is
     (Package_Name & ".Param_Strategies");

   function Type_Strat_Package_Name (Package_Name : String) return String;

   function Strip (Package_Name : String) return String;
   --  Strip the last dotted name. ??? What happens if no dotted name

   function Gen_Param_Function_Name
     (Subp_Data : Subprogram_Data;
      Param : Parameter_Data) return String is
     ((+Subp_Data.Name) & "_" & (+Param.Name));

   function Gen_Param_Full_Function_Name
     (Subp_Data : Subprogram_Data;
      Param : Parameter_Data) return String is
     (Param_Strat_Package_Name (+Subp_Data.Parent_Package) & "."
      & (+Subp_Data.Name) & "_" & (+Param.Name));

   package Param_Vectors_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Unbounded_Text_Type,
      Element_Type => Parameters_Data_Vector,
      "<" => Ada.Strings.Wide_Wide_Unbounded."<",
      "=" => Parameters_Data_Vectors."=");

   subtype Param_Vector_Map is Param_Vectors_Maps.Map;

   function Unit_To_Filename
     (Project   : Project_Type;
      Unit_Name : String;
      Part      : Unit_Parts) return String;
   --  Return the filename associates to unit Unit_Name

   function Is_Subprogram (Decl : Basic_Decl'Class) return Boolean is
     (not Decl.Is_Null
      and then (Decl.P_Is_Subprogram
                or else Decl.Kind in Ada_Generic_Subp_Decl_Range)
      and then not (Decl.Kind in Ada_Enum_Literal_Decl_Range));
   --  Checks if Decl is a subprogram excluding enum literals

   function Get_Subp_Params (Subp : Basic_Decl'Class) return Params
     with Inline,
          Pre => Is_Subprogram (Subp)
                 or else (not Subp.Is_Null
                          and then Subp.Kind in
                            Ada_Generic_Subp_Instantiation);
   --  Gets the Params node associated to Subp, if it exists.
   --  If it doesn't exist returns No_Params.

   function Get_Subp_Spec (Subp : Basic_Decl'Class) return Base_Subp_Spec
     with Inline,
          Pre => Is_Subprogram (Subp)
                 or else (not Subp.Is_Null
                          and then Subp.Kind in
                            Ada_Generic_Subp_Instantiation);
   --  Gets the Subp_Spec node associated to Subp

   function Get_Subp_Spec_Params
     (Subp_Spec : Base_Subp_Spec'Class)
      return Params;
   --  Gets the Params node associated to Subp_Spec, if it exists.
   --  If it doesn't exist returns No_Params.

   function Get_All_Types (Self : Typ'Class) return Typ_Set;
   --  Return the set of all types a type is composed of

   function Gen_Constrained_Function (Self : Typ'Class) return Subprogram_Data;
   --  TODO unused today (ony for dynamic generation of constrained records)

end TGen.Gen_Strategies_Utils;
