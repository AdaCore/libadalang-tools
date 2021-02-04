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
---
---  This package contains utilities related to the hierarchy of a subprogram.
---  The subprogram hierarchy includes primitive subprograms of base and
---  derived types, the rename tree associated to a subprogram, and
---  instantiations of a generic subprogram.

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

package Laltools.Subprogram_Hierarchy is

   type Generic_Subp_Instantiation_Array is
     array (Positive range <>) of Generic_Subp_Instantiation;

   type Subp_Renaming_Decl_Array is
     array (Positive range <>) of Subp_Renaming_Decl;

   function Find_All_Base_Subp_Declarations
     (Subp  : Basic_Decl)
      return Basic_Decl_Array
     with Pre => not Subp.Is_Null
     and then Subp.Kind in Ada_Subp_Body_Range
       | Ada_Subp_Decl_Range
         | Ada_Expr_Function_Range;
   --  If Subp is a subprogram, then this function returns an array with all
   --  declarations that Subp overrides or that is being overridden by.

   function Find_Generic_Subp_Instantiations
     (Refs : Ref_Result_Array)
      return Generic_Subp_Instantiation_Array;
   --  Returns an array with all references of 'Refs' which have a semantic
   --  parent that is of kind Ada_Generic_Subp_Instantation_Range.

   function Find_Generic_Subp_Instantiations
     (Subp  : Generic_Subp_Decl;
      Units : Analysis_Unit_Array)
      return Generic_Subp_Instantiation_Array
   is (Find_Generic_Subp_Instantiations
       (Subp.P_Defining_Name.P_Find_All_References (Units)));
   --  Returns an array with all instantiations of 'Gen_Subp'

   function Find_Subp_Renaming_Decls
     (Refs : Ref_Result_Array)
      return Subp_Renaming_Decl_Array;
   --  Returns an array with all references of 'Refs' which have a semantic
   --  parent that is of kind Ada_Subp_Renaming_Decl.

   function Find_Subp_Renaming_Decls
     (Subp  : Basic_Decl'Class;
      Units : Analysis_Unit_Array)
      return Subp_Renaming_Decl_Array
   is (Find_Subp_Renaming_Decls
       (Subp.P_Defining_Name.P_Find_All_References (Units)))
     with Pre => Subp.P_Is_Subprogram
     or else Subp.Kind in Ada_Generic_Subp_Decl_Range
       | Ada_Generic_Subp_Instantiation_Range;
   --  Returns an array with all renames of 'Subp'.

   function Find_Subp_Renaming_Decls_Hierarchy
     (Subp_Decl : Basic_Decl'Class;
      Units    : Analysis_Unit_Array)
      return Subp_Renaming_Decl_Array;
   --  Returns an array with all renames of 'Subp' including renames of
   --  renames, i.e., returns the entire rename tree.

   procedure Find_Subp_Relatives
     (Subp           : Basic_Decl'Class;
      Units          : Analysis_Unit_Array;
      Decl_Callback  : access procedure (Relative_Subp : Basic_Decl'Class);
      Find_Calls     : Boolean := False;
      Calls_Callback : access procedure (Call : Call_Stmt) := null)
       with Pre => (Subp.P_Is_Subprogram
                    or else Subp.Kind in Ada_Generic_Subp_Decl_Range)
       and then (if Find_Calls then Calls_Callback /= null else True);
   --  Finds the all relatives of 'Subp' (overwritten subprograms,
   --  overwritting subprogram, renames and generic instantiations) and calls
   --  'Decl_Callback' on them. If 'Find_Calls' then also calls
   --  'Calls_Callback' on their call statements.

   function Get_Subp_Hierarchy
     (Subp  : Basic_Decl;
      Units : Analysis_Unit_Array)
      return Basic_Decl_Array
     with Pre => not Subp.Is_Null
     and then Subp.Kind in Ada_Subp_Body_Range
       | Ada_Subp_Decl_Range
         | Ada_Expr_Function_Range;
   --  If Subp is a primitive subrogram of a type, then returns an array
   --  with all declaration overriding or that are being overridden by Subp.

end Laltools.Subprogram_Hierarchy;
