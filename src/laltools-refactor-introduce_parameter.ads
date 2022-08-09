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
--
--  This package contains refactoring tools that allow introducing parameters

private with Langkit_Support.Text;
private with Libadalang.Common;

package Laltools.Refactor.Introduce_Parameter is

   function Is_Introduce_Parameter_Available
     (Unit       : Analysis_Unit;
      SLOC_Range : Source_Location_Range)
      return Boolean;
   --  Checks if Unit and SLOC_Range represent an Object_Decl or Expr that can
   --  be introduced as a new parameter.
   --  An Object_Decl can be introduced iff it has a parent Subp_Body node.
   --  An Expr can be introduced iff it has a parent Subp_Body node it has
   --  a known expression type or expected type.
   --  If the result of this function is True, then Unit and SLOC_Range can be
   --  used in Parameter_Introducer's constructor Create_Parameter_Introducer.

   type Parameter_Introducer is new Refactoring_Tool with private;

   function Create_Parameter_Introducer
     (Unit       : Analysis_Unit;
      SLOC_Range : Source_Location_Range)
      return Parameter_Introducer
     with Pre => Is_Introduce_Parameter_Available (Unit, SLOC_Range);
   --  Parameter_Introducer constructor.
   --  SLOC_Range must be the SLOC of the object declaration Name'Class node
   --  that will be introduced or of an expresion with a know type or expected
   --  tyoe. Use Is_Introduce_Parameter_Available to check if Unit and
   --  SLOC_Range can be used in this constructor.

   overriding
   function Refactor
     (Self           : Parameter_Introducer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Introduces the Object_Decl or Expr identified by the Unit and SLOC_Range
   --  passed to the Parameter_Introducer constructor
   --  Create_Parameter_Introducer.

private
   use Langkit_Support.Text;
   use Libadalang.Common;

   type Introduction_Strategy is interface;

   function Introduce_Parameter
     (Self           : Introduction_Strategy;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits is abstract;
   --  This function needs to introduce a parameter in a subprogram spec

   function Is_Object_Decl_With_Enclosing_Subp_Body
     (Node : Ada_Node'Class)
      return Boolean;
   --  Checks if Node is the Name of a Defining_Name of an Object_Decl,
   --  declared in a subprogram.

   type Parameter_From_Object_Decl_Introducer is new Introduction_Strategy with
      record
         Definition  : Defining_Name;
         Object_Decl : Libadalang.Analysis.Object_Decl;
         Parent_Subp : Subp_Body;
      end record
     with Dynamic_Predicate =>
            Is_Object_Decl_With_Enclosing_Subp_Body (Definition.F_Name);

   overriding
   function Introduce_Parameter
     (Self           : Parameter_From_Object_Decl_Introducer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Introduces an Object_Decl as a parameter in its enclosing subprogram.
   --  Also pulls up any dependent declaration declared in this subprogram.

   function Compute_Object_Decl_Mode
     (Self : Parameter_From_Object_Decl_Introducer)
      return Ada_Mode;
   --  Compute which mode the introduced parameter from Self.Definition should
   --  have.

   function To_Text_In_Lower_Case
     (Ada_Mode : Libadalang.Common.Ada_Mode)
      return Text_Type;
   --  Converts Ada_Mode to Text_Type in lower case

   function Is_Expr_With_Non_Null_Type_And_Enclosing_Subp_Body
     (Node : Ada_Node'Class)
      return Boolean;
   --  Checks if Node is an Expr with known type or expected type, and if it's
   --  inside a Subp_Body.

   type Parameter_From_Expr_Introducer is new Introduction_Strategy with
      record
         Expr : Libadalang.Analysis.Expr;
      end record
     with Dynamic_Predicate =>
            Is_Expr_With_Non_Null_Type_And_Enclosing_Subp_Body (Expr);

   overriding
   function Introduce_Parameter
     (Self           : Parameter_From_Expr_Introducer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Introduces an Expr as a parameter in its enclosing subprogram

   type Parameter_Introducer is new Refactoring_Tool with
      record
         Target : Ada_Node;
      end record;

   function Define_Strategy
     (Self : Parameter_Introducer)
      return Introduction_Strategy'Class;

end Laltools.Refactor.Introduce_Parameter;
