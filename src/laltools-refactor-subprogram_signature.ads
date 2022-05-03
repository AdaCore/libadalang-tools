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
---
---  This package contains refactoring tools that allow changing the signature
---  of a subprogram and adjust its calls accordingly. Currently, it contains
---  the following tools:
---     - Add Parameter
---     - Change Parameter Mode
---     - Move Parameter Left/Right

with Libadalang.Common; use Libadalang.Common;

package Laltools.Refactor.Subprogram_Signature is

   type Parameter_Indices_Type is array (Positive range <>) of Positive;

   type Parameter_Indices_Range_Type is
      record
         First, Last  : Positive;
      end record
     with Dynamic_Predicate =>
       Parameter_Indices_Range_Type.First <= Parameter_Indices_Range_Type.Last;

   type Parameter_Indices_Ranges_Type is
     array (Positive range <>) of Parameter_Indices_Range_Type;

   function Is_Add_Parameter_Available
     (Unit                        : Analysis_Unit;
      Location                    : Source_Location;
      Requires_Full_Specification : out Boolean)
      return Boolean;
   --  Checks if we can add a parameter in the given Location of Unit.
   --  If so, Requires_Full_Specification specifies if the new parameter
   --  needs to be fully specified, i.e., a Param_Spec is expected.

   type Mode_Alternatives_Type is array (1 .. 3) of Ada_Mode;
   type Mode_Alternatives_Map_Type is array (Ada_Mode)
     of Mode_Alternatives_Type;

   Mode_Alternatives_Map : constant Mode_Alternatives_Map_Type :=
     [Ada_Mode_Default => [Ada_Mode_In,      Ada_Mode_Out, Ada_Mode_In_Out],
      Ada_Mode_In      => [Ada_Mode_Default, Ada_Mode_Out, Ada_Mode_In_Out],
      Ada_Mode_Out     => [Ada_Mode_Default, Ada_Mode_In,  Ada_Mode_In_Out],
      Ada_Mode_In_Out  => [Ada_Mode_Default, Ada_Mode_In,  Ada_Mode_Out]];

   function Is_Change_Mode_Available
     (Node                    : Ada_Node'Class;
      Subp                    : out Basic_Decl;
      Parameter_Indices_Range : out Parameter_Indices_Range_Type;
      Mode_Alternatives       : out Mode_Alternatives_Type)
      return Boolean
     with Pre => not Node.Is_Null,
          Post => (if Is_Change_Mode_Available'Result then
                     Is_Subprogram (Subp));
   --  Checks if from 'Node' we can unambiguously identify a parameter or a
   --  group of parameters. If so, then returns True. 'Subp',
   --  'Parameter_Indices_Range' and 'Mode_Alternatives' will have the
   --  necessary data to create a Mode_Changer object or to call
   --  'Change_Mode'.

   type Move_Direction_Type is (Backward, Forward);

   type Move_Direction_Availability_Type is
     array (Move_Direction_Type) of Boolean;

   Only_Backward   : constant Move_Direction_Availability_Type :=
     [True, False];
   Only_Forward    : constant Move_Direction_Availability_Type :=
     [False, True];
   Both_Directions : constant Move_Direction_Availability_Type :=
     [True, True];

   function Is_Move_Parameter_Available
     (Node            : Ada_Node'Class;
      Subp            : out Basic_Decl;
      Parameter_Index : out Positive;
      Move_Directions : out Move_Direction_Availability_Type)
      return Boolean
     with Pre => not Node.Is_Null,
          Post => (if Is_Move_Parameter_Available'Result then
                     Is_Subprogram (Subp));
   --  Checks if from 'Node' we can unambiguously identify a parameter. If so,
   --  then returns True. 'Subp', 'Parameter_Index' and 'Move_Directions' will
   --  have the necessary data to create a Parameter_Mover object or to call
   --  'Move_Left'/'Move_Right'.

   function Change_Mode
     (Subp                    : Basic_Decl;
      Parameter_Indices_Range : Parameter_Indices_Range_Type;
      New_Mode                : Ada_Mode;
      Units                   : Analysis_Unit_Array)
      return Text_Edit_Map
     with Pre => Is_Subprogram (Subp);
   --  Changes the parameter mode of the parameters defined by
   --  'Parameter_Indices_Range' to 'New_Mode'. The new mode is added to the
   --  entire subprogram hierarchy, as well as, all renames hierarchy.

   function Move_Backward
     (Subp            : Basic_Decl;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Text_Edit_Map
     with Pre => Is_Subprogram (Subp);
   --  Moves the parameter defined by 'Parameter_Index' backward. The
   --  parameter is moved backward in the entire subprogram hierarchy, as
   --  well as, all renames hierarchy.

   function Move_Right
     (Subp            : Basic_Decl;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Text_Edit_Map
   is (Move_Backward (Subp, Parameter_Index + 1, Units));
   --  Moves the parameter defined by 'Parameter_Index' forward. The
   --  parameter is moved forward in the entire subprogram hierarchy, as
   --  well as, all renames hierarchy.

   type Signature_Changer_Option_Type is (Include_Parents, Include_Children);

   type Signature_Changer_Configuration_Type is
     array (Signature_Changer_Option_Type) of Boolean;

   Default_Configuration : Signature_Changer_Configuration_Type :=
     [Include_Parents .. Include_Children  => True];

   type Subprogram_Signature_Changer is limited interface and Refactoring_Tool;

   type Parameter_Adder is new Subprogram_Signature_Changer with private;

   function Create
     (Unit          : Analysis_Unit;
      Location      : Source_Location;
      New_Parameter : Unbounded_String)
      return Parameter_Adder
     with Pre => Unit /= No_Analysis_Unit
                   and then Location /= No_Source_Location
                   and then New_Parameter /= Null_Unbounded_String;
   --  Creates a signature changer that adds a parameter

   overriding
   function Refactor
     (Self           : Parameter_Adder;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Returns an Edit_Map with all the refactoring edits needed to add
   --  a parameter.

   type Mode_Changer is new Subprogram_Signature_Changer with private;

   function Create
     (Target          : Basic_Decl;
      Parameter_Index : Natural;
      New_Mode        : Ada_Mode;
      Configuration   : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Mode_Changer;
   --  Creates a signature changer that changes a parameter mode. The parameter
   --  is defined by 'Parameter_Index' and the new mode is defined by
   --  'New_Mode'.

   function Create
     (Target                  : Basic_Decl;
      Parameter_Indices_Range : Parameter_Indices_Range_Type;
      New_Mode                : Ada_Mode;
      Configuration           : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Mode_Changer;
   --  Creates a signature changer that changes the mode of multiple parameters
   --  defined by 'Parameter_Indices_Range'. The new mode is defined by
   --  'New_Mode'.

   overriding
   function Refactor
     (Self           : Mode_Changer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Returns an Edit_Map with all the refactoring edits needed to change
   --  a parameter mode.

   type Parameter_Mover is interface and Subprogram_Signature_Changer;

   type Backward_Mover is new Parameter_Mover with private;

   function Create
     (Target          : Basic_Decl;
      Parameter_Index : Natural;
      Configuration   : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Backward_Mover;
   --  Creates a signature changer that moves a parameter backward.
   --  The parameter is defined by 'Parameter_Index'.

   overriding
   function Refactor
     (Self : Backward_Mover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Returns an Edit_Map with all the refactoring edits needed to move
   --  a parameter backward.

   type Forward_Mover is new Parameter_Mover with private;

   function Create
     (Target          : Basic_Decl;
      Parameter_Index : Natural;
      Configuration   : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Forward_Mover;
   --  Creates a signature changer that moves a parameter forward.
   --  The parameter is defined by 'Parameter_Index'.

   overriding
   function Refactor
     (Self : Forward_Mover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Returns an Edit_Map with all the refactoring edits needed to move
   --  a parameter forward.

private

   type Extended_Argument_Indicies_Type is
     array (Positive range <>) of Natural;

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

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function Generic_Array_Unique (Container : Array_Type) return Array_Type;
   --  Returns a sorted Array_Type with the unique elements of 'Container'

   type Relative_Position_Type is (Before, After);

   type Parameter_Relative_Position_Type is
      record
         Side  : Relative_Position_Type;
         Index : Positive;
      end record;

   type Parameter_Adder is new Subprogram_Signature_Changer with
      record
         Spec               : Subp_Spec;
         New_Parameter      : Unbounded_String;
         Relative_Position  : Parameter_Relative_Position_Type;
         Full_Specification : Boolean;
      end record;

   procedure Add_Full_Parameter_Specification
     (Self   : Parameter_Adder;
      Target : Basic_Decl'Class;
      Edits : in out Text_Edit_Map)
     with Pre => Target.P_Is_Subprogram
                   or else Target.Kind in Ada_Generic_Subp_Decl_Range;
   --  Adds a fully specified parameter (Self.New_Parameter) to Target.
   --  Must only be used if Self.Full_Specification is True.

   procedure Add_Parameter_Defining_Id_Or_Ids
     (Self   : Parameter_Adder;
      Target : Basic_Decl'Class;
      Edits  : in out Text_Edit_Map)
     with Pre => Target.P_Is_Subprogram
                   or else Target.Kind in Ada_Generic_Subp_Decl_Range;
   --  Adds a parameter or a list of parameters (Self.New_Parameter) to Target.
   --  Must only be used if Self.Full_Specification is False.

   type Mode_Changer is new Subprogram_Signature_Changer with
      record
         Subp                    : Basic_Decl;
         Parameter_Indices_Range : Parameter_Indices_Range_Type;
         New_Mode                : Ada_Mode;
         Configuration           : Signature_Changer_Configuration_Type;
      end record;

   type Backward_Mover is new Parameter_Mover with
      record
         Subp            : Basic_Decl;
         Parameter_Index : Positive;
         Configuration   : Signature_Changer_Configuration_Type;
      end record;

   type Forward_Mover is new Parameter_Mover with
      record
         Mover : Backward_Mover;
      end record;

end Laltools.Refactor.Subprogram_Signature;
