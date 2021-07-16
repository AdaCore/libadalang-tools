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
---  This package contains refactoring tools that allow changing the signature
---  of a subprogram and adjust its calls accordingly. Currently, it contains
---  the following tools:
---     - Add Parameter
---     - Change Parameter Mode
---     - Move Parameter Left/Right
---     - Remove Parameter

with Libadalang.Common; use Libadalang.Common;

package Laltools.Refactor.Subprogram_Signature is

   type Parameter_Data_Type is
      record
         Name            : Unbounded_String;
         Mode            : Unbounded_String;
         Type_Indication : Unbounded_String;
         Default_Expr    : Unbounded_String;
      end record;

   function Image (Data : Parameter_Data_Type) return Unbounded_String;
   --  Returns a human readable Unbounded_String with the description of
   --  a Parameter_Data_Type.

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
     (Node            : Ada_Node'Class;
      Subp            : out Basic_Decl;
      Parameter_Index : out Positive;
      Requires_Type   : out Boolean)
      return Boolean
     with Pre => not Node.Is_Null,
          Post => (if Is_Add_Parameter_Available'Result then
                     Is_Subprogram (Subp));
   --  Checks if from 'Node' we can add a parameter. If so, then returns True
   --  'Subp', 'Parameter_Index' and 'Requires_Type' will have the
   --  necessary data to create a Parameter_Adder object or to call
   --  'Add_Parameter'.

   type Mode_Alternatives_Type is array (1 .. 3) of Ada_Mode;
   type Mode_Alternatives_Map_Type is array (Ada_Mode)
     of Mode_Alternatives_Type;

   Mode_Alternatives_Map : constant Mode_Alternatives_Map_Type :=
     (Ada_Mode_Default => (Ada_Mode_In,      Ada_Mode_Out, Ada_Mode_In_Out),
      Ada_Mode_In      => (Ada_Mode_Default, Ada_Mode_Out, Ada_Mode_In_Out),
      Ada_Mode_Out     => (Ada_Mode_Default, Ada_Mode_In,  Ada_Mode_In_Out),
      Ada_Mode_In_Out  => (Ada_Mode_Default, Ada_Mode_In,  Ada_Mode_Out));

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
     (True, False);
   Only_Forward    : constant Move_Direction_Availability_Type :=
     (False, True);
   Both_Directions : constant Move_Direction_Availability_Type :=
     (True, True);

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

   function Is_Remove_Parameter_Available
     (Node                    : Ada_Node'Class;
      Subp                    : out Basic_Decl;
      Parameter_Indices_Range : out Parameter_Indices_Range_Type)
      return Boolean
     with Pre  => not Node.Is_Null,
          Post => (if Is_Remove_Parameter_Available'Result then
                     Is_Subprogram (Subp));
   --  Checks if from 'Node' we can unambiguously identify a parameter. If so,
   --  then returns True.
   --  Example 1:
   --  procedure Foo (A, B : Integer);
   --
   --  Is_Remove_Parameter_Available only returns True if Node refers to
   --  the Identifier nodes A and B. Otherwise, it's not possible to
   --  unambiguously identify which parameter Node might refer to.
   --
   --  Example 2:
   --  procedure Bar (A : Integer);
   --
   --  Is_Remove_Parameter_Available returns True if any node in of the
   --  Params node, inclusive, i.e., any node of "(A : Integer)".
   --  This is because since there is only one paremeter, we can unambigously
   --  identify it as long as Node refers to a Params node or any child of it.

   function Add_Parameter
     (Subp            : Basic_Decl;
      New_Parameter   : Parameter_Data_Type;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Text_Edit_Map
     with Pre => Is_Subprogram (Subp);
   --  Adds a parameter defined by 'New_Parameter' to position defined by
   --  'Index'. The new parameter is added to the entire subprogram hierarchy,
   --  as well as, all renames hierarchy.

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

   function Remove_Parameter
     (Subp            : Basic_Decl;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Text_Edit_Map
     with Pre => Is_Subprogram (Subp);
   --  Removes the parameter defined by Parameter_Index. The parameter is
   --  removed in the entire subprogram hierarchy, as well as, all renames
   --  hierarchy.

   function Remove_Parameters
     (Subp              : Basic_Decl;
      Parameter_Indices : Parameter_Indices_Type;
      Units             : Analysis_Unit_Array)
      return Text_Edit_Map;
   --  Removes the parameters defined by 'Parameter_Indices'. The parameter is
   --  removed in the entire subprogram hierarchy, as well as, all renames
   --  hierarchy.

   function Remove_Parameters
     (Subp                     : Basic_Decl;
      Parameter_Indices_Ranges : Parameter_Indices_Ranges_Type;
      Units                    : Analysis_Unit_Array)
      return Text_Edit_Map
     with Pre => Is_Subprogram (Subp);
   --  Removes the parameters defined by 'Parameter_Indices_Ranges'. The
   --  parameter is removed in the entire subprogram hierarchy, as well as, all
   --  renames hierarchy.

   function Remove_All_Parameters
     (Subp                    : Basic_Decl'Class;
      Units                   : Analysis_Unit_Array)
      return Text_Edit_Map
     with Pre => Is_Subprogram (Subp);
   --  Removes all parameters os 'Subp'. The parameters are removed in the
   --  entire subprogram hierarchy, as well as, all renames hierarchy.

   type Signature_Changer_Option_Type is (Include_Parents, Include_Children);

   type Signature_Changer_Configuration_Type is
     array (Signature_Changer_Option_Type) of Boolean;

   Default_Configuration : Signature_Changer_Configuration_Type :=
     (Include_Parents .. Include_Children  => True);

   type Signature_Changer is limited interface and Refactoring_Tool;

   type Parameter_Remover is new Signature_Changer with private;

   function Create
     (Target                  : Basic_Decl;
      Parameter_Indices_Range : Parameter_Indices_Range_Type;
      Configuration           : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Parameter_Remover;
   --  Creates a signature changer that removes parameters, defined by
   --  'Parameter_Indices_Range'.

   overriding
   function Refactor
     (Self           : Parameter_Remover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Returns an Edit_Map with all the refactoring edits needed to remove
   --  a parameter.

   type Parameter_Adder is new Signature_Changer with private;

   function Create
     (Target         : Basic_Decl;
      New_Parameter  : Parameter_Data_Type;
      Index          : Positive;
      Configuration  : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Parameter_Adder;
   --  Creates a signature changer that adds a parameter, defined by
   --  'New_Parameter'. Its position is defined by 'Index'.

   overriding
   function Refactor
     (Self           : Parameter_Adder;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Returns an Edit_Map with all the refactoring edits needed to add
   --  a parameter.

   type Mode_Changer is new Signature_Changer with private;

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

   type Parameter_Mover is interface and Signature_Changer;

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

   type Parameter_Adder is new Signature_Changer with
      record
         Subp            : Basic_Decl;
         New_Parameter   : Parameter_Data_Type;
         Parameter_Index : Natural;
         Configuration   : Signature_Changer_Configuration_Type;
      end record;

   type Mode_Changer is new Signature_Changer with
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

   type Parameter_Remover is new Signature_Changer with
      record
         Subp                    : Basic_Decl;
         Parameter_Indices_Range : Parameter_Indices_Range_Type;
         Configuration           : Signature_Changer_Configuration_Type;
      end record;

end Laltools.Refactor.Subprogram_Signature;
