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
--  This package contains refactoring tools that allow removing formal
--  parameters from subprogram specs and their corresponding actual
--  parameters on the subprogram calls.

package Laltools.Refactor.Subprogram_Signature.Remove_Parameter is

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

   type Parameter_Remover is new Subprogram_Signature_Changer with private;

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

private

   type Parameter_Remover is new Subprogram_Signature_Changer with
      record
         Subp                    : Basic_Decl;
         Parameter_Indices_Range : Parameter_Indices_Range_Type;
         Configuration           : Signature_Changer_Configuration_Type;
      end record;

end Laltools.Refactor.Subprogram_Signature.Remove_Parameter;
