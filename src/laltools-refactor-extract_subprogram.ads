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
--  This package contains refactoring tools that allow extracting statements
--  and expressions into new subprograms

with Libadalang.Common; use Libadalang.Common;

private with Ada.Containers;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Ordered_Sets;

package Laltools.Refactor.Extract_Subprogram is

   type Available_Subprogram_Kinds_Type is array (Ada_Subp_Kind) of Boolean;

   function Is_Extract_Subprogram_Available
     (Unit                       : Analysis_Unit;
      Section_To_Extract         : Source_Location_Range;
      Available_Subprogram_Kinds : out Available_Subprogram_Kinds_Type)
      return Boolean;
   --  Checks if Section_To_Extract is a set of statements that can be
   --  extracted into a subprogram.

   function Default_Extracted_Subprogram_Name
     (Unit            : Analysis_Unit;
      Location        : Source_Location)
      return Unbounded_String;
   --  Assuming the Location is within a set of statements that can be
   --  extracted, returns a name for the extracted subprogram.
   --  The name will be "Extracted_<_N>" where N is a suffix to avoid
   --  collisions in the declarative part where the statements will be
   --  extracted.

   type Subprogram_Extractor is new Refactoring_Tool with private;

   function Create_Subprogram_Extractor
     (Unit               : Analysis_Unit;
      Section_To_Extract : Source_Location_Range;
      Subprogram_Kind    : Ada_Subp_Kind;
      Subprogram_Name    : Unbounded_String)
      return Subprogram_Extractor;
   --  Subprogram_Extractor constructor.
   --  The section to extract needs to be validated by
   --  Is_Extract_Subprogram_Available.
   --  Subprogram_Kind must be one of the available kinds in the
   --  Available_Subprogram_Kinds parameter of Is_Extract_Subprogram_Available.
   --  Is the inputs are not valid, the Refactor might succeed but generate
   --  invalid code, or a Constrained_Error might be raised to to invalid
   --  node conversions.

   overriding
   function Refactor
     (Self           : Subprogram_Extractor;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;

private

   function "<" (L, R : Defining_Name) return Boolean is
     (L.Sloc_Range < R.Sloc_Range);
   --  < operator used for ordered sets, ordering the Defining_Names by
   --  SLOC

   package Defining_Name_Ordered_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets
       (Element_Type        => Defining_Name,
        "<"                 => Laltools.Refactor.Extract_Subprogram."<",
        "="                 => "=");

   subtype Defining_Name_Ordered_Set is Defining_Name_Ordered_Sets.Set;

   function Defining_Name_Hash
     (Node : Defining_Name'Class)
      return Ada.Containers.Hash_Type
   is (Hash (Node.As_Ada_Node));
   --  Casts Node to an Ada_Node and calls Libadalang.Analysis.Hash on it

   package Defining_Name_To_Ada_Mode_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Defining_Name'Class,
        Element_Type    => Ada_Mode,
        Hash            => Defining_Name_Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   subtype Parameters_Mode_Map is Defining_Name_To_Ada_Mode_Maps.Map;

   package Defining_Name_To_Basic_Decl_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Defining_Name'Class,
        Element_Type    => Basic_Decl'Class,
        Hash            => Defining_Name_Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   subtype Parameters_Type_Map is Defining_Name_To_Basic_Decl_Maps.Map;

   package String_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, String);

   subtype String_Vector is String_Vectors.Vector;

   package Stmt_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, Stmt'Class);

   subtype Stmt_Vector is Stmt_Vectors.Vector;

   type Subprogram_Extractor is new Refactoring_Tool with
      record
         Unit        : Analysis_Unit;
         Start_Token : Token_Reference;
         End_Token   : Token_Reference;
         Start_Stmt  : Stmt;
         End_Stmt    : Stmt;
         Name        : Unbounded_String;
         Kind        : Ada_Subp_Kind;
      end record;

   type Extract_Subprogram_Strategy is abstract tagged
      record
         Unit        : Analysis_Unit;
         Start_Token : Token_Reference;
         End_Token   : Token_Reference;
         Start_Stmt  : Stmt;
         End_Stmt    : Stmt;
         Name        : Unbounded_String;
      end record;

   function Extract
     (Self : Extract_Subprogram_Strategy'Class)
      return Refactoring_Edits;
   --  Extracts a subprogram

   function Get_Local_Declarations
     (Self : Extract_Subprogram_Strategy)
      return Defining_Name_Ordered_Set;
   --  Find all object declaration that are candidates to be parameters of
   --  the extracted subprogram

   function Get_Parameters_To_Extract
     (Self       : Extract_Subprogram_Strategy;
      Candidates : Defining_Name_Ordered_Set;
      Statements : Stmt_Vector)
      return Defining_Name_Ordered_Set is abstract;
   --  Filters Candidates by checking which ones are references in
   --  Statements. These will need to be parameters of the extracted function.
   --  Additional checks might be needed, depending what kind of subprogram
   --  we're extracting.

   function Get_Parameters_Mode
     (Self       : Extract_Subprogram_Strategy;
      Parameters : Defining_Name_Ordered_Set;
      Statements : Stmt_Vector)
      return Parameters_Mode_Map is abstract;
   --  For each Parameter of Parameters, compute its mode based on its usage in
   --  Statements, i.e., if it is read or written.

   function Get_Parameters_Type
     (Self       : Extract_Subprogram_Strategy;
      Parameters : Defining_Name_Ordered_Set)
      return Parameters_Type_Map;
   --  For each Parameter of Parameters, compute its type

   function Build_Extracted_Subprogram_Parameters
     (Self            : Extract_Subprogram_Strategy;
      Parameters      : Defining_Name_Ordered_Set;
      Parameters_Mode : Parameters_Mode_Map;
      Parameters_Type : Parameters_Type_Map)
      return String_Vector;
   --  Returns a String_Vector with all the Param_Specs of the extracted
   --  subprogram.

   function Build_Extracted_Subprogram_Statements
     (Self : Extract_Subprogram_Strategy)
      return String_Vector is abstract;
   --  Returns a String_Vector with all Stmts of the extracted subprogram

   function Build_Extracted_Subprogram
     (Self        : Extract_Subprogram_Strategy;
      Params      : String_Vector;
      Statements  : String_Vector;
      Offset      : Natural;
      Indentation : Natural := 3)
      return String is abstract;
   --  Returns the Ada code of the extracted subprogram

   function Build_Extracted_Subprogram_Call
     (Self       : Extract_Subprogram_Strategy;
      Name       : String;
      Parameters : String_Vector)
      return String is abstract;
   --  Returns the Ada code of the extracted subprogram call

   type Procedure_Extractor is new Extract_Subprogram_Strategy with
     null record;

   overriding
   function Get_Parameters_To_Extract
     (Self       : Procedure_Extractor;
      Candidates : Defining_Name_Ordered_Set;
      Statements : Stmt_Vector)
      return Defining_Name_Ordered_Set;

   overriding
   function Get_Parameters_Mode
     (Self       : Procedure_Extractor;
      Parameters : Defining_Name_Ordered_Set;
      Statements : Stmt_Vector)
      return Parameters_Mode_Map;

   overriding
   function Build_Extracted_Subprogram_Statements
     (Self : Procedure_Extractor)
      return String_Vector;

   overriding
   function Build_Extracted_Subprogram
     (Self        : Procedure_Extractor;
      Params      : String_Vector;
      Statements  : String_Vector;
      Offset      : Natural;
      Indentation : Natural := 3)
      return String;

   overriding
   function Build_Extracted_Subprogram_Call
     (Self       : Procedure_Extractor;
      Name       : String;
      Parameters : String_Vector)
      return String;

   type Function_Extractor is new Extract_Subprogram_Strategy with
     null record;

   overriding
   function Get_Parameters_To_Extract
     (Self       : Function_Extractor;
      Candidates : Defining_Name_Ordered_Set;
      Statements : Stmt_Vector)
      return Defining_Name_Ordered_Set;

   overriding
   function Get_Parameters_Mode
     (Self       : Function_Extractor;
      Parameters : Defining_Name_Ordered_Set;
      Statements : Stmt_Vector)
      return Parameters_Mode_Map;

   overriding
   function Build_Extracted_Subprogram_Statements
     (Self : Function_Extractor)
      return String_Vector;

   overriding
   function Build_Extracted_Subprogram
     (Self        : Function_Extractor;
      Params      : String_Vector;
      Statements  : String_Vector;
      Offset      : Natural;
      Indentation : Natural := 3)
      return String;

   overriding
   function Build_Extracted_Subprogram_Call
     (Self       : Function_Extractor;
      Name       : String;
      Parameters : String_Vector)
      return String;

end Laltools.Refactor.Extract_Subprogram;
