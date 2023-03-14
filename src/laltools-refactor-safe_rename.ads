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
--
--  This package contains LAL_Tools utilities to be used by refactoring
--  rename tools.

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Finalization;

with GNATCOLL.Projects;

with GPR2;

with Langkit_Support.Text; use Langkit_Support.Text;

private with Ada.Strings.Wide_Wide_Unbounded;

package Laltools.Refactor.Safe_Rename is

   type Rename_Problem is abstract new Refactoring_Diagnotic with private
     with Type_Invariant'Class => Check_Rename_Problem (Rename_Problem);

   function Check_Rename_Problem
     (Problem : Rename_Problem'Class)
      return Boolean
     with Inline;

   overriding
   function Filename (Self : Rename_Problem) return String;

   overriding
   function Location (Self : Rename_Problem) return Source_Location_Range;

   type Problem_Finder_Algorithm_Kind is (Map_References, Analyse_AST);

   type Attribute_Value_Provider_Access is access
     function
       (Attribute    : GNATCOLL.Projects.Attribute_Pkg_String;
        Index        : String := "";
        Default      : String := "";
        Use_Extended : Boolean := False) return String;

   type GPR2_Attribute_Value_Provider_Access is access
     function
       (Attribute    : GPR2.Q_Attribute_Id;
        Index        : String := "";
        Default      : String := "";
        Use_Extended : Boolean := False) return String;

   type Safe_Renamer is new Refactoring_Tool with private;

   function Create_Safe_Renamer
     (Definition               : Defining_Name'Class;
      New_Name                 : Unbounded_Text_Type;
      Algorithm                : Problem_Finder_Algorithm_Kind;
      Attribute_Value_Provider : Attribute_Value_Provider_Access := null)
      return Safe_Renamer
     with Pre => not Definition.Is_Null;

   function Create_Safe_Renamer
     (Definition               : Defining_Name'Class;
      New_Name                 : Unbounded_Text_Type;
      Algorithm                : Problem_Finder_Algorithm_Kind;
      Attribute_Value_Provider : GPR2_Attribute_Value_Provider_Access := null)
      return Safe_Renamer
     with Pre => not Definition.Is_Null;
   --  Safe_Renamer constructor.
   --  Creates a `Safe_Renamer` object that renames all references of
   --  `Definition` to `New_Name`.
   --  Uses the `Algorithm` kind to search for problems causes by the rename.
   --  Uses Attribute_Value_Provider to compute source file names when top
   --  level declarations are renamed, with the exception of top level
   --  declarations in an analysis unit with multiple compilation units.
   --  In that case, the source name must be hardcoded in the gpr file so it
   --  does not need to be renamed.

   overriding
   function Refactor
     (Self           : Safe_Renamer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Finds all references of the definition we want to rename and checks for
   --  problems caused by renaming such references.
   --  If the definition we want to rename is a primitive subrogram of a type,
   --  then the return refereces include references of supbprograms that it is
   --  overriding or that is being overridden by.
   --  If the definition we want to rename is a parameter of a subprogram
   --  that is a primitive of a type, then the returned refereces include
   --  parameter spec references of supbprograms that it is overriding or that
   --  is being overridden by.

private

   type New_Name_Type is record
      Original : Unbounded_Text_Type;
      Lower    : Unbounded_Text_Type;
   end record;

   function "=" (New_Name : New_Name_Type; Name : Text_Type) return Boolean;

   type New_Name_Access is access all New_Name_Type;

   function "=" (New_Name : New_Name_Access; Name : Text_Type) return Boolean;

   procedure Free (Object : in out New_Name_Access);

   type Rename_Problem is abstract new Refactoring_Diagnotic with
      record
         Canonical_Definition : Defining_Name;
         New_Name             : Unbounded_Text_Type;
         Conflicting_Id       : Name;
      end record;

   type Casing_Type is (lowercase, uppercase, mixedcase);

   type Naming_Scheme_Type is
      record
         Casing          : Casing_Type;
         Dot_Replacement : Unbounded_String;
         Spec_Suffix     : Unbounded_String;
         Body_Suffix     : Unbounded_String;
      end record;

   function Create_Naming_Scheme
     (Attribute_Value_Provider : not null GPR2_Attribute_Value_Provider_Access)
      return Naming_Scheme_Type;

   function Create_Naming_Scheme
     (Attribute_Value_Provider : not null Attribute_Value_Provider_Access)
     return Naming_Scheme_Type;
   --  Naming_Scheme_Type constructor.
   --  The formal parameters have the same meaning as the corresponding
   --  attributes in the Naming package of a .gpr project file.

   function Is_Valid (Self : Naming_Scheme_Type) return Boolean;
   --  Checks if Self is valid, i.e., not equal to Invalid_Naming_Scheme.

   Invalid_Naming_Scheme : constant Naming_Scheme_Type :=
     (Casing          => lowercase,
      Dot_Replacement => Null_Unbounded_String,
      Spec_Suffix     => Null_Unbounded_String,
      Body_Suffix     => Null_Unbounded_String);

   Default_Naming_Scheme : constant Naming_Scheme_Type :=
     (Casing          => lowercase,
      Dot_Replacement => To_Unbounded_String ("-"),
      Spec_Suffix     => To_Unbounded_String (".ads"),
      Body_Suffix     => To_Unbounded_String (".adb"));

   type Missing_Reference is new Rename_Problem with null record;

   overriding
   function Info (Self : Missing_Reference) return String;
   --  Returns a human readable message with the description of Self.

   type New_Reference is new Rename_Problem with null record;

   overriding
   function Info (Self : New_Reference) return String;
   --  Returns a human readable message with the description of Self.

   type Name_Collision is new Rename_Problem with null record;

   overriding
   function Info (Self : Name_Collision) return String;
   --  Returns a human readable message with the description of Self.

   type Overriding_Subprogram is new Rename_Problem with null record;

   overriding
   function Info (Self : Overriding_Subprogram) return String;
   --  Returns a human readable message with the description of Self.

   type Hiding_Name is new Rename_Problem with null record;

   overriding
   function Info (Self : Hiding_Name) return String;
   --  Returns a human readable message with the description of Self.

   type Hidden_Name is new Rename_Problem with null record;

   overriding
   function Info (Self : Hidden_Name) return String;
   --  Returns a human readable message with the description of Self.

   type Problem_Finder_Algorithm is limited interface;

   function Find (Self : in out Problem_Finder_Algorithm)
                  return Refactoring_Diagnotic_Vector is abstract;
   --  Finds problems caused by renaming a definition.

   function "<" (Left, Right : Source_Location_Range)
                    return Boolean;
   --  Starts by comparing if both Left and Right start lines. If equal, start
   --  columns are compared.

   package Slocs_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Source_Location_Range,
      "<"          => "<");

   package Slocs_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Positive,
        Element_Type => Slocs_Sets.Set,
        "="          => Slocs_Sets."=");

   function "<" (Left, Right : Analysis_Unit) return Boolean is
     (Left.Get_Filename < Right.Get_Filename);

   package Unit_Slocs_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Analysis_Unit,
      Element_Type => Slocs_Maps.Map,
      "<"          => "<",
      "="          => Slocs_Maps."=");

   package Unit_Buffers is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Analysis_Unit,
      Element_Type    => Unbounded_String,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Unit_Slocs_Maps_Diff is
      record
         Minus : Unit_Slocs_Maps.Map;
         Plus  : Unit_Slocs_Maps.Map;
      end record;

   type Reference_Mapper (Units_Length : Integer) is new
     Problem_Finder_Algorithm with
      record
         Canonical_Definition      : Defining_Name;
         Canonical_Definition_Unit : Analysis_Unit;
         Canonical_Definition_Sloc : Source_Location_Range;
         Units                     : Analysis_Unit_Array (1 .. Units_Length);
         Original_Name             : Unbounded_Text_Type;
         New_Name                  : Unbounded_Text_Type;
         Original_References       : Unit_Slocs_Maps.Map;
         New_References            : Unit_Slocs_Maps.Map;
         Temporary_Buffers         : Unit_Buffers.Map;
         References_Diff           : Unit_Slocs_Maps_Diff;
      end record;

   procedure Initialize
     (Self                 : out Reference_Mapper;
      Canonical_Definition : Defining_Name;
      New_Name             : Unbounded_Text_Type;
      Original_References  : Unit_Slocs_Maps.Map;
      Units                : Analysis_Unit_Array)
     with Pre => not Ada.Strings.Wide_Wide_Unbounded."="
                       (New_Name,
                        Ada.Strings.Wide_Wide_Unbounded.
                          Null_Unbounded_Wide_Wide_String)
                 and then not Canonical_Definition.Is_Null
                 and then Canonical_Definition.P_Canonical_Part =
                            Canonical_Definition;
   --  Initializes the Reference_Mapper algorithm by filling its elements
   --  with the necessary information to perform the Find function.

   overriding
   function Find (Self : in out Reference_Mapper)
                  return Refactoring_Diagnotic_Vector;
   --  Finds problems caused by renaming a definition. The strategy of this
   --  algorithm is to compare references before and after the rename.
   --  IMPORTANT NOTE: This function reparses all analysis units given to the
   --  Initialize procedure. All active references of nodes created before
   --  running thiss function will then be unreferenced.

   procedure Update_Canonical_Definition (Self : in out Reference_Mapper);
   --  Self.Parse_Temporary_Buffers which will invalidate all nodes.
   --  This procedure restores the canonical definition that the algorithm is\
   --  checking for rename problems.

   procedure Parse_Temporary_Buffers (Self : in out Reference_Mapper);
   --  For every unit, parses a temporary buffer with all the references
   --  renamed.

   procedure Parse_Original_Buffers (Self : in out Reference_Mapper);
   --  For every unit, parses it's original buffer.

   procedure Diff (Self : in out Reference_Mapper);
   --  Creates a diff between the original references and the new references
   --  after the rename is applied. Some references can be lost and some
   --  can be gained, therefore, they are stored seperately.

   type AST_Analyser (Units_Length : Integer) is new
     Ada.Finalization.Limited_Controlled and Problem_Finder_Algorithm with
      record
         Canonical_Definition : Defining_Name;
         New_Name             : New_Name_Access;
         Units                : Analysis_Unit_Array (1 .. Units_Length);
         References           : Base_Id_Vectors.Vector;
      end record;

   procedure Initialize
     (Self                 : out AST_Analyser;
      Canonical_Definition : Defining_Name;
      New_Name             : Unbounded_Text_Type;
      References           : Base_Id_Vectors.Vector;
      Units                : Analysis_Unit_Array)
     with Pre => not Ada.Strings.Wide_Wide_Unbounded."="
                       (New_Name,
                        Ada.Strings.Wide_Wide_Unbounded.
                          Null_Unbounded_Wide_Wide_String)
                 and then not Canonical_Definition.Is_Null
                 and then Canonical_Definition.P_Canonical_Part
                          = Canonical_Definition;
   --  Initializes the Reference_Mapper algorithm by filling its elements
   --  with the necessary information to perform the Find function.

   overriding
   function Find
     (Self : in out AST_Analyser)
      return Refactoring_Diagnotic_Vector;
   --  Finds problems caused by renaming a definition. The strategy of this
   --  algorithm is to analyse the AST and look for specific problems.

   overriding
   procedure Finalize (Self : in out AST_Analyser);

   package Rename_Problem_Indefinite_Holders is new
     Ada.Containers.Indefinite_Holders (Rename_Problem'Class);

   subtype Rename_Problem_Indefinite_Holder is
     Rename_Problem_Indefinite_Holders.Holder;

   type Specific_Problem_Finder_Result (Success : Boolean) is
      record
         case Success is
            when True =>
               null;
            when False =>
               Problem : Rename_Problem_Indefinite_Holder;
         end case;
      end record;

   function Create_Rename_Problem
     (Problem : Rename_Problem'Class)
      return Specific_Problem_Finder_Result
     with Post => Create_Rename_Problem'Result.Success = False;

   No_Rename_Problem : constant Specific_Problem_Finder_Result :=
     Specific_Problem_Finder_Result'(Success => True);

   type Specific_Problem_Finder is interface;

   function Find
     (Self : Specific_Problem_Finder)
      return Specific_Problem_Finder_Result is abstract;
   --  Finds specific problems caused by renaming a definition

   package Specific_Rename_Problem_Finder_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Natural,
        Element_Type => Specific_Problem_Finder'Class);

   type Name_Collision_Finder is new Specific_Problem_Finder with
      record
         Canonical_Definition : Defining_Name;
         New_Name             : New_Name_Access;
      end record;

   overriding
   function Find
     (Self : Name_Collision_Finder)
      return Specific_Problem_Finder_Result;
   --  Find name collisions created by a rename operation in the same scope.

   type Enum_Name_Collision_Finder is new Specific_Problem_Finder with
      record
         Canonical_Definition : Defining_Name;
         New_Name             : New_Name_Access;
      end record;

   overriding
   function Find
     (Self : Enum_Name_Collision_Finder)
      return Specific_Problem_Finder_Result;
   --  Find name collisions created by a rename operation of an enum literal

   type Collision_With_Compilation_Unit_Finder (Units_Length : Integer) is new
     Specific_Problem_Finder with
      record
         Canonical_Definition : Defining_Name;
         New_Name             : New_Name_Access;
         Units                : Analysis_Unit_Array (1 .. Units_Length);
      end record;

   overriding
   function Find
     (Self : Collision_With_Compilation_Unit_Finder)
      return Specific_Problem_Finder_Result;
   --  Find name collisions with a compilation unit created by a rename
   --  operation.
   --  Example: Is package Foo has a definition Bar, and a child compilation
   --  unit Foo.Baz, then renaming Bar to Baz creates a collision that is
   --  detected by this function.

   type Compilation_Unit_Collision_Finder (Units_Length : Integer) is new
     Specific_Problem_Finder with
      record
         Canonical_Definition : Defining_Name;
         New_Name             : New_Name_Access;
         Units                : Analysis_Unit_Array (1 .. Units_Length);
      end record;

   overriding
   function Find
     (Self : Compilation_Unit_Collision_Finder)
      return Specific_Problem_Finder_Result;
 --  Find name collisions between a compilation units created by a rename
 --  operation.

   type Subp_Overriding_Finder is new Specific_Problem_Finder with
      record
         Canonical_Definition : Defining_Name;
         New_Name             : New_Name_Access;
      end record;

   overriding
   function Find
     (Self : Subp_Overriding_Finder)
      return Specific_Problem_Finder_Result;
   --  Checks if renaming a subprogram will start overriding another one.
   --  TODO: Also check if it will start being overriden.

   type Param_Spec_Collision_Finder is new Specific_Problem_Finder with
      record
         Canonical_Definition : Defining_Name;
         New_Name             : New_Name_Access;
         Reference            : Base_Id := No_Base_Id;
      end record;

   overriding
   function Find
     (Self : Param_Spec_Collision_Finder)
      return Specific_Problem_Finder_Result;
   --  Checks if renaming a Param_Spec will cause a conflict with its type.

   type Name_Hiding_Finder is new Specific_Problem_Finder with
      record
         Canonical_Definition : Defining_Name;
         New_Name             : New_Name_Access;
      end record;

   overriding
   function Find
     (Self : Name_Hiding_Finder)
      return Specific_Problem_Finder_Result;
   --  Checks if renaming a definition will start hiding another one.

   type Subtype_Indication_Collision_Finder is new Specific_Problem_Finder with
      record
         Canonical_Definition : Defining_Name;
         References           : Base_Id_Vectors.Vector;
         New_Name             : New_Name_Access;
      end record;

   overriding
   function Find
     (Self : Subtype_Indication_Collision_Finder)
      return Specific_Problem_Finder_Result;
   --  Renaming a rype operation will also rename its references in
   --  subtype indications of subprograms specs. This function checks if
   --  renaming such references will cause a collision with its paramater.

   type Name_Hidden_Finder is new Specific_Problem_Finder with
      record
         Canonical_Definition : Defining_Name;
         References           : Base_Id_Vectors.Vector;
         New_Name             : New_Name_Access;
      end record;

   overriding
   function Find
     (Self : Name_Hidden_Finder)
      return Specific_Problem_Finder_Result;
   --  Checks if renaming a definition will make it hidden by another one

   type Safe_Renamer is new Refactoring_Tool with
      record
         --  Canonical Defining_Name that we want to rename
         Canonical_Definition          : Defining_Name;
         New_Name                      : Unbounded_Text_Type;
         Algorithm                     : Problem_Finder_Algorithm_Kind;
         Naming_Scheme                 : Naming_Scheme_Type;
         Attribute_Value_Provider      : Attribute_Value_Provider_Access;
         GPR2_Attribute_Value_Provider : GPR2_Attribute_Value_Provider_Access;
      end record;

   procedure Add_References_To_Edits
     (Self       : Safe_Renamer;
      References : Base_Id_Vectors.Vector;
      Edits      : in out Refactoring_Edits);
   --  Adds `References` to `Edits`

   function Is_Top_Level_Decl
     (Self : Safe_Renamer;
      Decl : Basic_Decl'Class)
      return Boolean;
   --  True if `Decl` is a unit top level declaration.
   --  These are the only declarations that lead to also renaming the source
   --  file.

   procedure Add_Files_Rename_To_Edits
     (Self       : Safe_Renamer;
      References : Base_Id_Vectors.Vector;
      Edits      : in out Refactoring_Edits)
     with Pre => Self.Is_Top_Level_Decl
                   (Self.Canonical_Definition.P_Basic_Decl);
   --  Iterates through all parts of `Self.Canonical_Definition` and adds the
   --  necessary file renames to `Edits`.

end Laltools.Refactor.Safe_Rename;
