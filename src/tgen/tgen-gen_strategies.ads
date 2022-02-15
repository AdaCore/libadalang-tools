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
with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.JSON;     use GNATCOLL.JSON;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Libadalang.Analysis; use Libadalang.Analysis;

with Templates_Parser;

with TGen.Context;              use TGen.Context;
with TGen.Gen_Strategies_Utils; use TGen.Gen_Strategies_Utils;
with TGen.Strings;              use TGen.Strings;
with TGen.Templates;            use TGen.Templates;
with TGen.Types;                use TGen.Types;
with TGen.Files; use TGen.Files;

package TGen.Gen_Strategies is

   procedure Initialize
     (Context    : in out Generation_Context;
      Project    : Project_Type;
      Output_Dir : Unbounded_String);

   procedure Generate_Artifacts (Context : in out Generation_Context);

   procedure Generate_Test_Vectors
     (Context  : in out Generation_Context;
      Nb_Tests : Positive;
      Subp     : Subprogram_Data;
      Subp_UID : Unbounded_String := Null_Unbounded_String);

   type Generated_Body is
      record
         With_Clauses : String_Sets.Set;
         Generated_Body : Unbounded_Text_Type;
      end record;

   function Generate_Tests_For
     (Context   : in out Generation_Context;
      Nb_Tests  : Positive;
      Subp      : Subp_Decl) return Generated_Body;
   --  Generate a body for the given procedure. Generation of artifact files
   --  (containing generation procedures) is deferred to the finalization of
   --  the Context. Return the generated body and the needed `with` clauses to
   --  insert with the body.

   type Type_Data is
      record
         Type_Name                 : Unbounded_Text_Type;
         Type_Fully_Qualified_Name : Unbounded_Text_Type;
         Type_Parent_Package       : Unbounded_Text_Type;
      end record;

   ----------------------------------
   -- Distinct_Type_Parent_Package --
   ----------------------------------

   function Distinct_Type_Parent_Package
     (Parameters_Data : Parameters_Data_Vector)
      return String_Ordered_Set;
   --  Returns a set with the Type_Parent_Package of Parameters_Data

   ----------------------------------
   -- Distinct_Type_Parent_Package --
   ----------------------------------

   function Distinct_Type_Parent_Package
     (Subprogram_Data : Subprograms_Data_Vector)
      return String_Ordered_Set;
   --  Returns a set with the Type_Parent_Package of Parameters_Data

   function Get_Unique_Strat_Name
     (F : Subp_Spec;
      P : Identifier) return String is
     (Image (Text (F.F_Subp_Name)) & "_" & Image (Text (P)));

   function Get_Strategy_Signature
     (Self : Typ'Class;
      F    : Subp_Spec;
      P    : Identifier) return String is
     ("function " & Get_Unique_Strat_Name (F, P) & " return "
      & Image (Self.Name.P_Fully_Qualified_Name) & ";");

   function Get_Strategy_Spec
     (Self : Typ;
      F    : Subp_Spec;
      P    : Identifier) return String is
     (Get_Strategy_Signature (Self, F, P) & ";");

   package Test_Generator is
      type Test_Generator is new Source_Code_Generator with private;

      overriding
      function Generate_Source_Code
        (Self : Test_Generator;
         Ctx  : TGen.Templates.Context'Class) return Wide_Wide_String;
      --  Generates the Data Factory source file based on the Data Factory
      --  template.

      function Create
        (Test_Template_File : GNATCOLL.VFS.Virtual_File;
         Subp               : Subprogram_Data) return Test_Generator;

   private
      type Test_Generator is new Source_Code_Generator with
         record
            Test_Template_File : GNATCOLL.VFS.Virtual_File;
            Subp               : Subprogram_Data;
         end record;

      type Test_Translator is new
        Translator_Container with
         record
            Subp : Subprogram_Data;
         end record;

      function Create_Test_Translator
        (Subp : Subprogram_Data;
         Next : access constant Translator'Class := null)
         return Test_Translator;
      --  With_Type_Parent_Package_Table_Translator constructor

      overriding
      procedure Translate_Helper
        (Self  : Test_Translator;
         Table : in out Templates_Parser.Translate_Set);
   end Test_Generator;

   function Indent (Amount : Natural; Str : String) return String;

   function Number_Of_Lines (Str : String) return Natural;

   procedure Generate_Type_Strategies
     (Context : Generation_Context);

   procedure Dump_JSON
     (Context : Generation_Context);

end TGen.Gen_Strategies;
