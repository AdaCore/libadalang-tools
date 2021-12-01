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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Libadalang.Analysis; use Libadalang.Analysis;

with Templates_Parser;

with TGen.Gen_Strategies_Utils; use TGen.Gen_Strategies_Utils;
with TGen.Strings;              use TGen.Strings;
with TGen.Templates;            use TGen.Templates;
with TGen.Types;                use TGen.Types;
with TGen.Files; use TGen.Files;

package TGen.Gen_Strategies is

   type Generation_Context is record
      Project : Project_Type;

      Output_Dir : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   GC : Generation_Context;

   function Get_Gen_Directory (Prj : Project_Type) return Virtual_File is
     (GNATCOLL.VFS.Create
        (Filesystem_String (TGen.Strings."+" (GC.Output_Dir))));

   function Get_Strat_ADB (Prj : Project_Type) return Virtual_File is
     (Get_Gen_Directory (Prj) / Strat_ADB);

   function Gen_File (File : String) return Virtual_File is
     (Get_Gen_Directory (Prj_Tree.Root_Project) /
          Filesystem_String (File));

   procedure Prepare_Output_Dirs (GC : Generation_Context);

   function "<" (L : Defining_Name; R : Defining_Name) return Boolean is
     (Image (L.P_Fully_Qualified_Name) < Image (R.P_Fully_Qualified_Name));

   function "<" (L, R : SP.Ref) return Boolean is
     (L.Get.Name < R.Get.Name);

   function "=" (L, R : SP.Ref) return Boolean is
     (L.Get.Name = R.Get.Name);

   package Typ_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => SP.Ref,
      "=" => SP."=");

   subtype Typ_Set is Typ_Sets.Set;

   package Type_Vectors_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Unbounded_Text_Type,
      Element_Type => Typ_Set,
      "<" => Ada.Strings.Wide_Wide_Unbounded."<",
      "=" => Typ_Sets."=");

   subtype Type_Vectors_Map is Type_Vectors_Maps.Map;

   Required_Type_Strategies : Type_Vectors_Map;
   --  Type for which we must generate generation strategies

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

   package Strategy_Generator is
      type Strat_Generator is new Source_Code_File_Generator with private;

      overriding
      procedure Generate_Source_Code
        (Self    : Strat_Generator;
         Ctx : TGen.Templates.Context'Class);
      --  Generates the Data Factory source file based on the Data Factory
      --  template.

      function Create
        (Strat_Source_File    : GNATCOLL.VFS.Virtual_File;
         Strat_Template_File  : GNATCOLL.VFS.Virtual_File;
         Pkg_Data         : Package_Data)
         return Strat_Generator;
      --  Constructor of a DF_Generator. 'DF_Source_File' is the output
      --  Fuzz Test Harness source file and 'DF_Template_File' is the template
      --  used to generate it.

   private
      type Strat_Generator is new Source_Code_File_Generator with
         record
            Strat_Source_File   : GNATCOLL.VFS.Virtual_File;
            Strat_Template_File : GNATCOLL.VFS.Virtual_File;
            Pkg_Data            : Package_Data;
         end record;

      type Param_Strategy_Translator is new
        Translator_Container with
         record
            Pkg_Data : Package_Data;
         end record;

      function Create_Param_Strategy_Translator
        (Pkg_Data : Package_Data;
         Next            : access constant Translator'Class := null)
         return Param_Strategy_Translator;
      --  With_Type_Parent_Package_Table_Translator constructor

      overriding
      procedure Translate_Helper
        (Self  : Param_Strategy_Translator;
         Table : in out Templates_Parser.Translate_Set);
   end Strategy_Generator;

   package Type_Strategy_Generator is
      type Type_Strat_Generator is new Source_Code_File_Generator with private;

      overriding
      procedure Generate_Source_Code
        (Self    : Type_Strat_Generator;
         Ctx : TGen.Templates.Context'Class);
      --  Generates the Data Factory source file based on the Data Factory
      --  template.

      function Create
        (Type_Strat_Source_File    : GNATCOLL.VFS.Virtual_File;
         Type_Strat_Template_File  : GNATCOLL.VFS.Virtual_File;
         Types                     : Typ_Set)
         return Type_Strat_Generator;
      --  Constructor of a DF_Generator. 'DF_Source_File' is the output
      --  Fuzz Test Harness source file and 'DF_Template_File' is the template
      --  used to generate it.

   private
      type Type_Strat_Generator is new Source_Code_File_Generator with
         record
            Type_Strat_Source_File   : GNATCOLL.VFS.Virtual_File;
            Type_Strat_Template_File : GNATCOLL.VFS.Virtual_File;
            Types            : Typ_Set;
         end record;

      type Type_Strategy_Translator is new
        Translator_Container with
         record
            Types : Typ_Set;
         end record;

      function Create_Type_Strategy_Translator
        (Types : Typ_Set;
         Next  : access constant Translator'Class := null)
         return Type_Strategy_Translator;
      --  With_Type_Parent_Package_Table_Translator constructor

      overriding
      procedure Translate_Helper
        (Self  : Type_Strategy_Translator;
         Table : in out Templates_Parser.Translate_Set);
   end Type_Strategy_Generator;

   package Test_Generator is
      type Test_Generator is new Source_Code_Generator with private;

      overriding
      function Generate_Source_Code
        (Self    : Test_Generator;
         Ctx : TGen.Templates.Context'Class) return Wide_Wide_String;
      --  Generates the Data Factory source file based on the Data Factory
      --  template.

      function Create
        (Test_Template_File  : GNATCOLL.VFS.Virtual_File;
         Subp                     : Subprogram_Data)
         return Test_Generator;

   private
      type Test_Generator is new Source_Code_Generator with
         record
            Test_Template_File : GNATCOLL.VFS.Virtual_File;
            Subp            : Subprogram_Data;
         end record;

      type Test_Translator is new
        Translator_Container with
         record
            Subp : Subprogram_Data;
         end record;

      function Create_Test_Translator
        (Subp : Subprogram_Data;
         Next  : access constant Translator'Class := null)
         return Test_Translator;
      --  With_Type_Parent_Package_Table_Translator constructor

      overriding
      procedure Translate_Helper
        (Self  : Test_Translator;
         Table : in out Templates_Parser.Translate_Set);
   end Test_Generator;

   procedure Generate_Type_Strategies;

end TGen.Gen_Strategies;
