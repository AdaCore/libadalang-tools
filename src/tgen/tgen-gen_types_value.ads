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
--  This unit provides functionnalities to generate source code to generate
--  Ada values at runtime. Used when static generation is not available for
--  a given type.
--  TODO: this unit is still a work in progress

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Templates_Parser; use Templates_Parser;

with TGen.Context;    use TGen.Context;
with TGen.Strategies; use TGen.Strategies;
with TGen.Templates;  use TGen.Templates;
with TGen.Types;      use TGen.Types;

package TGen.Gen_Types_Value is
   type Type_Value_Generator is new Source_Code_File_Generator with private;

   overriding
   procedure Generate_Source_Code
     (Self : Type_Value_Generator;
      Ctx  : TGen.Templates.Context'Class);
   --  Generate the package (body and spec) instantiating all the generation
   --  functions for the supported types.

   function Create
     (Context    : Generation_Context;
      Strategies : Strategy_Set) return Type_Value_Generator;
   --  Create Ada packages with generation function for the given (supported)
   --  types. Type_Depends is the list of typse needed to build instances of
   --  the given types (e.g. if A is a record type with an Integer component,
   --  Types will contain A, and Type_Depends will contain Integer.
   --
   --  Pkg_Name is an Ada qualified name (e.g. Foo.Bar), and is the parent
   --  package of the type declaration.

private
   type Type_Value_Generator is new Source_Code_File_Generator with
      record
         Context      : Generation_Context;
         Strategies   : Strategy_Set;
      end record;

   type Type_Value_Translator is abstract new Translator_Container with
      record
         Strategies : Strategy_Set;
      end record;

   type Type_Value_ADS_Translator is
     new Type_Value_Translator with record
      Context : Generation_Context;
   end record;
   type Type_Value_ADB_Translator is
     new Type_Value_Translator with record
      Context : Generation_Context;
   end record;

   function Create_Type_Value_ADS_Translator
     (Context    : Generation_Context;
      Strategies : Strategy_Set;
      Next    : access constant Translator'Class := null)
      return Type_Value_ADS_Translator;

   function Create_Type_Value_ADB_Translator
     (Context    : Generation_Context;
      Strategies : Strategy_Set;
      Next       : access constant Translator'Class := null)
      return Type_Value_ADB_Translator;

   overriding
   procedure Translate_Helper
     (Self  : Type_Value_ADS_Translator;
      Table : in out Templates_Parser.Translate_Set);

   overriding
   procedure Translate_Helper
     (Self  : Type_Value_ADB_Translator;
      Table : in out Templates_Parser.Translate_Set);

end TGen.Gen_Types_Value;
