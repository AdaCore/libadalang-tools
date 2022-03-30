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
--  Utlities to generate value generation code from templates.
--  TODO: this package is still a work in progress.

with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Templates_Parser;

package TGen.Templates is

   type Translator is interface;

   type Context is new Ada.Finalization.Controlled with record

      Project : Project_Type;

      Output_Dir : Unbounded_String;
      --  Directory for generated artifacts

   end record;

   procedure Translate
     (Self  : Translator;
      Table : in out Templates_Parser.Translate_Set)
   is abstract;
   --  Creates associations between template tags and the data they should be
   --  filled with.

   type Translator_Container (Next : access constant Translator'Class) is
     abstract new Translator with null record;
   --  This container type can be used to chain 'Translator's together and
   --  pass the request to 'Translate' the 'Parameters_Data' through all the
   --  linked translators.

   overriding
   procedure Translate
     (Self  : Translator_Container;
      Table : in out Templates_Parser.Translate_Set);
   --  Dispaches a call to Translate_Helper and and then to 'Next.Translate'
   --  if 'Next' is not null.

   procedure Translate_Helper
     (Self  : Translator_Container;
      Table : in out Templates_Parser.Translate_Set) is abstract;
   --  Fill Table with tag-value(s) associations

   type Source_Code_Generator is interface;

   function Generate_Source_Code
     (Self    : Source_Code_Generator;
      Ctx     : Context'Class) return Wide_Wide_String
   is abstract;
   --  Generates a source code file

   type Source_Code_File_Generator is interface;

   procedure Generate_Source_Code
     (Self    : Source_Code_File_Generator;
      Ctx     : Context'Class)
   is abstract;
   --  Generates a source code file

end TGen.Templates;
