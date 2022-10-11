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
--  Main entry point of the value generation part of this library. Provides
--  procedure to generate testing strategies and test values for subprograms
--  given as Libadalang nodes.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Libadalang.Analysis; use Libadalang.Analysis;

with TGen.Context;              use TGen.Context;
with TGen.JSON;        use TGen.JSON;
with TGen.Strings;              use TGen.Strings;
with TGen.Types;                use TGen.Types;
with TGen.Subprograms;          use TGen.Subprograms;

package TGen.Gen_Strategies is

   procedure Initialize
     (Context    : in out Generation_Context;
      Output_Dir : Unbounded_String);
   --  Set the output dir of Context, and prepare output directories. This does
   --  not clear the context.

   procedure Generate_Artifacts (Context : in out Generation_Context);
   --  Emit all artifacts after test generation is done. This includes the
   --  JSON statically (single pass) generated values and sources required to
   --  perform dynamic (two-pass) generation.

   procedure Generate_Test_Vectors
     (Context  : in out Generation_Context;
      Nb_Tests : Positive;
      Subp     : Subp_Decl'Class;
      Subp_UID : Unbounded_String := Null_Unbounded_String);
   --  Generate values for all types that support static (single pass)
   --  generation.
   --  TODO: Handle (or not) subprocedure renaming in the spec.
   --  This is not the case currently
   --  TODO: Incorporate dynamic (two-pass) generation?

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

   function Indent (Amount : Natural; Str : String) return String;

   function Number_Of_Lines (Str : String) return Natural;

   procedure Dump_JSON
     (Context : Generation_Context);
   --  Output the results of the static (single pass) generation to the output
   --  JSON files.

end TGen.Gen_Strategies;
