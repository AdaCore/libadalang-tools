------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                      Copyright (C) 2021-2022, AdaCore                    --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;

with TGen.Strings; use TGen.Strings;

package TGen.LAL_Utils is

   function "+" (Text : Text_Type) return String renames To_UTF8;

   function "+" (Str : String) return Text_Type renames From_UTF8;

   function "+" (T : Text_Type) return Unbounded_Text_Type
                 renames To_Unbounded_Text;

   function "+" (T : Unbounded_Text_Type) return Text_Type
                 renames To_Text;

   function "+" (Text : Unbounded_Text_Type) return String is
     (+(+Text));

   function "+" (Str : String) return Unbounded_Text_Type is
     (+(+Str));

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Name) return Ada_Qualified_Name;
   --  Return the qualified name corresponding to the given name from a parse
   --  tree.

   function Convert_Qualified_Name
     (Text_QN : Libadalang.Analysis.Unbounded_Text_Type_Array)
      return Ada_Qualified_Name;

   function JSON_Test_Filename
     (Subp : Libadalang.Analysis.Basic_Decl) return String with
     Pre => Subp.P_Is_Subprogram;
   --  Return the simple name for the JSON file in which the tests for Subp
   --  should be stored.

   function Default_Blob_Test_Filename
     (Subp : Libadalang.Analysis.Basic_Decl) return String with
     Pre => Subp.P_Is_Subprogram;
   --  Return the default simple name for the binary file in which a test
   --  for Subp should be stored. It has no extension, so it can be suffixed
   --  or prefixed with anything to differentiate various test cases for the
   --  same subprogram.

end TGen.LAL_Utils;
