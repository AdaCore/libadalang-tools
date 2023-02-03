------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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
with Ada.Text_IO; use Ada.Text_IO;

with Templates_Parser; use Templates_Parser;

with TGen.Types;             use TGen.Types;

package TGen.Type_Representation is

   procedure Generate_Type_Representation_For_Typ
     (F_Spec, F_Body     : File_Type;
      Typ                : TGen.Types.Typ'Class;
      Templates_Root_Dir : String;
      Init_Package_Code  : in out Tag);
   --  Generate the TGen type representation for the given type. Note that
   --  this function is not recursive, and must thus be called for all of
   --  the component types of this type that are not anonymous types.
   --
   --  For all of the component types that are anonymous types, type
   --  type definitions will be generated (they still require the ancestor
   --  type's type definition though).

end TGen.Type_Representation;
