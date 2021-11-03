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

with Libadalang.Analysis;

with Ada.Unchecked_Deallocation;

with GNATCOLL.Refcount; use GNATCOLL.Refcount;

package TGen.Types is

   package LAL renames Libadalang.Analysis;
   type Typ is tagged record
      Name : LAL.Defining_Name;
   end record;

   function Image (Self : Typ) return String;

   type Scalar_Typ (Is_Static : Boolean) is new Typ with null record;

   type Discrete_Typ is new Scalar_Typ with null record;

   function Lit_Image (Self : Discrete_Typ; Lit : Integer) return String;
   --  Returns the image of the Litteral whose "position" is Lit. For integer
   --  types, this is simply Lit'Image, for enum types, this correponds to
   --  the image of the enum litteral at position Lit.

   type Access_Typ is new Typ with null record;

   type Composite_Typ is new Typ with null record;

   package SP is new Shared_Pointers (Element_Type => Typ'Class);

   --  type Typ_Acc is access all Typ'Class;

   --  procedure Free is new Ada.Unchecked_Deallocation (Typ'Class, Typ_Acc);

end TGen.Types;
