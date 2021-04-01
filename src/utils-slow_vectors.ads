------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2013-2021, AdaCore                    --
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

with Ada.Containers.Vectors;

generic
   type Index_Type is range <>;
   type Element_Type is private;
   type Elements_Array is array (Index_Type range <>) of Element_Type;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Utils.Slow_Vectors is

   --  This is a wrapper around Ada.Containers.Vectors that provides a few
   --  extra operations. The intended usage of an instance X_Vectors is:
   --     subtype X_Vector is X_Vectors.Vector;
   --     use X_Vectors;
   --     use all type X_Vector;

   package Vectors is new Ada.Containers.Vectors
     (Index_Type,
      Element_Type,
      "=");

   subtype Vector is Vectors.Vector;

   --  Renamings of things that "use all type" won't make visible:

   subtype Extended_Index is Vectors.Extended_Index;
   No_Index : Extended_Index renames Vectors.No_Index;
   Empty_Vector : Vector renames Vectors.Empty_Vector;
   generic package Generic_Sorting renames Vectors.Generic_Sorting;

   --  Extra operations:

   procedure Free (Container : in out Vector);
   --  Same as Clear, but also frees storage

   function Slice
     (V     : Vector;
      First : Index_Type;
      Last  : Vectors.Extended_Index)
      return  Elements_Array with
      Post => Slice'Result'First = Index_Type'First;

   function To_Array (V : Vector) return Elements_Array with
      Post => To_Array'Result'First = Index_Type'First;

   procedure Append (V : in out Vector; A : Elements_Array);

end Utils.Slow_Vectors;
