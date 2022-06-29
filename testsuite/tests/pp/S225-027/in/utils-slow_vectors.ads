------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                               V E C T O R S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                      Copyright (C) 2013-2017, AdaCore                    --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
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

   package Vectors is new Ada.Containers.Vectors (Index_Type, Element_Type,
      "=");

   subtype Vector is Vectors.Vector;

   --  Renamings of things that "use all type" won't make visible:

   subtype Extended_Index is Vectors.Extended_Index;
   No_Index     : Extended_Index renames Vectors.No_Index;
   Empty_Vector : Vector renames Vectors.Empty_Vector;
   generic package Generic_Sorting renames Vectors.Generic_Sorting;

   --  Extra operations:

   procedure Free (Container : in out Vector);
   --  Same as Clear, but also frees storage

   function Slice
     (V : Vector; First : Index_Type; Last : Vectors.Extended_Index)
      return Elements_Array with
     Post => Slice'Result'First = Index_Type'First;

   function To_Array (V : Vector) return Elements_Array with
     Post => To_Array'Result'First = Index_Type'First;

   procedure Append (V : in out Vector; A : Elements_Array);

end Utils.Slow_Vectors;
