------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2013-2022, AdaCore                    --
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

with Ada.Containers; use Ada.Containers;
with Ada.Iterator_Interfaces;

private with Ada.Finalization;

generic
   type Index_Type is range <>;
   type Element_Type is private;
   type Elements_Array is array (Index_Type range <>) of Element_Type;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Utils.Fast_Vectors is

   --  This is a more efficient version of Ada.Containers.Vectors.

   pragma Suppress (All_Checks);

   pragma Assert (Index_Type'First = 1);
   pragma Assert (Index_Type'Last >= 2**24 - 1);
   --  These assumptions allow us to avoid a lot of horsing around. But we
   --  still inherit some such horsing from Ada.Containers.Vectors.

   subtype Extended_Index is
     Index_Type'
       Base range
       Index_Type'First - 1 ..
         Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   type Vector is tagged private with
      Constant_Indexing => Constant_Reference,
      Variable_Indexing => Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type;

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   package Vector_Iterator_Interfaces is new Ada.Iterator_Interfaces
     (Cursor,
      Has_Element);

   Empty_Vector : constant Vector;

   overriding function "=" (Left, Right : Vector) return Boolean;

   function Length (Container : Vector) return Count_Type;

   procedure Set_Length (Container : in out Vector; Length : Count_Type);

   function Is_Empty (Container : Vector) return Boolean;

   procedure Clear (Container : in out Vector);

   procedure Free (Container : in out Vector);
   --  Same as Clear, but also frees storage

   function To_Cursor
     (Container : Vector;
      Index     : Extended_Index)
      return      Cursor;

   function To_Index (Position : Cursor) return Extended_Index;

   function Element
     (Container : Vector;
      Index     : Index_Type)
      return      Element_Type;

   function Element (Position : Cursor) return Element_Type;

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is private with
      Implicit_Dereference => Element;

   type Reference_Type (Element : not null access Element_Type) is private with
      Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Vector;
      Position  : Cursor)
      return      Constant_Reference_Type;

   function Reference
     (Container : aliased in out Vector;
      Position  : Cursor)
      return      Reference_Type;

   function Constant_Reference
     (Container : aliased Vector;
      Index     : Index_Type)
      return      Constant_Reference_Type;

   function Reference
     (Container : aliased in out Vector;
      Index     : Index_Type)
      return      Reference_Type;

   procedure Move (Target : in out Vector; Source : in out Vector);

   procedure Append (Container : in out Vector; New_Item : Element_Type);
   procedure Push (Container : in out Vector; New_Item : Element_Type)
     renames Append;

   type Element_Access is access all Element_Type;
   function Last_Ptr (Container : in out Vector) return Element_Access;
   function Append (Container : in out Vector) return Element_Access;

   procedure Delete_Last (Container : in out Vector);
   procedure Pop (Container : in out Vector) renames Delete_Last;

   function First (Container : Vector) return Cursor;

   function Last_Index (Container : Vector) return Extended_Index;

   function Last (Container : Vector) return Cursor;

   function Last_Element (Container : Vector) return Element_Type;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;

   procedure Previous (Position : in out Cursor);

   procedure Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : Cursor));

   function Iterate
     (Container : Vector)
      return      Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate
     (Container : Vector;
      Start     : Cursor)
      return      Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   package Generic_Sorting is

      function Is_Sorted (Container : Vector) return Boolean;

      procedure Sort (Container : in out Vector);

      procedure Merge (Target : in out Vector; Source : in out Vector);

   end Generic_Sorting;

   --  Extra operations not in Ada.Containers.Vectors:

   subtype Big_Elements_Array is Elements_Array (Index_Type);
   type Big_Ptr is access constant Big_Elements_Array;
   pragma No_Strict_Aliasing (Big_Ptr);
   type Big_Ptr_Var is access all Big_Elements_Array;
   pragma No_Strict_Aliasing (Big_Ptr_Var);

   function Elems (Container : Vector) return Big_Ptr; -- with
--      Post => Elems'Result'First = Index_Type'First;
   function Elems_Var (Container : Vector) return Big_Ptr_Var; -- with
--      Post => Elems_Var'Result'First = Index_Type'First;
--  ???Above postconditions cause warnings These return a pointer to the
--  underlying data structure. This is of course dangerous. The idea is
--  that you can do:
   --
   --     X : Elems_Array renames Elems (V) (1 .. Last_Index (V));
   --
   --  But don't do Append (etc) while X still exists. Do not call these
   --  without the slicing.

   type Subrange is record
      First : Index_Type;
      Last : Extended_Index;
   end record;

   function Full_Range (Container : Vector) return Subrange is
     (1, Last_Index (Container));

   function Slice
     (Container : Vector;
      First     : Index_Type;
      Last      : Extended_Index)
      return      Elements_Array with
      Post => Slice'Result'First = Index_Type'First;

   function Slice (Container : Vector; R : Subrange) return Elements_Array is
     (Slice (Container, R.First, R.Last));

   function To_Array (Container : Vector) return Elements_Array with
      Post => To_Array'Result'First = Index_Type'First;

   procedure Append (Container : in out Vector; New_Items : Elements_Array);

   procedure Put
     (Container : Vector;
      Put : not null access procedure (Item : Element_Type);
      Put_Between : not null access procedure);
   --  Prints out the Container for debugging. We don't want to depend on any
   --  output packages here, so you must pass in Put (to print out an element)
   --  and Put_Between (to print out a separator between elements).

private

   pragma Inline (Append);
   pragma Inline (Constant_Reference);
   pragma Inline (Clear);
   pragma Inline (Reference);
   pragma Inline (Last_Index);
   pragma Inline (Element);
   pragma Inline (Last_Element);
   pragma Inline (Is_Empty);

   function "=" (L, R : Elements_Array) return Boolean is abstract;

   type Elements_Type (Last : Extended_Index) is limited record
      EA : aliased Elements_Array (Index_Type'First .. Last);
   end record;

   Empty_Elements : aliased Elements_Type := (Last => 0, EA => [others => <>]);

   type Elements_Access is access all Elements_Type;

   use Ada.Finalization;

   type Vector is new Controlled with record
      Elements : Elements_Access := Empty_Elements'Access;
      Last     : Extended_Index  := No_Index;
   end record;

   overriding procedure Adjust (Container : in out Vector);

   overriding procedure Finalize (Container : in out Vector);

   type Vector_Access is access all Vector;

   type Cursor is record
      Container : Vector_Access;
      Index     : Index_Type := Index_Type'First;
   end record;

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is null record;

   type Reference_Type (Element : not null access Element_Type) is null record;

   No_Element : constant Cursor := Cursor'(null, Index_Type'First);

   Empty_Vector : constant Vector := (Controlled with others => <>);

end Utils.Fast_Vectors;
