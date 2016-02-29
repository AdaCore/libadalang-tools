------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                               V E C T O R S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2016, AdaCore                     --
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

pragma Ada_2012;

with Ada.Containers.Generic_Array_Sort;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with System;
use type System.Address;

package body ASIS_UL.Fast_Vectors is

   pragma Suppress (All_Checks);

   procedure Free is new Ada.Unchecked_Deallocation
     (Elements_Type,
      Elements_Access);

   type Iterator is new Vector_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Vector_Access;
      Index     : Index_Type'Base;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor)
      return     Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor)
      return     Cursor;

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Vector) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Last /= Right.Last then
         return False;
      end if;

      for J in Index_Type range Index_Type'First .. Left.Last loop
         if Left.Elements.EA (J) /= Right.Elements.EA (J) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out Vector) is
   begin
      if Container.Last = No_Index then
         Container.Elements := Empty_Elements'Access;
         return;
      end if;

      declare
         L : constant Index_Type := Container.Last;
         EA :
           Elements_Array renames
           Container.Elements.EA (Index_Type'First .. L);

      begin
         Container.Elements := Empty_Elements'Access;

         --  Note: it may seem that the following assignment to Container.Last
         --  is useless, since we assign it to L below. However this code is
         --  used in case 'new Elements_Type' below raises an exception, to
         --  keep Container in a consistent state.

         Container.Last     := No_Index;
         Container.Elements := new Elements_Type'(L, EA);
         Container.Last     := L;
      end;
   end Adjust;

   procedure Append (Container : in out Vector; New_Item : Element_Type) is
   begin
      Append (Container).all := New_Item;
   end Append;

   function Append (Container : in out Vector) return Element_Access is
      pragma Assert (Index_Type'First = 1);
      New_Last : constant Index_Type'Base := Container.Last + 1;
      New_Elts : Elements_Access;
   begin
      if Container.Last = Container.Elements.Last then
         if Container.Last = 0 then
            pragma Assert (Container.Elements = Empty_Elements'Access);
            New_Elts := new Elements_Type (Last => 2**10);
         else
            New_Elts := new Elements_Type (Last => 2 * Container.Last);
            New_Elts.EA (1 .. Container.Last) := Container.Elements.EA;
            Free (Container.Elements);
         end if;
         Container.Elements := New_Elts;
      end if;

      Container.Last := New_Last;
      return Container.Elements.EA (New_Last)'Unrestricted_Access;
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Vector) is
   begin
      Container.Last := No_Index;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Vector;
      Position  : Cursor)
      return      Constant_Reference_Type
   is
   begin
      return R : constant Constant_Reference_Type :=
        (Element =>
           Container.Elements.EA (Position.Index)'Unrestricted_Access);
   end Constant_Reference;

   function Constant_Reference
     (Container : aliased Vector;
      Index     : Index_Type)
      return      Constant_Reference_Type
   is
   begin
      pragma Assert (Index in 1 .. Last_Index (Container));
      return R : constant Constant_Reference_Type :=
        (Element => Container.Elements.EA (Index)'Unrestricted_Access);
   end Constant_Reference;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in out Vector) is
   begin
      Container.Last := Container.Last - 1;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector;
      Index     : Index_Type)
      return      Element_Type
   is
   begin

      return Container.Elements.EA (Index);
   end Element;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Container.Elements.EA (Position.Index);
   end Element;

   --------------
   -- Elements --
   --------------

   function Elems (Container : Vector) return Big_Ptr is
      function Cast is new Ada.Unchecked_Conversion (System.Address, Big_Ptr);
   begin
      return Cast (Container.Elements.EA'Address);
   end Elems;

   ------------------
   -- Elems_Var --
   ------------------

   function Elems_Var (Container : Vector) return Big_Ptr_Var is
      function Cast is new Ada.Unchecked_Conversion
        (System.Address,
         Big_Ptr_Var);
   begin
      return Cast (Container.Elements.EA'Address);
   end Elems_Var;

   -----------
   -- First --
   -----------

   function First (Container : Vector) return Cursor is
   begin
      if Is_Empty (Container) then
         return No_Element;
      else
         return (Container'Unrestricted_Access, Index_Type'First);
      end if;
   end First;

   function First (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Index component influences the
      --  behavior of the First (and Last) selector function.

      --  When the Index component is No_Index, this means the iterator
      --  object was constructed without a start expression, in which case the
      --  (forward) iteration starts from the (logical) beginning of the entire
      --  sequence of items (corresponding to Container.First, for a forward
      --  iterator).

      --  Otherwise, this is iteration over a partial sequence of items.
      --  When the Index component isn't No_Index, the iterator object was
      --  constructed with a start expression, that specifies the position
      --  from which the (forward) partial iteration begins.

      if Object.Index = No_Index then
         return First (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Index);
      end if;
   end First;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Container : in out Vector) is
   begin
      if Container.Elements = Empty_Elements'Access then
         pragma Assert (Container.Last = No_Index);
      else
         Free (Container.Elements);
         Container.Elements := Empty_Elements'Access;
         Container.Last     := No_Index;
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Container : in out Vector) is
   begin
      Finalize (Container);
   end Free;

   ---------------------
   -- Generic_Sorting --
   ---------------------

   package body Generic_Sorting is

      ---------------
      -- Is_Sorted --
      ---------------

      function Is_Sorted (Container : Vector) return Boolean is
      begin
         if Container.Last <= Index_Type'First then
            return True;
         end if;

         declare
            EA : Elements_Array renames Container.Elements.EA;
         begin
            for J in Index_Type'First .. Container.Last - 1 loop
               if EA (J + 1) < EA (J) then
                  return False;
               end if;
            end loop;
         end;

         return True;
      end Is_Sorted;

      -----------
      -- Merge --
      -----------

      procedure Merge (Target, Source : in out Vector) is
         I : Index_Type'Base := Target.Last;
         J : Index_Type'Base;

      begin
         --  The semantics of Merge changed slightly per AI05-0021. It was
         --  originally the case that if Target and Source denoted the same
         --  container object, then the GNAT implementation of Merge did
         --  nothing. However, it was argued that RM05 did not precisely
         --  specify the semantics for this corner case. The decision of
         --  the ARG was that if Target and Source denote the same non-empty
         --  container object, then Program_Error is raised.

         if Source.Last < Index_Type'First then  -- Source is empty
            return;
         end if;

         if Target.Last < Index_Type'First then  -- Target is empty
            Move (Target => Target, Source => Source);
            return;
         end if;

         Target.Set_Length (Length (Target) + Length (Source));

         declare
            TA : Elements_Array renames Target.Elements.EA;
            SA : Elements_Array renames Source.Elements.EA;

         begin
            J := Target.Last;
            while Source.Last >= Index_Type'First loop
               pragma Assert
                 (Source.Last <= Index_Type'First
                  or else not (SA (Source.Last) < SA (Source.Last - 1)));

               if I < Index_Type'First then
                  TA (Index_Type'First .. J) :=
                    SA (Index_Type'First .. Source.Last);

                  Source.Last := No_Index;
                  return;
               end if;

               pragma Assert
                 (I <= Index_Type'First or else not (TA (I) < TA (I - 1)));

               if SA (Source.Last) < TA (I) then
                  TA (J) := TA (I);
                  I      := I - 1;

               else
                  TA (J)      := SA (Source.Last);
                  Source.Last := Source.Last - 1;
               end if;

               J := J - 1;
            end loop;
         end;
      end Merge;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out Vector) is
         procedure Sort is new Generic_Array_Sort
           (Index_Type   => Index_Type,
            Element_Type => Element_Type,
            Array_Type   => Elements_Array,
            "<"          => "<");

      begin
         if Container.Last <= Index_Type'First then
            return;
         end if;

         --  The exception behavior for the vector container must match that
         --  for the list container, so we check for cursor tampering here
         --  (which will catch more things) instead of for element tampering
         --  (which will catch fewer things). It's true that the elements of
         --  this vector container could be safely moved around while (say)
         --  an iteration is taking place (iteration only increments the busy
         --  counter), and so technically all we would need here is a test for
         --  element tampering (indicated by the lock counter), that's simply
         --  an artifact of our array-based implementation. Logically Sort
         --  requires a check for cursor tampering.

         Sort (Container.Elements.EA (Index_Type'First .. Container.Last));
      end Sort;

   end Generic_Sorting;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Container.Last < Index_Type'First;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for Indx in Index_Type'First .. Container.Last loop
         Process (Cursor'(Container'Unrestricted_Access, Indx));
      end loop;
   end Iterate;

   function Iterate
     (Container : Vector)
      return      Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
      V : constant Vector_Access := Container'Unrestricted_Access;
   begin
      --  The value of its Index component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Index
      --  component is No_Index (as is the case here), this means the iterator
      --  object was constructed without a start expression. This is a complete
      --  iterator, meaning that the iteration starts from the (logical)
      --  beginning of the sequence of items.

      --  Note: For a forward iterator, Container.First is the beginning, and
      --  for a reverse iterator, Container.Last is the beginning.

      return It : constant Iterator := (Container => V, Index => No_Index) do
         null;
      end return;
   end Iterate;

   function Iterate
     (Container : Vector;
      Start     : Cursor)
      return      Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
      V : constant Vector_Access := Container'Unrestricted_Access;
   begin
      --  It was formerly the case that when Start = No_Element, the partial
      --  iterator was defined to behave the same as for a complete iterator,
      --  and iterate over the entire sequence of items. However, those
      --  semantics were unintuitive and arguably error-prone (it is too easy
      --  to accidentally create an endless loop), and so they were changed,
      --  per the ARG meeting in Denver on 2011/11. However, there was no
      --  consensus about what positive meaning this corner case should have,
      --  and so it was decided to simply raise an exception. This does imply,
      --  however, that it is not possible to use a partial iterator to specify
      --  an empty sequence of items.

      --  The value of its Index component influences the behavior of the First
      --  and Last selector functions of the iterator object. When the Index
      --  component is not No_Index (as is the case here), it means that this
      --  is a partial iteration, over a subset of the complete sequence of
      --  items. The iterator object was constructed with a start expression,
      --  indicating the position from which the iteration begins. Note that
      --  the start position has the same value irrespective of whether this
      --  is a forward or reverse iteration.

      return
        It : constant Iterator := (Container => V, Index => Start.Index)
      do
         null;
      end return;
   end Iterate;

   ----------
   -- Last --
   ----------

   function Last (Container : Vector) return Cursor is
   begin
      if Is_Empty (Container) then
         return No_Element;
      else
         return (Container'Unrestricted_Access, Container.Last);
      end if;
   end Last;

   function Last (Object : Iterator) return Cursor is
   begin
      --  The value of the iterator object's Index component influences the
      --  behavior of the Last (and First) selector function.

      --  When the Index component is No_Index, this means the iterator
      --  object was constructed without a start expression, in which case the
      --  (reverse) iteration starts from the (logical) beginning of the entire
      --  sequence (corresponding to Container.Last, for a reverse iterator).

      --  Otherwise, this is iteration over a partial sequence of items.
      --  When the Index component is not No_Index, the iterator object was
      --  constructed with a start expression, that specifies the position
      --  from which the (reverse) partial iteration begins.

      if Object.Index = No_Index then
         return Last (Object.Container.all);
      else
         return Cursor'(Object.Container, Object.Index);
      end if;
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Vector) return Element_Type is
   begin
      return Container.Elements.EA (Container.Last);
   end Last_Element;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (Container : Vector) return Extended_Index is
   begin
      return Container.Last;
   end Last_Index;

   ------------
   -- Length --
   ------------

   function Length (Container : Vector) return Count_Type is
   begin
      pragma Assert (Index_Type'First = 1);
      return Count_Type (Container.Last);
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Vector; Source : in out Vector) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      declare
         Target_Elements : constant Elements_Access := Target.Elements;
      begin
         Target.Elements := Source.Elements;
         Source.Elements := Target_Elements;
      end;

      Target.Last := Source.Last;
      Source.Last := No_Index;
   end Move;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      elsif Position.Index < Position.Container.Last then
         return (Position.Container, Position.Index + 1);
      else
         return No_Element;
      end if;
   end Next;

   function Next (Object : Iterator; Position : Cursor) return Cursor is
      pragma Unreferenced (Object);
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      return Next (Position);
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      if Position.Container = null then
         return;
      elsif Position.Index < Position.Container.Last then
         Position.Index := Position.Index + 1;
      else
         Position := No_Element;
      end if;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      elsif Position.Index > Index_Type'First then
         return (Position.Container, Position.Index - 1);
      else
         return No_Element;
      end if;
   end Previous;

   function Previous (Object : Iterator; Position : Cursor) return Cursor is
      pragma Unreferenced (Object);
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      return Previous (Position);
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      if Position.Container = null then
         return;
      elsif Position.Index > Index_Type'First then
         Position.Index := Position.Index - 1;
      else
         Position := No_Element;
      end if;
   end Previous;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Container : aliased in out Vector;
      Position  : Cursor)
      return      Reference_Type
   is
   begin
      return R : constant Reference_Type :=
        (Element =>
           Container.Elements.EA (Position.Index)'Unrestricted_Access);
   end Reference;

   function Reference
     (Container : aliased in out Vector;
      Index     : Index_Type)
      return      Reference_Type
   is
   begin
      pragma Assert (Index in 1 .. Last_Index (Container));
      return R : constant Reference_Type :=
        (Element => Container.Elements.EA (Index)'Unrestricted_Access);
   end Reference;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for Indx in reverse Index_Type'First .. Container.Last loop
         Process (Cursor'(Container'Unrestricted_Access, Indx));
      end loop;
   end Reverse_Iterate;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length (Container : in out Vector; Length : Count_Type) is
      New_Last : constant Index_Type := Index_Type (Length);
      pragma Assert (Index_Type'First = 1);
      New_Elts : Elements_Access;
   begin
      if Container.Elements = Empty_Elements'Access then
         pragma Assert (Container.Last = 0);
         New_Elts           := new Elements_Type (Last => New_Last);
         Container.Elements := New_Elts;
      elsif New_Last > Container.Elements.Last then
         New_Elts := new Elements_Type (Last => New_Last);
         New_Elts.EA (1 .. Container.Last) :=
           Container.Elements.EA (1 .. Container.Last);
         Free (Container.Elements);
         Container.Elements := New_Elts;
      end if;

      Container.Last := New_Last;
   end Set_Length;

   ---------------
   -- To_Cursor --
   ---------------

   function To_Cursor
     (Container : Vector;
      Index     : Extended_Index)
      return      Cursor
   is
   begin
      if Index not in Index_Type'First .. Container.Last then
         return No_Element;
      else
         return (Container'Unrestricted_Access, Index);
      end if;
   end To_Cursor;

   --------------
   -- To_Index --
   --------------

   function To_Index (Position : Cursor) return Extended_Index is
   begin
      if Position.Container = null then
         return No_Index;
      end if;

      if Position.Index <= Position.Container.Last then
         return Position.Index;
      end if;

      return No_Index;
   end To_Index;

   --  Extra operations not in Ada.Containers.Vectors:

   function Slice
     (Container : Vector;
      First     : Index_Type;
      Last      : Extended_Index)
      return      Elements_Array
   is

      Jj : Extended_Index          := Index_Type'First;
      L  : constant Extended_Index :=
        (if Last < First then Jj - 1 else Last - First + Index_Type'First);
   --  Handle super-null slices properly

   begin
      return Result : Elements_Array (Index_Type'First .. L) do
         for J in First .. Last loop
            Result (Jj) := Elems (Container) (J);
            Jj          := Jj + 1;
         end loop;
         pragma Assert (Jj = Result'Last + 1);
      end return;
   end Slice;

   function To_Array (Container : Vector) return Elements_Array is
   begin
      return Elems (Container) (1 .. Container.Last);
   end To_Array;

   procedure Append (Container : in out Vector; New_Items : Elements_Array) is
      --  Straightforward code would be:
      --     for X of A loop
      --        Append (Container, X);
      --     end loop;
      --  The following is for efficiency.

      New_Last : constant Index_Type := Container.Last + New_Items'Length;
      pragma Assert (Index_Type'First = 1);
      New_Elts : Elements_Access;
   begin
      if Container.Elements = Empty_Elements'Access then
         pragma Assert (Container.Last = 0);
         New_Elts :=
           new Elements_Type (Last => Index_Type'Max (New_Last, 2**10));
         Container.Elements := New_Elts;
      elsif New_Last > Container.Elements.Last then
         New_Elts :=
           new Elements_Type
           (Last => Index_Type'Max (New_Last, 2 * Container.Last));
         New_Elts.EA (1 .. Container.Last) := Container.Elements.EA;
         Free (Container.Elements);
         Container.Elements := New_Elts;
      end if;

      Container.Elements.EA (Container.Last + 1 .. New_Last) := New_Items;
      Container.Last                                         := New_Last;
   end Append;

end ASIS_UL.Fast_Vectors;
