-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--with Report;
with Tctouch;
package body Fdb3a00 is

   Largest_Request_On_Record : System.Storage_Elements.Storage_Count := 0;

   procedure Allocate (Pool    : in out Stack_Heap;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count)
   is
      use type System.Storage_Elements.Storage_Offset;
   begin
      Tctouch.Touch
        ('A');  --------------------------------------------------- A

--Report.Comment ("Allocating" & System.Storage_Elements.Storage_Count'Image(Size_In_Storage_Elements) &
--   " from Pool " & Pool.TC_Id & " with Water_Line" &
--   System.Storage_Elements.Storage_Count'Image(Pool.Water_Line));

      -- set the pointer to the next correctly aligned available address
      Pool.Avail :=
        Pool.Avail +
        (Alignment - (Pool.Data (Pool.Avail)'Address mod Alignment));

      -- check for overflow
      if Pool.Avail + Size_In_Storage_Elements > Pool.Water_Line then
         if Pool.Avail > Pool.Water_Line
         then -- Keep Pool.Avail within the pool
            -- so the alignment calculation doesn't fail on a subsequent
            -- allocation:
            Pool.Avail := Pool.Water_Line;
         end if;
         raise Pool_Overflow
           with "Pool " & Pool.Tc_Id & " with Water_Line" &
           System.Storage_Elements.Storage_Count'Image (Pool.Water_Line);
      end if;

      -- set the resulting address to that address
      Storage_Address := Pool.Data (Pool.Avail)'Address;

      -- update the housekeeping
      Pool.Avail                := Pool.Avail + Size_In_Storage_Elements;
      Largest_Request_On_Record :=
        System.Storage_Elements.Storage_Count'Max (Largest_Request_On_Record,
           Size_In_Storage_Elements);
   end Allocate;

   procedure Deallocate (Pool  : in out Stack_Heap;
      Storage_Address          : in     System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count)
   is
   begin
      Tctouch.Touch
        ('D');  --------------------------------------------------- D

      -- for the purposes of validation, the simplest possible implementation
      -- of Deallocate is shown below:

      null;

   end Deallocate;

   function Storage_Size
     (Pool : in Stack_Heap) return System.Storage_Elements.Storage_Count
   is
   begin
      Tctouch.Touch
        ('S');  --------------------------------------------------- S
      return Pool.Water_Line;
   end Storage_Size;

   function Tc_Largest_Request return System.Storage_Elements.Storage_Count is
   begin
      return Largest_Request_On_Record;
   end Tc_Largest_Request;

end Fdb3a00;
