---------------------------------------------------------------- CDB0A02_2

with Fdb0a00.Pool2;
package Cdb0a02_2 is

   type Small_Cell;
   type Small_Tree is access Small_Cell;

   for Small_Tree'Storage_Pool use Fdb0a00.Pool2.Pond;  -- first usage

   type Small_Cell is record
      Data        : Character;
      Left, Right : Small_Tree;
   end record;

   procedure Insert (Item : Character; On_Tree : in out Small_Tree);

   procedure Traverse (The_Tree : Small_Tree);

   procedure Defoliate (The_Tree : in out Small_Tree);

   procedure Tc_Exceed_Pool;

   Pool_Max_Elements : constant := 6_000;
   -- to guarantee overflow in TC_Exceed_Pool

end Cdb0a02_2;
