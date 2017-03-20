     --==================================================================--

package body Cc51d01_0 is

   -- The implementations of Update_ID are purely artificial; the validity of
   -- their implementations in the context of the abstraction is irrelevant to
   -- the feature being tested.

   procedure Update_Id (Item : in out Blind_Id_Type) is
   begin
      Item.Ssn := "111223333";
   end Update_Id;

   procedure Update_Id (Item : in out Named_Id_Type) is
   begin
      Item.Ssn := "444556666";
      -- ... Other stuff.
   end Update_Id;

end Cc51d01_0;
