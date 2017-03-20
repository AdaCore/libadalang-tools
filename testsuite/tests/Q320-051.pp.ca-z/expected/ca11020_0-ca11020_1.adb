--==================================================================--

package body Ca11020_0.Ca11020_1 is

   procedure Iterate (B : in Bag) is

      -- Traverse each element in the bag.

      Elem : Bag := B;

   begin
      while Elem /= null loop
         Use_Element (Elem.The_Element);
         Elem := Elem.Next;
      end loop;

   end Iterate;

end Ca11020_0.Ca11020_1;
