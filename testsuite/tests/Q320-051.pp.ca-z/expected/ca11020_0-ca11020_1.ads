--==================================================================--

-- More operations on Bag.

generic

-- Parameters go here.

package Ca11020_0.Ca11020_1 is

   -- ... Other declarations.

   generic                            -- Generic iterator procedure.
      with procedure Use_Element (E : in Element);

   procedure Iterate (B : in Bag);    -- Called once per element in the bag.

   -- ... Various other operations.

end Ca11020_0.Ca11020_1;
