package body Wrong_2 is
   procedure Cannot_Stub (X : in out Missing.Some_Type) is
   begin
      null;
   end Cannot_Stub;
end Wrong_2;
