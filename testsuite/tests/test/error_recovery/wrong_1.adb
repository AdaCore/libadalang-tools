package body Wrong_1 is
   procedure Cannot_Stub (X : in out Missing.Something.Some_Type) is
   begin
      null;
   end Cannot_Stub;
end Wrong_1;
