package body Simple is

   function Inc (X : Integer) return Integer
   is
   begin
      return X + 1;
   end Inc;

   function Dec (X : Integer) return Integer
   is
   begin
      return X - 1;
   end Dec;

end Simple;
