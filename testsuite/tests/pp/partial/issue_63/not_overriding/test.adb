package body Test is
   overriding
   procedure Bar
     (Self : Baz;
      I    : Integer;
      J    : Integer;
      K, L : Integer)
   is
   begin
      null;
   end Bar;
end Test;
