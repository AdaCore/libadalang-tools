package body Simple
is

   procedure Test_Float (F : Float) is
   begin
      pragma Assert (F /= 0.000000000002);
   end;

end Simple;
