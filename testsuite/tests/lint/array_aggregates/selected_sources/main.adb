with My_Package;

procedure Main is
   subtype Index_Number is Natural range 0 .. Natural'Last - 1;
   type Main_Array is array (Index_Number range <>) of Positive;
   A : constant Main_Array := (1 .. 0 => <>);
   Z : constant array (1 .. 1) of Integer := (1 => 1);

   CA1 : constant My_Package.Y_Range_Array :=
     (My_Package.One => 1, My_Package.Two => 2, My_Package.Three => 3);

begin
   null;
end Main;
