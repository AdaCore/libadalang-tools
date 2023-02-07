with Other_Package;

package My_Package is

   type Y_Range is (One, Two, Three);
   type Y_Range_Array is array (Y_Range) of Integer;

   A : constant My_Package.Y_Range_Array :=
     (One => 1, Two => 2, Three => 3);

end My_Package;
