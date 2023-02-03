package Other_Package is

   type Private_Array is private;

private

   type Private_Array is array (1 .. 10) of String (1 .. 2);

   PA : constant Private_Array := (others => "ab");

end Other_Package;
