procedure Main is
   package Foo is
      type my_array is array (1..3) of Integer;
      A : Integer := 1;
   end Foo;

   package body Foo is
      procedure Initial_array (A : in out my_array) is
      begin
         for I in A'Range loop
            A (I) := -1;
         end loop;
      end Initial_array;
   end Foo;
begin
   Foo.A := Foo.A + 1;
end Main;

