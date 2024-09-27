package Standard is

   subtype Foo is Integer range 1 .. 100;

   type Test is record
      X, Y : Foo;
   end record;

end Standard;
