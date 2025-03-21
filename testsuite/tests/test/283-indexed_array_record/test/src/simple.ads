package Simple
with SPARK_Mode
is
   type Lifetime is range 0 .. 120;

   type People is record
      Age : Lifetime;
   end record;

   type Index is range 0 .. 2;

   type Population is array (0 .. 20) of People;

   type Town is record
      Name   : String (1 .. 15);
      People : Population;
   end record;

   procedure Check_Town (T : Town);

end Simple;
