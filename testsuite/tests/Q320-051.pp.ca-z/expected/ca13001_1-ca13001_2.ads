-- No bodies required for CA13001_1.

     --==================================================================--

-- Private child.

private package Ca13001_1.Ca13001_2 is

   type Transport is record
      In_Use : Boolean := False;
   end record;
   Vehicles : array (Transportation) of Transport;

   -- Other type definitions and procedure declarations in real application.

end Ca13001_1.Ca13001_2;
