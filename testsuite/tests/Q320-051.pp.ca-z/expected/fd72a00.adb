package body Fd72a00 is

   function Address_To_Hex (Adder : System.Address) return String is
      S : String (1 .. 64) :=
        "uninitializedDEFuninitializedDEFuninitializedDEFuninitializedDEF";
      Deblank : Positive := S'First;
   begin
      Num_Io.Put
        (S, Number (System.Storage_Elements.To_Integer (Adder)), Base => 16);
      while S (Deblank) = ' ' loop
         Deblank := Deblank + 1;
      end loop;
      return S (Deblank .. S'Last);
   end Address_To_Hex;

   function Hex_To_Address (Hex : access String) return System.Address is
      The_Number : Number;
      Tail       : Natural;
   begin
      Num_Io.Get (Hex.all, The_Number, Tail);
      return System.Storage_Elements.To_Address
          (System.Storage_Elements.Integer_Address (The_Number));
   end Hex_To_Address;

end Fd72a00;
