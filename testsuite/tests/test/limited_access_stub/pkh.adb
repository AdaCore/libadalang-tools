package body Pkh is

   function Foo return Pkg.A_String is
      Res : Pkg.A_String;
   begin
      Pkg.Get (new Pkg.My_Stream'(null record), Res);
      return Res;
   end Foo;

end Pkh;
