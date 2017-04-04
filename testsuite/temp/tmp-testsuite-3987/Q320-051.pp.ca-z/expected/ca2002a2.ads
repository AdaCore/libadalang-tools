package Ca2002a2 is

   procedure Proc (Y : out Integer);
   function Fun (Z : Integer := 3) return Boolean;

   package Pkg is
      I : Integer;
      procedure Pkg_Proc (Yy : in out Integer);
   end Pkg;

end Ca2002a2;
