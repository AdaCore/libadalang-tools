package Main_Package is
   procedure Foo (A : Integer);

   procedure Bar (A : Integer := 1);

   procedure Baz (A : Integer);

   generic
      type Element_Type is private;
   procedure Qux (A : Element_Type);

   generic
      type Element_Type is private;
      Default : Element_Type;
   procedure Quux (A : Element_Type := Default);

   function Garply (A : Integer) return Integer is (1);

   procedure Waldo (A : Integer);
end Main_Package;
