package body Main_Package is
   procedure Foo (A : Integer) is
   begin
      null;
   end Foo;

   procedure Bar (A : Integer := 1) is null;

   procedure Baz (A : Integer) renames Bar;

   procedure Qux (A : Element_Type) is
   begin
      Foo (1);
   end Qux;

   procedure Quux (A : Element_Type := Default) is
   begin
      null;
   end Quux;
   
   procedure Waldo (A : Integer) is null;
end Main_Package;
