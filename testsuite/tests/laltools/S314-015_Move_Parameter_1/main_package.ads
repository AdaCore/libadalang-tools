package Main_Package is
   type Foo_Type is tagged null record;

   procedure Foo (Self : Foo_Type; A : in Integer; B : out Integer);

   Another_F : Foo_Type;
end Main_Package;
