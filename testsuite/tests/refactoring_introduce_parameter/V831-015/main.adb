procedure Hello is
begin
   declare
      Z : constant Integer := 10;
      procedure Foo (X : Integer := Z);
      procedure Foo (X : Integer := Z)
      is
         Y : Integer := X;
      begin
         null;
      end Foo;
   begin
      Foo (Z);
   end;
end Hello;
