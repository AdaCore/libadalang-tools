package body Foo is
   procedure Swap (A, B : in out Foo_Type)
   is
      C : Foo_Type := A;
   begin
      A := B;
      B := A;
   end Swap;
   procedure Sneaky (A : Integer) is null;
end Foo;
