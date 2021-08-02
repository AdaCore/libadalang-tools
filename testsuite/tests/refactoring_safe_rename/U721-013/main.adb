with Foo; use Foo;
with Baz; use Baz;
with Bar; use Bar;

procedure Main is
   A : Integer := 1;
   B : Integer := 1;
   procedure Swap (A, B : in out Integer) is null;

   procedure Inst_1 is new Foo.Swap (Integer);
   procedure Inst_2 is new Foo.Copy_To (Integer);
   procedure Inst_3 is new Foo.Something (Integer);
begin
   --  Insert code here.
   Integer_Swap (A, B);
end Main;
