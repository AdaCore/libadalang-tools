package body Foo is

   procedure Swap (A, B : in out T) is
      C : T := A;
   begin
      A := B;
      B := C;
   end Swap;

   procedure Copy_To (A: T; B : out T) is
   begin
      B := A;
   end Copy_To;

   procedure Something (A, B: T; C : out T) is
      pragma Unreferenced (A, B, C);
   begin
      null;
   end Something;

end Foo;
