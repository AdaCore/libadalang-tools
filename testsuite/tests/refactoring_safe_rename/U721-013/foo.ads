package Foo is

   generic
      type T is private;
   procedure Swap (A, B : in out T);

   generic
      type T is private;
   procedure Copy_To (A: T; B : out T);

   generic
      type T is private;
   procedure Something (A, B: T; C : out T);

end Foo;
