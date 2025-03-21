package Simple
is

   type T is range 0 .. 10;

   type Index is range 0 .. 2;

   type My_Array is array (Index) of T;

   function Add_Array (A : My_Array) return T;

end Simple;
