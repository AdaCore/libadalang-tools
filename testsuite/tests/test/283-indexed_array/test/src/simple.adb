package body Simple
is

   function Add_Array (A : My_Array) return T is
   begin
      return A (1) + A (2) + A (0);
   end Add_Array;

end Simple;
