with Ada.Text_IO; use Ada.Text_IO;

procedure Arr_Greet is
   type My_Int is range 0 .. 1000;
   type Index is range 1 .. 5;

   --  my before comment
   type My_Int_Array is
     array (Index) of My_Int;
   --                 ^ Type of elements
   --       ^ Bounds of the array

   Arr : My_Int_Array := (2, 3, 5, 7, 11);
   --                    ^ Array literal
   --                      (aggregate)
   V : My_Int;

begin
   for I in Index loop
      V := Arr (I);
      --        ^ Take the Ith element
      Put (My_Int'Image (V));
   end loop;
   New_Line;
end Arr_Greet;
