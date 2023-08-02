package Pkg is

   subtype My_F is Long_Float range 0.9999999997 .. 0.9999999998;

   type Arr_T is array (Positive range <>) of My_F;

   type Many_Arr_Rec is record
      Arr_1  : Arr_T (1 .. 100);
      Arr_2  : Arr_T (1 .. 100);
      Arr_3  : Arr_T (1 .. 100);
      Arr_4  : Arr_T (1 .. 100);
      Arr_5  : Arr_T (1 .. 100);
      Arr_6  : Arr_T (1 .. 100);
      Arr_7  : Arr_T (1 .. 100);
      Arr_8  : Arr_T (1 .. 100);
      Arr_9  : Arr_T (1 .. 100);
      Arr_10 : Arr_T (1 .. 100);
   end record;

   procedure Process (Input : Many_Arr_Rec);

end Pkg;
