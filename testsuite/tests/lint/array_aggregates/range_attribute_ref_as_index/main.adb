procedure Main is
   type My_String is array (Positive range <>) of Character;
   Arr_1 : My_String := ('a', 'a', 'a', 'a', 'a');
   Arr_2 : My_String := (Arr_1'Range => 'b');
begin
   null;
end Main;

