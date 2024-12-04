package body Bar is

   function Subp_Under_Test
     (Test_Data_1 : Integer;
      Test_Data_2 : Element_Type;
      Test_Data_3 : out Float)
       return Bar_Array is ((11 .. 20 => <>));

end Bar;
