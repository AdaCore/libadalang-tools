generic
   type Element_Type is private;
package Bar is
   
   type Bar_Range is range 11 .. 20;
   type Bar_Array is array (Bar_Range) of Element_Type;

   function Subp_Under_Test
      (Test_Data_1 : Integer;
       Test_Data_2 : Element_Type;
       Test_Data_3 : out Float)
       return Bar_Array;

end Bar;
