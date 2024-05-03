with Sorting_Algorithms;
package Instantiations is

   package Sort_Integers is new Sorting_Algorithms (5, Integer);

   procedure Need_Body (X : in out Integer);

private

   type String_3 is new String (1 .. 3);

   package Sort_String_3 is new Sorting_Algorithms (3, String_3);
   use type Sort_String_3.Array_To_Sort;
   
   procedure Do_Stuff (X : in out Sort_String_3.Array_To_Sort);

end Instantiations;
