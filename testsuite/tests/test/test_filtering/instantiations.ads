with Sorting_Algorithms;
package Instantiations is

   package Sort_Integers is new Sorting_Algorithms (5, Integer);

   type String_3 is new String (1 .. 3);

   package Sort_String_3 is new Sorting_Algorithms (3, String_3);

end Instantiations;
