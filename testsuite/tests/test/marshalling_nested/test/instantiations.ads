with Sorting_Algorithms;

package Instantiations is

   type Range_3 is range 1 .. 3;
   type Range_5 is range 1 .. 5;

   type String_3 is new String (1 .. 3);
   type My_String_3 is array (Range_3) of Character;

   package Sort_Integer is new Sorting_Algorithms (Range_3, Integer);
   package Sort_String_3 is new Sorting_Algorithms (Range_3, String_3);
   package Sort_My_String_3 is new Sorting_Algorithms (Range_3, My_String_3);
end Instantiations;