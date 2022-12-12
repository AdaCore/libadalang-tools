generic
   type Index_Type is range <>;
   type Element_Type is private;
   with function "<" (L, R : Element_Type) return Boolean is <>;
package Sorting_Algorithms is

   type Array_To_Sort is array (Index_Type) of Element_Type;

   procedure Selection_Sort (X : in out Array_To_Sort);

   procedure Bubble_Sort (X : in out Array_To_Sort);

end Sorting_Algorithms;