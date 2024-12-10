with Top_Level_Child_Generic_Child;

generic
   type T is private;
package Top_Level_Child_Generic is
   function Identity (X : T) return T is (X);
   package Nested_Instance is new Top_Level_Child_Generic_Child (T);
end Top_Level_Child_Generic;
