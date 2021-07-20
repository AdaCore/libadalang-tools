package Static_Pred is

   type Some_Type is (Val_1, Val_2, Val_3, Val_4, Val_5);

   subtype Some_Subtype is Some_Type
     with Static_Predicate => Some_Subtype in Val_1 | Val_3 | Val_5;

   type Foo is (Val_1, Val_4, Val_5);

   function Self (X : Some_Subtype) return Some_Subtype is (X);

end Static_Pred;
