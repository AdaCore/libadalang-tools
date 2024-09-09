package Pkg is

   type Long_Array is array (Integer range 1 .. Integer'Last) of Integer;

   type Long_Array_2 is array (Integer) of Integer;

   subtype Large_Int is Positive range 1 .. 100000;

   subtype Long_Array_3 is String (Large_Int);

   type Big_Rec is record
      Long_Component : String (Large_Int);
   end record;

   function First (Arr : Long_Array) return Integer is (Arr (1));

   function First (Arr : Long_Array_2) return Integer is (Arr (Integer'First));

   function First (Arr : Long_Array_3) return Character is (Arr (1));

   function First (X : Big_Rec) return Character is (X.Long_Component (1));

   function Dummy (X : Positive) return Positive is (X);
   --  Just here so there is something to generate.

end Pkg;
