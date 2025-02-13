package Pkg is
   type Answer is (Yes, No, Unknown);

   subtype Decided_Answer is Answer
   with Static_Predicate => Decided_Answer in Yes | No;

   function Is_Yes (A : Decided_Answer) return Boolean
   is (A = Yes);
end Pkg;
