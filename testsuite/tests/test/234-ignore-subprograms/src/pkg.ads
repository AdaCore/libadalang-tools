package Pkg is
   function Inc (A : Integer) return Integer is (A + 1);
   function Dec (A : Integer) return Integer is (A - 1);
   function Mul (A : Integer; B : Integer) return Integer is (A * B);
   function Div (A : Integer; B : Integer) return Integer is (A / B);
end Pkg;
