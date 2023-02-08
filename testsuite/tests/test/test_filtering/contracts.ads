package Contracts is

   --  Returns sqrt for natural numbers (expected behavior)
   --  and -1 for negatives (error indication).
   function Sqrt (X : Float) return Float
   with Test_Case => (Name => "test case 1",
                      Mode => Nominal,
                      Requires => X < 16.0,
                      Ensures  => Sqrt'Result < 4.0),
        Pre => (X >= 0.0);
   pragma Post (Sqrt'Result >= 0.0);
   pragma Test_Case (Name     => "test case 2",
                     Mode     => Robustness,
                     Requires => X < 0.0,
                     Ensures  => Sqrt'Result = -1.0);

end Contracts;
