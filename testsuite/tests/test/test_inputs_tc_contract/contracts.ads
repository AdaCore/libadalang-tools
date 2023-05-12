package Contracts is

   function Sqrt (X : Float) return Float
   with Test_Case => (Name => "test case 1",
                      Mode => Nominal),
        Test_Case => (Name => "test case 2",
                      Mode => Nominal);

end Contracts;
