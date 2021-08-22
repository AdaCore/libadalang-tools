package Contracts is

   function Sqrt (X : Float) return Float
   with Test_Case => (Name => "test case 1",
                      Mode => Nominal),
        Test_Case => (Name => "test case 2",
                      Mode => Nominal);

   type T is tagged null record;

   function Self (X : T) return T is (X)
     with Test_Case => (Name => "tc 01",
                        Mode => Nominal);
end Contracts;
