package body P is

   function Fibonacci (Number : Natural) return Natural is
     (if Number in 0 | 1 then Number
      else Fibonacci (Number - 1) + Fibonacci (Number - 2));

   function Fibonacci_Rename (Number : Natural) return Natural renames
     Fibonacci;

   procedure Stub_Body (X : in out Natural; Y : in out Boolean) is null;

end P;
