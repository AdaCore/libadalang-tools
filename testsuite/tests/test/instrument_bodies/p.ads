package P is

   function Fibonacci (Number : Natural) return Natural;
   --  Has expression function as body (in package body)

   function Fibonacci_Rename (Number : Natural) return Natural;
   --  Renaming-as-body

   procedure Stub_Body (X : in out Natural; Y : in out Boolean);
   --  Body is a null procedure

end P;
