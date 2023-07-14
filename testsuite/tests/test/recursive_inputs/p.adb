package body P is

   function Fibonacci (Number : Integer) return Natural is
      Bad_Input : exception;
   begin
      if Number < 0 then
         raise Bad_Input;
      end if;

      if Number in 0 | 1 then
         return Number;
      else
         return Result : Natural do
            Result := Fibonacci (Number - 1) + Fibonacci (Number - 2);
         end return;
      end if;

   exception
      when Bad_Input =>
         raise Constraint_Error with "number must be positive";

   end Fibonacci;

   function Factorial (Number : Integer) return Positive is
   begin
      if Number < 0 then
         raise Constraint_Error;
      end if;

      if Number = 0 then
         return 1;
      else
         return Number * Factorial (Number - 1);
      end if;
   end Factorial;

end P;
