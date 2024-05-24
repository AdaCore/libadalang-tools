package body Protec is

   task body My_Task is
      Local_X : Integer;
   begin
      accept Bar (X : Integer) do
         Local_X := X;
      end Bar;
   end My_Task;

   function Foo (X : Integer) return Integer is
   begin
      return X;
   end Foo;

end Protec;
