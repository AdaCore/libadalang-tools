package body My_Package is

   function My_Procedure (I : Integer) return Boolean is
      B : Boolean := False;
   begin
      if I = 1 then
         declare
            function My_Nested_Function return Boolean is
            begin
               return True;
            end My_Nested_Function;
         begin
            B := My_Nested_Function;
         end;
      end if;
      null;
      return B;
   end My_Procedure;

end My_Package;
