package body Pkg is

   function And_Then return Boolean is
   begin
      if Glob_1 then
         if Glob_2 then
            return True;
         else
            return False;
         end if;
      else
         return False;
      end if;
   end And_Then;

end Pkg;
