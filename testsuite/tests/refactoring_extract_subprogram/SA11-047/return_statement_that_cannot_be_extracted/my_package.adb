package body My_Package is

   function My_Procedure (I : Integer) return Boolean is
   begin
      if I = 1 then
         return True;
      end if;
      null;
      return False;
   end My_Procedure;

end My_Package;
