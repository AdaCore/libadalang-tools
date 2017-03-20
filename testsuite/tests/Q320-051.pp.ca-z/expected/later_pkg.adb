package body Later_Pkg is

   function F (Y : Integer := 2) return Integer is
   begin
      return Y + 1;
   end F;

end Later_Pkg;
