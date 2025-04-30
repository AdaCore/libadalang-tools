package Pkg is

   procedure Public is null;

private

   function F (X : Integer) return Integer is (X);
   --  Used in Public...

end Pkg;
