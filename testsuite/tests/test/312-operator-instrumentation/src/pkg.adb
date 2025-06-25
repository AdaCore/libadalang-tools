package body Pkg is

   function "+" (X : Integer) return Positive is (abs X);

   function "*" (Y : Positive; X : Float) return Positive is
   begin
      return Y;
   end "*";

end Pkg;
