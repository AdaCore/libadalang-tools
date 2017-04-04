package body Ca15003a.Pure is
   function F (X : access Int) return Int is
   begin
      X.all := X.all + 1;
      return X.all;
   end F;
end Ca15003a.Pure;
