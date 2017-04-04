package body Ca15003a.Pure.Preelaborate is
   function F (X : access Int) return Int is
   begin
      X.all := X.all + One;
      return X.all;
   end F;
end Ca15003a.Pure.Preelaborate;
