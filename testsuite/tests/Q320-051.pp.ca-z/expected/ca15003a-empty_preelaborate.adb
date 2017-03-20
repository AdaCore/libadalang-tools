package body Ca15003a.Empty_Preelaborate is
   function F (X : access Big_Int) return Big_Int is
   begin
      X.all := X.all + One;
      return X.all;
   end F;
end Ca15003a.Empty_Preelaborate;
