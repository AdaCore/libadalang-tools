package body Priv.Child is
   function Do_Sum (L, R : T) return T is
   begin
      return L + R;
   end Do_Sum;
end Priv.Child;
