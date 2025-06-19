package Pkh is

   function "not" (X : Integer) return Boolean is (X /= 0);

   function "&" (L, R : Short_Integer) return Integer is
     (Integer (L) * (Integer (Short_Integer'Last) + 1) + Integer (R));

end Pkh;
