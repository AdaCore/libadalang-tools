package body C340001_2 is

   function "=" (L, R : in Transaction) return Boolean is
   -- There may be electronic and paper copies of the same transaction.
   -- The ID uniquely identifies a transaction. The medium (stored in the
   -- discriminant) is ignored.
   begin
      return L.Id = R.Id;
   end "=";

end C340001_2;
