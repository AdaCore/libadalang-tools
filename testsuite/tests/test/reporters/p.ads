package P is
   type T is tagged record
      X : Integer := 1;
   end record;

   function X1 (Self : T) return Integer is (Self.X);
   function X2 (Self : T) return Integer is (-Self.X);
end P;
