package R is
   function Ident (X : Integer) return Integer;
private
   function Ident (X : Integer) return Integer is (X);
end R;
