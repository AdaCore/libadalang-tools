package body Foo is
   function Qux (Self : Integer; Bar : Float) return Integer is
     (if Bar < 0.0 then Self else -Self);
   function ComputeSquarePlusCube (X : Integer) return Integer is
     (declare T : constant Integer := X; begin
      T * T * (1 + T));
   procedure Corge (X : Integer) is null;
end Foo;

