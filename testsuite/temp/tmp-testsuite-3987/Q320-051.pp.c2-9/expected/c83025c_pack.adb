package body C83025c_Pack is
   function Gen_Fun return T is
   begin
      return X;
   end Gen_Fun;

   function F is new Gen_Fun (Integer, Obj);

   function F is new Gen_Fun (Float, Flo);

   procedure Inner (X : in out Integer) is separate;

   procedure Inner2
     (X : in     Integer := C83025c_Pack.A;
      A : in out Integer) is separate;

   function Inner3 (X : Integer; Z : Enum := Y) return Integer is separate;

   function Inner4 (X : Integer; Z : Enum := Y) return Integer is separate;

   procedure Inner5
     (X : in out Integer;
      F : in     Float;
      Z :        Character := Y) is separate;
end C83025c_Pack;
