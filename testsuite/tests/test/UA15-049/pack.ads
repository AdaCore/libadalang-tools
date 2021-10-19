package Pack is
   X : Integer := 1;
private
   function X_Val return Integer is (X);

   type T is tagged null record;
   procedure Proc (Obj : T);

   type T2 is new T with null record;
   procedure Proc (Obj : T2);
end Pack;
