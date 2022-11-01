package P is
   type T is abstract tagged private;

   function Do_Stuff (Obj : T) return Integer;
   function Do_Other_Stuff (Obj : T) return Integer;

   type T2 is new T with private;

   function Do_Stuff (Obj : T2) return Integer;  --  violate lsp

private
    type T is abstract tagged record
      X : Integer := 0;
   end record;

   type T2 is new T with null record;
end P;
