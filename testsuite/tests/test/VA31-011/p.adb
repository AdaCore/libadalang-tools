package body P is
   function Do_Stuff (Obj : T) return Integer is
   begin
      return Obj.X + 1;
   end Do_Stuff;


   function Do_Other_Stuff (Obj : T) return Integer is
   begin
      return Obj.X + 2;
   end Do_Other_Stuff;

   function Do_Stuff (Obj : T2) return Integer is
   begin
      return
        Obj.X - 1;  --  violate lsp
   end Do_Stuff;

end P;
