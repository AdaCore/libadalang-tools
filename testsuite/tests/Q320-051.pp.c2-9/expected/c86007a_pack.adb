package body C86007a_Pack is
   procedure Swap (X, Y : in out Standard.C86007a_Pack.Item) is
      T : Standard.C86007a_Pack.Item;
   begin
      T := X;
      X := Y;
      Y := T;
   end Swap;

   procedure Proc is
      X : Standard.C86007a_Pack.Item := Ident_Int (10);
      W : Standard.C86007a_Pack.Acc;
   begin

      W     := new Standard.C86007a_Pack.Item;
      W.all := X;
      Standard.C86007a_Pack.Swap (X, Standard.C86007a_Pack.Y);
      if Standard.C86007a_Pack.Y /= Ident_Int (10) then
         Failed ("FAILED STANDARD.NAME CALL PROCEDURE - B-10");
      end if;
      if X /= Ident_Int (5) then
         Failed ("FAILED STANDARD.NAME CALL PROCEDURE - B-5");
      end if;
   end Proc;
end C86007a_Pack;
