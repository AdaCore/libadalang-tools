separate (C83030c_Decl2)
procedure C83030c_Proc2 (X : T) is
   A : T := T'First;
begin
   if Switch then
      Switch := False;
      C83030c_Proc2 (X);
      if Global /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR PROCEDURE CALL - 20");
      end if;
      Global := Ident_Int (3);
   else
      Global := Ident_Int (2);
   end if;
end C83030c_Proc2;
