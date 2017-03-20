separate (C83030c_Decl2)
procedure C83030c_Proc1 is
   A : Integer := Ident_Int (2);
begin
   if Switch then
      Switch := False;
      C83030c_Proc1;
      if Global /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR PROCEDURE CALL - 1");
      end if;
   end if;
   C83030c_Proc1 (A);
   if Global /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR PROCEDURE CALL - 2");
   end if;
   Global := Ident_Int (3);
end C83030c_Proc1;
