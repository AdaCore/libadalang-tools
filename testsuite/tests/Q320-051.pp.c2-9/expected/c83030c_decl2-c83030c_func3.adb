separate (C83030c_Decl2)
function C83030c_Func3 return Integer is
   A : Integer := Integer'Last;
begin
   if Switch then
      Switch := False;
      if C83030c_Func3 /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FROM FUNCTION CALL - 30");
      end if;
   end if;
   if C83030c_Func3 (A) /= Ident_Int (Integer'Last) then
      Failed ("INCORRECT VALUE FROM FUNCTION CALL - 31");
   end if;
   if C83030c_Func3 then
      Failed ("INCORRECT VALUE FROM FUNCTION CALL - 32");
   end if;
   return Ident_Int (3);
end C83030c_Func3;
