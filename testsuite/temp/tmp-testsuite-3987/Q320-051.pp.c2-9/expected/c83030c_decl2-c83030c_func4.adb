separate (C83030c_Decl2)
function C83030c_Func4 return T is
   A : T := T'Last;
begin
   if Switch then
      Switch := False;
      if C83030c_Func4 /= T'Last then
         Failed ("INCORRECT VALUE FROM FUNCTION CALL - 40");
      end if;
      return T'First;
   else
      if C83030c_Func4 then
         Failed ("INCORRECT VALUE FROM FUNCTION CALL - 41");
      end if;
      return T'Last;
   end if;
end C83030c_Func4;
