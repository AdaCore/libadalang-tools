package body C83024e_P2 is

   procedure Require_Body is
   begin
      null;
   end Require_Body;

   package body C83024e_Pack2 is
   begin
      if A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 10");
      end if;

      if C83024e_P2.A /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 11");
      end if;

      if C83024e_P2.B /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 12");
      end if;

      if C /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR INNER VARIABLE - 13");
      end if;

      if X /= Ident_Int (2) then
         Failed ("INCORRECT VALUE PASSED IN - 14");
      end if;

      if Equal (1, 1) then
         A := Ident_Int (4);
      else
         A := 1;
      end if;
   end C83024e_Pack2;
end C83024e_P2;
