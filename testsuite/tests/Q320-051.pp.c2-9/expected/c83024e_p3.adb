package body C83024e_P3 is

   procedure Require_Body is
   begin
      null;
   end Require_Body;

   package body C83024e_Pack3 is
      C : Integer := A;
      A : Integer := Ident_Int (3);
   begin
      if A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 20");
      end if;

      if C83024e_P3.A /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 21");
      end if;

      if C83024e_P3.B /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 22");
      end if;

      if C /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR INNER VARIABLE - 23");
      end if;

      if X /= Ident_Int (2) then
         Failed ("INCORRECT VALUE PASSED IN - 24");
      end if;

      if Equal (1, 1) then
         X := A;
      else
         null;
      end if;
   end C83024e_Pack3;
end C83024e_P3;
