with Ca2002a1, Ca2002a2;
with Report; use Report;
procedure Ca2002a0m is
begin

   Test
     ("CA2002A",
      "SUBUNITS WITH DIFFERENT ANCESTORS " & "CAN HAVE THE SAME NAME");

   declare
      Var1 : Integer;
      use Ca2002a1;
   begin

      Proc (Var1);
      if Var1 /= 1 then
         Failed ("CA2002A1 PROCEDURE NOT INVOKED CORRECTLY");
      end if;

      if not Fun then
         Failed ("CA2002A1 FUNCTION NOT INVOKED CORRECTLY");
      end if;

      if Pkg.I /= 1 then
         Failed ("CA2202A1 PKG VARIABLE NOT ACCESSED CORRECTLY");
      end if;

      Var1 := 5;
      Pkg.Pkg_Proc (Var1);
      if Var1 /= 4 then
         Failed ("CA2002A1 PKG SUBUNIT NOT INVOKED CORRECTLY");
      end if;

   end;

   declare
      Var2 : Integer;
      use Ca2002a2;
   begin

      Proc (Var2);
      if Var2 /= 2 then
         Failed ("CA2002A2 PROCEDURE NOT INVOKED CORRECTLY");
      end if;

      if Fun then
         Failed ("CA2002A2 FUNCTION NOT INVOKED CORRECTLY");
      end if;

      if Pkg.I /= 2 then
         Failed ("CA2002A2 PKG VARIABLE NOT ACCESSED CORRECTLY");
      end if;

      Var2 := 3;
      Pkg.Pkg_Proc (Var2);
      if Var2 /= 4 then
         Failed ("CA2002A2 PKG SUBUNIT NOT INVOKED CORRECTLY");
      end if;

   end;

   Result;

end Ca2002a0m;
