separate (Ca1108b_Pkg)
procedure Sub (X, Y : in out Integer) is
   procedure Sub2 (A, B : in out Integer) is separate;
begin

   Sub2 (Y, X);
   if Y /= 1 then
      Failed ("FIRST_PKG FUNCTION NOT VISIBLE IN SUBUNIT " & "OF SUBUNIT");
   end if;
   if X /= 3 then
      Failed ("LATER_PKG FUNCTION NOT VISIBLE IN SUBUNIT " & "OF SUBUNIT");
   end if;
   X := First_Pkg.F;
   Y := Later_Pkg.F;

end Sub;
