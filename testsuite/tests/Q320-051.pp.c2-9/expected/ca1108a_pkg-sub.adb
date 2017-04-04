separate (Ca1108a_Pkg)
procedure Sub (X, Y : in out Integer) is
   procedure Sub2 (Z : in out Integer) is separate;
begin

   X := I;
   Sub2 (Y);

end Sub;
