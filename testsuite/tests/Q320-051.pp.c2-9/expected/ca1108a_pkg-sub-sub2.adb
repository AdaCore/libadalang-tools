separate (Ca1108a_Pkg.Sub)
procedure Sub2 (Z : in out Integer) is
   I : Integer := 5;
begin

   Z := Other_Pkg.F (I);    -- Z => I + 1.

end Sub2;
