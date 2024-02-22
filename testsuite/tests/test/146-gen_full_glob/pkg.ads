package Pkg is

   Glob_1 : Boolean := False;

   Glob_2 : Boolean := False;

   function And_Then return Boolean with
     Global => (Glob_1, Glob_2);

end Pkg;
