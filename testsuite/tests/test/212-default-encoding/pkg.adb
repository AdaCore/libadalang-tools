package body Pkg is

   function Foo (X : Integer) return ZDep.Res_Type is (ZDep.Res_Type (X));

end Pkg;
