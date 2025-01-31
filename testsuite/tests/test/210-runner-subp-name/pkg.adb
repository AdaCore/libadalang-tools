package body Pkg is

   procedure Simple (X : Integer) is null;
   procedure With_TC (X : Integer) is null;

   procedure Inherited_Prim (X : Pkg_T) is null;
   procedure Overridden_Prim (X : Pkg_T) is null;

   procedure Generic_Proc (X : T) is null;

end Pkg;
