package Pkg is

   procedure Simple (X : Integer);
   procedure With_TC (X : Integer) with
     Test_Case => (Name => "Trivial_TC",
                   Mode => Nominal);

   type Pkg_T is tagged null record;

   procedure Inherited_Prim (X : Pkg_T);
   procedure Overridden_Prim (X : Pkg_T);

   generic
      type T is private;
   procedure Generic_Proc (X : T);

end Pkg;
