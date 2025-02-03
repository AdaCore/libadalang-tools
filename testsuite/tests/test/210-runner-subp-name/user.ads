with Pkg;
with Gen;

package User is

   type User_T is new Pkg.Pkg_T with null record;
   overriding procedure Overridden_Prim (X : User_T);

   procedure Inst is new Pkg.Generic_Proc (T => Integer);

   package Gen_Inst is new Gen (T => Integer);

end User;
