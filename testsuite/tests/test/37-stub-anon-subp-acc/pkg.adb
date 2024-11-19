with Dep;

package body Pkg is

   function Make_Acc (X : aliased Integer) return Dep.Int_Acc is
      CB : access function (X : aliased Integer) return Dep.Int_Acc :=
        Dep.Get_CB;
   begin
      return CB (X);
   end Make_Acc;

end Pkg;
