with Dep;

package body Pkg is

   function Make_Acc (X : aliased Integer) return Dep.Int_Acc is
   begin
      return Dep.Get_Acc (X);
   end Make_Acc;

end Pkg;
