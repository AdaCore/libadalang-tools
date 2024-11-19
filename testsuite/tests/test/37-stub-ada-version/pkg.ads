with Dep;

package Pkg is

   function Make_Acc (X : aliased Integer) return Dep.Int_Acc;

end Pkg;
