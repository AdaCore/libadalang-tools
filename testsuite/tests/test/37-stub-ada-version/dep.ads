package Dep is

   type Int_Acc is access all Integer;

   function Get_Acc (X : aliased Integer) return Int_Acc;

end Dep;
