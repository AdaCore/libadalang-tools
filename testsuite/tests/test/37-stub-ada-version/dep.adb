package body Dep is

   function Get_Acc (X : aliased Integer) return Int_Acc is
     (X'Unrestricted_Access);

end Dep;
