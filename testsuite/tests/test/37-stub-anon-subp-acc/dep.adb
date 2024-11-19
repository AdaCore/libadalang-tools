package body Dep is

   function Get_CB return access function (X : aliased Integer) return Int_Acc
   is
   begin
      return Get_Acc'Unrestricted_Access;
   end Get_CB;

   function Get_Acc (X : aliased Integer) return Int_Acc is
     (X'Unrestricted_Access);

end Dep;
