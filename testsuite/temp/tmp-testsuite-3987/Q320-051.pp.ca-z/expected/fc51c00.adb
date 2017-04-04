     --===================================================================--

package body Fc51c00 is

   Value : Concrete_Grandchild;

   function Func (P : Concrete_Root) return Concrete_Root is
   begin
      return P;
   end Func;

   function Func (P : Concrete_Grandchild) return Concrete_Grandchild is
   begin
      return P;
   end Func;

   procedure Proc (P : in out Concrete_Grandchild) is
   begin
      P := Value;
   end Proc;

   procedure New_Proc (P : out Concrete_Grandchild) is
   begin
      P := Value;
   end New_Proc;

end Fc51c00;
