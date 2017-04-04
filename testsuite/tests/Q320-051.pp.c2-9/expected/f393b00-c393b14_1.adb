-- Alert_Foundation.Public_Child

--=======================================================================--

with F393b00.C393b14_0;               -- private sibling is visible in the
-- Alert_Foundation.Private_Child -- body of a public sibling
package body F393b00.C393b14_1 is
   -- Alert_Foundation.Public_Child
   package Priv renames F393b00.C393b14_0;

   procedure Init is
   begin
      Priv.Pa.Private_Field     := 5;
      Priv.Pa.New_Private_Field := 10;
   end Init;

   procedure Modify is
   begin
      Priv.Handle (Priv.Pa);
   end Modify;

   function Check_Before return Boolean is
   begin
      return
        ((Priv.Pa.Private_Field = 5) and (Priv.Pa.New_Private_Field = 10));
   end Check_Before;

   function Check_After return Boolean is
   begin
      return ((Priv.Pa.Private_Field = 1) and (Priv.Pa.New_Private_Field = 2));
   end Check_After;

end F393b00.C393b14_1;
