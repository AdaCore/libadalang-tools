-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Tctouch;
with Report;
package body C392010_2 is

   procedure Proc_W_Non
     (Cp_Ap    : access Tagtype_Level_2;
      Noncp_Ap : access C392010_0.Tagtype_Level_0 :=
        Lev2_Level_0_Global_Object'Access)
   is
   begin
      Tctouch.Touch
        ('M');  --------------------------------------------------- M
      Tctouch.Touch
        (Cp_Ap.Ch_Item);  ----- depends on the value passed ------- ?
      Tctouch.Touch
        (Noncp_Ap.Ch_Item);  -- depends on the value passed ------- ?
   end Proc_W_Non;

   function A_Default_Value return Tagtype_Level_2 is
   begin
      return (Another_Int_Item | Int_Item => 0, Ch_Item => 'x');  -------- x
   end A_Default_Value;

   Own : aliased Tagtype_Level_2 :=
     (Another_Int_Item | Int_Item => 4, Ch_Item => 'g');

   function Func_W_Non
     (Cp_Ap    : access Tagtype_Level_2;
      Noncp_Ap : access C392010_0.Tagtype_Level_0 :=
        Lev2_Level_0_Global_Object'Access)
      return C392010_1.Access_Tagtype_Level_1
   is
   begin
      Tctouch.Touch
        ('N');  --------------------------------------------------- N
      Tctouch.Touch
        (Cp_Ap.Ch_Item);  ----- depends on the value passed ------- ?
      Tctouch.Touch
        (Noncp_Ap.Ch_Item);  -- depends on the value passed ------- ?
      return Own'
          Access;  ---------------------------------------------------- g
   end Func_W_Non;

   function Func_W_Cp_Ap_And_Cr
     (Cp_Ap : access Tagtype_Level_2) return Tagtype_Level_2
   is
   begin
      Tctouch.Touch
        ('P');  --------------------------------------------------- P
      Tctouch.Touch
        (Cp_Ap.Ch_Item);  ----- depends on the value passed ------- ?
      return (Another_Int_Item | Int_Item => 5, Ch_Item => 'h');  ---------- h
   end Func_W_Cp_Ap_And_Cr;

end C392010_2;
