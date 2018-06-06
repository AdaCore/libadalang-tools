-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Tctouch;
package body C392010_1 is

   procedure Proc_W_Ap_And_Cp
     (Ap : C392010_0.Access_Procedure; Cp : Tagtype_Level_1)
   is
   begin
      Tctouch.Touch
        ('G');  --------------------------------------------------- G
      Ap.all (C392010_0.Tagtype_Level_0 (Cp));
   end Proc_W_Ap_And_Cp;

   procedure Proc_W_Ap_And_Cp_W_Def
     (Ap : C392010_0.Access_Procedure := C392010_0.Proc_2'Access;
      Cp : Tagtype_Level_1            := A_Default_Value)
   is
   begin
      Tctouch.Touch
        ('H');  --------------------------------------------------- H
      Ap.all (C392010_0.Tagtype_Level_0 (Cp));
   end Proc_W_Ap_And_Cp_W_Def;

   procedure Proc_W_Cp_Ap (Cp_Ap : access Tagtype_Level_1) is
   begin
      Tctouch.Touch
        ('I');  --------------------------------------------------- I
      Tctouch.Touch
        (Cp_Ap.Ch_Item);  -- depends on the value passed ---------- ?
   end Proc_W_Cp_Ap;

   function A_Default_Value return Tagtype_Level_1 is
   begin
      return (Int_Item => 0, Ch_Item => 'y');  ---------------------------- y
   end A_Default_Value;

   function Func_W_Cp_Ap_And_Cr
     (Cp_Ap : access Tagtype_Level_1) return Tagtype_Level_1
   is
   begin
      Tctouch.Touch
        ('J');  --------------------------------------------------- J
      Tctouch.Touch
        (Cp_Ap.Ch_Item);  -- depends on the value passed ---------- ?
      return (Int_Item => 2, Ch_Item => 'd');  ----------------------------- d
   end Func_W_Cp_Ap_And_Cr;

   procedure Proc_W_Non
     (Cp_Ap    : access Tagtype_Level_1;
      Noncp_Ap : access C392010_0.Tagtype_Level_0 :=
        C392010_0.Level_0_Global_Object'Access)
   is
   begin
      Tctouch.Touch
        ('K');  --------------------------------------------------- K
      Tctouch.Touch
        (Cp_Ap.Ch_Item);  ----- depends on the value passed ------- ?
      Tctouch.Touch
        (Noncp_Ap.Ch_Item);  -- depends on the value passed ------- ?
   end Proc_W_Non;

   Own_Item : aliased Tagtype_Level_1 := (Int_Item => 3, Ch_Item => 'e');

   function Func_W_Non
     (Cp_Ap    : access Tagtype_Level_1;
      Noncp_Ap : access C392010_0.Tagtype_Level_0 :=
        C392010_0.Level_0_Global_Object'Access)
      return Access_Tagtype_Level_1
   is
   begin
      Tctouch.Touch
        ('L');  --------------------------------------------------- L
      Tctouch.Touch
        (Cp_Ap.Ch_Item);  ----- depends on the value passed ------- ?
      Tctouch.Touch
        (Noncp_Ap.Ch_Item);  -- depends on the value passed ------- ?
      return Own_Item'
          Access;  ----------------------------------------------- e
   end Func_W_Non;

end C392010_1;
