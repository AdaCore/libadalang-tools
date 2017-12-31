-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Tctouch;
package body C392010_0 is

   procedure Proc_1 (P : Tagtype_Level_0) is
   begin
      Tctouch.Touch
        ('A');  --------------------------------------------------- A
      Tctouch.Touch
        (P.Ch_Item);  -- depends on the value passed -------------- ?
   end Proc_1;

   procedure Proc_2 (P : Tagtype_Level_0) is
   begin
      Tctouch.Touch
        ('B');  --------------------------------------------------- B
      Tctouch.Touch
        (P.Ch_Item);  -- depends on the value passed -------------- ?
   end Proc_2;

   function A_Default_Value return Tagtype_Level_0 is
   begin
      return
        (Ch_Item => 'z');  ---------------------------------------------- z
   end A_Default_Value;

   procedure Proc_W_Ap_And_Cp (Ap : Access_Procedure; Cp : Tagtype_Level_0) is
   begin
      Tctouch.Touch
        ('C');  --------------------------------------------------- C
      Ap.all (Cp);
   end Proc_W_Ap_And_Cp;

   procedure Proc_W_Ap_And_Cp_W_Def (Ap : Access_Procedure := Proc_2'Access;
      Cp                                : Tagtype_Level_0  := A_Default_Value)
   is
   begin
      Tctouch.Touch
        ('D');  --------------------------------------------------- D
      Ap.all (Cp);
   end Proc_W_Ap_And_Cp_W_Def;

   procedure Proc_W_Cp_Ap (Cp_Ap : access Tagtype_Level_0) is
   begin
      Tctouch.Touch
        ('E');  --------------------------------------------------- E
      Tctouch.Touch
        (Cp_Ap.Ch_Item);  -- depends on the value passed ---------- ?
   end Proc_W_Cp_Ap;

   function Func_W_Cp_Ap_And_Cr
     (Cp_Ap : access Tagtype_Level_0) return Tagtype_Level_0
   is
   begin
      Tctouch.Touch
        ('F');  --------------------------------------------------- F
      Tctouch.Touch
        (Cp_Ap.Ch_Item);  -- depends on the value passed ---------- ?
      return (Ch_Item => 'b');  -------------------------------------------- b
   end Func_W_Cp_Ap_And_Cr;

end C392010_0;
