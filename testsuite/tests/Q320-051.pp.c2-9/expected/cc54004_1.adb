     --===================================================================--

package body Cc54004_1 is

   procedure Handle (A : in out Low_Alert) is
   begin
      A.Tc_Code := Low;
   end Handle;

   procedure Handle (A : in out Medium_Alert) is
   begin
      A.Tc_Code := Medium;
   end Handle;

end Cc54004_1;
