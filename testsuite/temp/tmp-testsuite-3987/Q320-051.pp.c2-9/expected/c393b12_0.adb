-- Alert_Functions

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

package body C393b12_0 is
   -- Alert_Functions

   procedure Handle (Ga : in out Generic_Alert_Type) is
   begin
      Ga.Status := Generic_Status_Enum'Last;
   end Handle;

   function Query_Status
     (Ga : Generic_Alert_Type) return Generic_Status_Enum
   is
   begin
      return Ga.Status;
   end Query_Status;

end C393b12_0;
