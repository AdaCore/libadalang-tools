     --==================================================================--

with Cc51006_0;  -- Weight class.
generic          -- Generic weight operations.
   type Weight is new Cc51006_0.Weight_Type;
package Cc51006_1 is

   procedure Output_Weight (Wt : in Weight; Tc_Return : out String);

   -- ... Other operations.

end Cc51006_1;
