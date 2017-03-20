     --==================================================================--

with Cc51006_1; -- Generic weight operations.
with Cc51006_2; -- Extensions to weight class.

with Report;
procedure Cc51006 is

   package Metric_Wts_G is new Cc51006_1 (Cc51006_2.Grams);      -- Unconstr.
   package Metric_Wts_Mg is new Cc51006_1 (Cc51006_2.Milligrams); -- Constr.
   package Us_Wts is new Cc51006_1 (Cc51006_2.Pounds);     -- Constr.

   Gms : Cc51006_2.Grams      := 113.451;
   Mgm : Cc51006_2.Milligrams := 0.549;
   Lbs : Cc51006_2.Pounds     := 24.52;

   subtype Tc_Buffers is String (1 .. 33);

   Tc_Expected : constant Tc_Buffers := "Root type's implementation called";
   Tc_Buffer   : Tc_Buffers;

begin
   Report.Test
     ("CC51006",
      "Check that, in an instance, each implicit " &
      "declaration of a primitive subprogram of a formal " &
      "(nontagged) type declares a view of the corresponding " &
      "primitive subprogram of the ancestor type");

   Metric_Wts_G.Output_Weight (Gms, Tc_Buffer);

   if Tc_Buffer /= Tc_Expected then
      Report.Failed ("Root operation not called for unconstrained derivative");
   end if;

   Metric_Wts_Mg.Output_Weight (Mgm, Tc_Buffer);

   if Tc_Buffer /= Tc_Expected then
      Report.Failed ("Root operation not called for constrained subtype");
   end if;

   Us_Wts.Output_Weight (Lbs, Tc_Buffer);

   if Tc_Buffer /= Tc_Expected then
      Report.Failed ("Root operation not called for constrained derivative");
   end if;

   Report.Result;
end Cc51006;
