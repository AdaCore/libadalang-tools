--------------------------------------------------------------------------

with Report;
package body Cxc6002_1 is

   procedure Check_Copy_Semantics (Unit : in out Generator;
      Copy                              : in     Generator)
   is
   begin
      Unit := ("Solar Collector", Year_In_Service => 1_994);
      if (Copy.Year_In_Service = 1_994) or (Copy.Kind = "Solar Collector") then
         Report.Failed ("Formal not passed by copy");
      end if;
   end Check_Copy_Semantics;

end Cxc6002_1;
