----------------------------------------------------------
----------------------------------------------------------

with System;
with Report;
with Impdef.Annex_D;
use type Impdef.Annex_D.Processor_Type;
with Cxd6002_1;
procedure Cxd6002 is
begin
   Report.Test
     ("CXD6002",
      "Check that an asynchronous transfer of control" &
      " takes place as soon" &
      " as the aborted statement is no longer in an" &
      " abort-deferred region.");

   -- the requirements on the abort being immediate are
   -- only placed upon uni-processor systems.
   if Impdef.Annex_D.Processor /= Impdef.Annex_D.Uni_Processor then
      Report.Not_Applicable ("Multi-Processor configuration");
      Report.Result;
      Cxd6002_1.Done;
      return;
   end if;

   Cxd6002_1.Simple_Case;
   Cxd6002_1.In_Rendezvous;
   Cxd6002_1.In_Protected;
   Cxd6002_1.In_Protected_Requeue;

   Cxd6002_1.Done;
   Report.Result;
end Cxd6002;
