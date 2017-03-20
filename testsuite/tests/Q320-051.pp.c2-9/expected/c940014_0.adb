with Report;
with Impdef;
package body C940014_0 is
   procedure Finalize (Object : in out Has_Finalization) is
   begin
      delay Impdef.Clear_Ready_Queue;
      Finalization_Occurred := True;
      if Verbose then
         Report.Comment ("in Finalize");
      end if;
   end Finalize;
end C940014_0;
