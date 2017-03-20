     --==================================================================--

with Report;
package body C3a2a02_2 is
   Ptro : Gao := Fobj'Access;

   procedure Dummy is
   begin
      null;
   end Dummy;
begin
   Ptrf := Xg'Access;

   -- Avoid optimization (dead variable removal of PtrO and/or PtrF):

   if not Report.Equal (Ptro.C, Ptro.C) then                -- Always false.
      Report.Failed ("Unexpected error in C3A2A02_2 instance: PtrO");
   end if;

   if not Report.Equal (Ptrf (Index).C, Ptrf (Index).C) then  -- Always false.
      Report.Failed ("Unexpected error in C3A2A02_2 instance: PtrF");
   end if;
end C3a2a02_2;
