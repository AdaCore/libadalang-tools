     --==================================================================--

with Report;
package body C3a2a01_0 is
   Ptr   : Faf     := X'Access;
   Index : Integer := F3a2a00.Array_Type'First;

   procedure Dummy is
   begin
      null;
   end Dummy;
begin
   -- Avoid optimization (dead variable removal of Ptr):

   if not Report.Equal (Ptr (Index).C, Ptr (Index).C) then   -- Always false.
      Report.Failed ("Unexpected error in C3A2A01_0 instance");
   end if;
end C3a2a01_0;
