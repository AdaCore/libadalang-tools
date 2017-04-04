     --==================================================================--

with Report;
package body C460a02_0 is
   Ptr : Target_Type := Target_Type (Fobj.D);

   procedure Dummy is
   begin
      null;
   end Dummy;

begin
   -- Avoid optimization (dead variable removal of Ptr):
   if not Report.Equal (Ptr.C, Ptr.C) then                  -- Always false.
      Report.Failed ("Unexpected error in C460A02_0 instance");
   end if;

end C460a02_0;
