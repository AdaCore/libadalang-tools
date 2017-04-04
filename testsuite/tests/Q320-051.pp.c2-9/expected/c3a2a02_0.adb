     --==================================================================--

with Report;
package body C3a2a02_0 is
   X : aliased Fd;

   procedure Proc is
      Ptr : F3a2a00.Acctagclass_L0 := X'Access;
   begin
      -- Avoid optimization (dead variable removal of Ptr):

      if not Report.Equal (Ptr.C, Ptr.C) then              -- Always false.
         Report.Failed ("Unexpected error in Proc");
      end if;
   end Proc;
end C3a2a02_0;
