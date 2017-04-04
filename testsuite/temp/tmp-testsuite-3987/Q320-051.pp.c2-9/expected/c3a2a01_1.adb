     --==================================================================--

with Report;
package body C3a2a01_1 is

   procedure Handle (R : out F3a2a00.Tc_Result_Kind) is
      Ptr : Faf;
   begin
      Ptr := Fobj'Access;
      R   := F3a2a00.Ok;

      -- Avoid optimization (dead variable removal of Ptr):

      if not Report.Equal (Ptr.C, Ptr.C) then              -- Always false.
         Report.Failed ("Unexpected error in Handle");
      end if;
   exception
      when Program_Error =>
         R := F3a2a00.P_E;
      when others =>
         R := F3a2a00.O_E;
   end Handle;

end C3a2a01_1;
