     --==================================================================--

with Report;
procedure C3a2a01_2 (P : access Fd; R : out F3a2a00.Tc_Result_Kind) is
   Ptr   : Faf;
   Index : Integer := F3a2a00.Array_Type'First;
begin
   Ptr := P.all'Access;
   R   := F3a2a00.Ok;

   -- Avoid optimization (dead variable removal of Ptr):

   if not Report.Equal (Ptr (Index).C, Ptr (Index).C) then   -- Always false.
      Report.Failed ("Unexpected error in C3A2A01_2 instance");
   end if;
exception
   when Program_Error =>
      R := F3a2a00.P_E;
   when others =>
      R := F3a2a00.O_E;
end C3a2a01_2;
