     --==================================================================--

with Report;
procedure C460a02_2
  (P : access Designated_Type'Class; Res : out F460a00.Tc_Result_Kind)
is
   Ptr : Target_Type;
begin
   Res := F460a00.Un_Init;
   Ptr := Target_Type (P);

   -- Avoid optimization (dead variable removal of Ptr):
   if not Report.Equal (Ptr.C, Ptr.C) then                  -- Always false.
      Report.Failed ("Unexpected error in C460A02_2 instance");
   end if;
   Res := F460a00.Ok;
exception
   when Program_Error =>
      Res := F460a00.Pe_Exception;
   when others =>
      Res := F460a00.Others_Exception;
end C460a02_2;
