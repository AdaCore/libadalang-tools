     --==================================================================--

with Report;
package body C460a01_2 is
   procedure Proc (P : Operand_Type; Res : out F460a00.Tc_Result_Kind) is
      Ptr : F460a00.Acctag_L0;
   begin
      Ptr := F460a00.Acctag_L0 (P);

      -- Avoid optimization (dead variable removal of Ptr):
      if not Report.Equal (Ptr.C, Ptr.C) then                  -- Always false.
         Report.Failed ("Unexpected error in C460A01_2 instance");
      end if;

      Res := F460a00.Ok;
   exception
      when Program_Error =>
         Res := F460a00.Pe_Exception;
      when others =>
         Res := F460a00.Others_Exception;
   end Proc;
end C460a01_2;
