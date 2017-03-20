package body Cxd3003_1 is
   protected body Protected_Object is

      procedure For_Ceiling_Check (Numb : Task_Number) is

      begin
         -- In order to verify the check of Ceiling_Priority
         -- we must ensure that this procedure actually gets called.
         -- If calls to this procedure were optimized out then this
         -- part of the test would become a no-op.  The Check_Called
         -- array is checked at the end preventing optimization.
         --
         Check_Called (Numb) := True;

      end For_Ceiling_Check;

      function Verify_Calls return Boolean is
      begin
         -- Both tasks should have registered.
         --
         return Check_Called (1) and Check_Called (2);
      end Verify_Calls;

      procedure The_Int_Handler is
      begin
         null;
      end The_Int_Handler;

   end Protected_Object;

end Cxd3003_1;
