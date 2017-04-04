-----------------------------------------------------------------------------

package Cxe4004_Shared is
   -- Support routines that are used in both of the partitions. Since this is a
   -- normal package, a copy of this package is present on each partition. The
   -- Chk routines are used to verify that a value is equal to the expected
   -- value. If not, Report.Failed is called with Note as the failure message.
   generic
      type The_Type is (<>);
   procedure Gen_Chk (Note : String; Actual, Expected : The_Type);

   procedure Start_Test_Section (Name : String);
end Cxe4004_Shared;
