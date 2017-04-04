     --===================================================================--

with Cc54004_0;
with Cc54004_1;
with Cc54004_3;

with Report;
procedure Cc54004 is
   Tc_Low_Ptr, Tc_Med_Ptr : Cc54004_0.Alert_Ptr;
   Tc_Low_Actual          : Cc54004_1.Low_Alert;
   Tc_Med_Actual          : Cc54004_1.Medium_Alert;

   use type Cc54004_0.Tc_Code_Type;
begin
   Report.Test
     ("CC54004",
      "Check that the designated type of a generic " &
      "formal pool-specific access type may be class-wide");

   -- Create stack of elements:

   Cc54004_3.Tc_Create_Alert_Stack;

   -- Commence dispatching operations on stack elements:

   Cc54004_3.Alert_Stacks.Process_Stack (Cc54004_3.Alert_List);

   -- Pop "handled" alerts off stack:

   Cc54004_3.Alert_Stacks.Pop (Cc54004_3.Alert_List, Tc_Med_Ptr);
   Cc54004_3.Alert_Stacks.Pop (Cc54004_3.Alert_List, Tc_Low_Ptr);

   -- Verify results:

   if Tc_Low_Ptr.all not in Cc54004_1.Low_Alert
     or else Tc_Med_Ptr.all not in Cc54004_1.Medium_Alert
   then
      Report.Failed ("Class-wide objects do not have expected tags");

   -- The explicit dereference of the "Pop"ed pointers results in views of
   -- the designated objects, the nominal subtypes of which are class-wide. In
   -- order to be able to reference the component TC_Code, these views must be
   -- converted to a specific type possessing that component.

   elsif Cc54004_1.Low_Alert (Tc_Low_Ptr.all).Tc_Code /= Cc54004_0.Low or
     Cc54004_1.Medium_Alert (Tc_Med_Ptr.all).Tc_Code /= Cc54004_0.Medium
   then
      Report.Failed ("Calls did not dispatch to expected operations");
   end if;

   Report.Result;
end Cc54004;
