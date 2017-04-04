------------------------------------------------------------------

with Ada.Task_Attributes;
with Cxc7002_0;
package Cxc7002_1 is new Ada.Task_Attributes
  (Attribute => Cxc7002_0.Int_Array,
   Initial_Value => Cxc7002_0.Countdown);   -- 3,2,1
