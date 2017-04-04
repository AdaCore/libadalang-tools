----------------------------------------------------------------- CC40001_3

generic
   type Formal_Private (<>) is private;
   Tc_Check_Object : in Formal_Private;
   with function Bad_Status (O : Formal_Private) return Boolean;
package Cc40001_0.Cc40001_3 is
   procedure Tc_Verify_State;
end Cc40001_0.Cc40001_3;
