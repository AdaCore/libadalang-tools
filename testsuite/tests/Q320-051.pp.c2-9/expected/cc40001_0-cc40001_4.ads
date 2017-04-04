----------------------------------------------------------------- CC40001_4

generic
   type Formal_Tagged_Private (<>) is tagged private;
   Tc_Check_Object : in Formal_Tagged_Private;
   with function Bad_Status (O : Formal_Tagged_Private) return Boolean;
package Cc40001_0.Cc40001_4 is
   procedure Tc_Verify_State;
end Cc40001_0.Cc40001_4;
