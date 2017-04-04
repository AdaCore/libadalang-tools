---
-- This package is pure so it cannot depend upon Report
---
package body Cxe4006_Common is

   procedure Single_Controlling_Operand
     (Rtt         : in out Root_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location)
   is
      Expected : Integer;
   begin
      case Test_Number is
         when 1_001 =>
            Expected := 100;
         when 2_001 =>
            Expected := 200;
         when others =>
            raise Failed_Check;
      end case;

      if Rtt.Common_Record_Field /= Expected then
         raise Failed_Check;
      end if;

      Rtt.Common_Record_Field := Expected + 5;
      Callee                  := Common_Spec;
   end Single_Controlling_Operand;

end Cxe4006_Common;
