-----------------------------------------------------------------------------

with Cxe4006_Common; use Cxe4006_Common;
package Cxe4006_Normal is
   type Normal_Spec_Tagged_Type is new Cxe4006_Common.Root_Tagged_Type with
   null record;

   procedure Single_Controlling_Operand
     (Rtt    : in out Normal_Spec_Tagged_Type; Test_Number : in Integer;
      Callee :    out Type_Decl_Location);
end Cxe4006_Normal;
