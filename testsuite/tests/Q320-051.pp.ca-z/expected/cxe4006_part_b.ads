-----------------------------------------------------------------------------

with Cxe4006_Common; use Cxe4006_Common;
with Cxe4006_Part_A1;
package Cxe4006_Part_B is
   pragma Remote_Call_Interface;

   -- tagged types that can be passed between partitions
   type B_Tagged_Type is new Root_Tagged_Type with null record;

   procedure Single_Controlling_Operand (Rtt : in out B_Tagged_Type;
      Test_Number : in     Integer; Callee : out Type_Decl_Location);

   procedure Wrapped_Around (X : in out Root_Tagged_Type'Class;
      Test_Number              : in Integer; Callee : out Type_Decl_Location);
end Cxe4006_Part_B;
