-----------------------------------------------------------------------------

with Cxe4005_Common; use Cxe4005_Common;
package Cxe4005_Remote_Types is
   pragma Remote_Types;

   type Rt_Tagged_Type is new Root_Tagged_Type with null record;

   procedure Single_Controlling_Operand (Rtt :    access Rt_Tagged_Type;
      Test_Number : in Integer; Obj_Sn : out Integer);
   procedure Dual_Controlling_Operands (Rtt1 :     access Rt_Tagged_Type;
      Rtt2 :     access Rt_Tagged_Type; Test_Number : in Integer;
      Obj_Sn1 : out Integer; Obj_Sn2 : out Integer);
end Cxe4005_Remote_Types;
