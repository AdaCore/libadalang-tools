-----------------------------------------------------------------------------

with Cxe4005_Common;
package Cxe4005_Normal is
   type Cant_Use_In_Remote_Call is new Cxe4005_Common.Root_Tagged_Type with
   null record;

   procedure Single_Controlling_Operand
     (Rtt    :     access Cant_Use_In_Remote_Call; Test_Number : in Integer;
      Obj_Sn : out Integer);
   procedure Dual_Controlling_Operands
     (Rtt1    :     access Cant_Use_In_Remote_Call;
      Rtt2    :     access Cant_Use_In_Remote_Call; Test_Number : in Integer;
      Obj_Sn1 : out Integer; Obj_Sn2 : out Integer);

   type Open_But_Not_For_Export is new Cxe4005_Common.Open_Tagged_Type with
   null record;
end Cxe4005_Normal;
