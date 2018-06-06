with Report;
package body Cxe4005_Normal is
   procedure Single_Controlling_Operand
     (Rtt    :     access Cant_Use_In_Remote_Call; Test_Number : in Integer;
      Obj_Sn : out Integer)
   is
   begin
      Report.Failed
        ("Call made where type is declared in a normal " &
         "package.  Test number " & Integer'Image (Test_Number));
      Obj_Sn := Serial_Number (Rtt);
   end Single_Controlling_Operand;

   procedure Dual_Controlling_Operands
     (Rtt1    :     access Cant_Use_In_Remote_Call;
      Rtt2    :     access Cant_Use_In_Remote_Call; Test_Number : in Integer;
      Obj_Sn1 : out Integer; Obj_Sn2 : out Integer)
   is
   begin
      Report.Failed
        ("Call made where type is declared in a normal " &
         "package.  Test number " & Integer'Image (Test_Number));
      Obj_Sn1 := Serial_Number (Rtt1);
      Obj_Sn2 := Serial_Number (Rtt2);
   end Dual_Controlling_Operands;
end Cxe4005_Normal;
