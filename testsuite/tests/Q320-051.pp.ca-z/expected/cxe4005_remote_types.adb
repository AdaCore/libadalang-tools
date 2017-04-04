package body Cxe4005_Remote_Types is
   --
   -- The serial number for all objects of RT_Tagged_Type will contain a 6 as
   -- the least significant digit. Make sure the correct object is passed to
   -- these routines.
   --

   procedure Single_Controlling_Operand
     (Rtt         :        access Rt_Tagged_Type;
      Test_Number : in     Integer;
      Obj_Sn      :    out Integer)
   is
   begin
      Obj_Sn := Serial_Number (Rtt);
      if Serial_Number (Rtt) mod 10 /= 6 then
         raise Wrong_Object;
      end if;
   end Single_Controlling_Operand;

   procedure Dual_Controlling_Operands
     (Rtt1        :        access Rt_Tagged_Type;
      Rtt2        :        access Rt_Tagged_Type;
      Test_Number : in     Integer;
      Obj_Sn1     :    out Integer;
      Obj_Sn2     :    out Integer)
   is
   begin
      Obj_Sn1 := Serial_Number (Rtt1);
      Obj_Sn2 := Serial_Number (Rtt2);
      if Serial_Number (Rtt1) mod 10 /= 6 or
        Serial_Number (Rtt2) mod 10 /= 6
      then
         raise Wrong_Object;
      end if;
   end Dual_Controlling_Operands;

end Cxe4005_Remote_Types;
