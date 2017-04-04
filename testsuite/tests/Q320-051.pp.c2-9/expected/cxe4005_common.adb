---
-- This package is pure so it cannot depend upon Report
---
package body Cxe4005_Common is
   Op_Is_Zero : exception;

   -- All objects that do not have an overriding definition of
   -- Single_Controlling_Operand and Dual_Controlling_Operands have a serial
   -- number with the least significant digit in the range from 1 to 5. If a
   -- wrong object is passed to these routines then the exception Wrong_Object
   -- is raised.

   procedure Single_Controlling_Operand
     (Rtt         :        access Root_Tagged_Type;
      Test_Number : in     Integer;
      Obj_Sn      :    out Integer)
   is
   begin
      Obj_Sn := Serial_Number (Rtt);
      if Rtt.Serial_Number mod 10 not in 1 .. 5 then
         raise Wrong_Object;
      end if;
   end Single_Controlling_Operand;

   procedure Dual_Controlling_Operands
     (Rtt1        :        access Root_Tagged_Type;
      Rtt2        :        access Root_Tagged_Type;
      Test_Number : in     Integer;
      Obj_Sn1     :    out Integer;
      Obj_Sn2     :    out Integer)
   is
   begin
      Obj_Sn1 := Rtt1.Serial_Number;
      Obj_Sn2 := Rtt2.Serial_Number;

      if Rtt1.Serial_Number mod 10 not in 1 .. 5 then
         raise Wrong_Object;
      end if;
      if Rtt2.Serial_Number mod 10 not in 1 .. 5 then
         raise Wrong_Object;
      end if;
   end Dual_Controlling_Operands;

   procedure Set_Serial_Number
     (Rtt :    access Root_Tagged_Type;
      Sn  : in Integer)
   is
   begin
      Rtt.Serial_Number := Sn;
   end Set_Serial_Number;

   function Serial_Number (Rtt : access Root_Tagged_Type) return Integer is
   begin
      return Rtt.Serial_Number;
   end Serial_Number;

   procedure Open_Op (Ott : Open_Tagged_Type) is
   begin
      if Ott.Field = 0 then
         raise Op_Is_Zero;
      end if;
   end Open_Op;

end Cxe4005_Common;
