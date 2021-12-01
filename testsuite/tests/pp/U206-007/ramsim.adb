with Ada.Text_IO;

package body RAMSim is
-- Test123456789ABCDEFGHIJKLMNOPQ

   overriding procedure Device_Setup
     (Self : in out RAMsim_Device)
   is
   begin
      Ada.Text_IO.Put_Line ("Device_Setup");
   end Device_Setup;

   overriding procedure Device_Init
     (Self : in out RAMsim_Device)
   is
   begin
      Ada.Text_IO.Put_Line ("Device_Init");
   end Device_Init;

   overriding procedure Device_Reset
     (Self : in out RAMsim_Device)
   is
   begin
      Ada.Text_IO.Put_Line ("Device_Reset");
   end Device_Reset;

   overriding procedure Device_Exit
     (Self : in out RAMsim_Device)
   is
   begin
      Ada.Text_IO.Put_Line ("Device_Exit");
   end Device_Exit;

   overriding procedure IO_Read
     (Self    : in out RAMsim_Device;
      Address :        Bus_Address;
      Length  :        Bus_Address;
      Value   :    out Bus_Data)
   is
   begin
      Ada.Text_IO.Put_Line ("IO_Read");
   end IO_Read;

   overriding procedure IO_Write
     (Self    : in out RAMsim_Device;
      Address :        Bus_Address;
      Length  :        Bus_Address;
      Value   :        Bus_Data)
   is
   begin
      Ada.Text_IO.Put_Line ("IO_Write");
   end IO_Write;

end RAMSim;
