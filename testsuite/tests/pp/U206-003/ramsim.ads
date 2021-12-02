with Bus_Devices;
with Bus_Types;
with RAMsim_Controller;

package RAMSim is
   use Bus_Devices;
   use Bus_Types;
   use RAMsim_Controller;

   type RAMsim_Device
     (Vendor_Id    : Id;
      Device_Id    : Id;
      Base_Address : Bus_Address;
      Port         : Integer)
   is
      new Bus_Device (Vendor_Id, Device_Id, Port, Native_Endian) with record
         Controller : RAMsim_Control;
      end record;

 -- A comment about RAMsim_Ref before
   type RAMsim_Ref is access all RAMsim_Device'Class;
 -- A comment about RAMsim_Ref

 -- A comment about Device_Setup
   overriding procedure Device_Setup
     (Self : in out RAMsim_Device);

 -- A comment about ..
   overriding procedure Device_Init
     (Self : in out RAMsim_Device);
   overriding procedure Device_Reset
     (Self : in out RAMsim_Device);
 -- A comment about Device_Exit
   overriding procedure Device_Exit
     (Self : in out RAMsim_Device);
   overriding procedure IO_Read
     (Self    : in out RAMsim_Device;
      Address :        Bus_Address;
      Length  :        Bus_Address;
      Value   :    out Bus_Data);
   overriding procedure IO_Write
     (Self    : in out RAMsim_Device;
      Address :        Bus_Address;
      Length  :        Bus_Address;
      Value   :        Bus_Data);
end RAMSim;
