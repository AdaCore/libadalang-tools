with Cxac007_Logger;
package body Cxac007_Test is

   procedure Do_Something (Log_Name : in String) is
   begin
      Cxac007_Logger.Log_Item (Log_Name, "Doing");
      null;
      Cxac007_Logger.Log_Item (Log_Name, "Do more");
   end Do_Something;

end Cxac007_Test;
