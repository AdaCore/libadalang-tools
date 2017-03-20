     --==================================================================--

package body C460a02_1 is
   procedure Dummy is
   begin
      null;
   end Dummy;
begin
   Fres := F460a00.Un_Init;
   Fobj := Target_Type (Ptr);
   Fres := F460a00.Ok;
exception
   when Program_Error =>
      Fres := F460a00.Pe_Exception;
   when others =>
      Fres := F460a00.Others_Exception;
end C460a02_1;
