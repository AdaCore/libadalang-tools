-----------------------------------------------------------------------------

package Cxe5002_Part_B is
   pragma Remote_Call_Interface;

   procedure Remote_Normal (X : Integer);
   procedure Remote_Async (X : Integer);
   pragma Asynchronous (Remote_Async);
end Cxe5002_Part_B;
