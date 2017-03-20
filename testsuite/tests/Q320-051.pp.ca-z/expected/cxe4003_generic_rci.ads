-----------------------------------------------------------------------------

generic
   Low : Integer;
   High : Integer;
package Cxe4003_Generic_Rci is
   pragma Remote_Call_Interface;
   pragma Elaborate_Body (Cxe4003_Generic_Rci);

   subtype Serial_Numbers is Integer range Low .. High;
   procedure Take_Call (X : Serial_Numbers);
   procedure Done;
end Cxe4003_Generic_Rci;
