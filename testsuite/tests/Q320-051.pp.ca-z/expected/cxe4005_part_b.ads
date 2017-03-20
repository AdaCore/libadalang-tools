-----------------------------------------------------------------------------

with Cxe4005_Common; use Cxe4005_Common;
with Cxe4005_Part_A1;
package Cxe4005_Part_B is
   pragma Remote_Call_Interface;

   -- provide remote access values to other partitions
   function Get_Racwt
     (Which_Type : Type_Selection) return Cxe4005_Part_A1.Racwt;
end Cxe4005_Part_B;
