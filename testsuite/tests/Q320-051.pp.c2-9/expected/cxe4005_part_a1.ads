-----------------------------------------------------------------------------

with Cxe4005_Common; use Cxe4005_Common;
package Cxe4005_Part_A1 is
   pragma Remote_Call_Interface;

   type Racwt is access all Cxe4005_Common.Root_Tagged_Type'Class;

   -- provide remote access values to other partitions
   function Get_Racwt
     (Which_Type : Type_Selection) return Cxe4005_Part_A1.Racwt;

   -- for checking E.4(18);6.0.
   procedure Takes_Class_Wide (X : Cxe4005_Common.Open_Tagged_Type'Class);
   function Return_Open_Tagged_Type_Class
     return Cxe4005_Common.Open_Tagged_Type'Class;

   -- coordination of test termination across partitions
   procedure Can_Quit;
   procedure Quit;

end Cxe4005_Part_A1;
