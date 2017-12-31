-----------------------------------------------------------------------------

with Cxe4004_Common; use Cxe4004_Common;
package Cxe4004_Part_A1 is
   pragma Remote_Call_Interface;

   procedure Check_In (Test_No : in Integer; Vec : in Integer_Vector;
      Dr1 : in Discriminated_Record; Dr2 : in Another_Discriminated_Record;
      Tr                       : in Extended_2);
   procedure Set_Out (Test_No : in     Integer; Vec : out Integer_Vector;
      Dr1 : out Discriminated_Record; Dr2 : out Another_Discriminated_Record;
      Tr                      :    out Extended_2);
   procedure Modify (Test_No : in     Integer; Vec : in out Integer_Vector;
      Dr1                    : in out Discriminated_Record;
      Dr2 : in out Another_Discriminated_Record; Tr : in out Extended_2);

   -- big parameter array tests
   function "+" (A, B : in Integer_Vector) return Integer_Vector;

   -- coordination of test termination across partitions
   procedure Can_Quit;
   procedure Quit;
end Cxe4004_Part_A1;
