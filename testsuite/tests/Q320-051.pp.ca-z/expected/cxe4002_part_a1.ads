-----------------------------------------------------------------------------

with Cxe4002_Common;
package Cxe4002_Part_A1 is
   pragma Remote_Call_Interface;

   -- for convenience, rename the imported types used for parameters
   subtype Little_Number is Cxe4002_Common.Little_Number;
   subtype Integer_Vector is Cxe4002_Common.Integer_Vector;
   subtype Description is Cxe4002_Common.Description;
   subtype Record_Data is Cxe4002_Common.Record_Data;

   -- simple integer and float tests
   procedure Check_In
     (Little : in Little_Number; Real : in Float; Int : in Integer);
   procedure Set_Out
     (Little : out Little_Number; Real : out Float; Int : out Integer);
   procedure Decr
     (Little : in out Little_Number; Real : in out Float;
      Int    : in out Integer);

   -- record tests
   function Current_Record (Name : Description) return Record_Data;
   procedure Update_Record
     (Old_Data : in Record_Data; New_Data : out Record_Data);

   -- array tests
   function "+" (A, B : in Integer_Vector) return Integer_Vector;
   procedure Incr_Vector (X : in out Integer_Vector);

   -- access test support
   procedure Call_With_4 (T : Integer);

   -- coordination of test termination across partitions
   procedure Can_Quit;
   procedure Quit;
end Cxe4002_Part_A1;
