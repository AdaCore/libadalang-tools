----------------------------------------------------------------- CC40001_1

with Ada.Calendar;
package Cc40001_0.Cc40001_1 is

   type Object_In_Time (Id : Character) is new Simple_Object (Id) with record
      Birth    : Ada.Calendar.Time;
      Activity : Ada.Calendar.Time;
   end record;

   procedure User_Operation (Cob : in out Object_In_Time; Name : String);

   procedure Initialize (Cob : in out Object_In_Time);
   procedure Adjust (Cob : in out Object_In_Time);
   procedure Finalize (Cob : in out Object_In_Time);

end Cc40001_0.Cc40001_1;
