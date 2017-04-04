-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
package body C760001_0 is

   Global_Unique_Counter : Unique_Id := 0;

   function Unique_Value return Unique_Id is
   begin
      Global_Unique_Counter := Global_Unique_Counter + 1;
      return Global_Unique_Counter;
   end Unique_Value;

   function Most_Recent_Unique_Value return Unique_Id is
   begin
      return Global_Unique_Counter;
   end Most_Recent_Unique_Value;

   procedure Initialize (R : in out Root_Controlled) is
   begin
      if Tc_Initialize_Calls_Is_Failing then
         Report.Failed ("Initialized incorrectly called");
      end if;
      R.My_Init_Id := Unique_Value;
   end Initialize;

   procedure Adjust (R : in out Root_Controlled) is
   begin
      R.My_Adj_Id := Unique_Value;
   end Adjust;

end C760001_0;
