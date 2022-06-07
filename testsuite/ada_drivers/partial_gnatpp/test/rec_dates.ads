package Rec_Dates is

   type Months is
     (January, February, March, April,
      May, June, July, August, September,
      October, November, December);

   type Date is record
      Day   : Integer range 1 .. 31;
      Month : Months;
      Year  : Integer range 1 .. 3000 := 2032;
   end record;

   procedure Increase_Month (Some_Day : in out Date);

   procedure Display_Month (Some_Day : Date);

end Rec_Dates;
