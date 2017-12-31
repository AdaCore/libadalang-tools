     --==================================================================--

with Cxf2003_0;
with Cxf2003_1;

with Report;
procedure Cxf2003 is

   Loop_Count : constant := 1_825;
   type Loop_Range is range 1 .. Loop_Count;

begin

   Report.Test
     ("CXF2003",
      "Check decimal multiplication and division, and " &
      "'Round, where the operand types are different");

   ---=---=---=---=---=---=---=---=---=---=---

   Radix_2_Subtests :
   declare
      package Radix_2 is new Cxf2003_0 (Cxf2003_1.Money_Radix2,
         Cxf2003_1.Interest_Rate);
      use type Cxf2003_1.Money_Radix2;
      use type Cxf2003_1.Interest_Rate;
   begin

      Radix_2_Multiplication :
      declare
         Rate   : Cxf2003_1.Interest_Rate := 0.198;
         Period : Integer                 := 365;
         Factor : Cxf2003_1.Interest_Rate := Rate / Period;

         Initial        : constant Cxf2003_1.Money_Radix2 := 1_000.00;
         Trunc_Expected : constant Cxf2003_1.Money_Radix2 := 2_662.94;
         Round_Expected : constant Cxf2003_1.Money_Radix2 := 2_678.34;

         Balance : Cxf2003_1.Money_Radix2;
      begin
         ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_2.Multiply_And_Truncate (Balance, Factor);
         end loop;

         if Balance /= Trunc_Expected then
            Report.Failed ("Wrong result: Radix 2 multiply and truncate");
         end if;

         ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_2.Multiply_And_Round (Balance, Factor);
         end loop;

         if Balance /= Round_Expected then
            Report.Failed ("Wrong result: Radix 2 multiply and round");
         end if;

         ---=---=---=---=---=---=---
      end Radix_2_Multiplication;

      Radix_2_Division :
      declare
         Rate    : Cxf2003_1.Interest_Rate := 0.129;
         Period  : Integer                 := 365;
         Factor  : Cxf2003_1.Interest_Rate := Rate / Period;
         Divisor : Cxf2003_1.Interest_Rate := 1.0 / Factor;

         Initial        : constant Cxf2003_1.Money_Radix2 := 14_626.52;
         Trunc_Expected : constant Cxf2003_1.Money_Radix2 := 27_688.26;
         Round_Expected : constant Cxf2003_1.Money_Radix2 := 27_701.12;

         Balance : Cxf2003_1.Money_Radix2;
      begin
         ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_2.Divide_And_Truncate (Balance, Divisor);
         end loop;

         if Balance /= Trunc_Expected then
            Report.Failed ("Wrong result: Radix 2 divide and truncate");
         end if;

         ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_2.Divide_And_Round (Balance, Divisor);
         end loop;

         if Balance /= Round_Expected then
            Report.Failed ("Wrong result: Radix 2 divide and round");
         end if;

         ---=---=---=---=---=---=---
      end Radix_2_Division;

   end Radix_2_Subtests;

   ---=---=---=---=---=---=---=---=---=---=---

   Radix_10_Subtests :
   declare
      package Radix_10 is new Cxf2003_0 (Cxf2003_1.Money_Radix10,
         Cxf2003_1.Interest_Rate);
      use type Cxf2003_1.Money_Radix10;
      use type Cxf2003_1.Interest_Rate;
   begin

      Radix_10_Multiplication :
      declare
         Rate   : Cxf2003_1.Interest_Rate := 0.063;
         Period : Integer                 := 365;
         Factor : Cxf2003_1.Interest_Rate := Rate / Period;

         Initial        : constant Cxf2003_1.Money_Radix10 := 314_036.10;
         Trunc_Expected : constant Cxf2003_1.Money_Radix10 := 428_249.48;
         Round_Expected : constant Cxf2003_1.Money_Radix10 := 428_260.52;

         Balance : Cxf2003_1.Money_Radix10;
      begin
         ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_10.Multiply_And_Truncate (Balance, Factor);
         end loop;

         if Balance /= Trunc_Expected then
            Report.Failed ("Wrong result: Radix 10 multiply and truncate");
         end if;

         ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_10.Multiply_And_Round (Balance, Factor);
         end loop;

         if Balance /= Round_Expected then
            Report.Failed ("Wrong result: Radix 10 multiply and round");
         end if;

         ---=---=---=---=---=---=---
      end Radix_10_Multiplication;

      Radix_10_Division :
      declare
         Rate    : Cxf2003_1.Interest_Rate := 0.273;
         Period  : Integer                 := 365;
         Factor  : Cxf2003_1.Interest_Rate := Rate / Period;
         Divisor : Cxf2003_1.Interest_Rate := 1.0 / Factor;

         Initial        : constant Cxf2003_1.Money_Radix10 := 25.72;
         Trunc_Expected : constant Cxf2003_1.Money_Radix10 := 79.05;
         Round_Expected : constant Cxf2003_1.Money_Radix10 := 97.46;

         Balance : Cxf2003_1.Money_Radix10;
      begin
         ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_10.Divide_And_Truncate (Balance, Divisor);
         end loop;

         if Balance /= Trunc_Expected then
            Report.Failed ("Wrong result: Radix 10 divide and truncate");
         end if;

         ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_10.Divide_And_Round (Balance, Divisor);
         end loop;

         if Balance /= Round_Expected then
            Report.Failed ("Wrong result: Radix 10 divide and round");
         end if;

         ---=---=---=---=---=---=---
      end Radix_10_Division;

   end Radix_10_Subtests;

   ---=---=---=---=---=---=---=---=---=---=---

   Report.Result;

end Cxf2003;
