     --==================================================================--

with Cxf2005_0;
with Cxf2005_1;

with Report;
procedure Cxf2005 is

   Loop_Count : constant := 25_000;
   type Loop_Range is range 1 .. Loop_Count;

begin

   Report.Test
     ("CXF2005",
      "Check decimal multiplication and division, " &
      "where one operand type is Integer");

   ---=---=---=---=---=---=---=---=---=---=---

   Radix_2_Subtests :
   declare
      package Radix_2 is new Cxf2005_0 (Cxf2005_1.Money_Radix2);
      use type Cxf2005_1.Money_Radix2;
   begin

      Radix_2_Multiplication :
      declare
         Rate   : constant Cxf2005_1.Interest_Rate := 0.127;
         Period : constant Integer                 := 12;

         Expected : constant Cxf2005_1.Money_Radix2 := 2_624.88;
         Balance  : Cxf2005_1.Money_Radix2          := 1_000.00;

         Operand   : Cxf2005_1.Money_Radix2;
         Increment : Cxf2005_1.Money_Radix2;
         Interval  : Integer;
      begin

         for I in Loop_Range loop
            Interval  := (Integer (I) mod Period) + 1;  -- Range from 1 to 12.
            Operand   := Cxf2005_1.Factor (Rate, Period);
            Increment := Radix_2.Multiply (Operand, Interval);
            Balance   := Balance + Increment;
         end loop;

         if Balance /= Expected then
            Report.Failed ("Error: Radix 2 multiply");
         end if;

      end Radix_2_Multiplication;

      Radix_2_Division :
      declare
         Rate   : constant Cxf2005_1.Interest_Rate := 0.377;
         Period : constant Integer                 := 12;

         Expected : constant Cxf2005_1.Money_Radix2 := 36_215.58;
         Balance  : Cxf2005_1.Money_Radix2          := 456_985.01;

         Operand   : Cxf2005_1.Money_Radix2;
         Increment : Cxf2005_1.Money_Radix2;
         Interval  : Integer;
      begin

         for I in Loop_Range loop
            Interval :=
              (Integer (I + 1_000) mod (200 * Period)) + 1; -- 1 .. 2400.
            Operand   := Cxf2005_1.Factor (Rate, Period);
            Increment := Radix_2.Divide (Balance, Interval);
            Balance   := Balance - (Operand * Increment);
         end loop;

         if Balance /= Expected then
            Report.Failed ("Error: Radix 2 divide");
         end if;

      end Radix_2_Division;

   end Radix_2_Subtests;

   ---=---=---=---=---=---=---=---=---=---=---

   Radix_10_Subtests :
   declare
      package Radix_10 is new Cxf2005_0 (Cxf2005_1.Money_Radix10);
      use type Cxf2005_1.Money_Radix10;
   begin

      Radix_10_Multiplication :
      declare
         Rate   : constant Cxf2005_1.Interest_Rate := 0.721;
         Period : constant Integer                 := 12;

         Expected : constant Cxf2005_1.Money_Radix10 := 9_875.62;
         Balance  : Cxf2005_1.Money_Radix10          := 126.34;

         Operand   : Cxf2005_1.Money_Radix10;
         Increment : Cxf2005_1.Money_Radix10;
         Interval  : Integer;
      begin

         for I in Loop_Range loop
            Interval  := (Integer (I) mod Period) + 1;  -- Range from 1 to 12.
            Operand   := Cxf2005_1.Factor (Rate, Period);
            Increment := Radix_10.Multiply (Operand, Interval);
            Balance   := Balance + Increment;
         end loop;

         if Balance /= Expected then
            Report.Failed ("Error: Radix 10 multiply");
         end if;

      end Radix_10_Multiplication;

      Radix_10_Division :
      declare
         Rate   : constant Cxf2005_1.Interest_Rate := 0.547;
         Period : constant Integer                 := 12;

         Expected : constant Cxf2005_1.Money_Radix10 := 26_116.37;
         Balance  : Cxf2005_1.Money_Radix10          := 770_082.46;

         Operand   : Cxf2005_1.Money_Radix10;
         Increment : Cxf2005_1.Money_Radix10;
         Interval  : Integer;
      begin

         for I in Loop_Range loop
            Interval :=
              (Integer (I + 1_000) mod (200 * Period)) + 1; -- 1 .. 2400.
            Operand   := Cxf2005_1.Factor (Rate, Period);
            Increment := Radix_10.Divide (Balance, Interval);
            Balance   := Balance - (Operand * Increment);
         end loop;

         if Balance /= Expected then
            Report.Failed ("Error: Radix 10 divide");
         end if;

      end Radix_10_Division;

   end Radix_10_Subtests;

   ---=---=---=---=---=---=---=---=---=---=---

   Report.Result;

end Cxf2005;
