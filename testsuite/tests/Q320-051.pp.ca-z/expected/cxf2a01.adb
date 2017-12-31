     --==================================================================--

with Cxf2a01_0.Cxf2a01_1;

with Report;
procedure Cxf2a01 is
   package Data renames Cxf2a01_0.Cxf2a01_1;

   use type Cxf2a01_0.Micro;
   use type Cxf2a01_0.Money;
   use type Cxf2a01_0.Cash;
   use type Cxf2a01_0.Broad;

   Micro_Cancel_Expected : constant Cxf2a01_0.Micro := 0.0;
   Money_Cancel_Expected : constant Cxf2a01_0.Money := 0.0;
   Cash_Cancel_Expected  : constant Cxf2a01_0.Cash  := 0.0;
   Broad_Cancel_Expected : constant Cxf2a01_0.Broad := 0.0;

   Micro_Cumul_Expected : constant Cxf2a01_0.Micro :=
     0.075_682_140_420_000_000;
   Money_Cumul_Expected : constant Cxf2a01_0.Money := -21_327_300.00;
   Cash_Cumul_Expected  : constant Cxf2a01_0.Cash  := 624_570_600.00;
   Broad_Cumul_Expected : constant Cxf2a01_0.Broad := -9_015_252.535_794_000;

   Micro_Actual : Cxf2a01_0.Micro;
   Money_Actual : Cxf2a01_0.Money;
   Cash_Actual  : Cxf2a01_0.Cash;
   Broad_Actual : Cxf2a01_0.Broad;
begin

   Report.Test ("CXF2A01", "Check decimal addition and subtraction");

   ---=---=---=---=---=---=---=---=---=---=---

   Micro_Actual :=
     Data.Test_Micro_Ops
       (0.0, Data.Micro_Optr_Table_Cancel, Data.Micro_Opnd_Table_Cancel);

   if Micro_Actual /= Micro_Cancel_Expected then
      Report.Failed ("Wrong cancellation result for type Micro");
   end if;

   ---=---=---=---=---=---=---

   Micro_Actual :=
     Data.Test_Micro_Ops
       (0.0, Data.Micro_Optr_Table_Cumul, Data.Micro_Opnd_Table_Cumul);

   if Micro_Actual /= Micro_Cumul_Expected then
      Report.Failed ("Wrong cumulation result for type Micro");
   end if;

   ---=---=---=---=---=---=---=---=---=---=---

   Money_Actual :=
     Data.Test_Money_Ops
       (0.0, Data.Money_Optr_Table_Cancel, Data.Money_Opnd_Table_Cancel);

   if Money_Actual /= Money_Cancel_Expected then
      Report.Failed ("Wrong cancellation result for type Money");
   end if;

   ---=---=---=---=---=---=---

   Money_Actual :=
     Data.Test_Money_Ops
       (0.0, Data.Money_Optr_Table_Cumul, Data.Money_Opnd_Table_Cumul);

   if Money_Actual /= Money_Cumul_Expected then
      Report.Failed ("Wrong cumulation result for type Money");
   end if;

   ---=---=---=---=---=---=---=---=---=---=---

   Cash_Actual :=
     Data.Test_Cash_Ops
       (0.0, Data.Cash_Optr_Table_Cancel, Data.Cash_Opnd_Table_Cancel);

   if Cash_Actual /= Cash_Cancel_Expected then
      Report.Failed ("Wrong cancellation result for type Cash");
   end if;

   ---=---=---=---=---=---=---

   Cash_Actual :=
     Data.Test_Cash_Ops
       (0.0, Data.Cash_Optr_Table_Cumul, Data.Cash_Opnd_Table_Cumul);

   if Cash_Actual /= Cash_Cumul_Expected then
      Report.Failed ("Wrong cumulation result for type Cash");
   end if;

   ---=---=---=---=---=---=---=---=---=---=---

   Broad_Actual :=
     Data.Test_Broad_Ops
       (0.0, Data.Broad_Optr_Table_Cancel, Data.Broad_Opnd_Table_Cancel);

   if Broad_Actual /= Broad_Cancel_Expected then
      Report.Failed ("Wrong cancellation result for type Broad");
   end if;

   ---=---=---=---=---=---=---

   Broad_Actual :=
     Data.Test_Broad_Ops
       (0.0, Data.Broad_Optr_Table_Cumul, Data.Broad_Opnd_Table_Cumul);

   if Broad_Actual /= Broad_Cumul_Expected then
      Report.Failed ("Wrong cumulation result for type Broad");
   end if;

   ---=---=---=---=---=---=---=---=---=---=---

   Report.Result;

end Cxf2a01;
