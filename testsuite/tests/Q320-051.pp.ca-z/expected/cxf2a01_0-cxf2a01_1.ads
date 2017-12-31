     --==================================================================--

with Fxf2a00;
package Cxf2a01_0.Cxf2a01_1 is

   ---=---=---=---=---=---=---=---=---=---=---

   type Micro_Ops is array (Fxf2a00.Optr_Range) of Micro_Optr_Ptr;
   type Micro_Opnds is array (Fxf2a00.Opnd_Range) of Micro;

   Micro_Optr_Table_Cancel : Micro_Ops :=
     (Micro_Add, Micro_Sub, Micro_Add, Micro_Sub, Micro_Add, Micro_Sub);

   Micro_Optr_Table_Cumul : Micro_Ops := (others => Micro_Add);

   Micro_Opnd_Table_Cancel : Micro_Opnds :=
     (0.001_025_000_235_111_997, 0.000_000_000_000_000_003,
      0.724_902_903_219_925_400, 0.000_459_228_020_000_011,
      0.049_832_104_921_096_533);

   Micro_Opnd_Table_Cumul : Micro_Opnds :=
     (0.000_002_309_540_000_000, 0.000_000_278_060_000_000,
      0.000_000_000_000_070_000, 0.000_010_003_000_000_000,
      0.000_000_023_090_000_000);

   function Test_Micro_Ops is new Fxf2a00.Operations_Loop
     (Decimal_Fixed  => Micro, Operator_Ptr => Micro_Optr_Ptr,
      Operator_Table => Micro_Ops, Operand_Table => Micro_Opnds);

   ---=---=---=---=---=---=---=---=---=---=---

   type Money_Ops is array (Fxf2a00.Optr_Range) of Money_Optr_Ptr;
   type Money_Opnds is array (Fxf2a00.Opnd_Range) of Money;

   Money_Optr_Table_Cancel : Money_Ops :=
     (Money_Add, Money_Add, Money_Sub, Money_Add, Money_Sub, Money_Sub);

   Money_Optr_Table_Cumul : Money_Ops := (others => Money_Sub);

   Money_Opnd_Table_Cancel : Money_Opnds :=
     (127.10, 5_600.44, 0.05, 189_662.78, 226_900_402.99);

   Money_Opnd_Table_Cumul : Money_Opnds :=
     (17.99, 500.41, 92.78, 0.38, 2_942.99);

   function Test_Money_Ops is new Fxf2a00.Operations_Loop
     (Decimal_Fixed  => Money, Operator_Ptr => Money_Optr_Ptr,
      Operator_Table => Money_Ops, Operand_Table => Money_Opnds);

   ---=---=---=---=---=---=---=---=---=---=---

   type Cash_Ops is array (Fxf2a00.Optr_Range) of Cash_Optr_Ptr;
   type Cash_Opnds is array (Fxf2a00.Opnd_Range) of Cash;

   Cash_Optr_Table_Cancel : Cash_Ops :=
     (Cash_Add, Cash_Add, Cash_Sub, Cash_Add, Cash_Sub, Cash_Sub);

   Cash_Optr_Table_Cumul : Cash_Ops := (others => Cash_Add);

   Cash_Opnd_Table_Cancel : Cash_Opnds :=
     (127.10, 5_600.44, 0.05, 189_662.78, 226_900_402.99);

   Cash_Opnd_Table_Cumul : Cash_Opnds :=
     (3.33, 100_056.14, 22.87, 3_901.55, 111.21);

   function Test_Cash_Ops is new Fxf2a00.Operations_Loop
     (Decimal_Fixed  => Cash, Operator_Ptr => Cash_Optr_Ptr,
      Operator_Table => Cash_Ops, Operand_Table => Cash_Opnds);

   ---=---=---=---=---=---=---=---=---=---=---

   type Broad_Ops is array (Fxf2a00.Optr_Range) of Broad_Optr_Ptr;
   type Broad_Opnds is array (Fxf2a00.Opnd_Range) of Broad;

   Broad_Optr_Table_Cancel : Broad_Ops :=
     (Broad_Sub, Broad_Add, Broad_Add, Broad_Sub, Broad_Sub, Broad_Add);

   Broad_Optr_Table_Cumul : Broad_Ops := (others => Broad_Sub);

   Broad_Opnd_Table_Cancel : Broad_Opnds :=
     (1.000_009_092, 732_919_479.445_022_293, 89_662.787_000_006,
      660.101_010_133, 1_121_127.999_905_594);

   Broad_Opnd_Table_Cumul : Broad_Opnds :=
     (12.000_450_223, 479.430_320_780, 0.003_492_096, 8.112_888_400,
      1_002.994_937_800);

   function Test_Broad_Ops is new Fxf2a00.Operations_Loop
     (Decimal_Fixed  => Broad, Operator_Ptr => Broad_Optr_Ptr,
      Operator_Table => Broad_Ops, Operand_Table => Broad_Opnds);

   ---=---=---=---=---=---=---=---=---=---=---

end Cxf2a01_0.Cxf2a01_1;
