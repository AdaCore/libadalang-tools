     --==================================================================--

with Fxf2a00;
package Cxf2a02_0.Cxf2a02_1 is

   ---=---=---=---=---=---=---=---=---=---=---

   type Micro_Ops is array (Fxf2a00.Optr_Range) of Micro_Optr_Ptr;
   type Micro_Opnds is array (Fxf2a00.Opnd_Range) of Micro;

   Micro_Mult_Operator_Table : Micro_Ops :=
     (Micro_Mult, Micro_Mult, Micro_Mult, Micro_Mult, Micro_Mult, Micro_Mult);

   Micro_Div_Operator_Table : Micro_Ops :=
     (Micro_Div, Micro_Div, Micro_Div, Micro_Div, Micro_Div, Micro_Div);

   Micro_Mult_Operand_Table : Micro_Opnds :=
     (2.351_19, 0.058_92, 9.581_22, 0.806_13, 0.934_62);

   Micro_Div_Operand_Table : Micro_Opnds :=
     (0.587_39, 4.900_12, 0.087_65, 0.715_77, 5.537_68);

   function Test_Micro_Ops is new Fxf2a00.Operations_Loop
     (Decimal_Fixed  => Micro,
      Operator_Ptr   => Micro_Optr_Ptr,
      Operator_Table => Micro_Ops,
      Operand_Table  => Micro_Opnds);

   ---=---=---=---=---=---=---=---=---=---=---

   type Basic_Ops is array (Fxf2a00.Optr_Range) of Basic_Optr_Ptr;
   type Basic_Opnds is array (Fxf2a00.Opnd_Range) of Basic;

   Basic_Mult_Operator_Table : Basic_Ops :=
     (Basic_Mult, Basic_Mult, Basic_Mult, Basic_Mult, Basic_Mult, Basic_Mult);

   Basic_Div_Operator_Table : Basic_Ops :=
     (Basic_Div, Basic_Div, Basic_Div, Basic_Div, Basic_Div, Basic_Div);

   Basic_Mult_Operand_Table : Basic_Opnds := (127.10, 0.02, 0.87, 45.67, 0.01);

   Basic_Div_Operand_Table : Basic_Opnds := (0.03, 0.08, 23.57, 0.11, 159.11);

   function Test_Basic_Ops is new Fxf2a00.Operations_Loop
     (Decimal_Fixed  => Basic,
      Operator_Ptr   => Basic_Optr_Ptr,
      Operator_Table => Basic_Ops,
      Operand_Table  => Basic_Opnds);

   ---=---=---=---=---=---=---=---=---=---=---

   type Broad_Ops is array (Fxf2a00.Optr_Range) of Broad_Optr_Ptr;
   type Broad_Opnds is array (Fxf2a00.Opnd_Range) of Broad;

   Broad_Mult_Operator_Table : Broad_Ops :=
     (Broad_Mult, Broad_Mult, Broad_Mult, Broad_Mult, Broad_Mult, Broad_Mult);

   Broad_Div_Operator_Table : Broad_Ops :=
     (Broad_Div, Broad_Div, Broad_Div, Broad_Div, Broad_Div, Broad_Div);

   Broad_Mult_Operand_Table : Broad_Opnds :=
     (589.720, 0.106, 21.018, 0.002, 0.381);

   Broad_Div_Operand_Table : Broad_Opnds :=
     (0.008, 0.793, 9.092, 214.300, 0.080);

   function Test_Broad_Ops is new Fxf2a00.Operations_Loop
     (Decimal_Fixed  => Broad,
      Operator_Ptr   => Broad_Optr_Ptr,
      Operator_Table => Broad_Ops,
      Operand_Table  => Broad_Opnds);

   ---=---=---=---=---=---=---=---=---=---=---

end Cxf2a02_0.Cxf2a02_1;
