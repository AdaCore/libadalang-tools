with Ada.Text_IO; use Ada.Text_IO;

with Pkg;               use Pkg;
with Pkg.TGen_Support;  use Pkg.TGen_Support;
with TGen.JSON;
with TGen.TGen_Support; use TGen.TGen_Support;

procedure Example_Gen is

   Unit_JSON : TGen.JSON.JSON_Value := TGen.JSON.Create_Object;

begin

   --  For the following procedure:
   --
   --  procedure Test_Cst (B : Boolean) with Global => Cst;
   --
   --  We should not produce a value for the Global Cst, as it is constant as
   --  its name indicates.

   pkg_test_cst_11dedb213c57ec3a_Dump_TC
     (TGen_Marshalling_B         => False,
      TGen_Marshalling_Unit_JSON => Unit_JSON,
      TGen_Marshalling_Origin    => "Dummy");

   --  For the following procedure:
   --
   --  procedure Test_Supported (B : Boolean)  with Global => Supported_Global;
   --
   --  We should produce a value for the global Supported_Global as it is an
   --  implicit input of the subprogram.

   pkg_test_supported_ff2074aeb77f6e9e_Dump_TC
     (TGen_Marshalling_B                    => False,
      TGen_Marshalling_Pkg_Supported_Global => 3,
      TGen_Marshalling_Unit_JSON            => Unit_JSON,
      TGen_Marshalling_Origin               => "Dummy");

   --  For the following procedure:
   --
   --   procedure Test_Output (B : Boolean)
   --     with Global => (Output => Supported_Global);
   --
   --  We should not produce a value for Supported_Global as it is an output
   --  of the subprogram.

   pkg_test_output_b0e4810aba0b5de7_Dump_TC
     (TGen_Marshalling_B         => False,
      TGen_Marshalling_Unit_JSON => Unit_JSON,
      TGen_Marshalling_Origin    => "Dummy");

   --  For the following procedure:
   --
   --  procedure Test_Input (B : Boolean)
   --    with Global => (Input => Supported_Global);
   --
   --  We should produce a value for the global Supported_Global as it is an
   --  explicit input of the subprogram.

   pkg_test_input_2517761d8d4f768b_Dump_TC
     (TGen_Marshalling_B                    => False,
      TGen_Marshalling_Pkg_Supported_Global => 3,
      TGen_Marshalling_Unit_JSON            => Unit_JSON,
      TGen_Marshalling_Origin               => "Dummy");

   --  For the following procedure:
   --
   --  procedure Test_In_Out (B : Boolean)
   --    with Global => (In_Out => Supported_Global);
   --
   --  We should produce a value for the global Supported_Global as it is an
   --  explicit input of the subprogram.

   pkg_test_in_out_e4a3f036cf7f496b_Dump_TC
     (TGen_Marshalling_B                    => False,
      TGen_Marshalling_Pkg_Supported_Global => 3,
      TGen_Marshalling_Unit_JSON            => Unit_JSON,
      TGen_Marshalling_Origin               => "Dummy");

   Put_Line (Unit_JSON.Write (Compact => False));

end;
