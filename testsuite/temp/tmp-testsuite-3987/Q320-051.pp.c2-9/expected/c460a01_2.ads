     --==================================================================--

with F460a00;
generic
   type Designated_Type (<>) is new F460a00.Tagged_Type with private;
   type Operand_Type is access Designated_Type;
package C460a01_2 is
   procedure Proc (P : Operand_Type; Res : out F460a00.Tc_Result_Kind);
end C460a01_2;
