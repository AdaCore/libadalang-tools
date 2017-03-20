     --==================================================================--

with F460a00;
generic
   type Designated_Type is private;
   type Target_Type is access all Designated_Type;
   Fobj : in out Target_Type;
   Fres : in out F460a00.Tc_Result_Kind;
package C460a02_1 is
   type Operand_Type is access Designated_Type;
   Ptr : Operand_Type := new Designated_Type;

   procedure Dummy; -- Needed to allow package body.
end C460a02_1;
