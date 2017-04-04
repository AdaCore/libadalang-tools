     --==================================================================--

with F3a2a00;
generic
   type Fd is new F3a2a00.Tagged_Type with private;
   type Faf is access all Fd;
   Fobj : in out Fd;
package C3a2a01_1 is
   procedure Handle (R : out F3a2a00.Tc_Result_Kind);
end C3a2a01_1;
