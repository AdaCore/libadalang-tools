     --==================================================================--

with F3a2a00;
generic
   type Fd is new F3a2a00.Array_Type;
   type Faf is access all Fd;
procedure C3a2a01_2 (P : access Fd; R : out F3a2a00.Tc_Result_Kind);
