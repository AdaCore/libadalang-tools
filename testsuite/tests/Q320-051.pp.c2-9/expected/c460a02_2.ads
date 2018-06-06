     --==================================================================--

with F460a00;
generic
   type Designated_Type is new F460a00.Tagged_Type with private;
   type Target_Type is access constant Designated_Type;
procedure C460a02_2
  (P : access Designated_Type'Class; Res : out F460a00.Tc_Result_Kind);
