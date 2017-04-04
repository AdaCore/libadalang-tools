--==================================================================--

-- Terminal_Driver.DOS_ANSI.
   package body Ca13002_0.Ca13002_3 is

   procedure Move_Cursor (Col : in Tc_Call_From) is
   begin
      Send_Control_Sequence (Third_Child, Col);
   end Move_Cursor;

   procedure Ca13002_5 is separate;

end Ca13002_0.Ca13002_3;
