--==================================================================--

-- Terminal_Driver.WYSE.
   package body Ca13002_0.Ca13002_4 is

   procedure Move_Cursor (Col : in Tc_Call_From) is
   begin
      Send_Control_Sequence (Fourth_Child, Col);
   end Move_Cursor;

   procedure Ca13002_5 is separate;

end Ca13002_0.Ca13002_4;
