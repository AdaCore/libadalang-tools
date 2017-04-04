--==================================================================--

-- Terminal_Driver.IBM3270.
   package body Ca13002_0.Ca13002_2 is

   procedure Move_Cursor (Col : in Tc_Call_From) is
   begin
      Send_Control_Sequence (Second_Child, Col);
   end Move_Cursor;

end Ca13002_0.Ca13002_2;
