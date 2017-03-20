--==================================================================--

-- Terminal_Driver.
   package body Ca13002_0 is

   procedure Send_Control_Sequence (Row : in Tc_Name; Col : in Tc_Call_From) is
   begin
      -- Reads a key and takes action.
      Tc_Calls (Row, Col) := True;
   end Send_Control_Sequence;

end Ca13002_0;
