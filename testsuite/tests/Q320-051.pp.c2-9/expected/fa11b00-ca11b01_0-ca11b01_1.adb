--=======================================================================--

package body Fa11b00.Ca11b01_0.Ca11b01_1 is     -- Application_Three_Widget

   procedure App3_Widget_Specific_Oper
     (The_Widget : in out App3_Widget; S : in Widget_Size)
   is
   begin
      The_Widget.Size := S;
   end App3_Widget_Specific_Oper;

end Fa11b00.Ca11b01_0.Ca11b01_1;                -- Application_Three_Widget
