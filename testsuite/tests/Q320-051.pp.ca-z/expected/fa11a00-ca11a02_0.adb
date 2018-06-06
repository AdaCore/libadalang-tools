--=======================================================================--

package body Fa11a00.Ca11a02_0 is     -- Color_Widget_Pkg

   procedure Set_Color
     (The_Widget : in out Color_Widget; C : in Widget_Color_Enum)
   is
   begin
      The_Widget.Color := C;
   end Set_Color;

end Fa11a00.Ca11a02_0;     -- Color_Widget_Pkg
