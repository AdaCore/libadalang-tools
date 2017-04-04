--=======================================================================--

with Fa11a00.Ca11a02_0;               -- Color_Widget_Pkg.

package Ca11a02_1 is

   type Label_Widget
     (Str_Disc : Integer)
   is new Fa11a00.Ca11a02_0.Color_Widget with
   record
      Label : String (1 .. Str_Disc);
   end record;

   -- Inherits (inherited) procedure Set_Width from Color_Widget.
   -- Inherits (inherited) procedure Set_Height from Color_Widget.
   -- Inherits procedure Set_Color from Color_Widget.

end Ca11a02_1;
