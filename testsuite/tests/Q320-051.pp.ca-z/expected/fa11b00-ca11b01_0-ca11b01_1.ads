--=======================================================================--

-- Grandchild package of FA11B00, child package of FA11B00.CA11B01_0.
   package Fa11b00.Ca11b01_0.Ca11b01_1 is     -- Application_Three_Widget
-- This public grandchild declares a derived type from its parent. It
-- represents processing of widgets in a window system.

   type App3_Widget is new App2_Widget;    -- Derived record of App2_Widget.

   -- Inherits (inherited) procedure Create_Widget from Application_One_Widget.
   -- Inherits procedure App2_Widget_Specific_Oper from App2_Widget.

   -- Primitive operation of type App3_Widget.
   procedure App3_Widget_Specific_Oper
     (The_Widget : in out App3_Widget; S : in Widget_Size);

end Fa11b00.Ca11b01_0.Ca11b01_1;           -- Application_Three_Widget
