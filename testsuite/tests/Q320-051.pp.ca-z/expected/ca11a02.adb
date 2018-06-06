--=======================================================================--

with Fa11a00.Ca11a02_0;               -- Color_Widget_Pkg,
-- implicitly with Widget_Pkg
with Ca11a02_1;

with Report;

procedure Ca11a02 is

   package Widget_Pkg renames Fa11a00;
   package Color_Widget_Pkg renames Fa11a00.Ca11a02_0;

   use Widget_Pkg;              -- All user-defined operators directly visible.

   procedure Set_Label
     (The_Widget : in out Ca11a02_1.Label_Widget; L : in String)
   is
   begin
      The_Widget.Label := L;
   end Set_Label;
   ---------------------------------------------------------
   procedure Set_Widget
     (The_Widget : in out Ca11a02_1.Label_Widget; The_Width : in Widget_Length;
      The_Height : in     Widget_Length;
      The_Color : in Color_Widget_Pkg.Widget_Color_Enum; The_Label : in String)
   is
   begin
      Ca11a02_1.Set_Width (The_Widget, The_Width);    -- Twice inherited.
      Ca11a02_1.Set_Height (The_Widget, The_Height);   -- Twice inherited.
      Ca11a02_1.Set_Color (The_Widget, The_Color);     -- Inherited.
      Set_Label (The_Widget, The_Label);              -- Explicitly declared.
   end Set_Widget;

   White_Widget : Ca11a02_1.Label_Widget (11);

begin

   Report.Test
     ("CA11A02",
      "Check that a type extended in a client of " &
      "a public child inherits primitive operations from parent");

   Set_Widget (White_Widget, 15, 21, Color_Widget_Pkg.White, "Alarm_Clock");

   if White_Widget.Width /= Widget_Length (Report.Ident_Int (15)) or
     White_Widget.Height /= Widget_Length (Report.Ident_Int (21)) or
     Color_Widget_Pkg."/=" (White_Widget.Color, Color_Widget_Pkg.White) or
     White_Widget.Label /= "Alarm_Clock" then
      Report.Failed ("Incorrect result for White_Widget");
   end if;

   Report.Result;

end Ca11a02;
