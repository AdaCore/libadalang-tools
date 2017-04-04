--==================================================================--

with Ca11022_0.Ca11022_2;              -- VT100 Graphic, implicitly with
-- CA11022_0, Graphic Manager.
with Ca11022_0.Ca11022_3;              -- IBM3270 Graphic.
with Report;

procedure Ca11022 is

begin

   Report.Test
     ("CA11022",
      "Check that body of a child unit can depend on " &
      "its generic sibling");

   -- Customized graphic functions for the VT100 terminal.
   Ca11022_0.Ca11022_2.Vt100_Graphic;

   if not Ca11022_0.Tc_Screen (4, 9) and
     not Ca11022_0.Tc_Screen (5, 10) and
     not Ca11022_0.Tc_Screen (9, 14) and
     not Ca11022_0.Tc_Draw_Circle and
     not Ca11022_0.Tc_Draw_Square
   then
      Report.Failed ("Wrong results for the VT100");
   end if;

   Ca11022_0.Tc_Draw_Circle := False;
   Ca11022_0.Tc_Draw_Square := False;

   -- Customized graphic functions for the IBM3270 terminal.
   Ca11022_0.Ca11022_3.Ibm3270_Graphic;

   if not Ca11022_0.Tc_Screen (12, 42) and
     not Ca11022_0.Tc_Screen (13, 43) and
     not Ca11022_0.Tc_Screen (14, 44) and
     not Ca11022_0.Tc_Screen (46, 18) and
     not Ca11022_0.Tc_Draw_Circle and
     not Ca11022_0.Tc_Draw_Square
   then
      Report.Failed ("Wrong results for the IBM3270");
   end if;

   Report.Result;

end Ca11022;
