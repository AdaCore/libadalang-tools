-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
with Tctouch;
package body C391002_4 is -- Communications

   procedure Creator
     (Plugs : in Modules.Electronics_Module; Gives : out Mil_Comm)
   is
   begin
      Gives := (Plugs with Tc_Mc => -1);
   end Creator;

   function Creator
     (Key : Integer; Plugs : in Modules.Electronics_Module) return Private_Comm
   is
   begin
      return (Plugs with Tc_Pc => Key);
   end Creator;

   procedure Setup (It : in out Public_Comm; Value : in Integer) is
   begin
      It.Tc_Vc := Value;
      Tctouch.Assert (Value = 1, "Public_Comm");
   end Setup;

   procedure Setup (It : in out Private_Comm; Value : in Integer) is
   begin
      It.Tc_Pc := Value;
      Tctouch.Assert (Value = 2, "Private_Comm");
   end Setup;

   procedure Setup (It : in out Mil_Comm; Value : in Integer) is
   begin
      It.Tc_Mc := Value;
      Tctouch.Assert (Value = 3, "Private_Comm");
   end Setup;

   function Selector (It : Public_Comm) return Integer is
   begin
      return It.Tc_Vc;
   end Selector;

   function Selector (It : Private_Comm) return Integer is
   begin
      return It.Tc_Pc;
   end Selector;

   function Selector (It : Mil_Comm) return Integer is
   begin
      return It.Tc_Mc;
   end Selector;

end C391002_4; -- Communications
