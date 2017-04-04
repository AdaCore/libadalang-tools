with Tctouch;
package body C392015_0 is

   function Fa return T0 is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('a');
      Tctouch.Touch ('0');
      return T0'(null record);
   end Fa;

   function Fc return T0'Class is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('c');
      Tctouch.Touch ('0');
      return T0'(null record);
   end Fc;

   procedure Pa (P : in T0 := Fa) is
   begin
      Tctouch.Touch ('P');
      Tctouch.Touch ('a');
      Tctouch.Touch ('0');
   end Pa;

   procedure Pb (P : in T0 := Fa) is
   begin
      Tctouch.Touch ('P');
      Tctouch.Touch ('b');
      Tctouch.Touch ('0');
   end Pb;

   procedure Pc (Param1, Param2 : in T0 := Fa) is
   begin
      Tctouch.Touch ('P');
      Tctouch.Touch ('c');
      Tctouch.Touch ('0');
   end Pc;

   function Fd (P : in T0 := Fa) return T0'Class is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('d');
      Tctouch.Touch ('0');
      return T0'Class (P);
   end Fd;

end C392015_0;
