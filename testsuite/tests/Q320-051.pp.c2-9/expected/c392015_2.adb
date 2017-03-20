with Tctouch;
package body C392015_2 is

   function Fa return T2 is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('a');
      Tctouch.Touch ('2');
      return T2'(null record);
   end Fa;

   function Fb return T2 is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('b');
      Tctouch.Touch ('2');
      return T2'(null record);
   end Fb;

   function Fc2 return T2'Class is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('c');
      Tctouch.Touch ('2');
      return T2'(null record);
   end Fc2;

   procedure Pb (P : in T2 := Fb) is
   begin
      Tctouch.Touch ('P');
      Tctouch.Touch ('b');
      Tctouch.Touch ('2');
   end Pb;

   function Fd (P : in T2 := Fa) return C392015_0.T0'Class is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('d');
      Tctouch.Touch ('2');
      return C392015_0.T0'Class (P);
   end Fd;

   function Ff (P : in T2 := Fa) return T2'Class is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('f');
      Tctouch.Touch ('2');
      return T2'Class (P);
   end Ff;

end C392015_2;
