with Tctouch;
package body C392015_1 is

   function Fa return T1 is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('a');
      Tctouch.Touch ('1');
      return T1'(null record);
   end Fa;

   procedure Pa (P : in T1 := Fa) is
   begin
      Tctouch.Touch ('P');
      Tctouch.Touch ('a');
      Tctouch.Touch ('1');
   end Pa;

   procedure Pb (P : in T1 := Fa) is
   begin
      Tctouch.Touch ('P');
      Tctouch.Touch ('b');
      Tctouch.Touch ('1');
   end Pb;

   procedure Pc (Param1, Param2 : in T1 := Fa) is
   begin
      Tctouch.Touch ('P');
      Tctouch.Touch ('c');
      Tctouch.Touch ('1');
   end Pc;

   procedure Pd (Param1 : in T1; Param2 : in C392015_0.T0 := C392015_0.Fa) is
   begin
      Tctouch.Touch ('P');
      Tctouch.Touch ('d');
      Tctouch.Touch ('1');
   end Pd;

   function Fc1 return T1'Class is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('c');
      Tctouch.Touch ('1');
      return T1'(null record);
   end Fc1;

   function Fd (P : in T1 := Fa) return C392015_0.T0'Class is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('d');
      Tctouch.Touch ('1');
      return C392015_0.T0'Class (P);
   end Fd;

   function Fe (P : in T1 := Fa) return T1'Class is
   begin
      Tctouch.Touch ('F');
      Tctouch.Touch ('e');
      Tctouch.Touch ('1');
      return T1'Class (P);
   end Fe;

end C392015_1;
