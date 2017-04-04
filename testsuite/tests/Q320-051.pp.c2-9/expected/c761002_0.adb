with Report;
with Tctouch;
package body C761002_0 is

   procedure Finalize (It : in out Global) is
   begin
      Tctouch.Touch ('F');  ------------------------------------------------- F
   end Finalize;

   procedure Finalize (It : in out Second) is
   begin
      Tctouch.Touch ('S');  ------------------------------------------------- S
   end Finalize;
end C761002_0;
