with Tctouch;
package body C761013_0 is

   procedure Initialize (Obj : in out Ctrl) is
   begin
      Obj.Finalized := False;
      Tctouch.Touch ('i'); ----------------------------------------- i
   end Initialize;

   procedure Finalize (Obj : in out Ctrl) is
   begin
      Obj.C         := 0;
      Obj.Finalized := True;
      Tctouch.Touch ('f'); ----------------------------------------- f
   end Finalize;

   function F return Ctrl is
      X : Ctrl; -- X will be finalized here generating an ----------- i
   begin
      Tctouch.Touch ('F'); ----------------------------------------- F
      return X; -- X will be finalized here generating a ------------ f
   end F;

   function U return User is
      X : User; -- X will be finalized here generating an ----------- i
   begin
      Tctouch.Touch ('U'); ----------------------------------------- U
      return X; -- X will be finalized here generating a ------------ f
   end U;

end C761013_0;
