with Tctouch;
with Ada.Numerics.Elementary_Functions;
package body Cc30003_2 is

   function Area (T : Right_Triangle) return Float is
   begin
      Tctouch.Touch ('t'); ----------------------------------------- t
      Tctouch.Touch ('a'); ----------------------------------------- a
      return (T.A * T.B) / 2.0;
   end Area;

   function Hypotenuse (T : Right_Triangle) return Float is
   begin
      Tctouch.Touch ('h'); ----------------------------------------- h
      return Ada.Numerics.Elementary_Functions.Sqrt (T.A**2 + T.B**2);
   end Hypotenuse;

   procedure Clear (T : in out Right_Triangle) is
   begin
      Tctouch.Touch ('t'); ----------------------------------------- t
      Tctouch.Touch ('c'); ----------------------------------------- c
      T.A := 0.0;
      T.B := 0.0;
   end Clear;

end Cc30003_2;
