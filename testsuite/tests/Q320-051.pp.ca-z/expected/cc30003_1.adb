with Tctouch;
package body Cc30003_1 is

   function Area (R : Rectangle) return Float is
   begin
      Tctouch.Touch ('r'); ----------------------------------------- r
      Tctouch.Touch ('a'); ----------------------------------------- a
      return R.Height * R.Width;
   end Area;

end Cc30003_1;
