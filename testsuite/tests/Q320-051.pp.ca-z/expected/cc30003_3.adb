with Tctouch;
package body Cc30003_3 is

   procedure Make_Square (S : in out Counted_Square; Side : in Float) is
   begin
      Tctouch.Touch ('s'); ----------------------------------------- t
      Tctouch.Touch ('m'); ----------------------------------------- a
      S.Height := Side;
      S.Width  := Side;
      Clear (S);
   end Make_Square;

   function Area (S : Counted_Square) return Float is
   begin
      Tctouch.Touch ('s'); ----------------------------------------- s
      Tctouch.Touch ('a'); ----------------------------------------- a
      return S.Width**2;
   end Area;

   procedure Bump (S : in out Counted_Square) is
   begin
      Tctouch.Touch ('s'); ----------------------------------------- s
      Tctouch.Touch ('b'); ----------------------------------------- b
      Counted_Rectangle_Inst.Bump (Counted_Rectangle_Inst.Counted_Type (S));
   end Bump;

end Cc30003_3;
