with Tctouch;
package body Cc30003_0 is

   procedure Clear (Obj : in out Counted_Type) is
   -- Clear the count of Obj.
   begin
      Tctouch.Touch ('C'); ----------------------------------------- C
      Obj.Count := 0;
   end Clear;

   procedure Bump (Obj : in out Counted_Type) is
   -- Increment the count of Obj.
   begin
      Tctouch.Touch ('B'); ----------------------------------------- B
      Obj.Count := Obj.Count + 1;
   end Bump;

   function Count (Obj : Counted_Type) return Natural is
   -- Return the count of Obj.
   begin
      Tctouch.Touch ('c'); ----------------------------------------- c
      return Obj.Count;
   end Count;

   procedure Double_Bump (Obj : in out Counted_Type'Class) is
   -- Increment the count of Obj twice.
   begin
      Tctouch.Touch ('D'); ----------------------------------------- D
      Bump (Obj);
      Bump (Obj);
   end Double_Bump;

end Cc30003_0;
