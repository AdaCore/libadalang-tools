-----------------------------------------------------------------------------

with Tctouch;
package body C3a0005_0 is

   procedure Push (B : access Button) is
   begin
      Tctouch.Touch ('P'); --------------------------------------------- P
      -- Invoking subprogram designated by access value
      B.Response (B);
   end Push;

   procedure Set_Response (B : access Button; R : in Button_Response_Ptr) is
   begin
      Tctouch.Touch ('S'); --------------------------------------------- S
      -- Set procedure value in record
      B.Response := R;
   end Set_Response;

   procedure Default_Response (B : access Button) is
   begin
      Tctouch.Touch ('D'); --------------------------------------------- D
      Default_Call := True;
   end Default_Response;

   procedure Emergency (B : access C3a0005_0.Button) is
   begin
      Tctouch.Touch ('E'); --------------------------------------------- E
      Emergency_Call := True;
   end Emergency;

end C3a0005_0;
