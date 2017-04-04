-----------------------------------------------------------------------------

with Tctouch;
package body C3a0007_0 is

   procedure Push (B : in out Button) is
   begin
      Tctouch.Touch ('P'); --------------------------------------------- P
      -- Invoking subprogram designated by access value
      B.Action (B);
   end Push;

   procedure Set_Response (B : in out Button; R : in Button_Response_Ptr) is
   begin
      Tctouch.Touch ('S'); --------------------------------------------- S
      -- Set procedure value in record
      B.Action := R;
   end Set_Response;

   procedure Response (B : in out Button) is
   begin
      Tctouch.Touch ('D'); --------------------------------------------- D
      Default_Call := True;
   end Response;

   procedure Default_Response (B : in out Button'Class) is
   begin
      Tctouch.Touch ('C'); --------------------------------------------- C
      Response (B);
   end Default_Response;

end C3a0007_0;
