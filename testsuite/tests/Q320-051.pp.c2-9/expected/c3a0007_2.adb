with Tctouch;
package body C3a0007_2 is
   procedure Emergency (B : in out C3a0007_0.Button'Class) is
   begin
      Tctouch.Touch ('E'); ------------------------------------------- E
      Emergency_Call := True;
   end Emergency;
end C3a0007_2;
