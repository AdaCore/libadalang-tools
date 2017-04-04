with Report; use Report;
with System; use System;
pragma Elaborate (Report);
package body C93005d_Enqueue is

   task T3 is
   end T3;

   task body T3 is
   begin
      T1.E;
      Failed ("ENQUEUED CALLER DID NOT GET EXCEPTION");
   exception
      when Tasking_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end T3;

   procedure Require_Body is
   begin
      null;
   end Require_Body;
begin                    -- T3 CALLS T1 HERE
   delay 1.0;            -- ENSURE THAT T3 EXECUTES
end C93005d_Enqueue;
