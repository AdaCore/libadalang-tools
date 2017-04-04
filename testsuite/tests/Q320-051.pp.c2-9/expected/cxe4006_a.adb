-----------------------------------------------------------------------------

with Cxe4006_Common;
with Cxe4006_Part_A1;
with Cxe4006_Part_A2;
with Report;
procedure Cxe4006_A is
begin
   -- this partition is a server that deals with calls from CXE4006_B.
   Report.Test ("CXE4006_A", "Remote dispatching calls (server)");
   Cxe4006_Part_A1.Can_Quit; -- OK to quit now.

   -- Report.Result is called in the body of CXE4006_Part_A1.
end Cxe4006_A;
