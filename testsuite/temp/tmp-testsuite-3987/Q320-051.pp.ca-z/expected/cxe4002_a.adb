-----------------------------------------------------------------------------

with Cxe4002_Common;
with Cxe4002_Part_A1;
with Cxe4002_Part_A2;
with Report;
procedure Cxe4002_A is
begin
   -- this partition is a server that deals with calls
   -- from CXE4002_B.
   Report.Test ("CXE4002_A", "Parameter passing across partitions (server)");
   Cxe4002_Part_A1.Can_Quit; -- OK to quit now.

   -- Report.Result is called in the body of CXE4002_Part_A1.
end Cxe4002_A;
