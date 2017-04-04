-----------------------------------------------------------------------------

with Cxe4004_Common;
with Cxe4004_Part_A1;
with Cxe4004_Part_A2;
with Report;
procedure Cxe4004_A is
begin
   -- this partition is a server that deals with calls from CXE4004_B.
   Report.Test ("CXE4004_A", "Parameter passing across partitions (server)");
   Cxe4004_Part_A1.Can_Quit; -- OK to quit now.

   -- Report.Result is called in the body of CXE4004_Part_A1.
end Cxe4004_A;
