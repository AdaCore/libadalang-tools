
-----------------------------------------------------------------------------

with CXE4004_Common;
with CXE4004_Part_A1;
with CXE4004_Part_A2;
with Report;
procedure CXE4004_A is
begin
  -- this partition is a server that deals with calls
  -- from CXE4004_B.
  Report.Test ("CXE4004_A",
               "Parameter passing across partitions (server)");
  CXE4004_Part_A1.Can_Quit; -- OK to quit now.

  -- Report.Result is called in the body of CXE4004_Part_A1.
end CXE4004_A;
