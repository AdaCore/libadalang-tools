-----------------------------------------------------------------------------

with Cxe4003_Part_B1;
with Cxe4003_Part_B2;
with Cxe4003_Part_B3;
with Cxe4003_Part_B4;
with Cxe4003_Part_B5;
with Report;
procedure Cxe4003_B is
begin
   Report.Test
     ("CXE4003_B",
      "Check the blocking properties of " & "remote calls");

   -- the call to Report.Result is made by the task in
   -- CXE4003_Part_B1 when partition A is finished.
end Cxe4003_B;
