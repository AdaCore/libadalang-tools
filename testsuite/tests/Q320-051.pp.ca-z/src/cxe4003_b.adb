
-----------------------------------------------------------------------------

with CXE4003_Part_B1;
with CXE4003_Part_B2;
with CXE4003_Part_B3;
with CXE4003_Part_B4;
with CXE4003_Part_B5;
with Report;
procedure CXE4003_B is
begin
  Report.Test ("CXE4003_B", "Check the blocking properties of " &
                            "remote calls");

  -- the call to Report.Result is made by the task in
  -- CXE4003_Part_B1 when partition A is finished.
end CXE4003_B;
