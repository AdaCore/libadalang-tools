

with Report;
package body CXE4004_Shared is
  -- The following constant controls whether or not comment
  -- messages are generated to show progress during the test.
  -- The setting of Verbose does not affect the result of the
  -- test.
  Verbose : constant Boolean := False;

  procedure Gen_Chk (Note     : String;
                     Actual,
                     Expected : The_Type) is
  begin
    if Actual /= Expected then
      Report.Failed (Note & "  Expected:" &
                     The_Type'Image (Expected) &
                     " Actual:" &
                     The_Type'Image (Actual));
    elsif Verbose then
      Report.Comment (Note);
    end if;
  end Gen_Chk;


  procedure Start_Test_Section (Name : String) is
  begin
    if Verbose then
      Report.Comment (Name);
    end if;
  end Start_Test_Section;
end CXE4004_Shared;
