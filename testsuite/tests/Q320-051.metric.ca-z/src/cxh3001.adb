
------------------------------------------------------------------- CXH3001

with Report;
with CXH3001_0;
procedure CXH3001 is
begin
  Report.Test("CXH3001", "Check pragma Reviewable as a configuration pragma");

  Block: declare
    A_Truth : Boolean;
    Message : String := Report.Ident_Str( "Bad value encountered" );
  begin
    begin
      A_Truth := Report.Ident_Bool( True ) or A_Truth;  -- (8) not initialized
      if not A_Truth then
        Report.Comment ("True or Uninit = False");
        A_Truth := Report.Ident_Bool (True);
      else
        A_Truth := Report.Ident_Bool (True);
          -- We do this separately on each branch in order to insure that a
          -- clever optimizer can find out little about this value. Ident_Bool
          -- is supposed to be opaque to any optimizer.
      end if;
    exception
      when Constraint_Error | Program_Error =>
           -- Possible results of accessing an uninitialized object.
        A_Truth := Report.Ident_Bool (True);
    end;

    CXH3001_0.PT.Set( A_Truth );

    CXH3001_0.Global_Variable := A_Truth;

    CXH3001_0.TT.Release;  -- (9) rendezvous with TT

    while CXH3001_0.TT'Callable loop
      delay 1.0; -- wait for TT to become non-callable
    end loop;

    if   not CXH3001_0.PT.Enquire
      or not CXH3001_0.Global_Variable
      or CXH3001_0.TT'Callable then
      Report.Failed(Message);
    end if;

  end Block;

  Report.Result;
end CXH3001;
