
with Report;
package body CXE4003_Generic_RCI is

  -- This package expects calls to come into Take_Call with the 
  -- Serial_Number parameter starting at Low and progressing to 
  -- High.  If any value is missed or repeated then the test fails.
  -- Done is called after all the calls to Take_Call have completed.

  Last_Call : Integer := Low - 1;
  
  procedure Take_Call (X : Serial_Numbers) is
  begin
    if X /= Last_Call + 1 then
      Report.Failed ("call out of sequence.  Expected " &
                     Integer'Image (Last_Call + 1) &
                     "  Received: " &
                     Integer'Image (X));
    end if;
    Last_Call := X;
    delay 0.0;  -- just to add some shuffling to the scheduling
  end Take_Call;

  procedure Done is
  begin
    if Last_Call /= High then
      Report.Failed ("not all the remote calls completed, only" &
                     Integer'Image (Last_Call) & " of" &
                     Integer'Image (High));
    end if;
  end Done;
end CXE4003_Generic_RCI;
