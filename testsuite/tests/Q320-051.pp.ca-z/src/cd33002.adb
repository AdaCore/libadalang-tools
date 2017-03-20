
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- there is no package body CD33002_0

------------------------------------------------------------------- CD33002

with Report;
with TCTouch;
with System.Storage_Elements;
with CD33002_0;
procedure CD33002 is

  use type System.Storage_Elements.Storage_Offset;

  A_Word   : CD33002_0.Word_Stuff(0..15);
 
  A_Double : CD33002_0.Double_Stuff(0..15);

  procedure Unexpected( Message : String; Wanted, Got: Integer ) is
  begin
    Report.Failed ( Message & " Wanted:"
                    & Integer'Image(Wanted) & " Got:" & Integer'Image(Got) );
  end Unexpected;

begin  -- Main test procedure.

  Report.Test ("CD33002", "Check that Component_Sizes that are multiples "
                        & "of the word size are supported. Check that for "
                        & "such Component_Sizes arrays contain no gaps "
                        & "between components" );

  if A_Word'Size /= CD33002_0.Word_Stuff'Component_Size * 16 then
    Unexpected("Word Size",
                CD33002_0.Word_Stuff'Component_Size * 16,
                A_Word'Size );
  end if;

  if A_Double'Size /= CD33002_0.Double_Stuff'Component_Size * 16 then
    Unexpected("Double word Size",
                CD33002_0.Double_Stuff'Component_Size * 16,
                A_Double'Size );
  end if;


  Report.Result;

end CD33002;
