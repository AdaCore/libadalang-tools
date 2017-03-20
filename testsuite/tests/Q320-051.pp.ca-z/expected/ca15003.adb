with Report;                        use Report;
with Ca15003a.Empty_Pure;
with Ca15003a.Empty_Preelaborate;
with Ca15003a.Empty_Elaborate_Body; use Ca15003a.Empty_Elaborate_Body;
use type Ca15003a.Big_Positive'Base;
procedure Ca15003 is
begin
   Test ("CA15003", "Placement of Program Unit Pragmas in Generic Packages");
   if Two /= 2 then
      Failed ("Two should be 2 now");
   end if;
   if Tres /= 3 then
      Failed ("Tres should be 3 now");
   end if;
   Result;
end Ca15003;
