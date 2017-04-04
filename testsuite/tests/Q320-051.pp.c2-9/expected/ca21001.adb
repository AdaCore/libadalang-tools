with Report; use Report;
with Ca21001_1;
procedure Ca21001 is
   X : Integer := 0;
begin
   Test
     ("CA21001",
      "Test that a package subunit whose parent is a preelaborated" &
      " subprogram need not be preelaborable");
   Ca21001_1 (X);
   if X /= 101 then
      Failed ("Bad value for X");
   end if;
   Result;
end Ca21001;
