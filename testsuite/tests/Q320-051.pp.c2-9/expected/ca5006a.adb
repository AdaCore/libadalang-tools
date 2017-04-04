-----------------------------------------------------------------------

with Report;   use Report;
with Ca5006a0; use Ca5006a0;
with Ca5006a1;
with Ca5006a2;

procedure Ca5006a is
begin
   if not P_E_Raised then
      Failed ("PROGRAM_ERROR NEVER RAISED");
   end if;

   Result;
end Ca5006a;
