with Report; use Report;
with Cxe2002_1;
with Cxe2002_2.Inst;
procedure Cxe2002_A is
   Y : Integer := 17;
begin
   Test
     ("CXE2002",
      "a remote call interface library unit may be a subprogram," &
      " or generic subprogram");

   Cxe2002_1 (1);

   Cxe2002_2.Inst (Y);
   if Y /= 2 then
      Failed ("Bad value returned from CXE2002_2.Gen:" & Integer'Image (Y));
   end if;

   Result;
end Cxe2002_A;
