with Cc51008_1;
with Cc51008_2;
with Cc51008_3;
with Report; use Report;
procedure Cc51008 is

   package Inst is new Cc51008_1 (Cc51008_2.R2, Cc51008_3.R3);

   X2 : constant Cc51008_2.R2 := (C => 2.0);
   X3 : constant Cc51008_3.R3 := (C => 3.0);

begin
   Test
     ("CC51008",
      "Check that operations are inherited for a formal derived " &
      "type whose ancestor is also a formal type as described in " &
      "RM95 12.5.1(21/1)");
   Inst.G (X2, X3);
   Result;
end Cc51008;
