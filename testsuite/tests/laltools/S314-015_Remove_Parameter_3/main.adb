with Ada.Text_IO; use Ada.Text_IO;
with Main_Package; use Main_Package;

procedure Main is

   procedure Bar
     (A : Integer; B, C, D, E, F, G, H, I : Integer)
      renames Foo;

   procedure Baz
     (A : Integer; B, C, D, E, F, G, H, I : Integer)
      renames Bar;

begin

   --  Foo
   Foo
     (G => 1,
      E => 1,
      F => 1,
      H => 2,
      I => 1,
      A => 1,
      B => 1,
      C => 1,
      D => 1);

   --  Bar
   Bar
     (E => 1,
      D => 1,
      F => 1,
      G => 1,
      H => 2,
      I => 1,
      A => 1,
      B => 1,
      C => 1);

   --  Baz
   Baz
     (A => 1,
      B => 1,
      D => 1,
      C => 1,
      G => 2,
      H => 1,
      F => 1,
      I => 1,
      E => 1);
end Main;
