with Ada.Text_IO; use Ada.Text_IO;
with Main_Package; use Main_Package;

procedure Main is

   procedure Bar
     (A : Integer; B, C, D, E, F, G, H, I : Integer)
      renames Foo;

   procedure Baz
     (A : Integer; B, C, D, E, F, G, H, I : Integer)
      renames Bar;

   procedure Qux
     (A : Integer; B, C, D, E, F, G, H, I : Integer)
      renames Bar;

   procedure Quux
     (A : Integer; B, C, D, E, F : Integer; G, H : Integer; I : Integer)
      renames Baz;

begin

   --  Foo
   Foo (1, 1, 1, 1, 1, 1, 1, 2, 1);
   Foo
     (A => 1,
      B => 1,
      C => 1,
      D => 1,
      E => 1,
      F => 1,
      G => 1,
      H => 2,
      I => 1);

   --  Bar
   Bar (1, 1, 1, 1, 1, 1, 1, 2, 1);
   Bar
     (A => 1,
      B => 1,
      C => 1,
      D => 1,
      E => 1,
      F => 1,
      G => 1,
      H => 2,
      I => 1);

   --  Baz
   Baz (1, 1, 1, 1, 1, 1, 1, 2, 1);
   Baz
     (A => 1,
      B => 1,
      C => 1,
      D => 1,
      E => 1,
      F => 1,
      G => 1,
      H => 2,
      I => 1);

   --  Qux
   Qux (1, 1, 1, 1, 1, 1, 1, 2, 1);
   Qux
     (A => 1,
      B => 1,
      C => 1,
      D => 1,
      E => 1,
      F => 1,
      G => 1,
      H => 2,
      I => 1);

   --  Quux
   Quux (1, 1, 1, 1, 1, 1, 1, 2, 1);
   Quux
     (A => 1,
      B => 1,
      C => 1,
      D => 1,
      E => 1,
      F => 1,
      G => 1,
      H => 2,
      I => 1);

end Main;