with Ada.Text_IO; use Ada.Text_IO;
with Main_Package; use Main_Package;

procedure Main is
   procedure Bar
     (A : in Integer;
      B, C, D, E, F : Integer;
      G, H, I : out Integer;
      J : in out Integer;
      K : in out Integer;
      L : in out Integer)
      renames Foo;

   procedure Baz
     (A : in Integer;
      B, C : Integer;
      D, E, F : in Integer;
      G, H : out Integer;
      I : out Integer;
      J : in out Integer;
      K, L : in out Integer)
      renames Bar;

   G, H, I : Integer;
   J : Integer := 1;
   K : Integer := 1;
   L : Integer := 1;

begin
   Foo (1, 1, 1, 1, 1, 1, G, H, I, J, K, L);
   Bar (1, 1, 1, 1, 1, 1, G, H, I, J, K, L);
   Baz (1, 1, 1, 1, 1, 1, G, H, I, J, K, L);
   Qux (1, 1, 1, 1);
end Main;
