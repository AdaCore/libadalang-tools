with Ada.Text_IO; use Ada.Text_IO;
with Main_Package; use Main_Package;

procedure Main is
   procedure Bar (A, B : in Integer; C : out Integer; D : out Integer) renames
     Foo;

   procedure Baz (A, B : in Integer; C, D : out Integer) renames Foo;

   C, D : Integer;

begin
   Foo (1, 2, C, D);
end Main;
