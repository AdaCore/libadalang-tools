with Ada.Text_IO; use Ada.Text_IO;
with Main_Package; use Main_Package;

procedure Main is
   procedure Corge is new Main_Package.Qux (Integer);

   procedure Grault is new Main_Package.Quux (Integer, 1);

   function Another_Garply (A : Integer) return Integer renames Garply;

   procedure Fred (A : Integer := 1) renames Waldo;

   procedure Plugh (A : Integer) renames Bar;

   procedure Xyzzy (A : Integer := 2) renames Plugh;

   procedure Thud (A : Integer) renames Corge;

begin
   Main_Package.Foo (Main_Package.Garply (1));
   Main_Package.Foo (Another_Garply (A => 1));
   Main_Package.Foo (A => 1);
   Bar;
   Bar (1);
   Bar (A => 1);
   Baz (1);
   Baz (A => 1);
   Corge (1);
   Corge (A => 1);
   Grault;
   Grault (1);
   Grault (A => 1);
   Waldo (1);
   Fred;
   Fred (A => 1);
   Fred (1);
   Plugh (A => 1);
   Plugh (1);
   Xyzzy;
   Xyzzy (1);
   Xyzzy (A => 1);
   Thud (1);
   Thud (A => 1);
end Main;
