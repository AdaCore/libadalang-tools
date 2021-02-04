with Ada.Text_IO; use Ada.Text_IO;
with Main_Package; use Main_Package;

procedure Main is
   F : Main_Package.Foo_Type;

   procedure Bar (Self : Foo_Type; A : in Integer; B : out Integer) renames
     Main_Package.Foo;

   B : Integer;

begin
   F.Foo (1, B);
   Main_Package.Foo (F, 1, B);
   Bar (F, 1, B);
   Another_F.Foo (1, B);
   Main_Package.Another_F.Foo (A => 1, B => B);
   Foo (Self => Another_F, A => 1, B => B);
   Bar (Self => Another_F, A => 1, B => B);
end Main;
