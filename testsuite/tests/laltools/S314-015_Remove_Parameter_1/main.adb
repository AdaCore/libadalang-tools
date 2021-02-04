with Ada.Text_IO; use Ada.Text_IO;

with Main_Package;
with Main_Package.Xyzzy;

procedure Main is
   procedure Foo (A : Integer; B : String; C : Float) renames
     Main_Package.Xyzzy;

begin
   Main_Package.Dummy_Procedure;
   Foo (1, "2", 3.0);
   Foo (A => 1,
        B => "2",
        C => 3.0);
end Main;
