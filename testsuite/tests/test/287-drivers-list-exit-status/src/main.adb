with Ada.Text_IO; use Ada.Text_IO;
with Pkg;
with Pkh;

procedure Main
is
    procedure A
    is
        Dummy : Boolean := False;
    begin
        Put_Line (":)");
        Pkg.Foo;
        Pkh.Foo2;
    end A;
begin
    A;
end Main;
