with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
    type Bar is array (1 .. 2) of Integer;
    procedure Foo is
        B : constant Bar := (1, 2);
    begin
        for I of B loop
           Put_Line (I'Image);
        end loop;
    end Foo;
begin
    for I in 1 .. 2 loop
       Put_Line (I'Image);
    end loop;
end Main;
