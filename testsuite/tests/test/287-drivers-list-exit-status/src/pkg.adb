with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
    procedure Foo is
    begin
        Put_Line (":)");
    end Foo;

    function Bar return Boolean is
    begin
        return False;
    end Bar;
end Pkg;
