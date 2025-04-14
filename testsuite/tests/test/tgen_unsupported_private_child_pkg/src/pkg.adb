with Pkg.Child;
with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

    procedure Do_Calculations (I : Integer) is
    begin
        Put_Line (Integer'Image (Pkg.Child.Foo (I)));
    end Do_Calculations;
end Pkg;
