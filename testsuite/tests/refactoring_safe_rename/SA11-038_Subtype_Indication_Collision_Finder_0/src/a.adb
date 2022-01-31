with Ada.Text_IO; use Ada.Text_IO;

package body A is
   procedure Foo (My_Bar : Bar) is
   begin
      Put_Line (My_Bar'Image);
   end Foo;

   procedure Foo_Bar (My_Bar, My_Other_Bar : Bar; Yet_Another_Bar : Bar) is
      My_Bar_Foo : Integer := 0;
   begin
      null;
   end Foo_Bar;
end A;
