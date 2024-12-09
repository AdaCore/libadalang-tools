package body Top_Level_Child_Generic_Child is
   procedure Foo (Bar : Integer) is
      pragma Unreferenced (Bar);
   begin
      null;
   end Foo;
end Top_Level_Child_Generic_Child;
