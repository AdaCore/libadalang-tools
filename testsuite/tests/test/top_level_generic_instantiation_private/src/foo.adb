package body Foo is
   procedure Do_Stuff (X : Integer) is
      pragma Unreferenced (X);
   begin
      Do_Private_Stuff;
   end Do_Stuff;

   procedure Do_Private_Stuff is
   begin
      null;
   end Do_Private_Stuff;
end Foo;
