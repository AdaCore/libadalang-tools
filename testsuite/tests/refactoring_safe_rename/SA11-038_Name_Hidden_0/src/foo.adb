package body Foo is
   Fred : Natural := 10;

   procedure Baz is
      Plugh : Natural := 11;
   begin
      null;
   end Baz;

   Xyzzy : Natural := 12;

   package body Qux is
      Wibble : Natural := 13;

      procedure Corge is
         Wobble : Natural := 14;
      begin
         null;
      end Corge;

      Wubble : Natural := 15;

      package body Grault is

         Fleb : Natural := 16;

         procedure Waldo is
            Flib : Natural := 17;
         begin
            declare
               Flob : Natural := 18;
            begin
               null;
            end;
         end Waldo;

         Flub : Natural := 19;
      end Grault;

      Flab : Natural :=20;
   end Qux;
end Foo;
