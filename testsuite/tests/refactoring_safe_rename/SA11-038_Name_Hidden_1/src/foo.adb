with Ada.Text_IO; use Ada.Text_IO;
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

         procedure Waldo (Jab : Natural) is
            Flib : Natural := 17;
         begin
            declare
               Flob : Natural := 18;
            begin
               Put_Line ("Waldo declare block: " & Ber'Image);
               Put_Line ("Waldo declare block: " & Qeex'Image);
               Put_Line ("Waldo declare block: " & Garply'Image);
               Put_Line ("Waldo declare block: " & Gerply'Image);
               Put_Line ("Waldo declare block: " & Qiix'Image);
               Put_Line ("Waldo declare block: " & Bir'Image);
               Put_Line ("Waldo declare block: " & Bor'Image);
               Put_Line ("Waldo declare block: " & Xyzzy'Image);
               Put_Line ("Waldo declare block: " & Wubble'Image);
               Put_Line ("Waldo declare block: " & Fleb'Image);
               Put_Line ("Waldo declare block: " & Flib'Image);
               Put_Line ("Waldo declare block: " & Jab'Image);
            end;
         end Waldo;

         Flub : Natural := 19;
      end Grault;

      Flab : Natural :=20;
   end Qux;
end Foo;
