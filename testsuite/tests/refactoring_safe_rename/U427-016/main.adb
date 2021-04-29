with A, E; use A, E;
with G; use G;
with H; use H;
with I; use I;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Baz : Integer := 3;
   --  Rename to Foo => Collision in A
   --  Bar => Collision in E
   --  Cursor => Collision in G or H
   --  List => Collision in I
   --  File_Type => Collision in Ada.Text_IO
   Qux   : Vector;
   Corge : List;

begin
   Qux.Append (4);
   Qux.Append (5);
   Corge.Append (6);
   Corge.Append (7);

   Put_Line (Foo'Image);
   Put_Line (Bar'Image);
   Put_Line (Baz'Image);
   for Q of Qux loop
      Put_Line (Q'Image);
   end loop;
   for C of Corge loop
      Put_Line (C'Image);
   end loop;
end Main;
