with Ada.Text_IO;
package body Foo is
   type Fred is tagged
      record
         Grault : Integer;
      end record;
   function Fred_Constructor return Fred is ((Grault => 1)); 

   function Fred_Constructor2 (B : Boolean) return Fred is
   begin
      return (if B then (Grault => 1) else (Grault => 0));
   end Fred_Constructor2;
   
   G : Fred := (Grault => 1); 
   
begin
   Ada.Text_IO.Put_Line ("Package elaboration");
end Foo;
