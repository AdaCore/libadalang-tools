with Ada.Streams; use Ada.Streams;
package My_Package.Types is
   subtype Int is Integer;
   subtype Pos is Positive;
   type Int_Vector is array (Pos range <>) of Int;
end My_Package.Types;
