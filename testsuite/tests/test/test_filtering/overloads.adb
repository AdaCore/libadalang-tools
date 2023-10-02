with Ada.Text_IO; use Ada.Text_IO;

package body Overloads is

   procedure Log (Value : String)
   is
   begin
      Put_Line ("Logging value: " & Value);
   end Log;
   
   procedure Log (Value : Integer)
   is
   begin
      Log (Value'Image);
   end Log;

   procedure Log (Value : Float)
   is
   begin
      Log (Value'Image);
   end Log;

end Overloads;
