with Ada.Characters.Latin_1;

package body TGen.Strings is

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Str : in out Unbounded_String) is
   begin
      Append (Str, Ada.Characters.Latin_1.LF);
   end New_Line;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line
     (Str  : in out Unbounded_String;
      Add  : String;
      Span : Natural)
   is
   begin
      Append (Str, (for I in 1 .. Span => ' '));
      Append (Str, Add);
      New_Line (Str);
   end Write_Line;

   -------------
   -- S_Write --
   -------------

   procedure S_Write
     (Str  : in out Unbounded_String;
      Add  : String;
      Span : Natural)
   is
   begin
      Append (Str, (for I in 1 .. Span => ' '));
      Append (Str, Add);
   end S_Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Str  : in out Unbounded_String;
      Add  : String)
   is
   begin
      Append (Str, Add);
   end Write;
end TGen.Strings;
