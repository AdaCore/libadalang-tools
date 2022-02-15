with Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;

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
     (Str : in out Unbounded_String;
      Add : String)
   is
   begin
      Append (Str, Add);
   end Write;

   -------------------
   -- Indent_String --
   -------------------

   procedure Indent_String
     (Str  : in out Unbounded_String;
      Span : Natural)
   is
      Indent_Str : String (1 .. Span) := (others => ' ');
      Index : Natural := 1;
      Length : Natural := Str.Length;
   begin
      if Indent_String.Length = 0 then
         return;
      end if;
      Str.Replace_Slice (1, 1, Indent_Str & Str.Element (1));

      while Index <= Str.Length loop
         Index :=
           Ada.Strings.Unbounded.Index
             (Str,
              To_Set (Ada.Characters.Latin_1.LF),
              Index);
         if Index = 0 then
            return;
         end if;

         Ada.Strings.Unbounded.Replace_Slice
           (Str, Index, Index, Ada.Characters.Latin_1.LF & Indent_Str);
         Index := @ + Span;
         Length := Str.Length;
      end loop;
   end Indent_String;

end TGen.Strings;
