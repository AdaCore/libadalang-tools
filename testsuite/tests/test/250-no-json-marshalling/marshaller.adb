with Ada.Text_IO;

package body Marshaller is
   function Create_Stream
     (File : out File_Type; Path : String)
      return Ada.Streams.Stream_IO.Stream_Access is
   begin
      Open (File, In_File, Path);
      return Stream (File);
   end Create_Stream;

   procedure Log_Values (Files : File_Array) is
   begin
      for Path of Files loop
         declare
            File      : File_Type;
            Full_Path : constant Unbounded_String := Input_Dir & Path;
            S         : constant Stream_Access :=
              Create_Stream (File, To_String (Full_Path));
            Result    : constant T := Marshal_Value (S);
         begin
            Close (File);
         end;
         Ada.Text_IO.Put_Line (Type_Name & ": " & "marshal OK");
      end loop;
   exception
      when others =>
         Ada.Text_IO.Put_Line (Type_Name & ": " & "marshal KO");
   end Log_Values;
end Marshaller;
