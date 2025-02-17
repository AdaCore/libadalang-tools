with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

generic
   type T is private;
   Type_Name : String;
   with
     function Marshal_Value
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return T;
package Marshaller is
   type File_Array is array (Positive range <>) of Unbounded_String;

   Input_Dir : constant Unbounded_String :=
     To_Unbounded_String ("./tgen_test_inputs/");

   function Create_Stream
     (File : out File_Type; Path : String)
      return Ada.Streams.Stream_IO.Stream_Access;

   procedure Log_Values (Files : File_Array);
end Marshaller;
