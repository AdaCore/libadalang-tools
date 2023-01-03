with Ada.Streams;
with Ada.Streams.Stream_IO;       use Ada.Streams.Stream_IO;
with Ada.Text_IO;                 use Ada.Text_IO;
with Interfaces;                  use Interfaces;
with Instantiations;              use Instantiations;
with Instantiations.TGen_Support; use Instantiations.TGen_Support;
with TGen.TGen_Support;           use TGen.TGen_Support;

procedure Example_Gen is
   use type Instantiations.Sort_Integer.Array_To_Sort;
   use type Instantiations.Sort_My_String_3.Array_To_Sort;

   F         : Ada.Streams.Stream_IO.File_Type;
   S         : Stream_Access;
   File_Name : constant String := "scratch_pad.bin";
   Arr_Int_1 : Sort_Integer.Array_To_Sort := (1 => 1, 2 => 2, 3 => 3);
   Arr_Str_1 : Sort_My_String_3.Array_To_Sort :=
     (1 => "abc",
      2 => "def",
      3 => "ghi");

begin
   Create (F, Out_File, File_Name);
   S := Stream (F);
   TGen_Marshalling_instantiations_sort_integer_array_to_sort_output (S, Arr_Int_1);
   TGen_Marshalling_instantiations_sort_my_string_3_array_to_sort_output (S, Arr_Str_1);
   Close (F);

   Open (F, In_File, File_Name);
   S := Stream (F);
   declare
      Arr_Int_2 : Sort_Integer.Array_To_Sort :=
        TGen_Marshalling_instantiations_sort_integer_array_to_sort_input (S);
   begin
      if Arr_Int_1 /= Arr_Int_2 then
         Put_Line ("Integer array FAILED");
      end if;
   end;

   declare
      Arr_Str_2 : Sort_My_String_3.Array_To_Sort :=
        TGen_Marshalling_instantiations_sort_my_string_3_array_to_sort_input (S);
   begin
      if Arr_Str_1 /= Arr_Str_2 then
         Put_Line ("String array FAILED");
      end if;
   end;

   Close (F);

end Example_Gen;
