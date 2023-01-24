------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2022-2023, AdaCore                    --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Langkit_Support.Slocs;

with VSS.Characters;
with VSS.Strings.Conversions;
with VSS.String_Vectors;
with VSS.Text_Streams;
with VSS.Text_Streams.File_Input;
with VSS.Text_Streams.File_Output;
with VSS.Strings.Cursors.Iterators;
with VSS.Strings.Cursors.Iterators.Characters;

package body Lint.File_Edits is

   -----------------
   -- Apply_Edits --
   -----------------

   procedure Apply_Edits
     (Edits : Laltools.Refactor.Text_Edit_Map)
   is
      use File_Name_To_Virtual_String_Maps;

      File_Edits   : constant File_Name_To_Virtual_String_Map :=
        Apply_Edits (Edits);
      Edits_Cursor : Cursor := File_Edits.First;

      Edits_Length_Image : constant String :=
        Ada.Strings.Fixed.Trim (File_Edits.Length'Image, Ada.Strings.Both);
      File_Counter       : Positive := 1;

   begin
      if File_Edits.Is_Empty then
         return;
      end if;

      Lint.Logger.Trace ("Editing files on disk");
      while Has_Element (Edits_Cursor) loop
         declare
            Output : VSS.Text_Streams.File_Output.File_Output_Text_Stream;
            Buffer : constant VSS.Strings.Virtual_String :=
              Element (Edits_Cursor);
            C_It   :
              VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
                Buffer.At_First_Character;
            Ignore : Boolean := True;

         begin
            Log_Progress
              (Current => File_Counter,
               Total   => Edits_Length_Image,
               Message => "Editing " & Key (Edits_Cursor));

            Output.Create
              (VSS.Strings.Conversions.To_Virtual_String (Key (Edits_Cursor)));
            while C_It.Has_Element loop
               Output.Put (C_It.Element, Ignore);
               Ignore := C_It.Forward;
            end loop;
            Output.Close;

         exception
            when E : others =>
               Lint.Logger.Trace ("Failed to edit " & Key (Edits_Cursor));
               Lint.Logger.Trace (E);
         end;

         Next (Edits_Cursor);
         File_Counter := @ + 1;
      end loop;
   end Apply_Edits;

   -----------------
   -- Apply_Edits --
   -----------------

   function Apply_Edits
     (Edits : Laltools.Refactor.Text_Edit_Map)
      return File_Name_To_Virtual_String_Map
   is
      use Ada.Strings.Unbounded;
      use Laltools.Refactor;
      use Laltools.Refactor.Text_Edit_Ordered_Maps;

      Edits_Cursor : Laltools.Refactor.Text_Edit_Ordered_Maps.Cursor :=
        Edits.First;
      Result       : File_Name_To_Virtual_String_Map;

      Edits_Length_Image : constant String :=
        Ada.Strings.Fixed.Trim (Edits.Length'Image, Ada.Strings.Both);
      File_Counter       : Positive := 1;

   begin
      if Edits.Is_Empty then
         return Result;
      end if;

      Lint.Logger.Trace
        ("Found edits for " & Edits_Length_Image & " files");

      while Has_Element (Edits_Cursor) loop
         declare
            use Langkit_Support.Slocs;
            use VSS.Characters;
            use VSS.Strings;
            use VSS.Strings.Conversions;
            use VSS.Strings.Cursors.Iterators.Characters;
            use VSS.String_Vectors;
            use VSS.Text_Streams;
            use VSS.Text_Streams.File_Input;

            Original_Filename : constant Virtual_String :=
              To_Virtual_String (Key (Edits_Cursor));
            Original_File     : File_Input_Text_Stream;
            Input_Buffer      : Virtual_String;
            Output_Buffer     : Virtual_String;

            Text_Edits        : constant Constant_Reference_Type :=
              Constant_Reference (Edits, Edits_Cursor);
            Text_Edits_Cursor : Text_Edit_Ordered_Sets.Cursor :=
              Text_Edits.First;
            Current_Text_Edit : Text_Edit :=
              Text_Edit_Ordered_Sets.Element (Text_Edits_Cursor);
            Inside_Text_Edit  : Boolean := False;

            Current_Line_Number   : Line_Number := 1;
            Current_Column_Number : Column_Number := 1;

            Current_Character : Virtual_Character;
            Success           : Boolean := False;

         begin
            Log_Progress
              (Current => File_Counter,
               Total   => Edits_Length_Image,
               Message =>
                 "Creating buffer with " & Key (Edits_Cursor) & " edits");

            Original_File.Open (Original_Filename);
            loop
               exit when Original_File.Is_End_Of_Stream;
               Original_File.Get (Current_Character, Success);
               Input_Buffer.Append (Current_Character);
            end loop;
            Original_File.Close;

            declare
               Lines          : constant Virtual_String_Vector :=
                 Input_Buffer.Split_Lines (Keep_Terminator => True);
               Lines_Iterator : constant Reversible_Iterator :=
                 VSS.String_Vectors.Iterate (Lines);
               Lines_Cursor   : VSS.String_Vectors.Cursor :=
                 First (Lines_Iterator);

               Current_Line : Virtual_String;

               Ignore : Boolean;

            begin
               while VSS.String_Vectors.Has_Element (Lines_Cursor) loop
                  Current_Line :=
                    VSS.String_Vectors.Element (Lines, Lines_Cursor);

                  declare
                     C_It : Character_Iterator :=
                       Current_Line.At_First_Character;

                  begin
                     while C_It.Has_Element loop
                        Current_Column_Number :=
                          Column_Number (C_It.Character_Index);
                        if Current_Text_Edit /= No_Text_Edit then
                           if Current_Line_Number =
                                Current_Text_Edit.Location.Start_Line
                             and then Current_Column_Number =
                                        Current_Text_Edit.Location.Start_Column
                           then
                              Output_Buffer.Append
                                (To_Virtual_String
                                   (To_String (Current_Text_Edit.Text)));
                              Inside_Text_Edit := True;
                           end if;

                           if Current_Line_Number =
                                Current_Text_Edit.Location.End_Line
                             and then Current_Column_Number =
                                        Current_Text_Edit.Location.End_Column
                           then
                              Text_Edit_Ordered_Sets.Next (Text_Edits_Cursor);
                              if Text_Edit_Ordered_Sets.
                                   Has_Element (Text_Edits_Cursor)
                              then
                                 Current_Text_Edit :=
                                   Text_Edit_Ordered_Sets.
                                     Element (Text_Edits_Cursor);
                              end if;
                              Inside_Text_Edit := False;
                           end if;
                        end if;

                        if not Inside_Text_Edit then
                           Output_Buffer.Append (C_It.Element);
                        end if;

                        Ignore := C_It.Forward;
                     end loop;
                  end;

                  Lines_Cursor :=
                    VSS.String_Vectors.Next (Lines_Iterator, Lines_Cursor);
                  Current_Line_Number := @ + 1;
               end loop;
            end;

            Result.Insert
              (Text_Edit_Ordered_Maps.Key (Edits_Cursor),
               Output_Buffer);

         exception
            when E : others =>
               Lint.Logger.Trace
                 ("Failed to process "
                  & Key (Edits_Cursor)
                  & " edits at"
                  & Current_Line_Number'Image
                  & ":"
                  & Ada.Strings.Fixed.Trim
                      (Current_Column_Number'Image, Ada.Strings.Both));
               Lint.Logger.Trace (E);
         end;

         Text_Edit_Ordered_Maps.Next (Edits_Cursor);
         File_Counter := @ + 1;
      end loop;

      return Result;
   end Apply_Edits;

end Lint.File_Edits;
