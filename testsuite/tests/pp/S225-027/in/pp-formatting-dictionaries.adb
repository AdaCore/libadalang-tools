------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                  G N A T P P . D I C T I O N A R I E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2001-2017, AdaCore                      --
--                                                                          --
-- GNATPP is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNATPP is  distributed in the  hope that it will  be  useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or  FITNESS  FOR A  PARTICULAR  PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write to the Free Software Foundation,  51 Franklin Street, Fifth Floor, --
-- Boston,                                                                  --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Containers.Hashed_Sets; use Ada.Containers;

package body Pp.Formatting.Dictionaries is
   package Syms renames Utils.Symbols;

   subtype Symbol is Syms.Symbol;
   subtype Opt_Symbol is Syms.Opt_Symbol;

   package Name_Sets is new Ada.Containers.Hashed_Sets (Symbol,
      Syms.Hash_Symbol, Syms.Case_Insensitive_Equal, Syms."=");

   use Name_Sets;
   subtype Name_Set is Name_Sets.Set;
   Whole_Word_Exceptions, Subword_Exceptions : Name_Set;

   type Opt_Casing_Exception_Kinds is
     (Not_A_Casing_Exception,  -- Wrong syntax of the exception string

      Whole_Word,              -- Name to be replaced as a whole

      Subword);
   subtype Casing_Exception_Kinds is
     Opt_Casing_Exception_Kinds range Whole_Word .. Subword;

   --  Subword is a part of the name delimited by '_' or by the beginning or
   --  end of the word and which does not contain any '_' inside.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Add_To_Dictionary
     (Name : String; Exception_Kind : Casing_Exception_Kinds);
   --  If Name does not exist in the dictionary, adds the corresponding
   --  dictionary entry. Otherwise replace the casing defined by the
   --  existing occurrence of this name by the casing given by Name

   function Find_In_Dictionary
     (Name : String; Exception_Kind : Casing_Exception_Kinds)
      return Opt_Symbol;
   --  Tries to find in the dictionary the entry which corresponds to Name
   --  without taking into account the character casing. (Exception_Kind
   --  is used to limit the search by the corresponding kind of dictionary
   --  entries). Return the Id of the corresponding dictionary entry, returns
   --  No_String if the dictionary does not contain such an entry.

   procedure Scan_Dictionary (Dictionary_Name : String);
   --  Scans the named dictionary file

   -----------------------
      -- Add_To_Dictionary --
      -----------------------

   procedure Add_To_Dictionary
     (Name : String; Exception_Kind : Casing_Exception_Kinds)
   is
      Id : constant Symbol := Syms.Intern (Name);
   begin
      case Exception_Kind is
         when Whole_Word =>
            Include (Whole_Word_Exceptions, Id);
         when Subword =>
            Include (Subword_Exceptions, Id);
      end case;
   end Add_To_Dictionary;

   ---------------------------
   -- Check_With_Dictionary --
   ---------------------------

   procedure Check_With_Dictionary
     (Ada_Name : in out Wide_String; Casing : PP_Casing)
   is
      Name : String := To_String (Ada_Name);

      Name_Last : constant Natural := Name'Last;
      SW_Start  : Integer          := Name'First;
      SW_End    : Integer          := Name_Last;
      --  Indexes of a subword in the Name

      Dictionary_String : Opt_Symbol;

      procedure Set_Subword;
      --  Provided that Name has subwords, and that the current settings of
      --  SW_Start and SW_End point to some subword, sets these indexes to
      --  point to the next subword. Set SW_Start and SW_End to 0 if there
      --  are no more subwords.

      --  This procedure does not check if we have one more subword to move
      --  these indexes to.

      function Capitalize_Subword
        (SW : String; Casing : PP_Casing) return String;
      --  Supposing that SW is a (sub)word having no '_' inside, returns
      --  the capitalized version of this subword according to the casing
      --  represented by Casing.

      -----------------
      -- Set_Subword --
      -----------------

      procedure Set_Subword is
      begin

         if SW_End = Name_Last then
            --  There is no more subwords
            SW_Start := 0;
            SW_End   := 0;
         else
            SW_Start := SW_End + 2;
            SW_End   := Name_Last;

            for J in SW_Start + 1 .. SW_End loop
               if Name (J) = '_' then
                  SW_End := J - 1;

                  exit;
               end if;
            end loop;

         end if;

      end Set_Subword;

      ------------------------
      -- Capitalize_Subword --
      ------------------------

      function Capitalize_Subword
        (SW : String; Casing : PP_Casing) return String
      is
         Result    : String           := SW;
         First_Idx : constant Natural := Result'First;

      begin
         case Casing is
            when Lower_Case =>
               Result := To_Lower (Result);
            when Upper_Case =>
               Result := To_Upper (Result);
            when Mixed =>
               Result             := To_Lower (Result);
               Result (First_Idx) := To_Upper (Result (First_Idx));

            when As_Declared =>
               --  Nothing to do!
               null;
         end case;

         return Result;
      end Capitalize_Subword;

   begin  --  Check_With_Dictionary
      for J in Name'Range loop
         if Name (J) = '_' then
            SW_End := J - 1;
            exit;
         end if;
      end loop;

      Dictionary_String :=
        Find_In_Dictionary (Name => Name, Exception_Kind => Whole_Word);

      if not Syms.Present (Dictionary_String) then
         --  May be we can apply the subword exception to the whole word
         Dictionary_String :=
           Find_In_Dictionary (Name => Name, Exception_Kind => Subword);
      end if;

      if Syms.Present (Dictionary_String) then
         Name := Syms.Str (Dictionary_String).S;
      else

         if SW_End < Name'Last then
            --  That is, the whole word is not in the dictionary and it has at
            --  least two subwords, and Name (SW_Start .. SW) is the first one

            while SW_End /= 0 loop
               Dictionary_String :=
                 Find_In_Dictionary
                   (Name           => Name (SW_Start .. SW_End),
                    Exception_Kind => Subword);

               if Syms.Present (Dictionary_String) then
                  Name (SW_Start .. SW_End) := Syms.Str (Dictionary_String).S;
               else
                  Name (SW_Start .. SW_End) :=
                    Capitalize_Subword (Name (SW_Start .. SW_End), Casing);
               end if;

               Set_Subword;
            end loop;

         else
            --  The case of a word with no subwords, the word is not in the
            --  dictionary

            Name := Capitalize_Subword (Name, Casing);
         end if;
      end if;

      Ada_Name := To_Wide_String (Name);
   end Check_With_Dictionary;

   ------------------------
   -- Find_In_Dictionary --
   ------------------------

   function Find_In_Dictionary
     (Name : String; Exception_Kind : Casing_Exception_Kinds) return Opt_Symbol
   is
      Id : constant Symbol           := Syms.Intern (Name);
      C  : constant Name_Sets.Cursor :=
        (case Exception_Kind is
           when Whole_Word => Find (Whole_Word_Exceptions, Id),
           when Subword    => Find (Subword_Exceptions, Id));
   begin
      return (if Has_Element (C) then Element (C) else Syms.No_Symbol);
   end Find_In_Dictionary;

   -----------------------
   -- Scan_Dictionaries --
   -----------------------

   procedure Scan_Dictionaries (Dictionary_File_Names : String_Ref_Array) is
      pragma Assert (Is_Empty (Whole_Word_Exceptions));
      pragma Assert (Is_Empty (Subword_Exceptions));
   begin
      for D_Name of Dictionary_File_Names loop
         if D_Name.all /= "-" then -- "--dictionary-" means no predef casing
            Dictionaries.Scan_Dictionary (D_Name.all);
         end if;
      end loop;
   end Scan_Dictionaries;

   ---------------------
   -- Scan_Dictionary --
   ---------------------

   procedure Scan_Dictionary (Dictionary_Name : String) is
      String_Buffer_Max_Len : constant Natural := 1_024;
      --  Should be enough, I hope...

      String_Buffer : String (1 .. String_Buffer_Max_Len);

      Len : Natural range 0 .. String_Buffer_Max_Len := 0;
      --  The length of the dictionary file line which is being processed

      Line_Num : Natural := 0;
      --  The number of the currently processed line

      Dictionary_File : File_Type;

      procedure Process_Dictionary_File_Line;
      --  Reads the next line from the dictionary file, parses it, and
      --  if founds the new definition of the casing exception, puts the
      --  corresponding word in the exception table

      ----------------------------------
         -- Process_Dictionary_File_Line --
         ----------------------------------

      procedure Process_Dictionary_File_Line is
         Start_Word : Natural := 0;
         End_Word   : Natural := 0;

         Exc_Kind : Opt_Casing_Exception_Kinds;

         function Skip_White_Spaces (Idx : Natural) return Natural;
         --  Starting from Idx (which is treated as an index in String_Buffer
         --  bounded by the current value of Len), computes the index of the
         --  first non-blank character. If there is no non-blank character on
         --  the right from Idx, or if the actual is greater than Len returns
         --  zero.

         function Skip_Non_Space_Chars (Idx : Natural) return Natural;
         --  Starting from Idx (which is treated as an index in String_Buffer
         --  bounded by the current value of Len, Idx is supposed to point to
         --  non-blank character), return the index of the character preceding
         --  the first white space character (or Ada comment beginning) being
         --  on the right of Idx (returns Len if there is no such white space).

         function Get_Exception_Kind return Opt_Casing_Exception_Kinds;
         --  Checks if String_Buffer (Start_Word .. End_Word) has a syntax of
         --  a casing exception and return the corresponding exception kind.
         --  Returns Not_A_Casing_Exception if this word cannot be interpreted
         --  as a casing exception. As a side effect, this function may correct
         --  the values of Start_Word and End_Word to skip '*' in case of a
         --  subword.

         function Is_White_Space (Ch : Character) return Boolean;
         --  Checks if Ch is a white space

         --------------------
         -- Is_White_Space --
         --------------------

         function Is_White_Space (Ch : Character) return Boolean is
         begin
            return False or else Ch = ' ' or else Ch = ASCII.HT;
         end Is_White_Space;

         -----------------------
         -- Skip_White_Spaces --
         -----------------------

         function Skip_White_Spaces (Idx : Natural) return Natural is
            Result : Natural := Idx;
         begin
            while Is_White_Space (String_Buffer (Result)) and then Result < Len
            loop
               Result := Result + 1;
            end loop;

            if Result > Len
              or else
              (Result = Len and then Is_White_Space (String_Buffer (Result)))
            then
               Result := 0;
            end if;

            return Result;
         end Skip_White_Spaces;

         --------------------------
         -- Skip_Non_Space_Chars --
         --------------------------

         function Skip_Non_Space_Chars (Idx : Natural) return Natural is
            Result : Natural := Idx;
         begin
            while Result < Len
              and then not
              (Is_White_Space (String_Buffer (Result))
               or else
               (String_Buffer (Result) = '-'
                and then String_Buffer (Result + 1) = '-'))
            loop
               Result := Result + 1;
            end loop;

            if Is_White_Space (String_Buffer (Result))
              or else String_Buffer (Result) = '-'
            then
               Result := Result - 1;
            end if;

            return Result;
         end Skip_Non_Space_Chars;

         ------------------------
         -- Get_Exception_Kind --
         ------------------------

         function Get_Exception_Kind return Opt_Casing_Exception_Kinds is
            Result : Opt_Casing_Exception_Kinds;

            Prev_Char_Is_Underline : Boolean := False;
         begin

            if String_Buffer (Start_Word) = '*'
              and then String_Buffer (End_Word) = '*'
            then
               Result     := Subword;
               Start_Word := Start_Word + 1;
               End_Word   := End_Word - 1;
            else
               Result := Whole_Word;
            end if;

            --  And now we have to check that String_Buffer (First_Idx ..
            --  Last_Idx) has a syntax of an identifier

            if Start_Word > End_Word or else String_Buffer (Start_Word) = '_'
              or else String_Buffer (End_Word) = '_'
            then
               Result := Not_A_Casing_Exception;
            else

               for J in Start_Word .. End_Word loop

                  if Is_Alphanumeric (String_Buffer (J)) then
                     Prev_Char_Is_Underline := False;
                  elsif String_Buffer (J) = '_' then

                     if not Prev_Char_Is_Underline then

                        Prev_Char_Is_Underline := True;

                        if Result = Subword then
                           Result := Not_A_Casing_Exception;
                           exit;
                        end if;

                     else
                        Result := Not_A_Casing_Exception;
                        exit;
                     end if;

                  else
                     Result := Not_A_Casing_Exception;
                     exit;
                  end if;
               end loop;
            end if;

            return Result;
         end Get_Exception_Kind;

      begin  --  Process_Dictionary_File_Line
         Get_Line (Dictionary_File, String_Buffer, Len);

         if Len = 0 then
            --  This is an empty line
            return;
         end if;

         Start_Word := Skip_White_Spaces (1);

         if Start_Word = 0
           or else
           (Start_Word < Len and then String_Buffer (Start_Word) = '-'
            and then String_Buffer (Start_Word + 1) = '-')
         then
            --  blank or comment line
            return;
         end if;

         End_Word := Skip_Non_Space_Chars (Start_Word);

         Exc_Kind := Get_Exception_Kind;

         if Exc_Kind = Not_A_Casing_Exception then
            Ada.Text_IO.Put_Line
              (Standard_Error,
               Dictionary_Name & ':' & Image (Line_Num) & ':' &
               Image (Start_Word) &
               ": wrong syntax of a casing exception, line ignored");

         else
            Add_To_Dictionary
              (String_Buffer (Start_Word .. End_Word), Exc_Kind);

            --  We have to check if we have something else in the dictionary
            --  file line. The only possible things are blank characters and
            --  comments

            if End_Word < Len and then String_Buffer (End_Word + 1) = '*' then
               --  Taking into account the side effect of Get_Exception_Kind
               End_Word := End_Word + 1;
            end if;

            if End_Word < Len then
               --  We have something else in this line

               Start_Word := Skip_White_Spaces (End_Word + 1);

               if not
                 (Start_Word = 0
                  or else
                  (Start_Word < Len and then String_Buffer (Start_Word) = '-'
                   and then String_Buffer (Start_Word + 1) = '-'))
               then
                  Ada.Text_IO.Put_Line
                    (Standard_Error,
                     Dictionary_Name & ':' & Image (Line_Num) & ':' &
                     Image (Start_Word) &
                     ": only one casing exception per line is allowed");
                  Ada.Text_IO.Put_Line
                    (Standard_Error,
                     Dictionary_Name & ':' & Image (Line_Num) & ':' &
                     Image (Start_Word) & ": end of line ignored");
               end if;
            end if;
         end if;
      end Process_Dictionary_File_Line;

   begin  --  Scan_Dictionary

      --  First trying to open the dictionary file: ???It would be cleaner to
      --  keep the file opening and error message handling in gnatpp.

      begin
         Open
           (File => Dictionary_File, Mode => In_File, Name => Dictionary_Name);
      exception
         when Name_Error =>
            Ada.Text_IO.Put_Line
              (Standard_Error,
               "gnatpp: dictionary file not found: " & Dictionary_Name);
            return;

         when Status_Error =>
            Ada.Text_IO.Put_Line
              (Standard_Error,
               "gnatpp: cannot open dictionary file: " & Dictionary_Name);
            Ada.Text_IO.Put_Line
              (Standard_Error,
               "        the file may be used by another process");
            return;
      end;

      while not End_Of_File (Dictionary_File) loop
         Line_Num := Line_Num + 1;
         Process_Dictionary_File_Line;
      end loop;

      if Is_Open (Dictionary_File) then
         Close (Dictionary_File);
      end if;

   end Scan_Dictionary;

end Pp.Formatting.Dictionaries;
