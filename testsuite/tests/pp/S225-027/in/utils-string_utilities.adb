------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                               S T R I N G S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

with Ada.Characters.Conversions;
with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Directories;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;
with Ada.Text_IO;
with Ada.Wide_Text_IO;             use Ada;

package body Utils.String_Utilities is

   ------------
   -- Append --
   ------------

   procedure Append (X : in out Bounded_Str; C : Character) is
   begin
      if X.Length = X.Max_Length then
         raise Constraint_Error with "Bounded_Str overflow";
      end if;
      X.Length           := X.Length + 1;
      X.Chars (X.Length) := C;
   end Append;

   procedure Append (X : in out Bounded_Str; S : String) is
   begin
      for C of S loop
         Append (X, C);
      end loop;
   end Append;

   procedure Append (X : in out Bounded_W_Str; C : W_Char) is
   begin
      if X.Length = X.Max_Length then
         raise Constraint_Error with "Bounded_W_Str overflow";
      end if;
      X.Length           := X.Length + 1;
      X.Chars (X.Length) := C;
   end Append;

   procedure Append (X : in out Bounded_W_Str; S : W_Str) is
   begin
      for C of S loop
         Append (X, C);
      end loop;
   end Append;

   -------------------
   -- Char_To_Digit --
   -------------------

   function Char_To_Digit (C : Character) return Digit is
   begin
      pragma Assert (C in '0' .. '9');
      return Character'Pos (C) - Character'Pos ('0');
   end Char_To_Digit;

   function Char_To_Digit (C : W_Char) return Digit is
   begin
      return Char_To_Digit (Characters.Conversions.To_Character (C));
   end Char_To_Digit;

   -----------
   -- Image --
   -----------

   function Image (X : Integer) return String is
      Result : constant String := X'Img;

   begin
      case Result (1) is
         when ' ' =>
            return Slide (Result (2 .. Result'Last));

         when '-' =>
            return Result;

         when others =>
            raise Program_Error;
      end case;
   end Image;

   function Image (X : Modular) return String is
      Result : constant String := X'Img;

   begin
      case Result (1) is
         when ' ' =>
            return Slide (Result (2 .. Result'Last));

         when '-' =>
            return Result;

         when others =>
            raise Program_Error;
      end case;
   end Image;

   ----------------
   -- Capitalize --
   ----------------

   procedure Capitalize (S : in out String) is
   begin
      for X in S'Range loop
         if X = S'First
           or else not (Is_Letter (S (X - 1)) or else Is_Digit (S (X - 1)))
         then
            S (X) := To_Upper (S (X));

         else
            S (X) := To_Lower (S (X));
         end if;
      end loop;
   end Capitalize;

   procedure Capitalize (S : in out W_Str) is
   begin
      for X in S'Range loop
         if X = S'First
           or else not (Is_Letter (S (X - 1)) or else Is_Digit (S (X - 1)))
         then
            S (X) := To_Upper (S (X));

         else
            S (X) := To_Lower (S (X));
         end if;
      end loop;
   end Capitalize;

   function Capitalize (S : String) return String is
   begin
      return Result : String (S'Range) do
         for X in S'Range loop
            if X = S'First
              or else not (Is_Letter (S (X - 1)) or else Is_Digit (S (X - 1)))
            then
               Result (X) := To_Upper (S (X));

            else
               Result (X) := To_Lower (S (X));
            end if;
         end loop;
      end return;
   end Capitalize;

   function Capitalize (S : W_Str) return W_Str is
   begin
      return Result : W_Str (S'Range) do
         for X in S'Range loop
            if X = S'First
              or else not (Is_Letter (S (X - 1)) or else Is_Digit (S (X - 1)))
            then
               Result (X) := To_Upper (S (X));

            else
               Result (X) := To_Lower (S (X));
            end if;
         end loop;
      end return;
   end Capitalize;

   ---------------------------
   -- Escape_String_Literal --
   ---------------------------

   function Escape_String_Literal (S : String) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;

   begin
      for C of S loop
         Append (Result, C);
         if C = '"' then
            Append (Result, C);
         end if;
      end loop;

      return To_String (Result);
   end Escape_String_Literal;

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (X, Prefix : String) return Boolean is
   begin
      if X'Length >= Prefix'Length then
         declare
            Slice : constant String :=
              To_Lower (X (X'First .. X'First + Prefix'Length - 1));
         begin
            return Slice = To_Lower (Prefix);
         end;
      end if;
      return False;
   end Has_Prefix;

   function Has_Prefix (X, Prefix : W_Str) return Boolean is
   begin
      if X'Length >= Prefix'Length then
         declare
            Slice : constant W_Str :=
              To_Lower (X (X'First .. X'First + Prefix'Length - 1));
         begin
            return Slice = To_Lower (Prefix);
         end;
      end if;
      return False;
   end Has_Prefix;

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix (X, Suffix : String) return Boolean is
   begin
      if X'Length >= Suffix'Length then
         declare
            Slice : constant String :=
              To_Lower (X (X'Last - Suffix'Length + 1 .. X'Last));
         begin
            return Slice = To_Lower (Suffix);
         end;
      end if;
      return False;
   end Has_Suffix;

   function Has_Suffix (X, Suffix : W_Str) return Boolean is
   begin
      if X'Length >= Suffix'Length then
         declare
            Slice : constant W_Str :=
              To_Lower (X (X'Last - Suffix'Length + 1 .. X'Last));
         begin
            return Slice = To_Lower (Suffix);
         end;
      end if;
      return False;
   end Has_Suffix;

   ------------------
   -- Strip_Prefix --
   ------------------

   function Strip_Prefix (X, Prefix : String) return String is
   begin
      if Has_Prefix (X, Prefix) then
         return X (X'First + Prefix'Length .. X'Last);
      end if;

      return X;
   end Strip_Prefix;

   function Strip_Prefix (X, Prefix : W_Str) return W_Str is
   begin
      if Has_Prefix (X, Prefix) then
         return X (X'First + Prefix'Length .. X'Last);
      end if;

      return X;
   end Strip_Prefix;

   ------------------
   -- Strip_Suffix --
   ------------------

   function Strip_Suffix (X, Suffix : String) return String is
   begin
      if Has_Suffix (X, Suffix) then
         return X (X'First .. X'Last - Suffix'Length);
      end if;

      return X;
   end Strip_Suffix;

   function Strip_Suffix (X, Suffix : W_Str) return W_Str is
   begin
      if Has_Suffix (X, Suffix) then
         return X (X'First .. X'Last - Suffix'Length);
      end if;

      return X;
   end Strip_Suffix;

   -----------
   -- Slide --
   -----------

   function Slide (X : String) return String is
   begin
      return Result : constant String (1 .. X'Length) := X;
   end Slide;

   function Slide (X : W_Str) return W_Str is
   begin
      return Result : constant W_Str (1 .. X'Length) := X;
   end Slide;

   -----------------
   -- Replace_All --
   -----------------

   function Replace_All
     (S, From, To : W_Str; Replaced : out Boolean) return W_Str;
   function Replace_All
     (S : W_Str_Access; From, To : W_Str; Replaced : out Boolean)
      return W_Str_Access;

   function Replace_All
     (S, From, To : W_Str; Replaced : out Boolean) return W_Str
   is
      use Ada.Strings.Wide_Unbounded;
      Result : Unbounded_Wide_String;

      J : Positive := S'First;

   begin
      Replaced := False;
      while J <= S'Last loop
         if J + From'Length - 1 <= S'Last
           and then S (J .. J + From'Length - 1) = From
         then
            Replaced := True;
            Append (Result, To);
            J := J + From'Length;

         else
            Append (Result, S (J));
            J := J + 1;
         end if;
      end loop;

      return To_Wide_String (Result);
   end Replace_All;

   function Replace_All
     (S : W_Str_Access; From, To : W_Str; Replaced : out Boolean)
      return W_Str_Access
   is
      Result : constant W_Str := Replace_All (S.all, From, To, Replaced);
      Temp   : W_Str_Access   := S;

   begin
      if Result'Length = Temp'Length then
         Temp.all := Result;

      else
         Free (Temp);
         Temp := new W_Str'(Result);
      end if;

      return Temp;
   end Replace_All;

   function Replace_All (S, From, To : W_Str) return W_Str is
      Ignore : Boolean;
   begin
      return Replace_All (S, From, To, Ignore);
   end Replace_All;

   function Replace_All
     (S : W_Str_Access; From, To : W_Str) return W_Str_Access
   is
      Ignore : Boolean;
   begin
      return Replace_All (S, From, To, Ignore);
   end Replace_All;

   function Must_Replace (S, From, To : W_Str) return W_Str is
      Replaced : Boolean;
   begin
      return Result : constant W_Str := Replace_All (S, From, To, Replaced) do
         pragma Assert (Replaced);
      end return;
   end Must_Replace;

   function Must_Replace
     (S : W_Str_Access; From, To : W_Str) return W_Str_Access
   is
      Replaced : Boolean;
   begin
      return
        Result : constant W_Str_Access := Replace_All (S, From, To, Replaced)
      do
         pragma Assert (Replaced);
      end return;
   end Must_Replace;

   --------------------
   -- Replace_String --
   --------------------

   function Replace_String (S, From, To : String) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;

      J : Positive := S'First;

   begin
      while J <= S'Last loop
         if J + From'Length - 1 <= S'Last
           and then S (J .. J + From'Length - 1) = From
         then
            Append (Result, To);
            J := J + From'Length;

         else
            Append (Result, S (J));
            J := J + 1;
         end if;
      end loop;

      return To_String (Result);
   end Replace_String;

   -------------------
   -- Strip_Article --
   -------------------

   function Strip_Article (S : String) return String is
   begin
      return Strip_Prefix (Strip_Prefix (S, Prefix => "A_"), Prefix => "AN_");
   end Strip_Article;

   function Strip_Article (S : W_Str) return W_Str is
   begin
      return Strip_Prefix (Strip_Prefix (S, Prefix => "A_"), Prefix => "AN_");
   end Strip_Article;

   ----------------------
   -- Text_IO_Put_Char --
   ----------------------

   procedure Text_IO_Put_Char (C : Character) is
   begin
      if C = ASCII.LF then
         Text_IO.New_Line;
      else
         Text_IO.Put (C);
      end if;
   end Text_IO_Put_Char;

   ---------------------------
   -- Wide_Text_IO_Put_Char --
   ---------------------------

   procedure Wide_Text_IO_Put_Char (C : Character) is
   begin
      Wide_Text_IO_Put_Char (To_Wide_Character (C));
   end Wide_Text_IO_Put_Char;

   procedure Wide_Text_IO_Put_Char (C : W_Char) is
   begin
      if C = NL then
         Wide_Text_IO.New_Line;
      else
         Wide_Text_IO.Put (C);
      end if;
   end Wide_Text_IO_Put_Char;

   ----------------------
   -- Std_Err_Put_Char --
   ----------------------

   procedure Std_Err_Put_Char (C : Character) is
   begin
      if C = ASCII.LF then
         Text_IO.New_Line (Text_IO.Standard_Error);

      else
         Text_IO.Put (Text_IO.Standard_Error, C);
      end if;
   end Std_Err_Put_Char;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (FD : File_Descriptor) return String_Access is
      Length : constant Natural := Natural (File_Length (FD));

      This_Read : Integer;
      Read_Ptr  : Natural := 1;

      Buffer : constant String_Access := new String (1 .. Length);
   begin
      loop
         This_Read :=
           Read (FD, A => Buffer.all'Address, N => Length + 1 - Read_Ptr);
         Read_Ptr := Read_Ptr + Integer'Max (This_Read, 0);
         exit when This_Read <= 0 or else Read_Ptr = Length + 1;
      end loop;

      if Read_Ptr /= Length + 1 then
         raise Program_Error with "Read_File failed";
      end if;

      return Buffer;
   end Read_File;

   function Read_File (File_Name : String) return String_Access is
      FD : constant File_Descriptor := Open_Read (File_Name, Fmode => Binary);
      Status : Boolean;
   begin
      if FD = Invalid_FD then
         raise File_Not_Found with "file not found: " & File_Name;
      end if;

      return Result : constant String_Access := Read_File (FD) do
         Close (FD, Status);
         if not Status then
            raise Program_Error with "read of " & File_Name & " failed";
         end if;
      end return;
   end Read_File;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File (FD : File_Descriptor; S : String) is
      Result : constant Integer := Write (FD, S'Address, S'Length);
   begin
      if Result /= S'Length then
         raise Program_Error with "Write_File failed";
      end if;
   end Write_File;

   procedure Write_File (File_Name : String; S : String) is
      FD : constant File_Descriptor :=
        Create_File (File_Name, Fmode => Binary);
      Status : Boolean;
   begin
      if FD = Invalid_FD then
         raise Program_Error with "write of " & File_Name & " failed";
      end if;
      Write_File (FD, S);
      Close (FD, Status);
      if not Status then
         raise Program_Error with "write of " & File_Name & " failed";
      end if;
   end Write_File;

   -----------------------
   -- Parallel_Make_Dir --
   -----------------------

   procedure Parallel_Make_Dir
     (New_Directory : String; Give_Message : Boolean := False)
   is
      use Ada.Directories;
   begin
      if not Exists (New_Directory) then
         begin
            Create_Path (New_Directory);
            if Give_Message then
               Ada.Text_IO.Put_Line ("Created directory " & New_Directory);
               Ada.Text_IO.Put_Line (" [" & Full_Name (New_Directory) & "]");
            end if;
         exception
            when Ada.Directories.Use_Error =>
               --  Ignore error; some other process probably created it. Check
               --  for that below.
               null;
         end;
      end if;
      if not Exists (New_Directory) or else Kind (New_Directory) /= Directory
      then
         raise Ada.Directories.Use_Error
           with "cannot create directory " & New_Directory;
      end if;
   end Parallel_Make_Dir;

   ---------------
   -- Move_File --
   ---------------

   procedure Move_File (Old_Name : String; New_Name : String) is
      Success, Delete_Success : Boolean;
   begin
      --  There are three reasons for the following shenanigans:
      --
      --  Rename_File is nonportable; on some systems it fails if the New_Name
      --  already exists.
      --
      --  If the New_Name is a (writable) file in a non-writable directory,
      --  we need to copy the file; deleting or renaming the file will fail.
      --
      --  If the New_Name is on a different disk partition than Old_Name,
      --  we need to copy the file.
      --
      --  So we first try to rename. If that fails, we copy to New_Name and
      --  then delete Old_Name.

      Rename_File (Old_Name, New_Name, Success);
      if not Success then
         Copy_File (Old_Name, New_Name, Success, Mode => Overwrite);
         Delete_File (Old_Name, Delete_Success);
         if not Success then
            raise Move_Failure
              with "unable to copy " & Old_Name & " to " & New_Name;
         end if;
         if not Delete_Success then
            raise Move_Failure with "unable to delete " & Old_Name;
         end if;
      end if;
   end Move_File;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : in out String) is
   begin
      for X in S'Range loop
         S (X) := To_Lower (S (X));
      end loop;
   end To_Lower;

   procedure To_Lower (S : in out W_Str) is
   begin
      for X in S'Range loop
         S (X) := To_Lower (S (X));
      end loop;
   end To_Lower;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Bounded_Str) return String is
   begin
      return X.Chars (1 .. X.Length);
   end To_String;

   function To_String (X : Bounded_W_Str) return W_Str is
   begin
      return X.Chars (1 .. X.Length);
   end To_String;

end Utils.String_Utilities;
