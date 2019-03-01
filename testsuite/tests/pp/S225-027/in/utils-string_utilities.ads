------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                               S T R I N G S                              --
--                                                                          --
--                                 S p e c                                  --
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

with Unchecked_Deallocation;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.Wide_Fixed;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Utils.String_Utilities is

   --  String-related utilities

   subtype W_Char is Wide_Character;
   subtype W_Str is Wide_String;
   type W_Str_Access is access all W_Str;
   procedure Free is new Unchecked_Deallocation (W_Str, W_Str_Access);

   W_NUL : constant W_Char := W_Char'Val (Character'Pos (ASCII.NUL));
   W_LF  : constant W_Char := W_Char'Val (Character'Pos (ASCII.LF));
   W_CR  : constant W_Char := W_Char'Val (Character'Pos (ASCII.CR));
   W_FF  : constant W_Char := W_Char'Val (Character'Pos (ASCII.FF));

   W_HT : constant W_Char := W_Char'Val (Character'Pos (ASCII.HT));
   W_VT : constant W_Char := W_Char'Val (Character'Pos (ASCII.VT));

   NL : constant W_Char := W_LF;
   --  Character used to represent new-line in output.

   function Image (X : Integer) return String;
   --  Return X'Img without the annoying blank.

   type Modular is mod 2**32;
   function Image (X : Modular) return String;

   function Capitalize (S : String) return String;
   function Capitalize (S : W_Str) return W_Str;
   procedure Capitalize (S : in out String);
   procedure Capitalize (S : in out W_Str);
   --  Capitalizes the first letter, and all letters following a
   --  non-letter-or-digit. Converts all others to lower case.

   procedure To_Lower (S : in out String);
   procedure To_Lower (S : in out W_Str);
   --  Same as the ones in Ada.[Wide_]Characters.Handling, except we use a
   --  procedure to avoid inefficient secondary stack usage.

   function Escape_String_Literal (S : String) return String;
   --  Double all the double quotes

   function Slide (X : String) return String;
   function Slide (X : W_Str) return W_Str;
   --  Return X with X'First = 1

   function Find
     (Source : Wide_String; Pattern : Wide_String) return Natural is
     (Ada.Strings.Wide_Fixed.Index (Source, Pattern));

   function Has_Prefix (X, Prefix : String) return Boolean;
   function Has_Prefix (X, Prefix : W_Str) return Boolean;
   --  True if Prefix is at the beginning of X, case insensitive. For example,
   --  Has_Prefix("An_Identifier", Prefix => "an_") is True.

   function Has_Suffix (X, Suffix : String) return Boolean;
   function Has_Suffix (X, Suffix : W_Str) return Boolean;
   --  True if Suffix is at the end of X, case insensitive

   function Strip_Prefix (X, Prefix : String) return String;
   function Strip_Prefix (X, Prefix : W_Str) return W_Str;
   --  If Prefix is at the beginning of X (case insensitive), strip it off

   function Strip_Suffix (X, Suffix : String) return String;
   function Strip_Suffix (X, Suffix : W_Str) return W_Str;
   --  If Suffix is at the end of X (case insensitive), strip it off

   function Strip_Article (S : String) return String;
   function Strip_Article (S : W_Str) return W_Str;
   --  Removes a leading "A_" or "An_" from the string. Case insensitive.

   function Replace_All (S, From, To : W_Str) return W_Str;
   function Replace_All
     (S : W_Str_Access; From, To : W_Str) return W_Str_Access;
   --  Replaces all occurrences of From in S with To. In the second form, S is
   --  freed.

   function Must_Replace (S, From, To : W_Str) return W_Str;
   function Must_Replace
     (S : W_Str_Access; From, To : W_Str) return W_Str_Access;
   --  Same as Replace_All, except these require that at least one substring be
   --  replaced.

   function Replace_String (S, From, To : String) return String;
   --  Same as Replace_All, but for String

   subtype Digit is Integer range 0 .. 9;
   function Char_To_Digit (C : Character) return Digit;
   function Char_To_Digit (C : W_Char) return Digit;
   --  Converts '0' ==> 0, etc

   BOM_8 : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
     Ada.Strings.UTF_Encoding.BOM_8;

   function To_UTF8
     (Item : W_Str; Output_BOM : Boolean := False)
      return Ada.Strings.UTF_Encoding.UTF_8_String renames
     Ada.Strings.UTF_Encoding.Wide_Strings.Encode;

   function From_UTF8
     (Item : Ada.Strings.UTF_Encoding.UTF_8_String) return W_Str renames
     Ada.Strings.UTF_Encoding.Wide_Strings.Decode;

   procedure Text_IO_Put_Char (C : Character);
   procedure Wide_Text_IO_Put_Char (C : Character);
   procedure Wide_Text_IO_Put_Char (C : W_Char);
   --  Put C to Current_Output. Used to instantiate Formatted_Output.

   procedure Std_Err_Put_Char (C : Character);
   --  Put C to Standard_Error. Used to instantiate Dbg_Out.

   function Read_File (FD : File_Descriptor) return String_Access;
   function Read_File (File_Name : String) return String_Access;
   --  Reads the entire contents of the file

   File_Not_Found : exception;
   --  Raised by Read_File if it can't find the file. The Exception_Message is
   --  appropriate for printing.

   procedure Write_File (FD : File_Descriptor; S : String);
   procedure Write_File (File_Name : String; S : String);
   --  Write S to the file, overwriting it if it already exists

   procedure Parallel_Make_Dir
     (New_Directory : String; Give_Message : Boolean := False);
   --  Creates a new directory with the given name if it does not already
   --  exist, creating parent directories as necessary. This is safe for
   --  parallel processing in the following sense: if two or more processes try
   --  to create the same directory name at the same time, the directory will
   --  be created (once), and no exception will be raised. We use this in case
   --  an ASIS tool is called from gprbuild in parallel using the -j switch.
   --  If Give_Message is True and the directory is successfully created, a
   --  message saying so is printed.
   --  Why don't we create the directory in the outer invocation, so it won't
   --  happen in parallel???

   Move_Failure : exception;
   procedure Move_File (Old_Name : String; New_Name : String);
   --  Same as GNAT.OS_Lib.Rename_File, but overwrites New_Name if it
   --  already exists. On failure, raises Move_Failure with an appropriate
   --  Exception_Message.

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive,
      String);
   subtype String_Vector is String_Vectors.Vector;

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   subtype String_Set is String_Sets.Set;

   ---------------------
   -- Bounded Strings --
   ---------------------

   --  Ada.Strings.Bounded_Strings is too much hassle; we use a simple
   --  bounded string type here.

   type Bounded_Str (Max_Length : Natural := 2**16 - 1) is limited record
      Length : Natural := 0;
      Chars  : String (1 .. Max_Length);
   end record;

   procedure Append (X : in out Bounded_Str; C : Character);
   procedure Append (X : in out Bounded_Str; S : String);
   function To_String (X : Bounded_Str) return String;
   function "+" (X : Bounded_Str) return String renames To_String;

   type Bounded_W_Str (Max_Length : Natural := 2**16 - 1) is limited record
      Length : Natural := 0;
      Chars  : W_Str (1 .. Max_Length);
   end record;

   procedure Append (X : in out Bounded_W_Str; C : W_Char);
   procedure Append (X : in out Bounded_W_Str; S : W_Str);
   function To_String (X : Bounded_W_Str) return W_Str;
   function "+" (X : Bounded_W_Str) return W_Str renames To_String;

end Utils.String_Utilities;
