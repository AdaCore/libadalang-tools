------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
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
      Append (Str, [for I in 1 .. Span => ' ']);
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
      Append (Str, [for I in 1 .. Span => ' ']);
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
      Indent_Str : constant String (1 .. Span) := [others => ' '];
      Index : Natural := 1;
   begin
      if Length (Str) = 0 then
         return;
      end if;
      Replace_Slice (Str, 1, 1, Indent_Str & Element (Str, 1));

      while Index <= Length (Str) loop
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
      end loop;
   end Indent_String;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Name : Ada_Qualified_Name) return String is
      Result : Unbounded_String;
   begin
      for Id of Name loop
         if Length (Result) > 0 then
            Append (Result, ".");
         end if;
         Append (Result, To_String (Id));
      end loop;

      return +Result;
   end To_Ada;

   -----------------------
   -- To_Qualified_Name --
   -----------------------

   function To_Qualified_Name (Name : String) return Ada_Qualified_Name
   is
      I      : Positive := 1;
      Result : Ada_Qualified_Name;
   begin
      for J in Name'Range loop
         if Name (J) = '.' then
            I := J + 1;
            Ada_Identifier_Vectors.Append
              (Result, To_Unbounded_String (Name (I .. J)));
         end if;
      end loop;
      Ada_Identifier_Vectors.Append
        (Result, To_Unbounded_String (Name (I .. Name'Last)));
      return Result;
   end To_Qualified_Name;
end TGen.Strings;
