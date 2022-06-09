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

with Libadalang.Common; use Libadalang.Common;

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

   -----------------------
   -- To_Qualified_Name --
   -----------------------

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Name) return Ada_Qualified_Name
   is
   begin
      return Result : Ada_Qualified_Name do
         case Ada_Name (Name.Kind) is
            when Ada_Dotted_Name =>
               declare
                  DN     : constant Libadalang.Analysis.Dotted_Name :=
                    Name.As_Dotted_Name;
                  Suffix : constant Ada_Qualified_Name := To_Qualified_Name
                     (DN.F_Suffix.As_Name);
               begin
                  Result := To_Qualified_Name (DN.F_Prefix);
                  Result.Append (Suffix);
               end;

            when Ada_Single_Tok_Node =>
               declare

                  --  ??? GNATCOLL.Projects does not specify how to encode
                  --  Unicode unit names as strings, so for now, assume that we
                  --  process only codepoints in the ASCII range and thus use
                  --  Langkit_Support.Text.Image.

                  Identifier : constant Ada_Identifier :=
                     To_Unbounded_String (Image (Name.Text));
               begin
                  Result.Append (Identifier);
               end;

            when others =>
               raise Constraint_Error
                  with "no qualified name for " & Name.Kind'Image & " nodes";
         end case;
      end return;
   end To_Qualified_Name;

   ----------------------------
   -- Convert_Qualified_Name --
   ----------------------------

   function Convert_Qualified_Name
     (Text_QN : Libadalang.Analysis.Unbounded_Text_Type_Array)
      return Ada_Qualified_Name
   is
      Res : Ada_Qualified_Name;
   begin
      for Ident of Text_QN loop
         Res.Append (To_Unbounded_String (Image (To_Text (Ident))));
      end loop;
      return Res;
   end Convert_Qualified_Name;

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

end TGen.Strings;
