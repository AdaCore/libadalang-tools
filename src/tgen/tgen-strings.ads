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

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Projects; use GNATCOLL.Projects;

package TGen.Strings is

   package String_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String,
      "<"          => Ada.Strings.Unbounded."<",
      "="          => Ada.Strings.Unbounded."=");

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => String,
      "<" => Ada.Strings.Less_Case_Insensitive,
      "=" => Ada.Strings.Equal_Case_Insensitive);
   subtype String_Set is String_Sets.Set;

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String);

   package String_Ordered_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets
       (Element_Type        => String,
        "<"                 => "<",
        "="                 => "=");

   subtype String_Ordered_Set is String_Ordered_Sets.Set;

   function "+"
     (S : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "+"
     (S : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   function "+" (Text : Text_Type) return String renames To_UTF8;

   function "+" (Str : String) return Text_Type renames From_UTF8;

   function "+" (T : Text_Type) return Unbounded_Text_Type
                 renames To_Unbounded_Text;

   function "+" (T : Unbounded_Text_Type) return Text_Type
                 renames To_Text;

   function "+" (Text : Unbounded_Text_Type) return String is
     (+(+Text));

   function "+" (Str : String) return Unbounded_Text_Type is
     (+(+Str));

   function Remove_Trailing_Comma_And_Spaces
     (Text : Unbounded_String) return Unbounded_String is
     (Trim (Trim (Text, Right), Null_Set, To_Set (',')));

   function Remove_Trailing_Comma_And_Spaces
     (Text : String) return String is
     (Trim (Trim (Text, Right), Null_Set, To_Set (',')));
   --  Remove the trailing spaces and comma of the given Text, e.g. passing
   --  "[a, b, " will return "[a, b".

   function Dot_To_Underscore (C : Character) return Character is
     ((if C = '.' then '_' else C));

   function Qualified_To_Unique_Name (Qualified_Name : Text_Type) return String
     is (Ada.Strings.Fixed.Translate
              (Source  => To_UTF8 (Qualified_Name),
               Mapping => Dot_To_Underscore'Access));

   procedure New_Line (Str : in out Unbounded_String);

   procedure Write_Line
     (Str  : in out Unbounded_String;
      Add  : String;
      Span : Natural);

   procedure S_Write
     (Str  : in out Unbounded_String;
      Add  : String;
      Span : Natural);

   procedure Write
     (Str  : in out Unbounded_String;
      Add  : String);
end TGen.Strings;
