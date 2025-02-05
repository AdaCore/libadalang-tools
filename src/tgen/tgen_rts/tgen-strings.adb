------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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

with Ada.Characters.Handling;

package body TGen.Strings is

   ----------
   -- Join --
   ----------

   function Join
     (V : String_Vector; Sep : Character := Ada.Characters.Latin_1.LF)
      return String
   is
      Result : Unbounded_String;
   begin
      for I in V.First_Index .. V.Last_Index loop
         Append (Result, V (I));
         if I /= V.Last_Index then
            Append (Result, Sep);
         end if;
      end loop;
      return +Result;
   end Join;

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
     (Str : in out Unbounded_String; Add : String; Span : Natural) is
   begin
      Append (Str, [for I in 1 .. Span => ' ']);
      Append (Str, Add);
      New_Line (Str);
   end Write_Line;

   -------------
   -- S_Write --
   -------------

   procedure S_Write
     (Str : in out Unbounded_String; Add : String; Span : Natural) is
   begin
      Append (Str, [for I in 1 .. Span => ' ']);
      Append (Str, Add);
   end S_Write;

   -----------
   -- Write --
   -----------

   procedure Write (Str : in out Unbounded_String; Add : String) is
   begin
      Append (Str, Add);
   end Write;

   -------------------
   -- Indent_String --
   -------------------

   procedure Indent_String (Str : in out Unbounded_String; Span : Natural) is
      Indent_Str : constant String (1 .. Span) := [others => ' '];
      Index      : Natural := 1;
   begin
      if Length (Str) = 0 then
         return;
      end if;
      Replace_Slice (Str, 1, 1, Indent_Str & Element (Str, 1));

      while Index <= Length (Str) loop
         Index :=
           Ada.Strings.Unbounded.Index
             (Str, To_Set (Ada.Characters.Latin_1.LF), Index);
         if Index = 0 then
            return;
         end if;

         Ada.Strings.Unbounded.Replace_Slice
           (Str, Index, Index, Ada.Characters.Latin_1.LF & Indent_Str);
         Index := @ + Span;
      end loop;
   end Indent_String;

   ---------------
   -- To_Symbol --
   ---------------

   function To_Symbol
     (Name : Ada_Qualified_Name; Sep : Character) return String
   is
      Result : Unbounded_String;
   begin
      for Id of Name loop
         if Length (Result) > 0 then
            Append (Result, Sep);
         end if;
         Append (Result, To_String (Id));
      end loop;

      return +Result;
   end To_Symbol;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Name : Ada_Qualified_Name) return String is
   begin
      if Name.Is_Empty then
         return "";
      end if;
      return To_Symbol (Name, Sep => '.');
   end To_Ada;

   -----------------
   -- To_Filename --
   -----------------

   function To_Filename (Name : Ada_Qualified_Name) return String is
      use Ada.Characters.Handling;
   begin
      return To_Lower (To_Symbol (Name, Sep => '-'));
   end To_Filename;

   -----------------------
   -- To_Qualified_Name --
   -----------------------

   function To_Qualified_Name (Name : String) return Ada_Qualified_Name is
      use Ada.Characters.Handling;
      I      : Positive := 1;
      Result : Ada_Qualified_Name;
   begin
      for J in Name'Range loop
         if Name (J) = '.' then
            Ada_Identifier_Vectors.Append
              (Result, To_Unbounded_String (To_Lower (Name (I .. J - 1))));
            I := J + 1;
         end if;
      end loop;
      Ada_Identifier_Vectors.Append
        (Result, To_Unbounded_String (To_Lower (Name (I .. Name'Last))));
      return Result;
   end To_Qualified_Name;

   -----------------
   -- Is_Operator --
   -----------------

   function Is_Operator (Op_Name : String) return Boolean
   is (Op_Name'Length >= 1 and then Op_Name (Op_Name'First) = '"');

   -----------------------
   -- Map_Operator_Name --
   -----------------------

   function Map_Operator_Name (Op_Name : String) return String is
   begin
      if Op_Name = """and""" then
         return "Op_And";
      elsif Op_Name = """or""" then
         return "Op_Or";
      elsif Op_Name = """xor""" then
         return "Op_Xor";
      elsif Op_Name = """<""" then
         return "Op_Less";
      elsif Op_Name = """<=""" then
         return "Op_Less_Eq";
      elsif Op_Name = """>""" then
         return "Op_More";
      elsif Op_Name = """>=""" then
         return "Op_More_Eq";
      elsif Op_Name = """=""" then
         return "Op_Eq";
      elsif Op_Name = """/=""" then
         return "Op_Neq";
      elsif Op_Name = """+""" then
         return "Op_Plus";
      elsif Op_Name = """-""" then
         return "Op_Minus";
      elsif Op_Name = """&""" then
         return "Op_Concat";
      elsif Op_Name = """*""" then
         return "Op_Mult";
      elsif Op_Name = """/""" then
         return "Op_Div";
      elsif Op_Name = """mod""" then
         return "Op_Mod";
      elsif Op_Name = """rem""" then
         return "Op_rem";
      elsif Op_Name = """**""" then
         return "Op_Pow";
      elsif Op_Name = """abs""" then
         return "Op_Abs";
      elsif Op_Name = """not""" then
         return "Op_Not";
      else
         --  Defensive code

         raise Constraint_Error with "Unknown operator name";
      end if;
   end Map_Operator_Name;

   ----------------------
   -- Copy_Delete_Last --
   ----------------------

   function Copy_Delete_Last
     (FQN : Ada_Qualified_Name) return Ada_Qualified_Name is
   begin
      return Res : Ada_Qualified_Name := FQN.Copy do
         Res.Delete_Last;
      end return;
   end Copy_Delete_Last;

   -----------------------
   -- Copy_Delete_First --
   -----------------------

   function Copy_Delete_First
     (FQN : Ada_Qualified_Name) return Ada_Qualified_Name is
   begin
      return Res : Ada_Qualified_Name := FQN.Copy do
         Res.Delete_First;
      end return;
   end Copy_Delete_First;

end TGen.Strings;
