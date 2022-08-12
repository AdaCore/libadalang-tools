------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Characters.Latin_1;

package body Edit_File is

   procedure Apply_Edits (Edits     : ReFac.Text_Edit_Map;
                          Path      : String) is
      function Is_Insert_Enter (Edit : ReFac.Text_Edit) return Boolean;

      function Is_Insert_Enter (Edit : ReFac.Text_Edit) return Boolean is
         Text : String := To_String (Edit.Text);
      begin
         if Integer (Edit.Location.Start_Column) =
           Integer (Edit.Location.End_Column) and
            Integer (Edit.Location.Start_Line) =
            Integer (Edit.Location.End_Line)
         then
            if Text (Text'Last) = Ada.Characters.Latin_1.LF then
               return True;
            end if;
         end if;
         return False;
      end Is_Insert_Enter;
   begin
      for File in Edits.Iterate loop
         declare
            File_Name : String := File.Key;
            Edits_Map : ReFac.Text_Edit_Ordered_Set := Edits (File);
            Src, Dst : File_Type;
            Line : Ada.Strings.Unbounded.Unbounded_String;
            Line_Count : Integer := 1;
            New_File_Name : Unbounded_String := Null_Unbounded_String;
            Name_Count, Name_Begin_Position : Integer := File_Name'Last;
         begin
            --  Open the Original file
            Open (Src, In_File, File_Name);
            loop
               Name_Count := Name_Count - 1;
               exit when File_Name (Name_Count) = '.';
            end loop;
            if Path = "." then
               Name_Begin_Position := 0;
            else
               loop
                  Name_Begin_Position := Name_Begin_Position - 1;
                  exit when File_Name (Name_Begin_Position) = '/';
               end loop;
            end if;
            for I in Name_Begin_Position + 1 .. Name_Count - 1 loop
               New_File_Name := New_File_Name & File_Name (I);
            end loop;
            New_File_Name := New_File_Name & "_correct";
            for I in Name_Count .. File_Name'Last loop
               New_File_Name := New_File_Name & File_Name (I);
            end loop;
            if Path /= "." then
               New_File_Name := Path & New_File_Name;
            end if;
            Create (File => Dst, Mode => Out_File,
                    Name => To_String (New_File_Name));
            for Text_Edit of Edits_Map loop
               loop
                  exit when Line_Count = Integer
                    (Text_Edit.Location.Start_Line);
                  Line_Count := Line_Count + 1;
                  Get_Line (Src, Line);
                  Put_Line (Dst, Line);
               end loop;
               Get_Line (Src, Line);
               declare
                  This_Line : String := To_String (Line);
                  New_Text : String := To_String (Text_Edit.Text);
                  Insert_Text : Unbounded_String := Null_Unbounded_String;
               begin
                  if This_Line'First /= Integer
                    (Text_Edit.Location.Start_Column)
                  then
                     for I in This_Line'First ..
                       Integer (Text_Edit.Location.Start_Column) - 1 loop
                        --  Put (Dst, This_Line (I));
                        Insert_Text := Insert_Text & This_Line (I);
                     end loop;
                  end if;
                  --  Put (Dst, New_Text);
                  Insert_Text := Insert_Text & New_Text;
                  if Is_Insert_Enter (Text_Edit) then
                     for I in 1 .. Integer (Text_Edit.Location.Start_Column)
                       - 1 loop
                        --  Put (Dst, ' ');
                        Insert_Text := Insert_Text & ' ';
                     end loop;
                  end if;
                  loop
                     exit when Line_Count = Integer
                       (Text_Edit.Location.End_Line);
                     Line_Count := Line_Count + 1;
                     Get_Line (Src, Line);
                  end loop;
                  for I in Integer (Text_Edit.Location.End_Column) ..
                    This_Line'Last loop
                     Insert_Text := Insert_Text & This_Line (I);
                  end loop;
                  Line_Count := Line_Count + 1;
                  declare
                     Text : String := To_String (Insert_Text);
                     Is_Blank : Boolean := True;
                  begin
                     for I in Text'First .. Text'Last loop
                        if Text (I) /= ' ' then
                           Is_Blank := False;
                           exit;
                        end if;
                     end loop;
                     if not Is_Blank then
                        Put_Line (Dst, Text);
                     end if;
                  end;
               end;
            end loop;
            loop
               exit when End_Of_File (Src);
               Get_Line (Src, Line);
               Put_Line (Dst, Line);
            end loop;
            Close (Dst);
         end;
      end loop;
   end Apply_Edits;

end Edit_File;
