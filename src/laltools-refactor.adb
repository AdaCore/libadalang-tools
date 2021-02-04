------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Text_IO; use Ada.Text_IO;

package body Laltools.Refactor is

   function Image (S : Source_Location_Range) return String;
   --  Return a Source_Location_Range as a string in a human readable format

   procedure Print (E : Edit);
   --  Print an Edit in an human readable format to the standard output

   procedure Print (S : Edit_Ordered_Set);
   --  Print an Edit_Ordered_Set in an human readable format to the standart
   --  output.

   -----------
   -- Image --
   -----------

   function Image (S : Source_Location_Range) return String is
   begin
      return Trim (S.Start_Line'Image, Both)
        & ":"
        & Trim (S.Start_Column'Image, Both)
        & "-"
        & Trim (S.End_Line'Image, Both)
        & ":"
        & Trim (S.End_Column'Image, Both);
   end Image;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Edits    : in out Edit_Map;
      Filename : String;
      Location : Langkit_Support.Slocs.Source_Location_Range;
      Text     : Ada.Strings.Unbounded.Unbounded_String)
   is
      Edits_Set : Edit_Ordered_Set;

   begin
      if Edits.Contains (Filename) then
         if not Edits.Reference (Filename).
           Contains (Edit'(Location, Text))
         then
            Edits.Reference (Filename).Insert (Edit'(Location, Text));
         end if;

      else
         Edits_Set.Insert (Edit'(Location, Text));
         Edits.Insert (Filename, Edits_Set);
      end if;
   end Insert;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Source : in out Edit_Map;
      Target : Edit_Map)
   is
      Map_Cursor : Edit_Ordered_Maps.Cursor := Target.First;

   begin
      while Edit_Ordered_Maps.Has_Element (Map_Cursor) loop
         if Source.Contains (Edit_Ordered_Maps.Key (Map_Cursor)) then
            declare
               Set_Cursor : Edit_Ordered_Sets.Cursor :=
                 Target.Constant_Reference (Map_Cursor).First;

            begin
               while Edit_Ordered_Sets.Has_Element (Set_Cursor) loop
                  Source.Reference (Edit_Ordered_Maps.Key (Map_Cursor)).Insert
                    (Edit_Ordered_Sets.Element (Set_Cursor));
                  Edit_Ordered_Sets.Next (Set_Cursor);
               end loop;
            end;

         else
            Source.Insert
              (Edit_Ordered_Maps.Key (Map_Cursor),
               Edit_Ordered_Maps.Element (Map_Cursor));
         end if;

         Edit_Ordered_Maps.Next (Map_Cursor);
      end loop;
   end Merge;

   -----------
   -- Print --
   -----------

   procedure Print (E : Edit) is
   begin
      Ada.Text_IO.Put_Line (Image (E.Location) & " " & To_String (E.Text));
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (S : Edit_Ordered_Set) is
   begin
      for E of S loop
         Print (E);
      end loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (M : Edit_Map) is
      use Edit_Ordered_Maps;
      C : Cursor := M.First;

   begin
      while Has_Element (C) loop
         Ada.Text_IO.Put_Line (Key (C));
         Print (Element (C));
         Next (C);
      end loop;

      Ada.Text_IO.New_Line;
   end Print;
end Laltools.Refactor;
