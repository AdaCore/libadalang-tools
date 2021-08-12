------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package body Printers is

   --------
   -- PP --
   --------

   procedure PP (Edits : Refactoring_Edits) is

      use Text_Edit_Ordered_Maps;

      procedure PP (C : Cursor);

      --------
      -- PP --
      --------

      procedure PP (C : Cursor) is
         Previous_Line : Line_Number := 0;

      begin
         Put_Line ("Unit:" & Simple_Name (Key (C)));
         for Text_Edit of Element (C) loop
            if Previous_Line /= Text_Edit.Location.Start_Line then
               Put_Line ("Line:" & Text_Edit.Location.Start_Line'Image);
            end if;
            Previous_Line := Text_Edit.Location.Start_Line;
            Put_Line ("Source Location Range:");
            Put_Line ("Start_Line:" & Text_Edit.Location.Start_Line'Image);
            Put_Line ("Start_Column:" & Text_Edit.Location.Start_Column'Image);
            Put_Line ("End_Line:" & Text_Edit.Location.End_Line'Image);
            Put_Line ("End_Column:" & Text_Edit.Location.End_Column'Image);
         end loop;
      end PP;

   begin
      Put_Line ("References:");
      Edits.Text_Edits.Iterate
        (PP'Access);
      if not Edits.File_Renames.Is_Empty then
         Put_Line ("File renames:");
         for File_Rename of Edits.File_Renames loop
            Put_Line
              (Simple_Name (To_String (File_Rename.Filepath))
               & " -> "
               & Simple_Name (To_String (File_Rename.New_Name)));
         end loop;
      end if;
      if not Edits.Diagnostics.Is_Empty then
         Put_Line ("Problems:");
         for Problem of Edits.Diagnostics loop
            Put_Line (Problem.Info);
         end loop;
      end if;
   end PP;

end Printers;
