------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2022-2023, AdaCore                    --
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
--
--  This package constains utilities to apply text edits on files

with Ada.Containers.Indefinite_Ordered_Maps;

with Laltools.Refactor;

with VSS.Strings;

package Lint.File_Edits is

   use type VSS.Strings.Virtual_String;

   package File_Name_To_Virtual_String_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Laltools.Refactor.File_Name_Type,
        Element_Type => VSS.Strings.Virtual_String);

   subtype File_Name_To_Virtual_String_Map is
     File_Name_To_Virtual_String_Maps.Map;

   procedure Apply_Edits
     (Edits : Laltools.Refactor.Text_Edit_Map);
   --  Apply Edits on disk. If an exception happens while trying to apply
   --  an edit to a file, that file is skipped and the procedure continues
   --  with the next one.

   function Apply_Edits
     (Edits : Laltools.Refactor.Text_Edit_Map)
      return File_Name_To_Virtual_String_Map;
   --  Apply Edits on and return buffers with the edits applied. If an
   --  exception occurs while trying to create a buffer with the edits,
   --  the returned map will not contain that buffer.

end Lint.File_Edits;
