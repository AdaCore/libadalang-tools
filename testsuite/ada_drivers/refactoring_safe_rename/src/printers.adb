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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;

package body Printers is

   --------
   -- PP --
   --------

   procedure PP (Sloc : Source_Location_Range) is
   begin
      Put_Line ("Start_Line:" & Sloc.Start_Line'Image);
      Put_Line ("Start_Column:" & Sloc.Start_Column'Image);
      Put_Line ("End_Line:" & Sloc.End_Line'Image);
      Put_Line ("End_Column:" & Sloc.End_Column'Image);
   end PP;

   --------
   -- PP --
   --------

   procedure PP (Set : Slocs_Sets.Set)
   is
      use Slocs_Sets;
      C : Cursor := Set.First;
   begin
      while Has_Element (C) loop
         Put_Line ("Source Location Range:");
         PP (Constant_Reference (Set, C));
         Next (C);
      end loop;
   end PP;

   --------
   -- PP --
   --------

   procedure PP (Map : Slocs_Maps.Map)
   is
      use Slocs_Maps;
      C : Cursor := Map.First;
   begin
      while Has_Element (C) loop
         Put_Line ("Line:" & Key (C)'Image);
         PP (Constant_Reference (Map, C));
         Next (C);
      end loop;
   end PP;

   --------
   -- PP --
   --------

   procedure PP (Map : Unit_Slocs_Ordered_Maps.Map)
   is
      use Unit_Slocs_Ordered_Maps;
      C : Cursor := Map.First;
   begin
      while Has_Element (C) loop
         Put_Line ("Unit:" & Simple_Name (Key (C).Get_Filename));
         PP (Constant_Reference (Map, C));
         Next (C);
      end loop;
   end PP;

   --------
   -- PP --
   --------

   procedure PP (References : Renamable_References) is
      Ordered_References : Unit_Slocs_Ordered_Maps.Map;

      C : Unit_Slocs_Maps.Cursor := References.References.First;
   begin
      while Unit_Slocs_Maps.Has_Element (C) loop
         Ordered_References.Insert
           (Unit_Slocs_Maps.Key (C),
            Unit_Slocs_Maps.Constant_Reference (References.References, C));
         Unit_Slocs_Maps.Next (C);
      end loop;
      Put_Line ("References:");
      PP (Ordered_References);
      if not References.Problems.Is_Empty then
         Put_Line ("Problems:");
         for Problem of References.Problems loop
            Put_Line (Problem.Info);
         end loop;
      end if;
   end PP;

end Printers;
