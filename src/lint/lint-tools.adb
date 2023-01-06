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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

with GNATCOLL.VFS;
with GNATCOLL.Projects;

with Langkit_Support.Diagnostics;

with Libadalang.Project_Provider;

package body Lint.Tools is

   -------------
   -- Convert --
   -------------

   function Convert (Arg : String) return Tool is
   begin
      return Tool'Value (Arg);
   exception
      when others =>
         raise Parse_Tool_Exception;
   end Convert;

   ---------------
   -- Tool_List --
   ---------------

   function Tool_List return String is
      use Ada.Characters.Handling;
      --  use Ada.Characters.Latin_1;
      use Ada.Strings.Unbounded;
      H : Unbounded_String;
   begin
      Append (H, To_Lower (Tool'Image (Tool'First)));
      --  if Tool'Range_Length = 1 then
      --     Append (H, To_Lower (Tool'Image (Tool'First)));
      --  else
      --     Append (H, To_Lower (Tool'Image (Tool'First)));
      --     for J in Tool'Succ (Tool'First) .. Tool'Last loop
      --        Append (H, (LF & "         " & To_Lower (Tool'Image (J))));
      --     end loop;
      --  end if;
      return To_String (H);
   end Tool_List;

   ---------------------------
   -- Find_First_Tool_Index --
   ---------------------------

   function Find_First_Tool_Index return Natural
   is
      package String_Hashed_Sets is new Ada.Containers.Indefinite_Hashed_Sets
        (Element_Type        => String,
         Hash                => Ada.Strings.Hash,
         Equivalent_Elements => "=");

      Tools_Set : String_Hashed_Sets.Set;

      use Ada.Characters.Handling;
      use Ada.Command_Line;

   begin
      for J in Tool'First .. Tool'Last loop
         Tools_Set.Insert (To_Lower (Tool'Image (J)));
      end loop;

      for J in 1 .. Argument_Count loop
         if Tools_Set.Contains (To_Lower (Argument (J))) then
            return J;
         end if;
      end loop;

      return 0;
   end Find_First_Tool_Index;

   --------------------------------
   -- Get_Project_Analysis_Units --
   --------------------------------

   function Get_Project_Analysis_Units
     (Project_Filename : String)
      return Libadalang.Analysis.Analysis_Unit_Array
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.Projects;
      use GNATCOLL.VFS;
      use Libadalang.Analysis;
      use Libadalang.Project_Provider;

      Project_Environment  : Project_Environment_Access;
      Project_Tree         : constant Project_Tree_Access :=
        new GNATCOLL.Projects.Project_Tree;
      Project_Virtual_File : constant Virtual_File :=
        Create (+Project_Filename);

      Context       : Analysis_Context;
      Unit_Provider : Unit_Provider_Reference;
      Sources       : Filename_Vectors.Vector;

   begin
      Initialize (Project_Environment);
      --  TODO: Use procedures in GNATCOLL.Projects to set scenario
      --  variables (Change_Environment), set the target
      --  and runtime (Set_Target_And_Runtime), etc.
      Project_Tree.Load
        (Root_Project_Path => Project_Virtual_File,
         Env               => Project_Environment);

      Unit_Provider :=
        Create_Project_Unit_Provider
          (Tree => Project_Tree,
           Env  => Project_Environment);
      Context := Create_Context (Unit_Provider => Unit_Provider);

      Sources := Source_Files (Project_Tree.all);

      return Units : Analysis_Unit_Array
                       (Sources.First_Index .. Sources.Last_Index)
      do
         for J in Units'Range loop
            declare
               use Langkit_Support.Diagnostics;

               Unit : constant Analysis_Unit :=
                 Context.Get_From_File (To_String (Sources.Element (J)));

            begin
               if Unit.Has_Diagnostics then
                  Logger.Trace
                    ("WARNING: Source "
                     & Unit.Get_Filename
                     & " has diagnostics");
                  for Diagnostic of Unit.Diagnostics loop
                     Logger.Trace (To_Pretty_String (Diagnostic));
                  end loop;

               else
                  Units (J) := Unit;
               end if;
            end;
         end loop;

         Lint.Logger.Trace ("Found" & Units'Length'Image & " units");
      end return;
   end Get_Project_Analysis_Units;

end Lint.Tools;
