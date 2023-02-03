------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with GNATCOLL.VFS;
with GNATCOLL.Projects;

with Langkit_Support.Diagnostics;

with Libadalang.Project_Provider;

with Lint.Command_Line;

package body Lint.Utils is

   ------------------------------------------
   -- Get_Analysis_Units_From_Sources_List --
   ------------------------------------------

   function Get_Analysis_Units_From_Sources_List
     (Sources          : Sources_List;
      Project_Filename : String := "")
      return Libadalang.Analysis.Analysis_Unit_Array
   is

   begin
      if Sources'Length = 0 then
         return [];
      end if;

      if Project_Filename /= "" then
         declare
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

            return Units : Analysis_Unit_Array (Sources'Range) do
               for J in Units'Range loop
                  declare
                     use Langkit_Support.Diagnostics;

                     Unit : constant Analysis_Unit :=
                       Context.Get_From_File (To_String (Sources (J)));

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
            end return;
         end;

      else
         declare
            use Ada.Strings.Unbounded;
            use Libadalang.Analysis;

            Context : constant Analysis_Context := Create_Context;

         begin
            return Units : Analysis_Unit_Array (Sources'Range) do
               for J in Units'Range loop
                  declare
                     use Langkit_Support.Diagnostics;

                     Unit : constant Analysis_Unit :=
                       Context.Get_From_File (To_String (Sources (J)));

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
            end return;
         end;
      end if;
   end Get_Analysis_Units_From_Sources_List;

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
      for Scenario_Variable of Lint.Command_Line.Scenario_Variables.Get loop
         declare
            Equal_Index : constant Natural :=
              Ada.Strings.Unbounded.Index (Scenario_Variable, "=");
            Name        : constant String :=
              Ada.Strings.Unbounded.Slice
                (Scenario_Variable, 1, Equal_Index - 1);
            Value        : constant String :=
              Ada.Strings.Unbounded.Slice
                (Scenario_Variable,
                 Equal_Index + 1,
                 Ada.Strings.Unbounded.Length (Scenario_Variable));
         begin
            Project_Environment.Change_Environment (Name, Value);
         end;
      end loop;
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

end Lint.Utils;
