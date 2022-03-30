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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Helpers; use Libadalang.Helpers;

with TGen.Context;              use TGen.Context;
with TGen.Files;                use TGen.Files;
with TGen.Gen_Strategies;       use TGen.Gen_Strategies;
with TGen.Gen_Strategies_Utils; use TGen.Gen_Strategies_Utils;
with TGen.Strings;              use TGen.Strings;
with TGen.Templates;
with TGen.Types.Translation;    use TGen.Types.Translation;
with TGen.Types;

procedure TGen_App is
   package Helpers renames Libadalang.Helpers;
   package LAL renames Libadalang.Analysis;

   package Text renames Langkit_Support.Text;

   GC : Generation_Context;

   procedure App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array);

   procedure App_Post_Process
     (Context : Helpers.App_Context;
      Jobs : Helpers.App_Job_Context_Array);

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);

   function Visit (Node : LAL.Ada_Node'Class) return Visit_Status;

   package App is new Helpers.App
     (Name         => "Libgen", Description => "Type generation value for Ada",
      Process_Unit => Process_Unit,
      App_Setup    => App_Setup,
      App_Post_Process => App_Post_Process);

   procedure App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array)
   is
      Prj : Project_Type renames Context.Provider.Project.Root_Project;
   begin
      Initialize (GC, Context.Provider.Project.Root_Project,
                  +Project_Output_Dir
                    (Context.Provider.Project.Root_Project));
   end App_Setup;

   procedure App_Post_Process
     (Context : Helpers.App_Context; Jobs : Helpers.App_Job_Context_Array)
   is
   begin
      --  Generate_Type_Strategies (GC);
      --  Generate_Artifacts (GC);
      TGen.Types.Translation.Print_Cache_Stats;
      TGen.Types.Translation.PP_Cache;
   end App_Post_Process;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      use LAL;
      Root : Ada_Node;
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line ("Unit has diagnostics");
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         return;
      end if;
      Prj_Tree := Job_Ctx.App_Ctx.Provider.Project;
      Put_Line ("Processing unit " & Get_Filename (Unit));
      New_Line;
      Root := Unit.Root;
      Traverse (Root, Visit'Access);
   end Process_Unit;

   function Visit (Node : LAL.Ada_Node'Class) return Visit_Status is
      use LAL;
      Param_Number : Positive := 1;

   begin
      if Kind (Node) in Ada_Subp_Spec_Range
        and then Kind (Parent (Node)) in Ada_Subp_Decl_Range
      then
         Put_Line ("Found subprogram spec :");
         Put_Line
           ("Subprogram Name : " &
            Text.Image (Node.As_Subp_Spec.F_Subp_Name.Text));
         if Is_Null (Node.As_Subp_Spec.F_Subp_Params) then
            return Over;
         end if;
         Put_Line ("Params:");
         for Param of Node.As_Subp_Spec.F_Subp_Params.F_Params loop
            declare
               Trans_Res : constant Translation_Result :=
                 Translate (Param.F_Type_Expr, Verbose => True);
            begin
               Put_Line ("Param" & Param_Number'Image & " : ");
               if Trans_Res.Success then
                  Put_Line (Trans_Res.Res.Get.Image);
               else
                  Put_Line ("Failed: " & To_String (Trans_Res.Diagnostics));
               end if;
            end;
            Param_Number := Param_Number + 1;
         end loop;
         return Over;
      elsif Kind (Node) in Ada_Subp_Decl then

         begin
            Generate_Test_Vectors
              (GC, 10, Node.As_Subp_Decl);
            return Into;
         exception
            when Exc : Program_Error =>
               Put_Line ("Could not generate testcase values for procedure "
                         & (+Node.As_Basic_Decl.P_Fully_Qualified_Name)
                         & ": " & Ada.Exceptions.Exception_Information (Exc));
               return Into;
         end;
      else
         return Into;
      end if;
   end Visit;

begin
   App.Run;
end TGen_App;
