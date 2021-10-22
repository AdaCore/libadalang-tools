------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Helpers;

with Langkit_Support.Text;

with TGen.Types;
with TGen.Types.Translation; use TGen.Types.Translation;

procedure TGen_App is
   package Helpers renames Libadalang.Helpers;
   package LAL renames Libadalang.Analysis;

   package Text renames Langkit_Support.Text;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);

   function Visit (Node : LAL.Ada_Node'Class) return Visit_Status;

   package App is new Helpers.App
     (Name         => "Libgen", Description => "Type generation value for Ada",
      Process_Unit => Process_Unit);

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      pragma Unreferenced (Job_Ctx);
      use LAL;
      Root : Ada_Node;
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         return;
      end if;
      Put_Line ("Processing unit " & Get_Filename (Unit));
      New_Line;
      Root := Unit.Root;
      Traverse (Root, Visit'Access);
   end Process_Unit;

   function Visit (Node : LAL.Ada_Node'Class) return Visit_Status is
      use LAL;
      Param_Number : Positive := 1;
   begin
      if Kind (Node) in Ada_Subp_Spec_Range then
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
               Res : constant Translation_Result :=
                 Translate (Param.F_Type_Expr);
            begin
               Put_Line ("Param" & Param_Number'Image & " : ");
               if Res.Success then
                  Put_Line (Res.Res.Image);
               else
                  Put_Line (To_String (Res.Diagnostics));
               end if;
            end;
            Param_Number := Param_Number + 1;
         end loop;
         return Over;
      else
         return Into;
      end if;
   end Visit;

begin
   App.Run;
end TGen_App;
