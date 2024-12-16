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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with Test.Actions;
with Test.Common;        use Test.Common;
with Test.Command_Lines;
with Test.Subprocess;    use Test.Subprocess;
with Utils_Debug;        use Utils_Debug;
with Utils.Drivers;      use Utils.Drivers;
with Utils.Environment;  use Utils.Environment;

with GNATCOLL.OS.Process;

with Libadalang.Common; use Libadalang.Common;

with Langkit_Support.Text;

with TGen.Libgen; use TGen.Libgen;
with TGen.Strings;

package body Test.Generation is

   Global_Aspect_Name : constant Langkit_Support.Text.Unbounded_Text_Type :=
     Langkit_Support.Text.To_Unbounded_Text ("Global");

   function Traverse_Helper (Node : Ada_Node'Class) return Visit_Status;
   --  If node is a subprogram declaration (regular or generic instantiation),
   --  include it in the Libgen context. Otherwise keep traversing the tree.

   ---------------------
   -- Traverse_Helper --
   ---------------------

   function Traverse_Helper (Node : Ada_Node'Class) return Visit_Status is
      use TGen.Strings;
      Diags : String_Vector;
   begin
      --  Do not traverse package bodies

      if Node.Kind in Ada_Package_Body then
         return Over;
      end if;

      if not Common_Subp_Node_Filter (Node) then
         return Over;
      end if;

      --  Skip any non-instantiated generic decl, they will be processed as
      --  part of a generic instantiation, if any.

      if Node.Kind in Ada_Generic_Decl
        and then Node.P_Generic_Instantiations'Length = 0
      then
         return Over;
      end if;

      --  Collect all types used as parameters in subprogram declarations.
      --  Skip generic subprogram declarations as we only care about the
      --  instantiations. Also skip subprograms with zero parameters if there
      --  is no Global aspect attached.
      --
      --  Ada_Subp_Decl designates all "regular" subprograms, excluding enum
      --  literals, entries, generic formal subprograms and abstract
      --  subprograms with the exception of expression functions, null
      --  procedures and subprogram renamings which are considered as
      --  subprogram bodies in LAL.

      if Node.Kind in Ada_Subp_Decl
        | Ada_Expr_Function
        | Ada_Null_Subp_Decl
        | Ada_Subp_Renaming_Decl
      then

         --  Check, if the subprogram has zero parameters. If so, only add it
         --  to the generation context if it has a global annotation.

         if Node.As_Basic_Decl.P_Subp_Spec_Or_Null.P_Params'Length = 0
           and then not Node.As_Basic_Decl.P_Has_Aspect (Global_Aspect_Name)
         then
            return Over;
         end if;

         for Inst of Node.As_Basic_Decl.P_Generic_Instantiations loop
            --  If it is top level generic package instantiation, we call
            --  `Include_Subp` but with the associated switch set.
            if Inst.Unit.Root.As_Compilation_Unit.F_Body.Kind
               = Libadalang.Common.Ada_Library_Item
              and then Inst
               .Unit
               .Root
               .As_Compilation_Unit
               .F_Body
               .As_Library_Item
               .F_Item = Inst.As_Basic_Decl
            then
               if not Include_Subp
                 (Test.Common.TGen_Libgen_Ctx, Node.As_Basic_Decl, Diags,
                  Is_Top_Level_Generic_Instantiation => True)
               then
                  Report_Err
                    ("Error while processing " & Node.Image & ":" & ASCII.LF
                     & Join (Diags) & ASCII.LF);
               end if;
               return Over;
            end if;
         end loop;

         if not Include_Subp
           (Test.Common.TGen_Libgen_Ctx, Node.As_Basic_Decl, Diags)
         then

            Report_Err
              ("Error while processing " & Node.Image & ":" & ASCII.LF
               & Join (Diags) & ASCII.LF);
         end if;
         return Over;
      end if;

      --  Traverse subprogram declarations in generic package instantiations
      if Node.Kind in Ada_Generic_Package_Instantiation then
         Node.As_Generic_Package_Instantiation.P_Designated_Generic_Decl
         .As_Generic_Package_Decl.F_Package_Decl
         .Traverse (Traverse_Helper'Access);
         return Over;
      end if;
      return Into;
   end Traverse_Helper;

   -------------------------
   -- Run_First_Pass_Tool --
   -------------------------

   procedure Run_First_Pass_Tool (Cmd : Command_Line) is
      pragma Unreferenced (Cmd);
      First_Pass_Tool : Test.Actions.Test_Tool;
      First_Pass_Cmd : Command_Line (Test.Command_Lines.Descriptor'Access);
   begin
      Driver
        (Cmd               => First_Pass_Cmd,
         Tool              => First_Pass_Tool,
         Tool_Package_Name => Test.Common.GT_Package);
   end Run_First_Pass_Tool;

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source (Unit : Analysis_Unit) is
   begin
      Traverse (Unit.Root, Traverse_Helper'Access);
   end Process_Source;

   ----------------------------
   -- Generate_Build_And_Run --
   ----------------------------

   procedure Generate_Build_And_Run (Cmd : Command_Line) is
      use GNATCOLL.OS.Process;
      Directory_Separator : Character renames GNAT.OS_Lib.Directory_Separator;

      Build_Args    : Argument_List;
      Run_Args      : Argument_List;
      Harness_Dir   : constant String :=
        Tool_Temp_Dir.all & Directory_Separator & "tgen_Harness";
      Ext_Acc       : GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Get_Executable_Suffix;
      Ext           : constant String := Ext_Acc.all;

   begin
      GNAT.OS_Lib.Free (Ext_Acc);

      --  Check if we have a non empty list of subprograms to generate test
      --  case vectors for.

      if not Test.Common.TGen_Libgen_Ctx.Is_Generation_Required then
         Report_Std ("No subprogram supported for test case generation.");
         return;
      end if;

      --  Generate the harness

      TGen.Libgen.Generate_Harness
        (Test.Common.TGen_Libgen_Ctx,
         Harness_Dir,
         Test.Common.JSON_Test_Dir.all,
         Test.Common.TGen_Strat_Kind,
         Test.Common.TGen_Num_Tests);

      --  Build the harness. For this, reuse the gpr options passed on the
      --  command line.

      Build_Args.Append ("gprbuild");
      if not Test.Common.Verbose and then not Debug_Flag_1 then
         Build_Args.Append ("-q");
      end if;
      Build_Args.Append ("-P");
      Build_Args.Append
        (Harness_Dir & Directory_Separator & "tgen_generation_harness.gpr");
      Populate_X_Vars (Build_Args, Cmd);

      --  Suppress all warning/info messages and style checks

      Build_Args.Append ("-cargs:ada");
      Build_Args.Append ("-gnatws");
      Build_Args.Append ("-gnatyN");

      if Debug_Flag_1 then
         Build_Args.Append ("-g");
         Build_Args.Append ("-O0");
         Build_Args.Append ("-bargs");
         Build_Args.Append ("-Es");
      end if;
      Run (Build_Args, "Build of the test generation harness");

      Run_Args.Append
        (Harness_Dir & Directory_Separator & "obj" & Directory_Separator
         & "generation_main" & Ext);
      Run (Run_Args, "Execution of the test generation harness");
   end Generate_Build_And_Run;

end Test.Generation;
