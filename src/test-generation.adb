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
with Test.Common;                use Test.Common;
with Test.Command_Lines;
with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
with Utils_Debug;                use Utils_Debug;
with Utils.Drivers;              use Utils.Drivers;
with Utils.Environment;          use Utils.Environment;

with GNATCOLL.OS.Process;

with Libadalang.Common; use Libadalang.Common;

with TGen.Libgen; use TGen.Libgen;

package body Test.Generation is

   function Traverse_Helper (Node : Ada_Node'Class) return Visit_Status;
   --  If node is a subprogram declaration (regular or generic instantiation),
   --  include it in the Libgen context. Otherwise keep traversing the tree.

   ---------------------
   -- Traverse_Helper --
   ---------------------

   function Traverse_Helper (Node : Ada_Node'Class) return Visit_Status is
      Diags : Unbounded_String;
   begin
      --  Collect all types used as parameters in subprogram declarations.
      --  Skip generic subprogram declarations as we only care about the
      --  instantiations. Also skip subprograms with zero parameters as we do
      --  not yet support generation for global variables as inputs.

      if Node.Kind in Ada_Basic_Decl
         and then Node.As_Basic_Decl.P_Is_Subprogram
         and then not (Node.Kind in Ada_Enum_Literal_Decl)
         and then Node.As_Basic_Decl.P_Subp_Spec_Or_Null.P_Params'Length > 0
      then
         --  Skip generic subp decls (these will be processed if they are
         --  instantiated).

         if Node.As_Basic_Decl.P_Canonical_Part.Kind in Ada_Generic_Decl
         then
            return Over;
         end if;

         if not Include_Subp
           (Test.Common.TGen_Libgen_Ctx, Node.As_Basic_Decl, Diags)
         then

            Report_Err
              ("Error while processing " & Node.Image & ":" & ASCII.LF
               & To_String (Diags));
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
      use Common_String_Seq_Switches;
      Directory_Separator : Character renames GNAT.OS_Lib.Directory_Separator;
      type Arg_List_Acc is access all Argument_List;
      Build_Args    : aliased Argument_List;
      Run_Args      : aliased Argument_List;
      Harness_Dir   : constant String :=
        Tool_Temp_Dir.all & Directory_Separator & "tgen_Harness";
      Ext_Vars : constant String_Ref_Array := Arg (Cmd, External_Variable);
      Ret_Status    : Integer;
      Ext_Acc       : GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Get_Executable_Suffix;
      Ext           : constant String := Ext_Acc.all;

      procedure PP_Cmd (Cmd : Arg_List_Acc);

      ------------
      -- PP_Cmd --
      ------------

      procedure PP_Cmd (Cmd : Arg_List_Acc) is
         Result : Unbounded_String;
      begin
         for Arg of Cmd.all loop
            Result := Result & Arg & " ";
         end loop;
         Report_Err ("Running " & To_String (Result));
      end PP_Cmd;

   begin
      GNAT.OS_Lib.Free (Ext_Acc);

      --  Generate the harness

      TGen.Libgen.Generate_Harness
           (Test.Common.TGen_Libgen_Ctx,
            Harness_Dir,
            Test.Common.JSON_Test_Dir.all,
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
      for Var of Ext_Vars loop
         if Var not in null then
            Build_Args.Append ("-X" & Var.all);
         end if;
      end loop;

      --  Suppress all warning/info messages and style checks

      Build_Args.Append ("-cargs:ada");
      Build_Args.Append ("-gnatws");
      Build_Args.Append ("-gnatyN");

      if Debug_Flag_1 then
         PP_Cmd (Build_Args'Access);
      end if;
      Ret_Status := Run (Build_Args);
      if Ret_Status /= 0 then
         Report_Err ("Build of generation harness exited with status"
                     & Ret_Status'Image);
         Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Run_Args.Append
        (Harness_Dir & Directory_Separator & "obj" & Directory_Separator
         & "generation_main" & Ext);

      if Debug_Flag_1 then
         PP_Cmd (Run_Args'Access);
      end if;
      Ret_Status := Run (Run_Args);
      if Ret_Status /= 0 then
         Report_Err ("Run of generation harness exited with status"
                     & Ret_Status'Image);
         Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end Generate_Build_And_Run;

end Test.Generation;
