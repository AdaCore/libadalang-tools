------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;
with Libadalang.Helpers;

with TGen.Libgen; use TGen.Libgen;

procedure TGen_Marshalling is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   Gen_Ctx : Libgen_Context;

   procedure App_Setup
     (Context : Libadalang.Helpers.App_Context;
      Jobs    : Libadalang.Helpers.App_Job_Context_Array);
   --  Create a generation context initialized with the various directories
   --  set from command line options.

   procedure Process_Unit
     (Job_Ctx : Libadalang.Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);
   --  From a unit containing a single package, go through the subprogram
   --  declarations in the package, detect those for which we can generate the
   --  marshalling and unmarshalling functions and store them in Used_Types.
   --  The generation is deferred to the App_Post_Process.

   procedure App_Post_Process
     (Context : Libadalang.Helpers.App_Context;
      Jobs    : Libadalang.Helpers.App_Job_Context_Array);
   --  Generation marshalling / unmarshalling functions for all of the types
   --  in Used_Types. The functions are generated in a child package of the
   --  package where the type is defined, to have visibility over the type
   --  definition and over the constituent types. It will create a pair of
   --  procedure Input and Output for each type occurring as a parameter
   --  type in an analyzed subprogram.

   package App is new Libadalang.Helpers.App
     (Name             => "Marshalling",
      Description      =>
         "Generation of marshalling and unmarshalling functions",
      App_Setup        => App_Setup,
      Process_Unit     => Process_Unit,
      App_Post_Process => App_Post_Process);

   package Output_Dirs is new Parse_Option
     (Parser   => App.Args.Parser,
      Short    => "-o",
      Long     => "--output-dir",
      Help     => "Directory in which the marshalling lib will be generated",
      Arg_Type => Unbounded_String,
      Default_Val => Null_Unbounded_String);

   package Templates_Dirs is new Parse_Option
     (Parser   => App.Args.Parser,
      Long     => "--templates-dir",
      Help     => "Directory in which the TGen templates can be found",
      Arg_Type => Unbounded_String,
      Default_Val => Null_Unbounded_String);

   ---------------
   -- App_Setup --
   ---------------

   procedure App_Setup
     (Context : Libadalang.Helpers.App_Context;
      Jobs    : Libadalang.Helpers.App_Job_Context_Array)
   is
      pragma Unreferenced (Context, Jobs);
      User_Project_Path : constant Unbounded_String :=
        App.Args.Project_File.Get;
      Templates_Dir     : Unbounded_String := Templates_Dirs.Get;
      Output_Dir        : Unbounded_String := Output_Dirs.Get;
   begin
      if User_Project_Path = Null_Unbounded_String then
         Libadalang.Helpers.Abort_App ("Project file required");
      end if;

      if Output_Dir = Null_Unbounded_String then
         Output_Dir := To_Unbounded_String ("tgen_support");
      end if;

      Gen_Ctx := Create
        (Output_Dir         => To_String (Output_Dir),
         User_Project_Path  => To_String (User_Project_Path),
         Root_Templates_Dir => To_String (Templates_Dir));
   end App_Setup;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Job_Ctx : Libadalang.Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      pragma Unreferenced (Job_Ctx);
      Diags : Unbounded_String;

      function Traverse_Helper
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;

      function Traverse_Helper
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
      is
      begin
         --  Collect all types used as parameters in subprogram declarations.
         --  Skip generic subprogram declarations as we only care about the
         --  instantiations.

         if Node.Kind in Ada_Basic_Decl
           and then Node.As_Basic_Decl.P_Is_Subprogram
           and then not (Node.Kind in Ada_Enum_Literal_Decl)
         then
            if Node.As_Basic_Decl.P_Canonical_Part.Kind in Ada_Generic_Decl
            then
               return LALCO.Over;
            end if;

            if not Include_Subp (Gen_Ctx, Node.As_Basic_Decl, Diags) then
               Libadalang.Helpers.Abort_App
                  ("Error during parameter translation:"
                  & To_String (Diags));
            end if;
            return LALCO.Over;
         end if;
         if Node.Kind in Ada_Generic_Package_Instantiation then
            Node.As_Generic_Package_Instantiation.P_Designated_Generic_Decl
            .As_Generic_Package_Decl.F_Package_Decl
            .Traverse (Traverse_Helper'Access);
            return Over;
         end if;
         return LALCO.Into;
      end Traverse_Helper;

   begin
      --  Report parsing errors, if any

      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;

      --  Check that the file contains a compilation unit for a package
      --  declaration.

      elsif Unit.Root.Kind /= LALCO.Ada_Compilation_Unit then
         Put_Line ("Unit is not a compilation unit");
      elsif Unit.Root.As_Compilation_Unit.P_Decl.Kind /= LALCO.Ada_Package_Decl
      then
         Put_Line ("Unit does not contain a package declaration");
      else
         LAL.Traverse (Unit.Root, Traverse_Helper'Access);
      end if;
   end Process_Unit;

   ----------------------
   -- App_Post_Process --
   ----------------------

   procedure App_Post_Process
     (Context : Libadalang.Helpers.App_Context with Unreferenced;
      Jobs    : Libadalang.Helpers.App_Job_Context_Array with Unreferenced)
   is
   begin
      Generate (Gen_Ctx, Marshalling_Part);
   end App_Post_Process;
begin
   App.Run;
end TGen_Marshalling;
