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

with Ada.Containers;               use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings;                  use Ada.Strings;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Text_IO;                  use Ada.Text_IO;

with Langkit_Support.Text;
with Libadalang.Analysis;
with Libadalang.Common;            use Libadalang.Common;
with Libadalang.Helpers;

with Templates_Parser;             use Templates_Parser;

with TGen.Marshalling;             use TGen.Marshalling;
with TGen.Types;                   use TGen.Types;
with TGen.Types.Translation;       use TGen.Types.Translation;

procedure TGen_Marshalling is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   use type LALCO.Ada_Node_Kind_Type;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Hash_Defining_Name
     (Node : LAL.Defining_Name) return Ada.Containers.Hash_Type is
       (Node.As_Ada_Node.Hash);

   package Type_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LAL.Defining_Name,
      Element_Type    => SP.Ref,
      Hash            => Hash_Defining_Name,
      Equivalent_Keys => LAL."=",
      "="             => SP."=");

   Used_Types : Type_Maps.Map;

   function Get_All_Types
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;
   --  If Node is a subprogram declaration, collect the type of its parameters
   --  in Used_Types.

   procedure Process_Unit
     (Job_Ctx : Libadalang.Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);
   --  From a unit containing a single package, go through the subprogram
   --  declarations in the package, detect those for which we can generate the
   --  marshalling and unmarshalling functions, and do the generation.
   --  This creates a child TAGAda_Parsing for the input unit containing a
   --  pair of procedures Input and Output for each type occurring as a
   --  parameter type in a supported subprograms of the unit.

   package App is new Libadalang.Helpers.App
     (Name         => "Marshalling",
      Description  => "Generation of marshalling and unmarshalling functions",
      Process_Unit => Process_Unit);

   -------------------
   -- Get_All_Types --
   -------------------

   function Get_All_Types (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
      Local_Types : Type_Maps.Map;
   begin
      if Node.Kind = LALCO.Ada_Subp_Decl then
         for Param of Node.As_Subp_Decl.F_Subp_Spec.P_Params loop
            declare
               Typ       : constant LAL.Type_Expr := Param.F_Type_Expr;
               Is_Anon   : constant Boolean :=
                 Typ.Kind = LALCO.Ada_Anonymous_Type;
               Typ_Decl  : LAL.Base_Type_Decl;
               Typ_Trans : Translation_Result;
            begin
               if not Is_Anon then
                  Typ_Decl := Typ.P_Designated_Type_Decl;
                  Typ_Trans := Translate (Typ_Decl);
               end if;

               if Is_Anon
                 or else not Typ_Trans.Success
                 or else not Is_Supported_Type (Typ_Trans.Res.Get)
               then
                  if not Typ_Trans.Success then
                     Put_Line ("Failed: " & To_String (Typ_Trans.Diagnostics));
                  end if;
                  declare
                     Type_Name : constant String :=
                       (if Is_Anon
                        then Param.F_Type_Expr.Image
                        else Typ_Decl.P_Defining_Name.F_Name.Image);
                  begin
                     Put_Line ("Type " & Type_Name & " is not supported yet");
                  end;
                  Put_Line ("Subp " &
                              Node.As_Subp_Decl.P_Defining_Name.F_Name.Image &
                              " is ignored");
                  Local_Types.Clear;
                  exit;
               end if;

               Local_Types.Include (Typ_Decl.F_Name, Typ_Trans.Res);
            end;
         end loop;

         for Cu in Local_Types.Iterate loop
            Used_Types.Include (Type_Maps.Key (Cu), Type_Maps.Element (Cu));
         end loop;
         Local_Types.Clear;
      end if;
      return LALCO.Over;
   end Get_All_Types;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Job_Ctx : Libadalang.Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      pragma Unreferenced (Job_Ctx);
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

      --  Collect all types used as parameters in subprogram declarations
      else
         for Decl of Unit.Root.As_Compilation_Unit.P_Decl.As_Package_Decl.
           F_Public_Part.F_Decls
         loop
            Decl.Traverse (Get_All_Types'Access);
         end loop;

         declare
            Pack_Name : constant String := Langkit_Support.Text.Image
              (Unit.Root.As_Compilation_Unit.P_Decl.
                 As_Package_Decl.F_Package_Name.Text);
            F_Spec    : File_Type;
            F_Body    : File_Type;
            File_Name : constant String :=
              (for C of Pack_Name =>
                 (case C is
                     when '.'           => '-',
                     when 'A' .. 'Z'    =>
                       Character'Val
                    (Character'Pos (C) - Character'Pos ('A')
                     + Character'Pos ('a')),
                     when others        => C))
               & "-tagada_parsing";
         begin
            Create (F_Spec, Out_File, File_Name & ".ads");
            Put_Line (F_Spec, "with Ada.Streams; use Ada.Streams;");
            Put_Line (F_Spec, "package " & Pack_Name & ".TAGAda_Parsing is");
            New_Line (F_Spec);

            Create (F_Body, Out_File, File_Name & ".adb");
            Put (F_Body, "with TAGAda_Marshalling_Lib; ");
            Put_Line (F_Body, "use TAGAda_Marshalling_Lib;");
            Put (F_Body, "with Interfaces; ");
            Put_Line (F_Body, "use Interfaces;");
            Put_Line
              (F_Body, "package body " & Pack_Name & ".TAGAda_Parsing is");
            New_Line (F_Body);

            --  Disable predicate checks in the marshalling and unmarshalling
            --  functions.

            Put_Line
              (F_Body, "   pragma Assertion_Policy (Predicate => Ignore);");
            New_Line (F_Body);

            for Cu in Used_Types.Iterate loop
               Generate_Marshalling_Functions_For_Typ
                 (F_Spec, F_Body, Type_Maps.Element (Cu).Get);
            end loop;

            Put_Line (F_Body, "end " & Pack_Name & ".TAGAda_Parsing;");
            Close (F_Body);
            Put_Line (F_Spec, "end " & Pack_Name & ".TAGAda_Parsing;");
            Close (F_Spec);
         end;
      end if;
   end Process_Unit;
begin
   App.Run;
end TGen_Marshalling;
