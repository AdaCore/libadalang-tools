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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.VFS;

with TGen.Types.Record_Types; use TGen.Types.Record_Types;

package body TGen.Gen_Strategies_Utils is

   -------------------------
   -- Get_Subprogram_Data --
   -------------------------

   function Extract_Subprogram_Data
     (Subp : Basic_Decl'Class)
      return Subprogram_Data
   is
      Spec : constant Subp_Spec := Subp.P_Subp_Spec_Or_Null.As_Subp_Spec;

      Name                 : constant Text_Type :=
        Subp.P_Defining_Name.F_Name.Text;
      Fully_Qualified_Name : constant Text_Type := Subp.P_Fully_Qualified_Name;
      Parent_Package       : constant Text_Type :=
        Subp.P_Top_Level_Decl (Subp.Unit).P_Defining_Name.F_Name.Text;
      Params_Data          : constant Parameters_Data_Vector :=
        Extract_Parameters_Data (Subp);
      Kind                 : constant Ada_Subp_Kind := Spec.F_Subp_Kind;
      Precondition         : Unbounded_Text_Type;

      Result : Subprogram_Data (Kind);

      Pre_Expr : constant Expr :=
        Subp.P_Get_Aspect_Spec_Expr (+String'("Pre"));
   begin
      begin
         Precondition := +(if Is_Null (Pre_Expr)
                          then To_Text ("True")
                          else Pre_Expr.Text);
      exception
         when Property_Error =>
            --  No precondition => generated values are always valid

            Precondition := +String'("True");
      end;
      case Kind is
         when Ada_Subp_Kind_Function =>
            return
              (Kind                             => Ada_Subp_Kind_Function,
               Name                             => +Name,
               Fully_Qualified_Name             => +Fully_Qualified_Name,
               Parent_Package                   => +Parent_Package,
               Parameters_Data                  => Params_Data,
               Return_Type_Fully_Qualified_Name =>
                  +(if not Is_Null (Spec.P_Return_Type)
                    then Spec.P_Return_Type.P_Fully_Qualified_Name
                    else ""),
               Return_Type_Parent_Package       =>
                  +Spec.P_Top_Level_Decl (Spec.Unit).P_Fully_Qualified_Name,
               Precondition                     => Precondition,
               others                           => <>);

         when Ada_Subp_Kind_Procedure =>
            return
              (Kind                 => Ada_Subp_Kind_Procedure,
               Name                 => +Name,
               Fully_Qualified_Name => +Fully_Qualified_Name,
               Parent_Package       => +Parent_Package,
               Parameters_Data      => Params_Data,
               Precondition         => Precondition,
               others               => <>);
      end case;
      return Result;
   end Extract_Subprogram_Data;

   --------------------------
   -- Extract_Package_Data --
   --------------------------

   function Extract_Package_Data
     (Pkg_Decl : Package_Decl)
      return Package_Data
   is
      Subpackages : Package_Data_Vectors.Vector;
      Subprograms : Subprograms_Data_Vectors.Vector;

      function Visit (Node : Ada_Node'Class) return Visit_Status;

      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Kind (Node) in Ada_Package_Decl then
            Subpackages.Append
              (new Package_Data'
                 (Extract_Package_Data (Node.As_Package_Decl)));
            return Into;
         elsif Kind (Node) in Ada_Subp_Decl then
            Subprograms.Append
              (Extract_Subprogram_Data
                 (Node.As_Basic_Decl));
            return Over;
         end if;
         return Over;
      end Visit;
   begin
      for Decl of Pkg_Decl.F_Public_Part.F_Decls loop
         Decl.Traverse (Visit'Access);
      end loop;
      if not Pkg_Decl.F_Private_Part.Is_Null then
         for Decl of Pkg_Decl.F_Private_Part.F_Decls loop
            Decl.Traverse (Visit'Access);
         end loop;
      end if;
      return Package_Data'(Subpackages, Subprograms, Pkg_Decl);
   end Extract_Package_Data;

   -----------------------------
   -- Extract_Parameters_Data --
   -----------------------------

   function Extract_Parameters_Data
     (Subp : Basic_Decl'Class)
      return Parameters_Data_Vector
   is
      Subp_Params : constant Params :=
        (if Subp.Is_Null then
            raise Program_Error
         else
            Get_Subp_Params (Subp));

      Params_Data : Parameters_Data_Vector;
      Index : Positive := 1;

   begin
      if Subp_Params.Is_Null then
         return Params_Data;
      end if;
      for Subp_Param_Spec of Subp_Params.F_Params loop
         declare
            Parameters_Type : constant Defining_Name :=
              Subp_Param_Spec.F_Type_Expr.
                P_Designated_Type_Decl.P_Defining_Name;

            Param_Mode : constant Parameter_Mode :=
              (case Kind (Subp_Param_Spec.F_Mode) is
                  when Ada_Mode_Default | Ada_Mode_In => In_Mode,
                  when Ada_Mode_In_Out => In_Out_Mode,
                  when Ada_Mode_Out => Out_Mode,
                  when others => Out_Mode);

            Type_Name                 : constant Unbounded_Text_Type :=
              To_Unbounded_Text (
                if Is_Null (Parameters_Type)
                then ""
                else (Parameters_Type.F_Name.Text));
            Type_Fully_Qualified_Name : constant Unbounded_Text_Type :=
              To_Unbounded_Text
                (if Is_Null (Parameters_Type)
                 then ""
                 else Parameters_Type.P_Basic_Decl.P_Fully_Qualified_Name);
            Type_Parent_Package       : constant Unbounded_Text_Type :=
              To_Unbounded_Text
                (if Is_Null (Parameters_Type)
                 then ""
                 else Parameters_Type.P_Basic_Decl.P_Top_Level_Decl
                        (Parameters_Type.Unit).P_Defining_Name.F_Name.Text);
         begin
            for Parameter of Subp_Param_Spec.F_Ids loop
               declare
                  Name : constant Unbounded_Text_Type :=
                    To_Unbounded_Text (Parameter.F_Name.Text);

               begin

                  Params_Data.Append
                    (Parameter_Data'
                       (Name                      => Name,
                        Index                     => Index,
                        Mode                      => Param_Mode,
                        Type_Name                 => Type_Name,
                        Type_Fully_Qualified_Name => Type_Fully_Qualified_Name,
                        Type_Parent_Package       => Type_Parent_Package));
               end;

               Index := Index + 1;
            end loop;
         end;
      end loop;

      return Params_Data;
   end Extract_Parameters_Data;

   --  This is duplicated code from lal_tools. For now we want to avoid
   --  bringing a dependency to lal_tools.

   ----------------------
   -- Unit_To_Filename --
   ----------------------

   function Unit_To_Filename
     (Project   : Project_Type;
      Unit_Name : String;
      Part      : Unit_Parts) return String
   is
      use GNATCOLL.VFS;
   begin
      return +Project.File_From_Unit
        (Unit_Name => Unit_Name,
         Part      => Part,
         Language  => "Ada",
         File_Must_Exist => False);
   end Unit_To_Filename;

   ---------------------
   -- Get_Subp_Params --
   ---------------------

   function Get_Subp_Params
     (Subp : Basic_Decl'Class)
      return Params is
     (Get_Subp_Spec_Params (Get_Subp_Spec (Subp)));

   -------------------
   -- Get_Subp_Spec --
   -------------------

   function Get_Subp_Spec (Subp : Basic_Decl'Class) return Base_Subp_Spec is
     (if Subp.Is_Null then No_Base_Subp_Spec
      else Subp.P_Subp_Spec_Or_Null (True));

   --------------------------
   -- Get_Subp_Spec_Params --
   --------------------------

   function Get_Subp_Spec_Params
     (Subp_Spec : Base_Subp_Spec'Class)
      return Params is
   begin
      if Subp_Spec.Is_Null then
         return No_Params;
      end if;

      case Ada_Base_Subp_Spec (Subp_Spec.Kind) is
         when Ada_Entry_Spec_Range
            => return Subp_Spec.As_Entry_Spec.F_Entry_Params;
         when Ada_Enum_Subp_Spec_Range
            => return No_Params;
         when Ada_Subp_Spec_Range
            => return Subp_Spec.As_Subp_Spec.F_Subp_Params;
         when Ada_Synthetic_Binary_Spec
            => return No_Params;
         when Ada_Synthetic_Unary_Spec
            => return No_Params;
      end case;
   end Get_Subp_Spec_Params;

   -----------------------------
   -- Type_Strat_Package_Name --
   -----------------------------

   function Type_Strat_Package_Name (Package_Name : String) return String is
   begin
      if Package_Name = "Standard" then
         return "Standard_Type_Strategies";
      else
         return Package_Name & ".Type_Strategies";
      end if;
   end Type_Strat_Package_Name;

   -----------
   -- Strip --
   -----------

   function Strip (Package_Name : String) return String is
   begin
      return Package_Name
        (Package_Name'First
         .. (Index (Package_Name, ".", Package_Name'Last, Backward) - 1));
   end Strip;

   -------------------
   -- Get_All_Types --
   -------------------

   function Get_All_Types (Self : Typ'Class) return Typ_Set is
      Result : Typ_Set;
   begin
      case Kind (Self) is
         when Non_Disc_Record_Kind =>
            for Comp of Nondiscriminated_Record_Typ (Self).Component_Types loop
               Result.Insert (Comp);
            end loop;
         when Disc_Record_Kind =>
            declare
               R : constant Discriminated_Record_Typ :=
                 Discriminated_Record_Typ (Self);
            begin

               --  For now, we do not insert anonymous types. Left as a TODO

               for T of R.Component_Types loop
                  if not T.Get.Is_Anonymous then
                     Result.Include (T);
                  end if;
               end loop;

               for T of R.Discriminant_Types loop
                  if not T.Get.Is_Anonymous then
                     Result.Include (T);
                  end if;
               end loop;

               if R.Variant /= null then
                  for V of R.Variant.Variant_Choices loop
                     Result.Union
                       (Get_All_Types
                          (Discriminated_Record_Typ'
                               (Constrained        => False,
                                Name               => R.Name,
                                Static_Gen         => R.Static_Gen,
                                Component_Types    => V.Components,
                                Mutable            => False,
                                Discriminant_Types => Component_Maps.Empty_Map,
                                Variant            => V.Variant)));
                  end loop;
               end if;
            end;

            --  TODO: add arrays

         when others =>
            null;
      end case;

      return Result;
   end Get_All_Types;

   ------------------------------
   -- Gen_Constrained_Function --
   ------------------------------

   function Gen_Constrained_Function (Self : Typ'Class) return Subprogram_Data
   is
      Result : Subprogram_Data (Ada_Subp_Kind_Function);
      F_Prefix : constant String := "Constrained_";
      Ret_Type : constant String := Self.Fully_Qualified_Name;
   begin
      case Self.Kind is
         when Disc_Record_Kind =>
            declare
               Disc_Record : constant Discriminated_Record_Typ :=
                 Discriminated_Record_Typ (Self);
               F_Name : constant Unbounded_Text_Type :=
                 +(F_Prefix & Self.Gen_Random_Function_Name);
               Parent_Package : constant Unbounded_Text_Type :=
                 Generation_Package_For_Type (Self);
               Fully_Qualified_Name : constant Unbounded_Text_Type :=
                 Parent_Package
                 & Unbounded_Wide_Wide_String'(+String'("."))
                 & F_Name;
               Index  : Positive := 1;
            begin
               Result.Fully_Qualified_Name := Fully_Qualified_Name;
               Result.Parent_Package := Parent_Package;
               Result.Name := F_Name;
               Result.Return_Type_Fully_Qualified_Name := +Ret_Type;
               for D in Disc_Record.Discriminant_Types.Iterate loop
                  declare
                     use Component_Maps;

                     D_Name : constant String := +Key (D);
                     D_Type : constant SP.Ref := Element (D);
                     P      : Parameter_Data;
                  begin
                     P.Name := +D_Name;
                     P.Index := Index;
                     P.Type_Name := +D_Type.Get.Type_Name;
                     P.Type_Fully_Qualified_Name :=
                       +D_Type.Get.Fully_Qualified_Name;
                     P.Type_Parent_Package :=
                       +D_Type.Get.Parent_Package_Name;
                     Result.Parameters_Data.Append (P);
                     Index := Index + 1;
                  end;
               end loop;
            end;

         --  TODO arrays

         when others =>
            raise Program_Error with "Type not constrained";
      end case;
      return Result;
   end Gen_Constrained_Function;

end TGen.Gen_Strategies_Utils;
