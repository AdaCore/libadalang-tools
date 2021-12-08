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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with TGen.Types.Translation; use TGen.Types.Translation;

with TGen.Strings; use TGen.Strings;

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

      function "+" (T : Text_Type) return Unbounded_Text_Type
        renames To_Unbounded_Text;

      Result : Subprogram_Data (Kind);
   begin
      case Kind is
         when Ada_Subp_Kind_Function =>
            return
              (Kind => Ada_Subp_Kind_Function,
               Name => +Name,
               Fully_Qualified_Name => +Fully_Qualified_Name,
               Parent_Package => +Parent_Package,
               Parameters_Data => Params_Data,
               Return_Type_Fully_Qualified_Name =>
                  +Spec.P_Return_Type.P_Fully_Qualified_Name,
               Return_Type_Parent_Package =>
                  +Spec.P_Top_Level_Decl (Spec.P_Return_Type.Unit).
                     P_Fully_Qualified_Name);

         when Ada_Subp_Kind_Procedure =>
            return
              (Kind => Ada_Subp_Kind_Procedure,
               Name => +Name,
               Fully_Qualified_Name => +Fully_Qualified_Name,
               Parent_Package => +Parent_Package,
               Parameters_Data => Params_Data);
      end case;
      return Result;
   end Extract_Subprogram_Data;

   function Extract_Package_Data
     (Pkg_Decl : Package_Decl)
      return Package_Data
   is
      Subpackages : Package_Data_Vectors.Vector;
      Subprograms : Subprograms_Data_Vectors.Vector;

      use type Package_Data_Vectors.Vector;

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

            Type_Name                 : constant Unbounded_Text_Type :=
              To_Unbounded_Text
                (Parameters_Type.F_Name.Text);
            Type_Fully_Qualified_Name : constant Unbounded_Text_Type :=
              To_Unbounded_Text
                (Parameters_Type.P_Basic_Decl.P_Fully_Qualified_Name);
            Type_Parent_Package       : constant Unbounded_Text_Type :=
              To_Unbounded_Text
                (Parameters_Type.P_Basic_Decl.P_Top_Level_Decl
                   (Parameters_Type.Unit).P_Defining_Name.F_Name.Text);
            Type_Kind                 : constant Ada_Node_Kind_Type :=
              Parameters_Type.Kind;

            Translation_Res : Translation_Result :=
              Translate (Subp_Param_Spec.F_Type_Expr.P_Designated_Type_Decl);
            My_Typ : SP.Ref;

         begin

            if Translation_Res.Success then
               My_Typ := Translation_Res.Res;
            else
               raise Program_Error with "Translation Error";
            end if;

            for Parameter of Subp_Param_Spec.F_Ids loop
               declare
                  Name : constant Unbounded_Text_Type :=
                    To_Unbounded_Text (Parameter.F_Name.Text);

               begin

                  Params_Data.Append
                    (Parameter_Data'
                       (Name                      => Name,
                        Index                     => Index,
                        Type_Name                 => Type_Name,
                        Type_Fully_Qualified_Name => Type_Fully_Qualified_Name,
                        Type_Parent_Package       => Type_Parent_Package,
                        Type_Repr => My_Typ));
               end;

               Index := Index + 1;
            end loop;
         end;
      end loop;

      return Params_Data;
   end Extract_Parameters_Data;

   --  This is duplicated code from lal_tools. For now we want to avoid
   --  bringing a dependency to lal_tools.

   -----------------------
   -- Unit_To_File_Name --
   -----------------------

   function Unit_To_File_Name (Old : String) return String is
      T : String_Access;
   begin
      T := new String'(Old);
      for J in T.all'First .. T.all'Last loop
         if T.all (J) = '.' then
            if J = T.all'First + 1 and then
              T.all (J - 1) in 'a' | 's' | 'i' | 'g' | 'A' | 'S' | 'I' | 'G'
            then
               T.all (J) := '~';
            else
               T.all (J) := '-';
            end if;
         end if;
      end loop;

      return To_Lower (T.all);
   end Unit_To_File_Name;

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
         when Ada_Predefined_Bin_Op_Spec
            => return No_Params;
         when Ada_Predefined_Un_Op_Spec
            => return No_Params;
      end case;
   end Get_Subp_Spec_Params;
end TGen.Gen_Strategies_Utils;
