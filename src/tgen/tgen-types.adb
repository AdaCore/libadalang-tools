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

with Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Libadalang.Common; use Libadalang.Common;

with TGen.Context; use TGen.Context;
with TGen.Strategies; use TGen.Strategies;

package body TGen.Types is

   -----------
   -- Image --
   -----------

   function Image (Self : Typ) return String is
   begin
      return (if Self.Name = Ada_Identifier_Vectors.Empty_Vector
              then "Anonymous"
              else Self.Type_Name);
   end Image;

   ------------------
   -- Is_Anonymous --
   ------------------

   function Is_Anonymous (Self : Typ) return Boolean is
   begin
      return Self.Name = Ada_Identifier_Vectors.Empty_Vector;
   end Is_Anonymous;

   -----------------------------------
   -- Generate_Constrained_Strategy --
   -----------------------------------

   function Generate_Constrained_Strategy
     (Self     : Typ;
      Var_Name : Unbounded_Text_Type)
      return Static_Strategy_Type'Class
     is (Unimplemented_Static_Strategy_Type'(null record));

   ----------
   -- Kind --
   ----------

   function Kind (Self : Typ) return Typ_Kind is (Invalid_Kind);

   -----------
   -- Image --
   -----------

   function Image (Self : Access_Typ) return String is
     (Typ (Self).Image & ": access type");

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (Self : Typ) return String is
      Parent_Package : Ada_Qualified_Name := Self.Name.Copy;
   begin
      Parent_Package.Delete_Last;
      return To_Ada (Parent_Package);
   end Package_Name;

   -----------------------
   -- Dot_To_Underscore --
   -----------------------

   function Dot_To_Underscore (C : Character) return Character is
     ((if C = '.' then '_' else C));

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self    : Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
   begin
      return raise Program_Error with "Static strategy not implemented";
   end Generate_Static;

   -------------------------
   -- Try_Generate_Static --
   -------------------------

   function Try_Generate_Static
     (Self    : SP.Ref;
      Context : in out TGen.Context.Generation_Context)
      return TGen.Strategies.Static_Strategy_Type'Class
   is
   begin
      if Self.Get.Supports_Static_Gen then
         return Self.Get.Generate_Static (Context);
      elsif Context.Unsupported_Type_Behavior = Commented_Out then
         return TGen.Strategies.Commented_Out_Strategy_Type'(null record);
      else
         return raise Program_Error with
           "Type " & To_Ada (Self.Get.Name)
           & " does not support static generation";
      end if;
   end Try_Generate_Static;

   ------------------------------
   -- Generate_Random_Strategy --
   ------------------------------

   function Generate_Random_Strategy
     (Self    : Typ;
      Context : in out Generation_Context) return Strategy_Type'Class
   is
      Res : Unimplemented_Strategy_Type;
   begin
      raise Program_Error
        with "Generation of dynamic strategy not implemented";
      return Res;
   end Generate_Random_Strategy;

   ------------------------------------------
   -- Generate_Constrained_Random_Strategy --
   ------------------------------------------

   function Generate_Constrained_Random_Strategy
     (Self    : Typ;
      Context : Generation_Context) return Strategy_Type'Class
   is
      Res : Unimplemented_Strategy_Type;
   begin
      raise Program_Error
        with "Generation of dynamic strategy not implemented";
      return Res;
   end Generate_Constrained_Random_Strategy;

   ---------------------------------
   -- Generation_Package_For_Type --
   ---------------------------------

   function Generation_Package_For_Type
     (Self : Typ'Class) return Unbounded_Text_Type
   is
      Pkg_Name : constant Unbounded_Text_Type :=
        +(String'("Type_Strategies"));
   begin
      return Unbounded_Wide_Wide_String'(+Self.Parent_Package_Name)
        & Unbounded_Wide_Wide_String'(+String'("."))
        & Pkg_Name;
   end Generation_Package_For_Type;

   ------------------------------
   -- Random_Strategy_Function --
   ------------------------------

   function Random_Strategy_Function
     (Self : Typ) return Subprogram_Data
   is
      Result : Subprogram_Data (Kind => Ada_Subp_Kind_Function);
   begin
      Result.Name := +Self.Gen_Random_Function_Name;
      Result.Fully_Qualified_Name :=
        Generation_Package_For_Type (Self)
        & Result.Name;
      Result.Parent_Package :=
        Generation_Package_For_Type (Self);
      Result.Precondition := +(String'(""));

      Result.Return_Type_Fully_Qualified_Name := +Self.Fully_Qualified_Name;
      Result.Return_Type_Parent_Package := +Self.Parent_Package_Name;
      return Result;
   end Random_Strategy_Function;

   ----------
   -- Slug --
   ----------

   function Slug (Self : Typ) return String is
   begin
      return Ada.Strings.Fixed.Translate
        (Source  => Self.Fully_Qualified_Name,
         Mapping => Dot_To_Underscore'Access);
   end Slug;

   ------------------
   -- Free_Content --
   ------------------

   procedure Free_Content_Wide (Self : in out Typ'Class) is
   begin
      Self.Free_Content;
   end Free_Content_Wide;

end TGen.Types;
