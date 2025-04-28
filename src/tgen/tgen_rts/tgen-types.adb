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

with Ada.Containers;
with Ada.Strings.Equal_Case_Insensitive;

with TGen.Strategies; use TGen.Strategies;

package body TGen.Types is

   --------------------------
   -- Generic_Package_Name --
   --------------------------

   function Generic_Package_Instance_Name
     (Pack_Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is
      Prefix             : constant Ada_Identifier :=
        Ada_Identifier (+"TGen_Generic_Instantiation_");
      First_Element_Name : constant Ada_Identifier :=
        Prefix & Pack_Name.First_Element;
      Result             : Ada_Qualified_Name;

      use Ada_Identifier_Vectors;
   begin
      Result.Append (First_Element_Name);
      Result.Append (Ada_Identifier (+"Instance"));
      for I in
        Extended_Index'Succ (Pack_Name.First_Index) .. Pack_Name.Last_Index
      loop
         Result.Append (Pack_Name.Element (I));
      end loop;

      return Result;
   end Generic_Package_Instance_Name;

   -----------
   -- Image --
   -----------

   function Image (Self : Typ) return String is
   begin
      return
        (if Self.Name = Ada_Identifier_Vectors.Empty_Vector
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

   ----------
   -- Kind --
   ----------

   function Kind (Self : Typ) return Typ_Kind
   is (Invalid_Kind);

   -----------
   -- Image --
   -----------

   function Image (Self : Access_Typ) return String
   is (Typ (Self).Image & ": access type");

   ---------
   -- FQN --
   ---------

   function FQN
     (Self              : Typ;
      No_Std            : Boolean := False;
      Top_Level_Generic : Boolean := False) return String
   is
      Name : constant Ada_Qualified_Name :=
        (if Top_Level_Generic
         then Generic_Package_Instance_Name (Self.Name)
         else Self.Name);

      function Append_Class_Wide_If_Needed (Type_Name : String) return String
      is ((if Self.Is_Class_Wide then Type_Name & "'Class" else Type_Name));
   begin
      if not No_Std
        or else not Ada.Strings.Equal_Case_Insensitive
                      (+Unbounded_String (Name.First_Element), "standard")
      then
         return Append_Class_Wide_If_Needed (To_Ada (Name));
      end if;
      declare
         Stripped : Ada_Qualified_Name := Name;
      begin
         Stripped.Delete_First;
         return Append_Class_Wide_If_Needed (To_Ada (Stripped));
      end;
   end FQN;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (Self : Typ) return Ada_Qualified_Name is
      Pack_Name : Ada_Qualified_Name := Self.Name.Copy;
   begin
      Pack_Name.Delete_Last;
      return Pack_Name;
   end Package_Name;

   ---------------------------
   -- Compilation_Unit_Name --
   ---------------------------

   function Compilation_Unit_Name (Self : Typ) return Ada_Qualified_Name is
      use Ada_Identifier_Vectors;
      use Ada.Containers;
      Pack_Name : Ada_Qualified_Name := Self.Name.Copy;
   begin
      Pack_Name.Delete
        (Index => Self.Last_Comp_Unit_Idx + 1,
         Count => Count_Type (Self.Name.Last_Index - Self.Last_Comp_Unit_Idx));
      return Pack_Name;
   end Compilation_Unit_Name;

   function Compilation_Unit_Name (Self : Typ) return String
   is (To_Ada (Self.Compilation_Unit_Name));

   ------------
   -- Encode --
   ------------

   function Encode (Self : Typ; Val : JSON_Value) return JSON_Value
   is (Val);

   ----------------------
   -- Default_Strategy --
   ----------------------

   function Default_Strategy (Self : Typ) return Strategy_Type'Class is
   begin
      return raise Program_Error with "Static strategy not implemented";
   end Default_Strategy;

   ---------------------------
   -- Default_Enum_Strategy --
   ---------------------------

   function Default_Enum_Strategy
     (Self : Typ) return TGen.Strategies.Enum_Strategy_Type'Class is
   begin
      return raise Program_Error with "Enumerative strategy not implemented";
   end Default_Enum_Strategy;

   -------------------------
   -- Try_Generate_Static --
   -------------------------

   function Try_Generate_Static
     (Self : Typ_Access) return TGen.Strategies.Strategy_Type'Class is
   begin
      if Self.all.Supports_Static_Gen then
         return Self.all.Default_Enum_Strategy;
      else
         return
           raise Program_Error
             with
               "Type "
               & To_Ada (Self.all.Name)
               & " does not support static generation";
      end if;
   end Try_Generate_Static;

   ----------
   -- Slug --
   ----------

   function Slug
     (Self : Typ; Top_Level_Generic : Boolean := False) return String
   is
      Name : constant Ada_Qualified_Name :=
        (if Top_Level_Generic
         then Generic_Package_Instance_Name (Self.Name)
         else Self.Name);
   begin
      return To_Symbol (Name, '_');
   end Slug;

   ------------------
   -- Free_Content --
   ------------------

   procedure Free_Content_Wide (Self : in out Typ'Class) is
   begin
      Self.Free_Content;
   end Free_Content_Wide;

   ---------------------
   -- Get_Diagnostics --
   ---------------------

   function Get_Diagnostics
     (Self : Unsupported_Typ; Prefix : String := "") return String_Vector
   is
      Diag : Unbounded_String;
   begin
      if Prefix'Length /= 0 then
         Diag := +Prefix & ": ";
      end if;
      Diag :=
        Diag
        & To_Ada (Self.Name)
        & " is not supported ("
        & (+Self.Reason)
        & ")";
      return String_Vectors.To_Vector (Diag, 1);
   end Get_Diagnostics;

end TGen.Types;
