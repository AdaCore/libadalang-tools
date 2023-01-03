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
with Ada.Strings.Fixed;

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

   function Package_Name (Self : Typ) return Ada_Qualified_Name is
      Pack_Name : Ada_Qualified_Name := Self.Name.Copy;
   begin
      Pack_Name.Delete_Last;
      return Pack_Name;
   end Package_Name;

   function Package_Name (Self : Typ) return String is
    (To_Ada (Self.Package_Name));

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

   function Compilation_Unit_Name (Self : Typ) return String is
    (To_Ada (Self.Compilation_Unit_Name));

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
      Context : in out Generation_Context)
      return TGen.Strategies.Static_Strategy_Type'Class
   is
   begin
      if Self.Get.Supports_Static_Gen then
         return Self.Get.Generate_Static (Context);
      else
         return raise Program_Error with
           "Type " & To_Ada (Self.Get.Name)
           & " does not support static generation";
      end if;
   end Try_Generate_Static;

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
