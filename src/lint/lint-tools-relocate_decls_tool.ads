------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2022-2023, AdaCore                    --
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
--
--  Lint make_type_private tool

with Ada.Strings.Unbounded;
with Libadalang.Analysis;
with Laltools.Refactor;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with VSS.Text_Streams;

package Lint.Tools.Relocate_Decls_Tool is
   package LAL renames Libadalang.Analysis;
   package ReFac renames Laltools.Refactor;
   Parser : Argument_Parser :=
     Create_Argument_Parser (Help => "Suppress Params");

   function "<" (L, R : LAL.Ada_Node) return Boolean;

   function "<" (L, R : LAL.Defining_Name) return Boolean
   is (L.As_Ada_Node < R.As_Ada_Node);

   function "<" (L, R : LAL.Object_Decl) return Boolean
   is (L.As_Ada_Node < R.As_Ada_Node);

   package Project is new Parse_Option
     (Parser      => Parser,
      Short       => "-P",
      Long        => "--project",
      Help        => "Project",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   package Source is new Parse_Option
     (Parser      => Parser,
      Short       => "-S",
      Long        => "--source",
      Help        => "Project",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   package Decl_Name_To_Edit_Map is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type            => LAL.Defining_Name,
        Element_Type        => ReFac.Text_Edit_Map,
        "<"                 => "<",
        "="                 => ReFac.Text_Edit_Ordered_Maps."=");

   package Obj_Decl_To_Edit_Map is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type            => LAL.Object_Decl,
        Element_Type        => ReFac.Text_Edit_Map,
        "<"                 => "<",
        "="                 => ReFac.Text_Edit_Ordered_Maps."=");

   package Defining_Name_Ordered_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets
       (Element_Type => LAL.Defining_Name,
        "<"          => "<",
        "="          => LAL."=");

   package Obj_Decl_To_Defining_Name is
      new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type            => LAL.Object_Decl,
        Element_Type        => Defining_Name_Ordered_Sets.Set,
        "<"                 => "<",
        "="                 => Defining_Name_Ordered_Sets."=");

   type Modify_Info is record
      Object_To_Names : Obj_Decl_To_Defining_Name.Map;
      Edit_Of_Obj_Decl : Obj_Decl_To_Edit_Map.Map;
      Edit_Of_Other_Decl : Decl_Name_To_Edit_Map.Map;
   end record;

   function Find_Decl_Private (Unit_Array : LAL.Analysis_Unit_Array)
                               return Modify_Info;

   procedure Run (Unit_Array : LAL.Analysis_Unit_Array;
                  Stream     : in out
                               VSS.Text_Streams.Output_Text_Stream'Class);

end Lint.Tools.Relocate_Decls_Tool;
