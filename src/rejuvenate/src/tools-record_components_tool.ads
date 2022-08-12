------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
--  Rejuvenate record components tool

with Ada.Strings.Unbounded;
with Libadalang.Analysis;
with Laltools.Refactor;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with VSS.Text_Streams.Memory_UTF8_Output;

package Tools.Record_Components_Tool is
   package LAL renames Libadalang.Analysis;
   package ReFac renames Laltools.Refactor;
   package Slocs renames Langkit_Support.Slocs;

   function "<" (L, R : LAL.Record_Def) return Boolean;

   function "<" (L, R : LAL.Defining_Name) return Boolean is
     (Slocs.Start_Sloc (LAL.Sloc_Range (L)) <
                      Slocs.Start_Sloc (LAL.Sloc_Range (R)));

   function Defining_Name_Hash (Element : LAL.Defining_Name)
                                return Ada.Containers.Hash_Type is
   (LAL.Hash (Element.As_Ada_Node));

   package Defining_Name_Ordered_Sets is new
   Ada.Containers.Indefinite_Ordered_Sets
    (Element_Type        => LAL.Defining_Name,
     "<"                 => "<",
     "="                 => LAL."=");

   package Record_Def_To_Text_Edit_Map_Ordered_Maps is new
   Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type            => LAL.Record_Def,
        Element_Type        => ReFac.Text_Edit_Map,
        "<"                 => "<",
        "="                 => ReFac.Text_Edit_Ordered_Maps."="
       );

   subtype Record_Text_Edits is Record_Def_To_Text_Edit_Map_Ordered_Maps.Map;

   package Defining_Name_To_Defining_Name_Ordered_Set_Ordered_Maps is new
   Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type            => LAL.Defining_Name,
        Element_Type        => Defining_Name_Ordered_Sets.Set,
        Hash                => Defining_Name_Hash,
        Equivalent_Keys     => LAL."=",
        "="                 => Defining_Name_Ordered_Sets."="
       );

   subtype Deletable_Record_Componenets is
     Defining_Name_To_Defining_Name_Ordered_Set_Ordered_Maps.Map;

   Parser : Argument_Parser :=
     Create_Argument_Parser (Help => "Record Components");

   package Project is new Parse_Option
     (Parser      => Parser,
      Short       => "-P",
      Long        => "--project",
      Help        => "Project",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   type Delete_Infos is record
      Deletable_Names : Deletable_Record_Componenets;
      Texts_Edit : Record_Text_Edits;
   end record;

   --  this delete_info give the two hash map with record node as keys
   --  Record_Deletable_Names gives the components names set in the record
   --  Texts_Edit gives the text_edit related to the record

   function Get_Record_Name (Node : LAL.Record_Def'Class)
                            return LAL.Defining_Name;
   --  Return the defining name of the given record node.

   function Find_Unused_Components (Unit_Array : LAL.Analysis_Unit_Array)
                                    return Delete_Infos;
   --  this procedure finds all the unused components in records in
   --  all project files, gives the delete infomation as output.

   procedure Run (Unit_Array : LAL.Analysis_Unit_Array;
                  Stream     : in out
                               VSS.Text_Streams.Output_Text_Stream'Class);
   --  Record_Components_Tool main procedure

end Tools.Record_Components_Tool;
