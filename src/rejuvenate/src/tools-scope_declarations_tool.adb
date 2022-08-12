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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Langkit_Support.Text;
with Libadalang.Common;
with Ada.Containers; use Ada.Containers;
with GNATCOLL.VFS;
with Ada.Assertions;
with VSS.Stream_Element_Vectors.Conversions;
with Output;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Hashed_Maps;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Edit_File;

package body Tools.Scope_Declarations_Tool is
   package LALCO renames Libadalang.Common;
   package Text renames Langkit_Support.Text;

   function Hash (N : LAL.Defining_Name) return Ada.Containers.Hash_Type is
     (N.As_Ada_Node.Hash);

   function Hash (N : LAL.Object_Decl) return Ada.Containers.Hash_Type is
     (N.As_Ada_Node.Hash);

   function Hash (N : LAL.Declarative_Part) return Ada.Containers.Hash_Type is
     (N.As_Ada_Node.Hash);

   package Defining_Name_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => LAL.Defining_Name,
        Hash                => Hash,
        Equivalent_Elements => LAL."=",
        "="                 => LAL."=");

   package Object_Decl_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => LAL.Object_Decl,
        Hash                => Hash,
        Equivalent_Elements => LAL."=",
        "="                 => LAL."=");

   package Defining_Name_To_Decl_Part is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => LAL.Defining_Name,
        Element_Type    => LAL.Declarative_Part,
        Hash            => Hash,
        Equivalent_Keys => LAL."=",
        "="             => LAL."=");

   package Defining_Name_To_Int is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => LAL.Defining_Name,
        Element_Type    => Integer,
        Hash            => Hash,
        Equivalent_Keys => LAL."=",
        "="             => "=");

   package Decl_Part_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => LAL.Declarative_Part,
        Hash                => Hash,
        Equivalent_Elements => LAL."=",
        "="                 => LAL."=");

   package Integer_Sets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Integer);

   type Positions is (First, Middle, Last);

   Edit_Info : Modify_Info;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : LAL.Ada_Node) return Boolean is
   begin
      if L.Unit.Get_Filename < R.Unit.Get_Filename then
         return True;
      else
         if L.Unit.Get_Filename > R.Unit.Get_Filename then
            return False;
         else
            return (Start_Sloc (LAL.Sloc_Range (L)) <
                      Start_Sloc (LAL.Sloc_Range (R)));
         end if;
      end if;
   end "<";

   ------------------------
   -- Scope_Declarations --
   ------------------------

   function Scope_Declarations (Unit_Array : LAL.Analysis_Unit_Array)
                                return Modify_Info is
      Edit_Info : Obj_Decl_To_Edit_Map.Map;
      --  Store the Edit_Map to the relevant Object Declaration node

      Obj_Decl_To_Names : Obj_Decl_To_Defining_Name.Map;
      --  Store the variables that can be scoped more narrowly

      Decl_To_Check : Defining_Name_Sets.Set;
      --  The set stores the defining_names that still haven't been refered

      Last_Decl_For_Name : Defining_Name_To_Decl_Part.Map;
      --  This map stores the defining_names that can be scoped more narrowly
      --  and the destination declarative part that the defining name can be
      --  moved to.

      Name_Ref_Count : Defining_Name_To_Int.Map;
      --  Count the number of reference of the defining name in the recent
      --  subprogram.
      Obj_Decl_To_Modify : Object_Decl_Sets.Set;
      --  Stores the Object declarations to be modified

      Decl_Parts : Decl_Part_Sets.Set;
      --  Stores the declarative part nodes.

      Removable_Decl_Part : Decl_Part_To_Edit_Map.Map;
      --  Stores the Declarative Part that can be removed.

      procedure Process_Decl (Decl_Part : LAL.Declarative_Part);
      --  Process the declarative part
      --  1. first check all the defining name in the decl_to_check that how
      --     many time it is refered in the recent subfunction, if the number
      --     is equal to the previous count, which means this defining name is
      --     not refered in other part, i.e. could be scoped to this decl_part
      --     add or update this defining name and the relevent decl_part to the
      --     last_decl_for_name.
      --  2. If the number is positive but not equal to the previous, which
      --     means the name are both refer in this subpart and other parts,
      --     i.e. this name can't be scoped more narrowly, so we delete it from
      --     the set.
      --  3. Add all the variables delared in this declarative_part and their
      --     reference number.
      function Find_Decl_Part (Node : LAL.Ada_Node'Class)
                               return LALCO.Visit_Status;
      --  go through the AST to find all the decl part.
      procedure Generate_Edit (Decl_Part : LAL.Declarative_Part);
      --  generate the text edit information

      ------------------
      -- Process_Decl --
      ------------------

      procedure Process_Decl (Decl_Part : LAL.Declarative_Part) is
         Father : LAL.Ada_Node := Decl_Part.Parent;
      begin
         declare
            Name_Checked_Done : Defining_Name_Sets.Set;
         begin
            for Name of Decl_To_Check loop
               --  We previously concluded that the amount of refs
               --  in this declarative part is smaller than
               --  the total amount of refs of this name.
               --  So, if there is at least one ref in this declarative
               --  part, then for sure we cannot move.
               if Name.P_Find_Refs (Father)'Length = Name_Ref_Count (Name)
               then
                  Last_Decl_For_Name.Include (Name, Decl_Part);
                  Obj_Decl_To_Modify.Include
                    (Name.P_Basic_Decl.As_Object_Decl);
               else
                  if Name.P_Find_Refs (Father)'Length > 0 then
                     Name_Checked_Done.Include (Name);
                  end if;
               end if;
            end loop;
            for Name of Name_Checked_Done loop
               Decl_To_Check.Exclude (Name);
            end loop;
         end;
         for Decl of Decl_Part.F_Decls loop
            if Decl.Kind in LALCO.Ada_Object_Decl_Range then
               for Name of Decl.As_Object_Decl.F_Ids loop
                  --  We initialize the amount of refs for each defining name
                  --  since the P_Find_Refs will return also the decl itself
                  --  we put -1 in the end.
                  Name_Ref_Count.Include
                    (Name.As_Defining_Name,
                     Name.P_Find_Refs (Father)'Length - 1);
                  Decl_To_Check.Include (Name.As_Defining_Name);
               end loop;
            end if;
         end loop;
      end Process_Decl;

      --------------------
      -- Find_Decl_Part --
      --------------------

      function Find_Decl_Part (Node : LAL.Ada_Node'Class)
                               return LALCO.Visit_Status is
      begin
         if Node.Kind in LALCO.Ada_Declarative_Part_Range then
            Decl_Parts.Include (Node.As_Declarative_Part);
            Process_Decl (Node.As_Declarative_Part);
         end if;
         return LALCO.Into;
      end Find_Decl_Part;

      -------------------
      -- Generate_Edit --
      -------------------

      procedure Generate_Edit (Decl_Part : LAL.Declarative_Part) is
         Is_Empty : Boolean := True;
         Has_Decl : Boolean := False;
         Edit_Map_Of_Decl_Part : ReFac.Text_Edit_Map;
         procedure Generate_Edit (Obj : LAL.Object_Decl);

         procedure Generate_Edit (Obj : LAL.Object_Decl) is
            Text_To_Add, Tmp_Text    : Unbounded_String;
            Edit_Texts               : ReFac.Text_Edit_Map;
            Text_Edit                : ReFac.Text_Edit;
            Name_Count, Modify_Count : Integer := 0;
            Names                    : Defining_Name_Ordered_Sets.Set;
            function Check_Position (List_Node : LAL.Ada_Node'Class;
                                     Name_Node : LAL.Defining_Name'Class)
                                  return Positions;
            --  Check the position of the name in the name list

            function Delete_Position (Name     : LAL.Ada_Node'Class;
                                      Position : Positions)
                                   return Source_Location_Range;
            --  Get the delete sloc by the location

            procedure Delete_Names_in_List;
            --  Generate the edit text of delete name in the list

            --------------------
            -- Check_Position --
            --------------------

            function Check_Position (List_Node : LAL.Ada_Node'Class;
                                     Name_Node : LAL.Defining_Name'Class)
                                  return Positions
            is
            begin
               if Name_Node.Child_Index = 0 then
                  return First;
               else
                  if Name_Node.Child_Index =
                    LAL.Children (List_Node)'Length - 1
                  then
                     return Last;
                  else
                     return Middle;
                  end if;
               end if;
            end Check_Position;

            ---------------------
            -- Delete_Position --
            ---------------------

            function Delete_Position (Name     : LAL.Ada_Node'Class;
                                      Position : Positions)
                                   return Source_Location_Range
            is
               Deletable_Range, Next_Range,
               Previous_Range : Source_Location_Range;
            begin
               Deletable_Range := LAL.Sloc_Range (Name);
               if Position /= Last then
                  Next_Range :=
                    LAL.Sloc_Range (LAL.Next_Sibling (Name));
                  Deletable_Range.End_Column := Next_Range.Start_Column;
                  Deletable_Range.End_Line := Next_Range.Start_Line;
               end if;
               if Position /= First then
                  Previous_Range :=
                    LAL.Sloc_Range (LAL.Previous_Sibling (Name));
                  Deletable_Range.Start_Column := Previous_Range.End_Column;
                  Deletable_Range.Start_Line := Previous_Range.End_Line;
               end if;
               return Deletable_Range;
            end Delete_Position;

            --------------------------
            -- Delete_Names_in_List --
            --------------------------

            procedure Delete_Names_in_List
            is
               Name_List                  : LAL.Defining_Name_List :=
                                            Obj.F_Ids;
               Name_Position              : Positions;
               Last_Position              : Positions := First;
               Last_Sloc, Last_Sloc_Start : Source_Location :=
                                            No_Source_Location;
               Delete_Last                : Boolean := False;
               All_used_component         : Boolean := True;
               Deletable_Range            : Source_Location_Range;
            begin
               for Name_Node of Name_List loop
                  if Names.Contains (Name_Node.As_Defining_Name) then
                     Name_Position := Check_Position (Name_List, Name_Node);
                     if Name_Position = Last then
                        Delete_Last := True;
                     end if;
                     Deletable_Range := Delete_Position
                       (Name_Node.As_Ada_Node, Name_Position);
                     if Start_Sloc (Deletable_Range) < Last_Sloc then
                        Last_Sloc := End_Sloc (Deletable_Range);
                     else
                        if Last_Sloc /= No_Source_Location then
                           if Last_Position = First then
                              Text_Edit.Location := Make_Range
                                (Last_Sloc_Start, Last_Sloc);
                              Text_Edit.Text := Null_Unbounded_String;
                              ReFac.Safe_Insert (Edit_Texts,
                                                 Obj.Unit.Get_Filename,
                                                 Text_Edit);
                           else
                              Text_Edit.Location := Make_Range
                                (Last_Sloc_Start, Last_Sloc);
                              Text_Edit.Text := To_Unbounded_String (", ");
                              ReFac.Safe_Insert (Edit_Texts,
                                                 Obj.Unit.Get_Filename,
                                                 Text_Edit);
                           end if;
                           Last_Position := Name_Position;

                        end if;
                        Last_Position := Name_Position;
                        Last_Sloc_Start := Start_Sloc (Deletable_Range);
                        Last_Sloc := End_Sloc (Deletable_Range);
                     end if;
                  end if;
               end loop;
               if Delete_Last or Last_Position = First then
                  Text_Edit.Location := Make_Range
                                        (Last_Sloc_Start, Last_Sloc);
                  Text_Edit.Text := Null_Unbounded_String;
                  ReFac.Safe_Insert (Edit_Texts,
                                     Obj.Unit.Get_Filename,
                                     Text_Edit);
               else
                  Text_Edit.Location := Make_Range
                                        (Last_Sloc_Start, Last_Sloc);
                  Text_Edit.Text := To_Unbounded_String (", ");
                  ReFac.Safe_Insert (Edit_Texts,
                                     Obj.Unit.Get_Filename,
                                     Text_Edit);
               end if;
            end Delete_Names_in_List;
         begin
            for Name of Obj.F_Ids loop
               Name_Count := Name_Count + 1;
               if Last_Decl_For_Name.Contains (Name.As_Defining_Name) then
                  Modify_Count := Modify_Count + 1;
                  Names.Include (Name.As_Defining_Name);
               end if;
            end loop;
            Obj_Decl_To_Names.Include (Obj, Names);
            Tmp_Text := Null_Unbounded_String;
            declare
               Delete_Lenth : Integer;
               Tmp_String : String := Text.Image (Obj.Text);
            begin
               Delete_Lenth := Integer (Obj.F_Ids.Sloc_Range.End_Column -
                                          Obj.Sloc_Range.Start_Column) + 1;
               for I in Tmp_String'Range loop
                  if I > Delete_Lenth then
                     Tmp_Text := Tmp_Text & Tmp_String (I);
                  end if;
               end loop;
            end;

            if Name_Count = Modify_Count then
               Text_Edit.Location := Obj.Sloc_Range;
               Text_Edit.Text := Null_Unbounded_String;
               ReFac.Safe_Insert (Edit_Texts,
                                  Obj.Unit.Get_Filename,
                                  Text_Edit);
            else
               Delete_Names_in_List;
               Is_Empty := False;
            end if;
            for Name of Names loop
               Text_To_Add := Null_Unbounded_String;
               Text_To_Add := Text_To_Add & Text.Image (Name.Text);
               Text_To_Add := Text_To_Add & Tmp_Text
                              & Ada.Characters.Latin_1.LF;
               declare
                  Last_Decl : LAL.Declarative_Part :=
                    Last_Decl_For_Name (Name);
                  Location : Source_Location_Range :=
                    Last_Decl.Child (1).Sloc_Range;
               begin
                  Location.End_Column := Location.Start_Column;
                  Location.End_Line := Location.Start_Line;
                  Text_Edit.Location := Location;
                  Text_Edit.Text := Text_To_Add;
                  ReFac.Safe_Insert (Edit_Texts,
                                     Last_Decl.Unit.Get_Filename,
                                     Text_Edit);
                  ReFac.Safe_Insert (Edit_Map_Of_Decl_Part,
                                     Last_Decl.Unit.Get_Filename,
                                     Text_Edit);
               end;
            end loop;
            Edit_Info.Insert (Obj, Edit_Texts);
         end Generate_Edit;
      begin
         for Decl of Decl_Part.F_Decls loop
            if Decl.Kind in LALCO.Ada_Object_Decl_Range then
               if not Obj_Decl_To_Modify.Contains (Decl.As_Object_Decl) then
                  Is_Empty := False;
               else
                  Generate_Edit (Decl.As_Object_Decl);
               end if;
            else
               Is_Empty := False;
            end if;
            Has_Decl := True;
         end loop;
         if Is_Empty and Has_Decl then
            declare
               Text_Delete : ReFac.Text_Edit;
            begin
               Text_Delete.Text := Null_Unbounded_String;
               Text_Delete.Location := Decl_Part.Sloc_Range;
               ReFac.Safe_Insert (Edit_Map_Of_Decl_Part,
                                  Decl_Part.Unit.Get_Filename,
                                  Text_Delete);
            end;
            Removable_Decl_Part.Insert (Decl_Part, Edit_Map_Of_Decl_Part);
         end if;
      end Generate_Edit;
   begin
      for Unit of Unit_Array loop
         Unit.Root.Traverse (Find_Decl_Part'Access);
      end loop;
      for Decl_Part of Decl_Parts loop
         Generate_Edit (Decl_Part);
      end loop;
      return (Obj_Decl_To_Names, Edit_Info, Removable_Decl_Part);
   end Scope_Declarations;

   procedure Run (Unit_Array : LAL.Analysis_Unit_Array;
                  Stream     : in out
                    VSS.Text_Streams.Output_Text_Stream'Class) is
   begin
      Edit_Info := Scope_Declarations (Unit_Array);

      Output.JSON_Serialize (Edit_Info, Stream);
   end Run;

   function Interact return ReFac.Text_Edit_Map is
      Count : Integer := 0;
      Last : Integer;
      Index_Set : Integer_Sets.Set;
      Output_Map : ReFac.Text_Edit_Map;
      Is_Apply_All : Boolean := False;
   begin
      Put_Line ("There is(are)"
                & Edit_Info.Object_To_Decl.Length'Image
                & " object declaration(s) that could be scoped:");
      for Obj_Decl in Edit_Info.Object_To_Decl.Iterate loop
         Count := Count + 1;
         Put_Line (Count'Image & ". "
                   & Text.Image (Obj_Decl.Key.Text)
                   & " in Line"
                   & Obj_Decl.Key.Sloc_Range.Start_Line'Image);
      end loop;
      Put_Line ("Please Enter the Changes that you would like to apply, "
                & "entre 0 to apply all the changes");
      declare
         Selection : String (1 .. 3 * Count) := (others => ' ');
         Index_Select : Integer := 0;
         Flag : Boolean := False;
      begin
         Get_Line (Selection, Last);
         for I in 1 .. Last loop
            case Selection (I) is
            when ' ' =>
               if Flag then
                  Flag := False;
                  if Index_Select > Count then
                     Put_Line ("Please Enter Legal Value");
                  else
                     Index_Set.Insert (Index_Select);
                  end if;
                  Index_Select := 0;
               end if;
            when '0' =>
               if not Flag then
                  Is_Apply_All := True;
                  exit;
               else
                  Index_Select := Index_Select * 10;
               end if;
            when '1' =>
               Flag := True;
               Index_Select := Index_Select * 10 + 1;
            when '2' =>
               Flag := True;
               Index_Select := Index_Select * 10 + 2;
            when '3' =>
               Flag := True;
               Index_Select := Index_Select * 10 + 3;
            when '4' =>
               Flag := True;
               Index_Select := Index_Select * 10 + 4;
            when '5' =>
               Flag := True;
               Index_Select := Index_Select * 10 + 5;
            when '6' =>
               Flag := True;
               Index_Select := Index_Select * 10 + 6;
            when '7' =>
               Flag := True;
               Index_Select := Index_Select * 10 + 7;
            when '8' =>
               Flag := True;
               Index_Select := Index_Select * 10 + 8;
            when '9' =>
               Flag := True;
               Index_Select := Index_Select * 10 + 9;
            when others =>
               Put_Line ("Please Enter Legal Value");
            end case;
         end loop;
         if Flag then
            Flag := False;
            if Index_Select > Count then
               Put_Line ("Please Enter Legal Value");
            else
               Index_Set.Insert (Index_Select);
            end if;
               Index_Select := 0;
         end if;
      end;
      Count := 0;
      for Obj_Decl in Edit_Info.Object_To_Decl.Iterate loop
         Count := Count + 1;
         if Index_Set.Contains (Count) or Is_Apply_All then
            ReFac.Merge (Output_Map, Edit_Info.Edit_Info (Obj_Decl.Key));
         end if;
      end loop;
      return Output_Map;
   end Interact;

end Tools.Scope_Declarations_Tool;
