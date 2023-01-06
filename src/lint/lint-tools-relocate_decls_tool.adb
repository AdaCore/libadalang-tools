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

with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Langkit_Support.Text;
with Libadalang.Common;
with Ada.Containers; use Ada.Containers;
with Output;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package body Lint.Tools.Relocate_Decls_Tool is
   package LALCO renames Libadalang.Common;
   package Text renames Langkit_Support.Text;

   type Positions is (First, Middle, Last);

   function Hash (N : LAL.Defining_Name) return Ada.Containers.Hash_Type is
     (N.As_Ada_Node.Hash);

   function Hash (N : LAL.Object_Decl) return Ada.Containers.Hash_Type is
     (N.As_Ada_Node.Hash);

   package Defining_Name_To_Last_Call is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => LAL.Defining_Name,
        Element_Type    => LAL.Ada_Node,
        Hash            => Hash,
        Equivalent_Keys => LAL."=",
        "="             => LAL."=");

   package Defining_Name_To_Integer is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => LAL.Defining_Name,
        Element_Type    => Integer,
        Hash            => Hash,
        Equivalent_Keys => LAL."=");

   package Object_Decl_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => LAL.Object_Decl,
        Hash                => Hash,
        Equivalent_Elements => LAL."=",
        "="                 => LAL."=");

   ---------
   -- "<" --
   ---------

   function "<" (L, R : LAL.Ada_Node) return Boolean
   is
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

   function Find_Decl_Private (Unit_Array : LAL.Analysis_Unit_Array)
                               return Modify_Info is
      Edit_Of_Obj_Decl : Obj_Decl_To_Edit_Map.Map;
      --  Store the Edit_Map to the relevant object declaration node

      Edit_Of_Other_Decls : Decl_Name_To_Edit_Map.Map;
      --  Store the Edit_Map to the relevant other declarations

      Last_Decl_For_Name : Defining_Name_To_Last_Call.Map;
      --  This map stores the defining_names that can be moved private
      --  and the destination declarative part that the defining name can be
      --  moved to.

      Obj_Decl_To_Names : Obj_Decl_To_Defining_Name.Map;
      --  Store the variables that can be scoped more narrowly

      Other_Decls_To_Modify : Defining_Name_Ordered_Sets.Set;
      --  Stores the Type declarations to be modified

      Obj_Decls_To_Modify : Object_Decl_Sets.Set;
      --  Stores the object decls to be modified

      Obj_Names_To_Modify : Defining_Name_Ordered_Sets.Set;
      --  Stores the names in object decls to be modified

      Ref_Counts : Defining_Name_To_Integer.Map;
      --  Stores the time of refs of declarations

      Names_Checked_Done : Defining_Name_Ordered_Sets.Set;
      --  Stores the Names that has cheked done

      procedure Process_Decl (Decl_Part : LAL.Package_Decl);
      --  This function process the package_decl node

      function Find_Package_Decl (Node : LAL.Ada_Node'Class)
                                  return LALCO.Visit_Status;
      --  This function is to travarse the AST to find all the
      --  package_decl node.

      procedure Generate_Edit (Name : LAL.Defining_Name);
      --  This function generates the edit_map for the defining names of
      --  declarations other than object declarations;

      procedure Generate_Edit (Obj : LAL.Object_Decl);
      --  This function generates the edit_map for object_declarations,
      --  this have to be dealed separately since object_decl node could
      --  have previous defining names.

      ------------------
      -- Process_Decl --
      ------------------

      procedure Process_Decl (Decl_Part : LAL.Package_Decl) is
         Public_Node : constant LAL.Public_Part := Decl_Part.F_Public_Part;
         Private_Node : constant LAL.Private_Part := Decl_Part.F_Private_Part;
         Body_Part : constant LAL.Package_Body := Decl_Part.P_Body_Part;

         function My_Find_Ref (A : LAL.Defining_Name; Node : LAL.Ada_Node)
                               return Integer;
         --  This function is to avoid the Null Ada Node exception case
         --  when Node is null, we set the reference number to 0.

         function Is_Refered_Outside (Name : LAL.Defining_Name)
                                      return Boolean;
         --  This function checks if the defining name
         --  is refered outside the package.

         procedure Find_Last_Call (Name : LAL.Defining_Name);
         --  This function is to find the most suitable location for the
         --  defining name except for the package where the name
         --  first declared.

         procedure Find_Last_Call_First_Time (Name : LAL.Defining_Name);
         --  This function is to find the most suitable location for the
         --  defining name in the package where the name first declared.

         procedure Initialize_Count (Name   : LAL.Defining_Name;
                                     Is_Obj : Boolean := False);
         --  This function initialize the map from name to reference count
         --  Note that P_Find_Refs will count the first declaration thus we
         --  do minus one.

         -----------------
         -- My_Find_Ref --
         -----------------

         function My_Find_Ref (A : LAL.Defining_Name; Node : LAL.Ada_Node)
                               return Integer is
         begin
            if Node.Is_Null then
               return 0;
            else
               return A.P_Find_Refs (Node)'Length;
            end if;
         end My_Find_Ref;

         ------------------------
         -- Is_Refered_Outside --
         ------------------------

         function Is_Refered_Outside (Name : LAL.Defining_Name)
                                      return Boolean is
         begin
            if Name.P_Find_All_References (Unit_Array)'Length + 1 =
              My_Find_Ref (Name, Decl_Part.As_Ada_Node) +
              My_Find_Ref (Name, Body_Part.As_Ada_Node)
            then
               return False;
            else
               return True;
            end if;
         end Is_Refered_Outside;

         -------------------------------
         -- Find_Last_Call_First_Time --
         -------------------------------

         procedure Find_Last_Call_First_Time (Name : LAL.Defining_Name) is
         begin
            if My_Find_Ref (Name, Public_Node.As_Ada_Node) = 1 then
               if My_Find_Ref (Name, Private_Node.As_Ada_Node) > 0 then
                  Last_Decl_For_Name.Include (Name, Private_Node.As_Ada_Node);
               else
                  if not Body_Part.Is_Null then
                     Last_Decl_For_Name.Include (Name, Body_Part.As_Ada_Node);
                  end if;
               end if;
            else
               if My_Find_Ref (Name, Private_Node.As_Ada_Node) > 0 or
                 My_Find_Ref (Name, Body_Part.As_Ada_Node) > 0
               then
                  Names_Checked_Done.Include (Name);
               end if;
            end if;
         end Find_Last_Call_First_Time;

         --------------------
         -- Find_Last_Call --
         --------------------

         procedure Find_Last_Call (Name : LAL.Defining_Name) is
         begin
            if not Names_Checked_Done.Contains (Name) then
               if My_Find_Ref (Name, Decl_Part.As_Ada_Node) +
                 My_Find_Ref (Name, Body_Part.As_Ada_Node) > 0
               then
                  if My_Find_Ref (Name, Decl_Part.As_Ada_Node) +
                    My_Find_Ref (Name, Body_Part.As_Ada_Node)
                    = Ref_Counts (Name)
                  then
                     if My_Find_Ref (Name, Public_Node.As_Ada_Node) > 0 then
                        Last_Decl_For_Name.Include
                          (Name, Public_Node.As_Ada_Node);
                     else
                        if My_Find_Ref (Name, Private_Node.As_Ada_Node) > 0
                        then
                           Last_Decl_For_Name.Include
                             (Name, Private_Node.As_Ada_Node);
                        else
                           if not Body_Part.Is_Null then
                              Last_Decl_For_Name.Include
                                (Name, Body_Part.As_Ada_Node);
                           end if;
                        end if;
                     end if;
                  else
                     Names_Checked_Done.Include (Name);
                  end if;
               end if;
            end if;
         end Find_Last_Call;

         ----------------------
         -- Initialize_Count --
         ----------------------

         procedure Initialize_Count (Name   : LAL.Defining_Name;
                                     Is_Obj : Boolean := False) is
         begin
            if not Is_Refered_Outside (Name) then
               if Is_Obj then
                  Obj_Names_To_Modify.Include (Name);
               else
                  Other_Decls_To_Modify.Include (Name);
               end if;
               if Name.P_Find_All_References (Unit_Array)'Length = 0 then
                  Names_Checked_Done.Include (Name);
               else
                  Ref_Counts.Include (Name,
                                      Name.P_Find_All_References
                                      (Unit_Array)'Length);
                  Find_Last_Call_First_Time (Name);
               end if;
            end if;
         end Initialize_Count;
      begin
         for Name of Obj_Names_To_Modify loop
            Find_Last_Call (Name);
         end loop;
         for Name of Other_Decls_To_Modify loop
            Find_Last_Call (Name);
         end loop;
         for Decl of Public_Node.F_Decls loop
            case Decl.Kind is
               when LALCO.Ada_Object_Decl_Range =>
                  for Name of Decl.As_Basic_Decl.P_Defining_Names loop
                     Initialize_Count (Name, True);
                  end loop;
                  Obj_Decls_To_Modify.Include (Decl.As_Object_Decl);
               when LALCO.Ada_Base_Type_Decl =>
                  Initialize_Count (Decl.As_Base_Type_Decl.F_Name);
               when LALCO.Ada_Subp_Decl_Range =>
                  Initialize_Count (Decl.As_Subp_Decl.P_Defining_Name);
               when LALCO.Ada_Expr_Function_Range =>
                  Initialize_Count (Decl.As_Expr_Function.P_Defining_Name);
               when others =>
                  null;
            end case;
         end loop;
      end Process_Decl;

      -------------------
      -- Generate_Edit --
      -------------------

      procedure Generate_Edit (Obj : LAL.Object_Decl) is
         Text_To_Add, Tmp_Text : Unbounded_String;
         Edit_Texts : ReFac.Text_Edit_Map;
         Text_Edit : ReFac.Text_Edit;
         Name_Count, Modify_Count : Integer := 0;
         Names_To_Modify : Defining_Name_Ordered_Sets.Set;

         function Check_Position (List_Node : LAL.Ada_Node'Class;
                                  Name_Node : LAL.Defining_Name'Class)
                                  return Positions;
         --  Check the position of the name in the name list.

         function Delete_Position (Name : LAL.Ada_Node'Class;
                            Position : Positions)
                                   return Source_Location_Range;
         --  Get the delete sloc by the location.

         procedure Delete_Names_in_List;
         --  generate the edit text of delete name in the list.

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
               if Name_Node.Child_Index = LAL.Children (List_Node)'Length - 1
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

         function Delete_Position (Name : LAL.Ada_Node'Class;
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
            Names : constant Defining_Name_Ordered_Sets.Set :=
              Obj_Decl_To_Names (Obj);
            Name_List : constant LAL.Defining_Name_List := Obj.F_Ids;
            Name_Position : Positions;
            Last_Position : Positions := First;
            Last_Sloc, Last_Sloc_Start : Source_Location := No_Source_Location;
            Delete_Last : Boolean := False;
            Deletable_Range : Source_Location_Range;
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
               Text_Edit.Location := Make_Range (Last_Sloc_Start, Last_Sloc);
               Text_Edit.Text := Null_Unbounded_String;
               ReFac.Safe_Insert (Edit_Texts,
                                  Obj.Unit.Get_Filename,
                                  Text_Edit);
            else
               Text_Edit.Location := Make_Range (Last_Sloc_Start, Last_Sloc);
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
               Names_To_Modify.Include (Name.As_Defining_Name);
            end if;
         end loop;
         if Modify_Count > 0 then
            Obj_Decl_To_Names.Include (Obj, Names_To_Modify);
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
            end if;
            for Name of Obj_Decl_To_Names (Obj) loop
               Text_To_Add := Null_Unbounded_String;
               Text_To_Add := Text_To_Add & Text.Image (Name.Text);
               Text_To_Add := Text_To_Add & Tmp_Text
                              & Ada.Characters.Latin_1.LF;
               declare
                  Decl_Part : constant LAL.Ada_Node :=
                    Last_Decl_For_Name (Name);
                  Location : Source_Location_Range :=
                    Decl_Part.Sloc_Range;
               begin
                  Location.Start_Column := Location.Start_Column + 3;
                  Location.Start_Line := Location.Start_Line + 1;
                  Location.End_Column := Location.Start_Column;
                  Location.End_Line := Location.Start_Line;
                  Text_Edit.Location := Location;
                  Text_Edit.Text := Text_To_Add;
                  ReFac.Safe_Insert (Edit_Texts,
                                     Decl_Part.Unit.Get_Filename,
                                     Text_Edit);
               end;
            end loop;
            Edit_Of_Obj_Decl.Insert (Obj, Edit_Texts);
         end if;
      end Generate_Edit;

      -------------------
      -- Generate_Edit --
      -------------------

      procedure Generate_Edit (Name : LAL.Defining_Name) is
         Text_To_Add : Unbounded_String;
         Edit_Texts  : ReFac.Text_Edit_Map;
         Text_Edit   : ReFac.Text_Edit;
         Location    : Source_Location_Range :=
                       Last_Decl_For_Name (Name).Sloc_Range;
      begin
         Text_Edit.Location := Name.P_Basic_Decl.Sloc_Range;
         Text_Edit.Text := Null_Unbounded_String;
         ReFac.Safe_Insert (Edit_Texts, Name.Unit.Get_Filename, Text_Edit);
         Text_To_Add := Text_To_Add & Text.Image (Name.P_Basic_Decl.Text);
         Text_To_Add := Text_To_Add & Ada.Characters.Latin_1.LF;
         Text_Edit.Text := Text_To_Add;
         Location.Start_Column := Location.Start_Column + 3;
         Location.Start_Line := Location.Start_Line + 1;
         Location.End_Column := Location.Start_Column;
         Location.End_Line := Location.Start_Line;
         Text_Edit.Location := Location;
         ReFac.Safe_Insert (Edit_Texts,
                            Last_Decl_For_Name (Name).Unit.Get_Filename,
                            Text_Edit);
         Edit_Of_Other_Decls.Insert (Name, Edit_Texts);
      end Generate_Edit;

      -----------------------
      -- Find_Package_Decl --
      -----------------------

      function Find_Package_Decl (Node : LAL.Ada_Node'Class)
                                  return LALCO.Visit_Status is
      begin
         if Node.Kind in LALCO.Ada_Package_Decl_Range then
            Process_Decl (Node.As_Package_Decl);
         end if;
         return LALCO.Into;
      end Find_Package_Decl;
   begin
      for Unit of Unit_Array loop
         Unit.Root.Traverse (Find_Package_Decl'Access);
      end loop;
      for Obj_Decl of Obj_Decls_To_Modify loop
         Generate_Edit (Obj_Decl);
      end loop;
      for Name of Other_Decls_To_Modify loop
         if Last_Decl_For_Name.Contains (Name) then
            Generate_Edit (Name);
         end if;
      end loop;
      return (Obj_Decl_To_Names, Edit_Of_Obj_Decl, Edit_Of_Other_Decls);
   end Find_Decl_Private;

   ---------
   -- Run --
   ---------

   procedure Run (Unit_Array : LAL.Analysis_Unit_Array;
                  Stream     : in out
                               VSS.Text_Streams.Output_Text_Stream'Class) is
      Edit_Info : Modify_Info;
   begin
      Edit_Info := Find_Decl_Private (Unit_Array);
      Output.JSON_Serialize (Edit_Info, Stream);
   end Run;

end Lint.Tools.Relocate_Decls_Tool;
