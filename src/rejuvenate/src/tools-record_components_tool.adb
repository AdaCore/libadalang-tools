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
with Langkit_Support.Text;
with Libadalang.Common;
with Ada.Containers; use Ada.Containers;
with Libadalang.Project_Provider;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;
with Ada.Assertions;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Text_Streams.Memory_UTF8_Output;
with Output;

pragma Unreferenced (Langkit_Support.Text);
pragma Unreferenced (Ada.Assertions);
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

package body Tools.Record_Components_Tool is
   package LALCO renames Libadalang.Common;
   package Text renames Langkit_Support.Text;
   package LAL_GPR renames Libadalang.Project_Provider;
   use type LALCO.Ada_Node_Kind_Type;
   package GPR renames GNATCOLL.Projects;

   type Ref_Status is (Read, Write);
   --  The status of Reference which is Read of Write.

   type Positions is (First, Middle, Last);
   --  The Position of defining name in the list

   type Record_Deletability is (Del_All, Del_Part, Del_None);
   --  The deletability of the record:
   --  Del_All: the whole record can be deleted
   --  Del_Part: Part of the record can be deleted
   --  Del_None: the whole record should de preserved

   package Record_Vectors is new
      Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => LAL.Record_Def,
       "="          => LAL."=");

   package Record_Hashed_Maps is new
   Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type            => LAL.Defining_Name,
        Element_Type        => Record_Deletability,
        Hash                => Defining_Name_Hash,
        Equivalent_Keys     => LAL."="
       );

   function Component_Hash (Element : LAL.Defining_Name_List)
                                return Ada.Containers.Hash_Type is
   (LAL.Hash (Element.As_Ada_Node));

   package Component_Hashed_Sets is new
   Ada.Containers.Hashed_Sets
    (Element_Type        => LAL.Defining_Name_List,
     Hash                => Component_Hash,
     Equivalent_Elements => LAL."=",
     "="                 => LAL."=");

   function "<" (L, R : LAL.Record_Def) return Boolean
   is
   begin
      if L.Unit.Get_Filename < R.Unit.Get_Filename then
         return True;
      else
         if L.Unit.Get_Filename > R.Unit.Get_Filename then
            return False;
         else
            return (Slocs.Start_Sloc (LAL.Sloc_Range (L)) <
                      Slocs.Start_Sloc (LAL.Sloc_Range (R)));
         end if;
      end if;
   end "<";

   ---------------------
   -- Get_Record_Name --
   ---------------------

   function Get_Record_Name (Node : LAL.Record_Def'Class)
                            return LAL.Defining_Name
   is
      Type_Decl_Node : constant LAL.Ada_Node := Node.Parent.Parent;
   begin
      return Type_Decl_Node.As_Type_Decl.P_Defining_Name;
   end Get_Record_Name;

   ----------------------------
   -- Find_Unused_Components --
   ----------------------------

   function Find_Unused_Components (Unit_Array : LAL.Analysis_Unit_Array)
                                    return Delete_Infos is
      -- spec of procedure and functions --

      Record_Texts_Map : Record_Text_Edits;

      --  The hashmap use record as key with text_edit infomations
      --  related to this record.

      RV : Record_Vectors.Vector;

      --  Record Nodes which contain the deletable components

      Deletable_Names : Defining_Name_Ordered_Sets.Set;

      --  Set of Deletable Defining Names

      Deletable_Components : Component_Hashed_Sets.Set;

      --  Set of Completed Deletable Component

      Texts_Edit : ReFac.Text_Edit_Ordered_Maps.Map;

      --  The texts we want to edit.

      Records_Deletability : Record_Hashed_Maps.Map;

      --  The Hash Map which stores the deletability of Records

      Record_Deletable_Name : Deletable_Record_Componenets;

      --  The Hash Map Use Record name as key stores its deletable
      --  Components' Defining names.

      function Find_Record_Node (Node : LAL.Ada_Node'Class)
                             return LALCO.Visit_Status;
      --  Find All the Record_Def Node in AST

      procedure Count_Reference (Node : LAL.Record_Def'Class);
      --  Count how many times one defining name is referenced.

      procedure Print_Deletable
        (Delete_Range : Slocs.Source_Location_Range);
      --  Print method for deletable message, only for debug.

      pragma Unreferenced (Print_Deletable);

      function Delete_Position (Name : LAL.Ada_Node'Class;
                            Position : Positions)
                                return Slocs.Source_Location_Range;
      --  Delete node with its position in the list known.
      --  return the delete location range.

      procedure Delete_Record_Reference (Node : LAL.Record_Def'Class);
      --  Delete all the reference of the given record.

      procedure Delete_Range (Deletable_Range : Slocs.Source_Location_Range;
                              Filename : ReFac.File_Name_Type;
                              Text : Ada.Strings.Unbounded.Unbounded_String :=
                                Ada.Strings.Unbounded
                              .To_Unbounded_String (""));
      --  Insert the delete information into the text edit map.

      procedure Delete_Names_in_List (Component_node : LAL.Ada_Node'Class);
      --  Delete all the deletable defining names in the Component List

      procedure Delete_Assoc_in_List (Assoc_List : LAL.Assoc_List'Class);
      --  Delete all the deletable defining names in the Assoc_List

      function Check_Position (List_Node : LAL.Ada_Node'Class;
                                  Name_Node : LAL.Defining_Name'Class)
                               return Positions;
      --  Return the position of the defining name in the list.

      function Check_Position (List_Node : LAL.Ada_Node'Class;
                                  Assoc_Node : LAL.Basic_Assoc'Class)
                               return Positions;
      --  Return the position of the Basic_Assoc in the Assoc_List

      procedure Del_All_Var (Node : LAL.Defining_Name'Class);
      --  Delete the record which can be all deleted.

      procedure Del_Part_Var (Node : LAL.Defining_Name'Class);
      --  Delete the record which can only be deleted partly.

      procedure Process_Func (Func_Node : LAL.Ada_Node'Class;
                              Deletablility : Record_Deletability);
      --  Find all the functions that use the deletable record as return type.

      ------------------
      -- Process_Func --
      ------------------

      procedure Process_Func (Func_Node : LAL.Ada_Node'Class;
                             Deletablility : Record_Deletability)
      is
         Deletable_Range : Slocs.Source_Location_Range;
         function Find_Assoc_List (Node : LAL.Ada_Node'Class)
                                   return LALCO.Visit_Status;

         --  Find the node of assoc_list

         function Find_Return (Node : LAL.Ada_Node'Class)
                               return LALCO.Visit_Status;

         --  Find the node of return statement

         function Find_Paren (Node : LAL.Ada_Node'Class)
                              return LALCO.Visit_Status;

         --  Find the node of Paren_Expr

         function Find_Assoc_List (Node : LAL.Ada_Node'Class)
                                   return LALCO.Visit_Status
         is
         begin
            if Node.Kind = LALCO.Ada_Assoc_List then
               if Deletablility = Del_Part then
                  Delete_Assoc_in_List (Node.As_Assoc_List);
               else
                  Deletable_Range := LAL.Sloc_Range (Node);
                  Delete_Range (Deletable_Range => Deletable_Range,
                                Filename => Node.Unit.Get_Filename,
                                Text => Ada.Strings.Unbounded
                                   .To_Unbounded_String ("others => <>")
                               );
               end if;
               --  return LALCO.Stop;
            end if;
            return LALCO.Into;
         end Find_Assoc_List;

         -----------------
         -- Find_Return --
         -----------------

         function Find_Return (Node : LAL.Ada_Node'Class)
                                   return LALCO.Visit_Status
         is
         begin
            if Node.Kind = LALCO.Ada_Return_Stmt then
               LAL.Traverse (Node, Find_Assoc_List'Access);
            end if;
            return LALCO.Into;
         end Find_Return;

         ----------------
         -- Find_Paren --
         ----------------

         function Find_Paren (Node : LAL.Ada_Node'Class)
                                   return LALCO.Visit_Status
         is
         begin
            if Node.Kind = LALCO.Ada_Paren_Expr then
               LAL.Traverse (Node, Find_Assoc_List'Access);
            end if;
            return LALCO.Into;
         end Find_Paren;
      begin
         if Func_Node.Kind = LALCO.Ada_Subp_Body then
            LAL.Traverse (Func_Node, Find_Return'Access);
         else
            LAL.Traverse (Func_Node, Find_Paren'Access);
         end if;
      end Process_Func;

      -----------------
      -- Del_All_Var --
      -----------------

      procedure Del_All_Var (Node : LAL.Defining_Name'Class)
      is
         Deletable_Range : Slocs.Source_Location_Range;
      begin
         for Reference_Node of LAL.P_Find_All_References (Node, Unit_Array)
         loop
            for Parent_node of
               LAL.Parents (LAL.Ref (Reference_Node).As_Ada_Node) loop
               if Parent_node.Kind = LALCO.Ada_Assign_Stmt then
                  Deletable_Range := LAL.Sloc_Range (Parent_node);
                  Delete_Range (Deletable_Range => Deletable_Range,
                                   Filename => Parent_node.Unit.Get_Filename);

               end if;
            end loop;
         end loop;
      end Del_All_Var;

      ------------------
      -- Del_Part_Var --
      ------------------

      procedure Del_Part_Var (Node : LAL.Defining_Name'Class)
      is
         Deletable_Range : Slocs.Source_Location_Range;
         Assoc_List : LAL.Assoc_List;
         Parent_node : LAL.Ada_Node;
         function Find_Assoc_List (Node : LAL.Ada_Node'Class)
                                   return LALCO.Visit_Status;
         --  Find the node of Assoc List

         ---------------------
         -- Find_Assoc_List --
         ---------------------

         function Find_Assoc_List (Node : LAL.Ada_Node'Class)
                                   return LALCO.Visit_Status
         is
         begin
            if Node.Kind = LALCO.Ada_Assoc_List then
               Assoc_List := Node.As_Assoc_List;
               return LALCO.Stop;
            end if;
            return LALCO.Into;
         end Find_Assoc_List;
      begin
         for Reference_Node of LAL.P_Find_All_References (Node, Unit_Array)
         loop
            if LAL.P_Is_Write_Reference (LAL.Ref (Reference_Node).As_Name)
            then
               Parent_node := LAL.Ref (Reference_Node).As_Ada_Node.Parent;
               if Parent_node.Kind = LALCO.Ada_Dotted_Name then
                  if Deletable_Names.Contains
                    (LAL.P_Referenced_Defining_Name
                       (Parent_node.Child (2).As_Name))
                  then
                     Deletable_Range := LAL.Sloc_Range (Parent_node.Parent);
                     Delete_Range (Deletable_Range => Deletable_Range,
                                   Filename => Parent_node.Unit.Get_Filename);
                  end if;

               else
                  LAL.Traverse (Parent_node, Find_Assoc_List'Access);
                  Delete_Assoc_in_List (Assoc_List => Assoc_List);
               end if;
            end if;
         end loop;
      end Del_Part_Var;

      -----------------------------
      -- Delete_Record_Reference --
      -----------------------------

      procedure Delete_Record_Reference (Node : LAL.Record_Def'Class)
      is
         Record_Defining_Name : constant LAL.Defining_Name
           := Get_Record_Name (Node);
         Deletable_Range : Slocs.Source_Location_Range;
         Assoc_List : LAL.Assoc_List;
         Parent_node : LAL.Ada_Node;
         function Find_Assoc_List (Node : LAL.Ada_Node'Class)
                                   return LALCO.Visit_Status;
         --  Find all the Assoc_List Node in the child tree.

         ---------------------
         -- Find_Assoc_List --
         ---------------------

         function Find_Assoc_List (Node : LAL.Ada_Node'Class)
                                   return LALCO.Visit_Status
         is
         begin
            if Node.Kind = LALCO.Ada_Assoc_List then
               Assoc_List := Node.As_Assoc_List;
               return LALCO.Stop;
            end if;
            return LALCO.Into;
         end Find_Assoc_List;
      begin
         for Reference_Node of LAL.P_Find_All_References
           (Record_Defining_Name, Unit_Array)
         loop
            Parent_node := LAL.Ref (Reference_Node).As_Ada_Node.Parent.Parent;
            if Parent_node.Kind = LALCO.Ada_Object_Decl then
               if Records_Deletability (Record_Defining_Name) = Del_All then
                  Deletable_Range := LAL.Sloc_Range (Parent_node);
                  Deletable_Range.Start_Column := LAL.Sloc_Range
                       (LAL.Ref (Reference_Node)).End_Column;
                  Delete_Range (Deletable_Range => Deletable_Range,
                                   Filename => Parent_node.Unit.Get_Filename,
                                  Text => Ada.Strings.Unbounded
                                   .To_Unbounded_String (";"));
                  Del_All_Var (LAL.P_Defining_Name
                                  (Parent_node.As_Basic_Decl));
               else
                  LAL.Traverse (Parent_node, Find_Assoc_List'Access);
                  Delete_Assoc_in_List (Assoc_List => Assoc_List);
                  Del_Part_Var (LAL.P_Defining_Name
                                  (Parent_node.As_Basic_Decl));
               end if;
            end if;
            if Parent_node.Kind = LALCO.Ada_Subp_Spec then
               Process_Func (Parent_node.Parent,
                             Records_Deletability (Record_Defining_Name));
            end if;
         end loop;

      end Delete_Record_Reference;

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
            if Name_Node.Child_Index = LAL.Children (List_Node)'Length - 1 then
               return Last;
            else
               return Middle;
            end if;
         end if;
      end Check_Position;

      --------------------
      -- Check_Position --
      --------------------

      function Check_Position (List_Node : LAL.Ada_Node'Class;
                                  Assoc_Node : LAL.Basic_Assoc'Class)
                                  return Positions
      is
      begin
         if Assoc_Node.Child_Index = 0 then
            return First;
         else
            if Assoc_Node.Child_Index = LAL.Children (List_Node)'Length - 1
            then
               return Last;
            else
               return Middle;
            end if;
         end if;
      end Check_Position;

      --------------------------
      -- Delete_Assoc_in_List --
      --------------------------

      procedure Delete_Assoc_in_List (Assoc_List : LAL.Assoc_List'Class)
      is
         Assoc_Position : Positions;
         Last_Position : Positions := First;
         Last_Sloc, Last_Sloc_Start, Leave_One_Sloc : Slocs.Source_Location
              := Slocs.No_Source_Location;
         Delete_Last : Boolean := False;
         All_used_component : Boolean := True;
         Deletable_Range : Slocs.Source_Location_Range;
         Others_deletable : Boolean := True;
         Count_Rest : Integer := 0;
         Leave_One_Name : LAL.Defining_Name;
      begin
         Count_Rest := 0;
         for Assoc_Node of Assoc_List loop
            if Text.Image (Assoc_Node.As_Ada_Node.Child (1).Text)'Length > 0
            then
               Count_Rest := 2;
            end if;
            Others_deletable := True;
            if LAL.P_Get_Params (Assoc_Node)'Length > 1 then
               for Name_Node of LAL.P_Get_Params (Assoc_Node) loop
                  if not Deletable_Names.Contains (Name_Node) then
                     Others_deletable := False;
                  end if;
                  exit when not Others_deletable;
               end loop;
            else
               if LAL.P_Get_Params (Assoc_Node)'Length < 1 then
                  Others_deletable := False;
               else
                  if not Deletable_Names.Contains
                    (LAL.P_Get_Params (Assoc_Node)(1))
                  then
                     Others_deletable := False;
                  end if;
               end if;
            end if;
            if Others_deletable then
               All_used_component := False;
               Assoc_Position := Check_Position
                 (Assoc_List.As_Ada_Node, Assoc_Node);
               if Assoc_Position = Last then
                  Delete_Last := True;
               end if;
               Deletable_Range := Delete_Position
                       (Assoc_Node.As_Ada_Node, Assoc_Position);
               if Slocs.Start_Sloc (Deletable_Range) < Last_Sloc then
                  Last_Sloc := Slocs.End_Sloc (Deletable_Range);
               else
                  if Last_Sloc /= Slocs.No_Source_Location then
                     if Last_Position = First then
                        Delete_Range (Deletable_Range => Slocs.Make_Range
                                            (Last_Sloc_Start, Last_Sloc),
                                     Filename => Assoc_List.Unit.Get_Filename);
                     else
                        Delete_Range (Deletable_Range => Slocs.Make_Range
                                            (Last_Sloc_Start, Last_Sloc),
                                            Filename => Assoc_List
                                            .Unit.Get_Filename,
                                            Text => Ada.Strings.Unbounded
                                            .To_Unbounded_String (", "));
                     end if;
                        Last_Position := Assoc_Position;

                  end if;
                  Last_Position := Assoc_Position;
                  Last_Sloc_Start := Slocs.Start_Sloc (Deletable_Range);
                  Last_Sloc := Slocs.End_Sloc (Deletable_Range);
               end if;
            else
               if Count_Rest = 0 then
                  Leave_One_Sloc.Column :=
                    LAL.Sloc_Range (Assoc_Node).Start_Column;
                  Leave_One_Sloc.Line :=
                    LAL.Sloc_Range (Assoc_Node).Start_Line;
                  Leave_One_Name := LAL.P_Get_Params (Assoc_Node)(1);
               end if;
               Count_Rest := Count_Rest + 1;
            end if;
         end loop;
         if not All_used_component then
            if Delete_Last or Last_Position = First then
               Delete_Range (Deletable_Range => Slocs.Make_Range
                                      (Last_Sloc_Start, Last_Sloc),
                             Filename => Assoc_List.Unit.Get_Filename);
            else
               Delete_Range (Deletable_Range => Slocs.Make_Range
                                          (Last_Sloc_Start, Last_Sloc),
                             Filename => Assoc_List.Unit.Get_Filename,
                             Text => Ada.Strings.Unbounded
                                     .To_Unbounded_String (", "));
            end if;
         end if;
         if Count_Rest = 1 then
            Delete_Range (Deletable_Range => Slocs.Make_Range
                                            (Leave_One_Sloc, Leave_One_Sloc),
                          Filename => Assoc_List.Unit.Get_Filename,
                          Text => Ada.Strings.Unbounded.To_Unbounded_String
                               (Text.Image (Leave_One_Name.Text) & " => "));
         end if;
      end Delete_Assoc_in_List;

      --------------------------
      -- Delete_Names_in_List --
      --------------------------

      procedure Delete_Names_in_List (Component_node : LAL.Ada_Node'Class)
      is
         Name_Position : Positions;
         Last_Position : Positions := First;
         Last_Sloc, Last_Sloc_Start : Slocs.Source_Location
              := Slocs.No_Source_Location;
         Delete_Last : Boolean := False;
         All_used_component : Boolean := True;
         Deletable_Range : Slocs.Source_Location_Range;
      begin
         for Name_Node of Component_node.As_Defining_Name_List loop
            if Deletable_Names.Contains (Name_Node.As_Defining_Name) then
               All_used_component := False;
               Name_Position := Check_Position (Component_node, Name_Node);
               if Name_Position = Last then
                  Delete_Last := True;
               end if;
               Deletable_Range := Delete_Position
                          (Name_Node.As_Ada_Node, Name_Position);
               if Slocs.Start_Sloc (Deletable_Range) < Last_Sloc then
                  Last_Sloc := Slocs.End_Sloc (Deletable_Range);
               else
                  if Last_Sloc /= Slocs.No_Source_Location then
                     if Last_Position = First then
                        Delete_Range (Deletable_Range => Slocs.Make_Range
                                      (Last_Sloc_Start, Last_Sloc),
                                      Filename => Component_node
                                      .Unit.Get_Filename);
                     else
                        Delete_Range (Deletable_Range => Slocs.Make_Range
                                      (Last_Sloc_Start, Last_Sloc),
                                      Filename => Component_node
                                      .Unit.Get_Filename,
                                      Text => Ada.Strings.Unbounded
                                      .To_Unbounded_String (", "));
                     end if;
                     Last_Position := Name_Position;

                  end if;
                  Last_Position := Name_Position;
                  Last_Sloc_Start := Slocs.Start_Sloc (Deletable_Range);
                  Last_Sloc := Slocs.End_Sloc (Deletable_Range);
               end if;
            end if;
         end loop;
         if not All_used_component then
            if Delete_Last or Last_Position = First then
               Delete_Range (Deletable_Range => Slocs.Make_Range
                                      (Last_Sloc_Start, Last_Sloc),
                             Filename => Component_node.Unit.Get_Filename);
            else
               Delete_Range (Deletable_Range => Slocs.Make_Range
                                          (Last_Sloc_Start, Last_Sloc),
                             Filename => Component_node.Unit.Get_Filename,
                             Text => Ada.Strings.Unbounded
                                      .To_Unbounded_String (", "));
            end if;
         end if;
      end Delete_Names_in_List;

      --------------------
      -- Find_Body_Node --
      --------------------

      function Find_Record_Node (Node : LAL.Ada_Node'Class)
                             return LALCO.Visit_Status
      is
      begin
         if Node.Kind in LALCO.Ada_Record_Def then
            RV.Append (Node.As_Record_Def);
         end if;
         return LALCO.Into;
      end Find_Record_Node;

      ---------------------
      -- Print_deletable --
      ---------------------

      procedure Print_Deletable
        (Delete_Range : Slocs.Source_Location_Range)
      is
      begin
         Put_Line ("Line"
                   & Slocs.Line_Number'Image (Delete_Range.Start_Line)
                   & " Column"
                   & Slocs.Column_Number'Image (Delete_Range.Start_Column)
                   & " to Line"
                   & Slocs.Line_Number'Image (Delete_Range.End_Line)
                   & " Column"
                   & Slocs.Column_Number'Image (Delete_Range.End_Column)
                   & " can be deleted.");
      end Print_Deletable;

      ---------------------
      -- Delete_Position --
      ---------------------

      function Delete_Position (Name : LAL.Ada_Node'Class;
                            Position : Positions)
                            return Slocs.Source_Location_Range
      is
         Deletable_Range, Next_Range,
         Previous_Range : Slocs.Source_Location_Range;
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

      ------------------
      -- Delete_Range --
      ------------------

      procedure Delete_Range (Deletable_Range : Slocs.Source_Location_Range;
                              Filename : ReFac.File_Name_Type;
                              Text : Ada.Strings.Unbounded.Unbounded_String :=
                                Ada.Strings.Unbounded.To_Unbounded_String (""))
      is
         Text_Delete : ReFac.Text_Edit;
      begin
         Text_Delete.Location := Deletable_Range;
         Text_Delete.Text := Text;
         ReFac.Safe_Insert (Texts_Edit, Filename, Text_Delete);
      end Delete_Range;

      ---------------------
      -- Count_Reference --
      ---------------------

      procedure Count_Reference (Node : LAL.Record_Def'Class) is

         Record_Deletable : Boolean := True;
         Deletable_Range : Slocs.Source_Location_Range;

         function Get_Ref_Status (Ref_Node : LAL.Ref_Result)
                                  return Ref_Status;
         --  Get the status of the reference

         function Process_name (Defining_name_node : LAL.Defining_Name'Class)
                                return Boolean;
         --  Check if the defining name is deletable

         function find_all_defining_name (Component_node : LAL.Ada_Node'Class)
                                          return LALCO.Visit_Status;
         --  Find all the deletable defining name

         function Delete_Components (Component_node : LAL.Ada_Node'Class)
                                     return LALCO.Visit_Status;
         --  Delete the component with all its defining names deletable

         --------------------
         -- Get_Ref_Status --
         --------------------

         function Get_Ref_Status (Ref_Node : LAL.Ref_Result)
                                  return Ref_Status
         is
         begin
            if LAL.P_Is_Write_Reference (LAL.Ref (Ref_Node).As_Name) then
               return Write;
            else
               return Read;
            end if;
         end Get_Ref_Status;

         ------------------
         -- Process_name --
         ------------------

         function Process_name (Defining_name_node : LAL.Defining_Name'Class)
                                return Boolean
         is
         begin
            declare
               Read_Flag : Boolean :=  False;
            begin
               for ref of LAL.P_Find_All_References
                 (Defining_name_node, Unit_Array) loop
                  declare
                     Ref_statu : Ref_Status;
                  begin
                     Ref_statu := Get_Ref_Status (ref);
                     --  Put_Line (Ref_Status'Image (Ref_statu));
                     --  if Ref_statu = Write then
                     --     Write_Flag  := True;
                     --  end if;
                     if Ref_statu = Read then
                        Read_Flag  := True;
                     end if;
                     if Read_Flag then
                        return False;
                     end if;
                  end;

               end loop;
               return True;
            end;
         end Process_name;

         ----------------------------
         -- find_all_defining_name --
         ----------------------------

         function find_all_defining_name (Component_node : LAL.Ada_Node'Class)
                                          return LALCO.Visit_Status
         is
            Component_Deletable : Boolean := True;
            Deletable : Boolean;
         begin
            if Component_node.Kind = LALCO.Ada_Defining_Name_List then
               if LAL.Children (Component_node)'Length = 1 then
                  Component_Deletable := Process_name
                    (Component_node.As_Defining_Name_List.List_Child (1));
                  if Component_Deletable then
                     Deletable_Names.Insert
                       (Component_node.As_Defining_Name_List.List_Child (1));
                  end if;
               else
                  for Name_Node of Component_node.As_Defining_Name_List loop
                     Deletable := Process_name (Name_Node);
                     if Deletable then
                        Records_Deletability (Get_Record_Name (Node))
                          := Del_Part;
                        Deletable_Names.Insert (Name_Node.As_Defining_Name);
                     else
                        Component_Deletable := False;
                     end if;
                  end loop;
               end if;
               if Component_Deletable then
                  Records_Deletability (Get_Record_Name (Node)) := Del_Part;
                  Deletable_Components.Insert
                    (Component_node.As_Defining_Name_List);
               else
                  Record_Deletable := False;
               end if;
            end if;
            return LALCO.Into;
         end find_all_defining_name;

         -----------------------
         -- Delete_Components --
         -----------------------

         function Delete_Components (Component_node : LAL.Ada_Node'Class)
                                    return LALCO.Visit_Status
         is
         begin
            if Component_node.Kind = LALCO.Ada_Defining_Name_List then
               if Deletable_Components.Contains
                 (Component_node.As_Defining_Name_List)
               then
                  for Parent of Component_node.Parents (False) loop
                     if Parent.Kind = LALCO.Ada_Component_Decl then
                        Deletable_Range := LAL.Sloc_Range (Parent);
                        Delete_Range (Deletable_Range => Deletable_Range,
                                      Filename => Component_node
                                      .Unit.Get_Filename);
                     end if;
                  end loop;
               else
                  Delete_Names_in_List (Component_node => Component_node);
               end if;
            end if;
            return LALCO.Into;
         end Delete_Components;
      begin
         for component of LAL.F_Components (Node).F_Components loop
            LAL.Traverse (component.As_Ada_Node,
                          find_all_defining_name'Access);
         end loop;
         if Record_Deletable then
            Records_Deletability (Get_Record_Name (Node)) := Del_All;
            Deletable_Range := LAL.Sloc_Range (Node);
            Delete_Range (Deletable_Range => Deletable_Range,
                          Filename => Node.Unit.Get_Filename,
                          Text => Ada.Strings.Unbounded
                          .To_Unbounded_String ("null record"));
         else
            for component of LAL.F_Components (Node).F_Components loop
               LAL.Traverse (component.As_Ada_Node,
                             Delete_Components'Access);
            end loop;
         end if;
      end Count_Reference;

   begin
      for Unit of Unit_Array loop
         Unit.Root.Traverse (Find_Record_Node'Access);
      end loop;

      for Record_Node of RV loop
         Records_Deletability.Include (Get_Record_Name (Record_Node),
                                       Del_None);
         Count_Reference (Record_Node);
         if Records_Deletability (Get_Record_Name (Record_Node)) /= Del_None
         then
            Record_Deletable_Name.Include (Get_Record_Name (Record_Node),
                                       Deletable_Names);
            Delete_Record_Reference (Record_Node);
            Record_Texts_Map.Include (Record_Node, Texts_Edit);
            Texts_Edit.Clear;
            Deletable_Names.Clear;
         end if;
      end loop;
      return (Record_Deletable_Name, Record_Texts_Map);
   end Find_Unused_Components;

   ---------
   -- Run --
   ---------

   procedure Run is
      package Project_Tree renames Project;
      Env     : GPR.Project_Environment_Access;
      Project : constant GPR.Project_Tree_Access :=
        new GPR.Project_Tree;

      use type GNATCOLL.VFS.Filesystem_String;

      Context  : LAL.Analysis_Context;
      Provider : LAL.Unit_Provider_Reference;
      Project_Filename : constant String
        := Ada.Strings.Unbounded.To_String (Project_Tree.Get);
      My_Project_File     : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create (+Project_Filename);
      Edit_Info : Delete_Infos;
   begin
      --  Ada.Text_IO.Put_Line ("Record_Components");
      --  Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Project.Get));
      GPR.Initialize (Env);
      --  Use procedures in GNATCOLL.Projects to set scenario
      --  variables (Change_Environment), to set the target
      --  and the runtime (Set_Target_And_Runtime), etc.

      Project.Load (My_Project_File, Env);
      Provider := LAL_GPR.Create_Project_Unit_Provider
        (Tree => Project, Env => Env);
      Context := LAL.Create_Context (Unit_Provider => Provider);

      declare
         Source_Files : constant
           Libadalang.Project_Provider.Filename_Vectors.Vector
             := Libadalang.Project_Provider.Source_Files
               (Project.all, Libadalang.Project_Provider.Root_Project);
         AUA : LAL.Analysis_Unit_Array (Source_Files.First_Index
                                        .. Source_Files.Last_Index);
         Stream : aliased VSS.Text_Streams.Memory_UTF8_Output.
        Memory_UTF8_Output_Stream;
      begin
         for I in Source_Files.First_Index .. Source_Files.Last_Index loop
            declare
               Unit     : constant LAL.Analysis_Unit :=
                 Context.Get_From_File (Ada.Strings.Unbounded.To_String
                                        (Source_Files.Element (I)));
            begin
               --  Report parsing errors, if any
               if Unit.Has_Diagnostics then
                  for D of Unit.Diagnostics loop
                     Put_Line (Unit.Format_GNU_Diagnostic (D));
                  end loop;
                  --  Otherwise, look for object declarations
               else
                  AUA (I) := Unit;
               end if;
            end;
         end loop;
         Edit_Info := Find_Unused_Components (AUA);
         Output.JSON_Serialize (Edit_Info, Stream);
         Put_Line (VSS.Stream_Element_Vectors.Conversions.Unchecked_To_String
           (Stream.Buffer));
      end;
   end Run;

end Tools.Record_Components_Tool;
