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

with Ada.Containers.Hashed_Sets;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Lint.File_Edits;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Common; use Libadalang.Common;

with Lint.Command_Line;

with VSS.Strings.Conversions;

package body Lint.Tools.Array_Aggregates_Tool is
   use Libadalang.Analysis;

   function To_Text_Edit_Map
     (Map : Aggregate_Edits)
      return Laltools.Refactor.Text_Edit_Map;
   --  Converts an Aggregates_To_Text_Edit_Ordered_Set_Map into a Text_Edit_Map

   function Defining_Name_Hash
     (Element : Defining_Name)
      return Ada.Containers.Hash_Type is
     (Hash (Element.As_Ada_Node));
   --  Casts Element as Ada_Node and dispatches to Libdalang.Analysis.Hash.

   package Defining_Name_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Defining_Name,
        Hash                => Defining_Name_Hash,
        Equivalent_Elements => "=",
        "="                 => "=");

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Aggregate) return Boolean
   is
   begin
      if L.Unit.Get_Filename < R.Unit.Get_Filename then
         return True;
      else
         if L.Unit.Get_Filename > R.Unit.Get_Filename then
            return False;
         else
            return (Start_Sloc (Sloc_Range (L)) <
                      Start_Sloc (Sloc_Range (R)));
         end if;
      end if;
   end "<";

   function To_Text_Edit_Map
     (Map : Aggregate_Edits)
      return Laltools.Refactor.Text_Edit_Map
   is
      use Laltools.Refactor;

      Result : Text_Edit_Map;

   begin
      for Map_Entry in Map.Iterate loop
         for Text_Edit of Map_Entry.Element loop
            Safe_Insert
              (Result, Map_Entry.Key.Unit.Get_Filename, Text_Edit);
         end loop;
      end loop;
      return Result;
   end To_Text_Edit_Map;

   ------------------------------
   -- Upgrade_Array_Aggregates --
   ------------------------------

   function Upgrade_Array_Aggregates
     (Units : Analysis_Unit_Array)
      return Aggregate_Edits
   is
      Edit_Texts : Aggregate_Edits;
      --  This Map stores all the aggregates to edit_info.

      Array_Type_Names : Defining_Name_Sets.Set;
      --  This Set stores all the name defined as array type.

      function Find_Array_Def
        (Node : Ada_Node'Class)
         return Visit_Status;
      --  The Final goal is find All the defining name which
      --  is defined as the Array type in the project
      --  First step: find all the Array_Type_Def node
      --  Should have two type: Anonymous / Array_Type_Decl
      --  If it's anonymous, it can only be a Object Declare
      --  If it's Type_Decl, We get the type name and find
      --  all the reference.

      procedure Modify_Aggregate (Node : Aggregate'Class);
      --  This procedure generates the edit_info for the aggregate

      function Find_Aggregate
        (Aggregate_Node : Ada_Node'Class)
         return Visit_Status;
      --  Find all the aggregate node in the project

      procedure Safe_Insert
        (Edits : in out Laltools.Refactor.Text_Edit_Ordered_Set;
         Edit  : Laltools.Refactor.Text_Edit);
      --  insert the text_edit information to the set

      procedure Safe_Insert
        (Edits          : in out Aggregates_To_Text_Edit_Ordered_Set_Maps.Map;
         Aggregate_Node : Aggregate;
         Edit           : Laltools.Refactor.Text_Edit);
      --  insert the text_edit information with aggregate to the
      --  Aggregates_to_edit_text map

      -----------------
      -- Safe_Insert --
      -----------------

      procedure Safe_Insert
        (Edits : in out Laltools.Refactor.Text_Edit_Ordered_Set;
         Edit  : Laltools.Refactor.Text_Edit) is
      begin
         if not Edits.Contains (Edit) then
            Edits.Insert (Edit);
         end if;
      end Safe_Insert;

      -----------------
      -- Safe_Insert --
      -----------------

      procedure Safe_Insert
        (Edits          : in out Aggregates_To_Text_Edit_Ordered_Set_Maps.Map;
         Aggregate_Node : Aggregate;
         Edit           : Laltools.Refactor.Text_Edit)
      is
         Edits_Set : Laltools.Refactor.Text_Edit_Ordered_Set;
      begin
         if Edits.Contains (Aggregate_Node) then
            Safe_Insert (Edits.Reference (Aggregate_Node), Edit);
         else
            Edits_Set.Insert (Edit);
            Edits.Insert (Aggregate_Node, Edits_Set);
         end if;
      end Safe_Insert;

      --------------------
      -- Find_Body_Node --
      --------------------

      function Find_Array_Def
        (Node : Ada_Node'Class)
         return Visit_Status
      is
         procedure Process_Decl (Node : Basic_Decl'Class);
         --  Store all the definig names

         procedure Find_All_Ref_Type (Node : Ada_Node'Class);
         --  Find all the references for the type_decl

         -----------------
         -- Process_Obj --
         -----------------

         procedure Process_Decl (Node : Basic_Decl'Class) is
         begin
            for Type_Name of Node.P_Defining_Names loop
               Array_Type_Names.Include (Type_Name);
            end loop;
         end Process_Decl;

         -----------------------
         -- Find_All_Ref_Type --
         -----------------------

         procedure Find_All_Ref_Type (Node : Ada_Node'Class) is
            Grand_P : constant Ada_Node := Node.Parent.Parent;
         begin
            case Grand_P.Kind is
               when Ada_Basic_Decl =>
                  Process_Decl (Grand_P.As_Basic_Decl);
               when Ada_Component_Def =>
                  if Grand_P.Parent.Kind in Ada_Basic_Decl then
                     Process_Decl (Grand_P.Parent.As_Basic_Decl);
                  end if;
               when others =>
                  null;
            end case;
         end Find_All_Ref_Type;

      begin
         if Node.Kind = Ada_Array_Type_Def then
            if Node.Parent.Kind in Ada_Concrete_Type_Decl then
               declare
                  Type_Name : constant Defining_Name :=
                    F_Name (Node.Parent.As_Base_Type_Decl);

               begin
                  if not Type_Name.Is_Null then
                     Array_Type_Names.Include (Type_Name);
                     begin
                        for Type_Ref of
                          P_Find_All_References (Type_Name, Units)
                        loop
                           Find_All_Ref_Type (Type_Ref.Ref.As_Ada_Node);
                        end loop;
                     exception
                        when Property_Error => null;
                     end;
                  end if;
               end;
            elsif Node.Parent.Parent.Parent.Kind in Ada_Basic_Decl then
               Process_Decl (Node.Parent.Parent.Parent.As_Basic_Decl);
            end if;
         end if;
         return Into;
      end Find_Array_Def;

      --------------------
      -- Find_Aggregate --
      --------------------

      function Find_Aggregate
        (Aggregate_Node : Ada_Node'Class)
         return Visit_Status
      is
         function Is_Array (Node : Ada_Node'Class) return Boolean;
         --  Check if this aggregate is a Array type

         --------------
         -- Is_Array --
         --------------

         function Is_Array (Node : Ada_Node'Class) return Boolean is
            Father : constant Ada_Node := Node.Parent;
            Grand_P : constant Ada_Node := Father.Parent;
            Type_Array : Boolean := False;

            function Process_Assign_Stmt (Node : Assign_Stmt'Class)
                                       return Boolean;
            --  check if a aggregate in assign_stmt is a Array type.

            function Process_Return_Stmt (Node : Return_Stmt'Class)
                                          return Boolean;
            --  check if a aggregate in return_stmt is a Array type.

            -------------------------
            -- Process_Assign_Stmt --
            -------------------------

            function Process_Assign_Stmt (Node : Assign_Stmt'Class)
                                       return Boolean is
            begin
               if Node.F_Dest.P_Expression_Type.Is_Null then
                  return False;
               end if;
               return Node.F_Dest.P_Expression_Type.P_Is_Array_Type;
            end Process_Assign_Stmt;

            function Process_Return_Stmt (Node : Return_Stmt'Class)
                                          return Boolean is
            begin
               return Node.F_Return_Expr.P_Expression_Type.P_Is_Array_Type;
            end Process_Return_Stmt;
         begin
            begin
               if Aggregate_Node.As_Base_Aggregate.
                    P_Expression_Type.Is_Null
               then
                  case Father.Kind is
                  when Ada_Assign_Stmt_Range =>
                     Type_Array := Process_Assign_Stmt (Father.As_Assign_Stmt);
                  when Ada_Paren_Expr_Range =>
                     if Grand_P.Kind in Ada_Expr_Function_Range then
                        Type_Array :=
                          Grand_P.As_Expr_Function.F_Subp_Spec.F_Subp_Returns.
                            P_Designated_Type_Decl.P_Is_Array_Type;
                     end if;
                  when Ada_Qual_Expr_Range =>
                     if Grand_P.Kind in Ada_Assign_Stmt then
                        Type_Array := Process_Assign_Stmt
                          (Grand_P.As_Assign_Stmt);
                     else
                        if Grand_P.Parent.Kind in Ada_Return_Stmt_Range
                        then
                           Type_Array := Father.As_Qual_Expr.F_Prefix
                             .P_Name_Designated_Type.P_Is_Array_Type;
                        end if;
                     end if;
                  when Ada_Basic_Assoc =>
                     if P_Get_Params (Father.As_Basic_Assoc)'Length /= 0 then
                        if Array_Type_Names.Contains
                          (P_Get_Params (Father.As_Basic_Assoc) (1))
                        then
                           Type_Array := True;
                        end if;
                     end if;
                  when Ada_Basic_Decl =>
                     Type_Array := Father.As_Basic_Decl.P_Type_Expression
                       .P_Designated_Type_Decl.P_Is_Array_Type;
                  when Ada_Bin_Op_Range =>
                     Type_Array := Father.As_Bin_Op
                       .P_Expression_Type.P_Is_Array_Type;
                  when Ada_Return_Stmt =>
                     Type_Array := Process_Return_Stmt (Father.As_Return_Stmt);
                  when Ada_If_Expr =>
                     Type_Array := Is_Array (Father);
                  when Ada_Aspect_Assoc_Range =>
                     if not Father.As_Aspect_Assoc.F_Expr
                       .P_Expression_Type.Is_Null
                     then
                        Type_Array := Father.As_Aspect_Assoc.F_Expr
                          .P_Expression_Type.P_Is_Array_Type;
                     end if;
                  when Ada_Enum_Rep_Clause_Range =>
                     null;
                  when Ada_Elsif_Expr_Part_Range =>
                     if Grand_P.Parent.Parent.Kind in Ada_If_Expr_Range then
                        Type_Array := Is_Array (Grand_P.Parent.Parent);
                     end if;
                  when others =>
                     null;
                  end case;
               else
                  Type_Array :=
                    Aggregate_Node.As_Base_Aggregate.P_Expression_Type.
                      P_Is_Array_Type;
               end if;
            exception
               when Property_Error => null;
            end;
            return Type_Array;
         end Is_Array;

      begin
         if Aggregate_Node.Kind = Ada_Aggregate then
            if Is_Array (Aggregate_Node) then
               Modify_Aggregate (Aggregate_Node.As_Aggregate);
            end if;
         end if;
         return Into;
      end Find_Aggregate;

      ----------------------
      -- Modify_Aggregate --
      ----------------------

      procedure Modify_Aggregate (Node : Aggregate'Class) is
         Assoc_List_Node : constant Assoc_List
           := F_Assocs (Node.As_Base_Aggregate);
         Location_Delete : constant Source_Location_Range
           := Sloc_Range (Node);
         Grand_Child : Ada_Node;
         Text_Delete : Laltools.Refactor.Text_Edit;

         procedure Paren_To_Square;
         --  Replace (<array_aggregate_contents>) with
         --  [<array_aggregate_contents>]

         procedure Change_Null;
         --  Replace empty aggregates of the form (1..0 => <value>) by []

         procedure Change_One (Val_Location : Source_Location_Range);
         --  Replace one element aggregates by [Value]

         ---------------------
         -- Paren_To_Square --
         ---------------------

         procedure Paren_To_Square is
         begin
            Text_Delete.Location.Start_Column :=
              Location_Delete.Start_Column;
            Text_Delete.Location.End_Column :=
              Location_Delete.Start_Column + 1;
            Text_Delete.Location.Start_Line := Location_Delete.Start_Line;
            Text_Delete.Location.End_Line := Location_Delete.Start_Line;
            Text_Delete.Text :=
              Ada.Strings.Unbounded.To_Unbounded_String ("[");
            Safe_Insert (Edit_Texts, Node.As_Aggregate, Text_Delete);

            Text_Delete.Location.Start_Column
              := Location_Delete.End_Column - 1;
            Text_Delete.Location.End_Column := Location_Delete.End_Column;
            Text_Delete.Location.Start_Line := Location_Delete.End_Line;
            Text_Delete.Location.End_Line := Location_Delete.End_Line;
            Text_Delete.Text :=
              Ada.Strings.Unbounded.To_Unbounded_String ("]");
            Safe_Insert (Edit_Texts, Node.As_Aggregate, Text_Delete);
         end Paren_To_Square;

         ----------------
         -- Change_One --
         ----------------

         procedure Change_One (Val_Location : Source_Location_Range) is
         begin
            Text_Delete.Location.Start_Column := Location_Delete.Start_Column;
            Text_Delete.Location.End_Column := Val_Location.Start_Column;
            Text_Delete.Location.Start_Line := Location_Delete.Start_Line;
            Text_Delete.Location.End_Line := Val_Location.Start_Line;
            Text_Delete.Text :=
              Ada.Strings.Unbounded.To_Unbounded_String ("[");
            Safe_Insert (Edit_Texts, Node.As_Aggregate, Text_Delete);

            Text_Delete.Location.Start_Column :=
              Location_Delete.End_Column - 1;
            Text_Delete.Location.End_Column := Location_Delete.End_Column;
            Text_Delete.Location.Start_Line := Location_Delete.End_Line;
            Text_Delete.Location.End_Line := Location_Delete.End_Line;
            Text_Delete.Text :=
              Ada.Strings.Unbounded.To_Unbounded_String ("]");
            Safe_Insert (Edit_Texts, Node.As_Aggregate, Text_Delete);
         end Change_One;

         -----------------
         -- Change_Null --
         -----------------

         procedure Change_Null is
         begin
            Text_Delete.Location.Start_Column := Location_Delete.Start_Column;
            Text_Delete.Location.End_Column := Location_Delete.End_Column;
            Text_Delete.Location.Start_Line := Location_Delete.Start_Line;
            Text_Delete.Location.End_Line := Location_Delete.End_Line;
            Text_Delete.Text :=
              Ada.Strings.Unbounded.To_Unbounded_String ("[]");
            Safe_Insert (Edit_Texts, Node.As_Aggregate, Text_Delete);
         end Change_Null;

      begin
         case Assoc_List_Node.Children'Length is
            when 1 =>
               Grand_Child := Assoc_List_Node.Child (1).As_Aggregate_Assoc
                 .F_Designators.Child (1);
               if Grand_Child.Kind in Ada_Bin_Op_Range then
                  declare
                     Start_Val : constant Expr :=
                       Grand_Child.As_Bin_Op.F_Left;
                     End_Val : constant Expr :=
                       Grand_Child.As_Bin_Op.F_Right;
                  begin
                     if Start_Val.Kind in Ada_Int_Literal_Range and
                       End_Val.Kind in Ada_Int_Literal_Range
                     then
                        if Start_Val.P_Eval_As_Int > End_Val.P_Eval_As_Int then
                           Change_Null;
                        elsif Start_Val.P_Eval_As_Int <
                                End_Val.P_Eval_As_Int
                        then
                           Paren_To_Square;
                        else
                           Change_One
                             (Sloc_Range
                                (Grand_Child.Parent.Next_Sibling));
                        end if;
                     else
                        Paren_To_Square;
                     end if;
                  end;

               elsif Grand_Child.Kind in Ada_Others_Designator then
                  Paren_To_Square;

               elsif Grand_Child.Kind in
                 Ada_Identifier | Ada_Dotted_Name_Range
               then
                  --  The designator can either be an index or the entire index
                  --  type.
                  --  Try to resolve it and check if it resolved to a
                  --  Base_Type_Decl. If it was not able to resolve, play safe
                  --  and simply change to square brackets but keep the
                  --  index.
                  declare
                     Index_Decl : constant Basic_Decl :=
                       Grand_Child.As_Name.P_Referenced_Decl;

                  begin
                     if Index_Decl.Is_Null
                       or else Index_Decl.Kind in Ada_Base_Type_Decl
                     then
                        Paren_To_Square;

                     else
                        Change_One
                          (Sloc_Range
                             (Grand_Child.Parent.Next_Sibling));
                     end if;
                  end;

               else
                  Change_One
                    (Sloc_Range
                       (Grand_Child.Parent.Next_Sibling));
               end if;
            when others =>
               Paren_To_Square;
         end case;
      end Modify_Aggregate;

      use Ada.Strings;
      use Ada.Strings.Fixed;

      Units_Count : constant String  := Trim (Units'Length'Image, Both);

   begin
      if Units'Length > 0 then
         Lint.Logger.Trace ("Finding all array definitions");
         for J in Units'Range loop
            Log_Progress
              (J, Units_Count, "Processing " & Units (J).Get_Filename);
            Units (J).Root.Traverse (Find_Array_Def'Access);
         end loop;
         Lint.Logger.Trace ("Finding all array objects");
         for J in Units'Range loop
            Log_Progress
              (J, Units_Count, "Processing " & Units (J).Get_Filename);
            Units (J).Root.Traverse (Find_Aggregate'Access);
         end loop;
      end if;

      return Edit_Texts;
   end Upgrade_Array_Aggregates;

   ------------------------------
   -- Upgrade_Array_Aggregates --
   ------------------------------

   function Upgrade_Array_Aggregates
     (Units : Analysis_Unit_Array)
      return Laltools.Refactor.Text_Edit_Map
   is (To_Text_Edit_Map (Upgrade_Array_Aggregates (Units)));

   ---------
   -- Run --
   ---------

   procedure Run is
      use Lint.File_Edits;

      Units : constant Analysis_Unit_Array :=
        Get_Project_Analysis_Units
          (Ada.Strings.Unbounded.To_String
             (Lint.Command_Line.Project.Get));

      Edits : constant Laltools.Refactor.Text_Edit_Map :=
        Upgrade_Array_Aggregates (Units);

   begin
      if Lint.Command_Line.Pipe.Get then
         declare
            use Ada.Text_IO;
            use File_Name_To_Virtual_String_Maps;
            use VSS.Strings.Conversions;

            File_Edits        : constant File_Name_To_Virtual_String_Map :=
              Apply_Edits (Edits);
            File_Edits_Cursor : Cursor := File_Edits.First;

         begin
            while Has_Element (File_Edits_Cursor) loop
               Put_Line (Key (File_Edits_Cursor));
               Put_Line (To_UTF_8_String (Element (File_Edits_Cursor)));
               Next (File_Edits_Cursor);
            end loop;
         end;

      else
         Apply_Edits (Edits);
      end if;
   end Run;

end Lint.Tools.Array_Aggregates_Tool;
