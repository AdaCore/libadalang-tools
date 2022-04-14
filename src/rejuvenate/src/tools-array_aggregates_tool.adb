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
with GNATCOLL.VFS;
with Libadalang.Common;
with GNATCOLL.Projects;
with Libadalang.Project_Provider;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Ada.Containers.Hashed_Sets;
with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;
with VSS.Stream_Element_Vectors.Conversions;
with Output;
with VSS.Text_Streams;
with VSS.Text_Streams.Memory_UTF8_Output;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Tools.Array_Aggregates_Tool is
   package LALCO renames Libadalang.Common;
   package GPR renames GNATCOLL.Projects;
   package LAL_GPR renames Libadalang.Project_Provider;

   use type LALCO.Ada_Node_Kind_Type;

   function Defining_Name_Hash (Element : LAL.Defining_Name)
                                return Ada.Containers.Hash_Type is
   (LAL.Hash (Element.As_Ada_Node));

   package Defining_Name_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => LAL.Defining_Name,
        Hash                => Defining_Name_Hash,
        Equivalent_Elements => LAL."=",
        "="                 => LAL."=");

   ---------
   -- "<" --
   ---------

   function "<" (L, R : LAL.Aggregate) return Boolean
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

   ------------------------------
   -- Find_Arrays_To_Aggregate --
   ------------------------------

   function Find_Arrays_To_Aggregate (Unit_Array : LAL.Analysis_Unit_Array)
                                    return Aggregates_To_Edit_Text.Map is
      Edit_Texts : Aggregates_To_Edit_Text.Map;
      --  This Map stores all the aggregates to edit_info.

      Array_Type_Names : Defining_Name_Sets.Set;
      --  This Set stores all the name defined as array type.

      function Find_Array_Def (Node : LAL.Ada_Node'Class)
                             return LALCO.Visit_Status;
      --  The Final goal is find All the defining name which
      --  is defined as the Array type in the project
      --  First step: find all the Array_Type_Def node
      --  Should have two type: Anonymous / Array_Type_Decl
      --  If it's anonymous, it can only be a Object Declare
      --  If it's Type_Decl, We get the type name and find
      --  all the reference.

      procedure Modify_Aggregate (Node : LAL.Aggregate'Class);
      --  This procedure generates the edit_info for the aggregate

      function Find_Aggregate (Aggregate_Node : LAL.Ada_Node'Class)
                               return LALCO.Visit_Status;
      --  Find all the aggregate node in the project

      procedure Safe_Insert
        (Edits : in out ReFac.Text_Edit_Ordered_Set;
         Edit  : ReFac.Text_Edit);
      --  insert the text_edit information to the set

      procedure Safe_Insert
        (Edits          : in out Aggregates_To_Edit_Text.Map;
         Aggregate_Node : LAL.Aggregate;
         Edit           : ReFac.Text_Edit);
      --  insert the text_edit information with aggregate to the
      --  Aggregates_to_edit_text map

      -----------------
      -- Safe_Insert --
      -----------------

      procedure Safe_Insert
        (Edits : in out ReFac.Text_Edit_Ordered_Set;
         Edit  : ReFac.Text_Edit) is
      begin
         if not Edits.Contains (Edit) then
            Edits.Insert (Edit);
         end if;
      end Safe_Insert;

      -----------------
      -- Safe_Insert --
      -----------------

      procedure Safe_Insert
        (Edits          : in out Aggregates_To_Edit_Text.Map;
         Aggregate_Node : LAL.Aggregate;
         Edit           : ReFac.Text_Edit)
      is
         Edits_Set : ReFac.Text_Edit_Ordered_Set;
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

      function Find_Array_Def (Node : LAL.Ada_Node'Class)
                               return LALCO.Visit_Status is
         procedure Process_Decl (Node : LAL.Basic_Decl'Class);
         --  Store all the definig names

         procedure Find_All_Ref_Type (Node : LAL.Ada_Node'Class);
         --  Find all the references for the type_decl

         -----------------
         -- Process_Obj --
         -----------------

         procedure Process_Decl (Node : LAL.Basic_Decl'Class) is
         begin
            for Type_Name of Node.P_Defining_Names loop
               Array_Type_Names.Include (Type_Name);
            end loop;
         end Process_Decl;

         -----------------------
         -- Find_All_Ref_Type --
         -----------------------

         procedure Find_All_Ref_Type (Node : LAL.Ada_Node'Class) is
            Grand_P : constant LAL.Ada_Node := Node.Parent.Parent;
         begin
            case Grand_P.Kind is
               when LALCO.Ada_Basic_Decl =>
                  Process_Decl (Grand_P.As_Basic_Decl);
               when LALCO.Ada_Component_Def =>
                  if Grand_P.Parent.Kind in LALCO.Ada_Basic_Decl then
                     Process_Decl (Grand_P.Parent.As_Basic_Decl);
                  end if;
               when others =>
                  null;
                  --  Put_Line ("This Array Type with name"
                  --            & Text.Image (Node.Text)
                  --            & " is referenced with the situation: "
                  --            & Text.Image (Grand_P.Text));
                  --  Put_Line (Grand_P.Unit.Get_Filename);
                  --  Attention here!
                  --  There should be lots of case that without a name so
                  --  in the final version we should set others=> null
                  --  However here in order to test if we have some ignored
                  --  case, we put this print to debug.
            end case;
         end Find_All_Ref_Type;

      begin
         if Node.Kind = LALCO.Ada_Array_Type_Def then
            if Node.Parent.Kind = LALCO.Ada_Type_Decl then
               declare
                  Type_Name : LAL.Defining_Name;
               begin
                  Type_Name := LAL.F_Name (Node.Parent.As_Base_Type_Decl);
                  Array_Type_Names.Include (Type_Name);
                  for Type_Ref of LAL.P_Find_All_References
                    (Type_Name, Unit_Array) loop
                     Find_All_Ref_Type (Type_Ref.Ref.As_Ada_Node);
                  end loop;
               end;
            else
               Process_Decl (Node.Parent.Parent.Parent.As_Basic_Decl);
            end if;
         end if;
         return LALCO.Into;
      end Find_Array_Def;

      --------------------
      -- Find_Aggregate --
      --------------------

      function Find_Aggregate (Aggregate_Node : LAL.Ada_Node'Class)
                               return LALCO.Visit_Status is
         function Is_Array (Node : LAL.Ada_Node'Class) return Boolean;
         --  Check if this aggregate is a Array type

         --------------
         -- Is_Array --
         --------------

         function Is_Array (Node : LAL.Ada_Node'Class) return Boolean is
            Father : constant LAL.Ada_Node := Node.Parent;
            Grand_P : constant LAL.Ada_Node := Father.Parent;
            Type_Array : Boolean := False;

            function Process_Assign_Stmt (Node : LAL.Assign_Stmt'Class)
                                       return Boolean;
            --  check if a aggregate in assign_stmt is a Array type.

            function Process_Return_Stmt (Node : LAL.Return_Stmt'Class)
                                          return Boolean;
            --  check if a aggregate in return_stmt is a Array type.

            -------------------------
            -- Process_Assign_Stmt --
            -------------------------

            function Process_Assign_Stmt (Node : LAL.Assign_Stmt'Class)
                                       return Boolean is
            begin
               if Node.F_Dest.P_Expression_Type.Is_Null then
                  return False;
               end if;
               return Node.F_Dest.P_Expression_Type.P_Is_Array_Type;
            end Process_Assign_Stmt;

            function Process_Return_Stmt (Node : LAL.Return_Stmt'Class)
                                          return Boolean is
            begin
               return Node.F_Return_Expr.P_Expression_Type.P_Is_Array_Type;
            end Process_Return_Stmt;
         begin
            if Aggregate_Node.As_Base_Aggregate.P_Expression_Type.Is_Null then
               case Father.Kind is
               when LALCO.Ada_Assign_Stmt_Range =>
                  Type_Array := Process_Assign_Stmt (Father.As_Assign_Stmt);
               when LALCO.Ada_Paren_Expr_Range =>
                  if Grand_P.Kind in LALCO.Ada_Expr_Function_Range then
                     Type_Array := Grand_P.As_Expr_Function.F_Subp_Spec
                       .F_Subp_Returns.P_Designated_Type_Decl.P_Is_Array_Type;
                  end if;
               when LALCO.Ada_Qual_Expr_Range =>
                  if Grand_P.Kind in LALCO.Ada_Assign_Stmt then
                     Type_Array := Process_Assign_Stmt
                       (Grand_P.As_Assign_Stmt);
                  else
                     if Grand_P.Parent.Kind in LALCO.Ada_Return_Stmt_Range
                     then
                        Type_Array := Father.As_Qual_Expr.F_Prefix
                          .P_Name_Designated_Type.P_Is_Array_Type;
                     end if;
                  end if;
               when LALCO.Ada_Basic_Assoc =>
                  if LAL.P_Get_Params (Father.As_Basic_Assoc)'Length /= 0 then
                     if Array_Type_Names.Contains
                       (LAL.P_Get_Params (Father.As_Basic_Assoc) (1))
                     then
                        Type_Array := True;
                     end if;
                  end if;
               when LALCO.Ada_Basic_Decl =>
                  Type_Array := Father.As_Basic_Decl.P_Type_Expression
                    .P_Designated_Type_Decl.P_Is_Array_Type;
               when LALCO.Ada_Bin_Op_Range =>
                  Type_Array := Father.As_Bin_Op
                    .P_Expression_Type.P_Is_Array_Type;
               when LALCO.Ada_Return_Stmt =>
                  Type_Array := Process_Return_Stmt (Father.As_Return_Stmt);
               when LALCO.Ada_If_Expr =>
                  Type_Array := Is_Array (Father);
               when LALCO.Ada_Aspect_Assoc_Range =>
                  if not Father.As_Aspect_Assoc.F_Expr
                    .P_Expression_Type.Is_Null
                  then
                     Type_Array := Father.As_Aspect_Assoc.F_Expr
                       .P_Expression_Type.P_Is_Array_Type;
                  end if;
               when LALCO.Ada_Enum_Rep_Clause_Range =>
                  null;
               when LALCO.Ada_Elsif_Expr_Part_Range =>
                  if Grand_P.Parent.Parent.Kind in LALCO.Ada_If_Expr_Range then
                     Type_Array := Is_Array (Grand_P.Parent.Parent);
                  end if;
               when others =>
                  null;
               end case;
            else
               Type_Array := Aggregate_Node.As_Base_Aggregate.P_Expression_Type
                 .P_Is_Array_Type;
            end if;
            return Type_Array;
         end Is_Array;

      begin
         if Aggregate_Node.Kind = LALCO.Ada_Aggregate then
            if Is_Array (Aggregate_Node) then
               Modify_Aggregate (Aggregate_Node.As_Aggregate);
            end if;
         end if;
         return LALCO.Into;
      end Find_Aggregate;

      ----------------------
      -- Modify_Aggregate --
      ----------------------

      procedure Modify_Aggregate (Node : LAL.Aggregate'Class) is
         Assoc_List_Node : constant LAL.Assoc_List
           := LAL.F_Assocs (Node.As_Base_Aggregate);
         Location_Delete : constant Source_Location_Range
           := LAL.Sloc_Range (Node);
         Grand_Child : LAL.Ada_Node;
         Text_Delete : ReFac.Text_Edit;

         procedure Paren_To_Square;
         --  this procedure replace () with [] for all array aggregates
         --  **note: this method only changes the () but no others.

         procedure Change_Null;
         --  replace empty aggregates (in particular we can recognize
         --  the common (1 .. 0 => xxxx) case by []

         procedure Change_One (Val_Location : Source_Location_Range);
         --  replace one element aggregates
         --  when the index is the first index of the
         --  array by removing the index. In other words, replace:
         --  (1 => xxxx) by [xxxx]
         --  (1..1 => xxxx) by [xxxx] as well

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
            Text_Delete.Location.Start_Line := Location_Delete.Start_Line;
            Text_Delete.Location.End_Line := Location_Delete.Start_Line;
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
               if Grand_Child.Kind in LALCO.Ada_Bin_Op_Range then
                  declare
                     Start_Val : constant LAL.Expr :=
                       Grand_Child.As_Bin_Op.F_Left;
                     End_Val : constant LAL.Expr :=
                       Grand_Child.As_Bin_Op.F_Right;
                  begin
                     if Start_Val.Kind in LALCO.Ada_Int_Literal_Range and
                       End_Val.Kind in LALCO.Ada_Int_Literal_Range
                     then
                        if Start_Val.P_Eval_As_Int > End_Val.P_Eval_As_Int then
                           Change_Null;
                        end if;
                        if Start_Val.P_Eval_As_Int < End_Val.P_Eval_As_Int then
                           Paren_To_Square;
                        end if;
                        if Start_Val.P_Eval_As_Int = End_Val.P_Eval_As_Int then
                           Change_One
                             (LAL.Sloc_Range
                                (Grand_Child.Parent.Next_Sibling));
                        end if;
                     else
                        Paren_To_Square;
                     end if;
                  end;
               else
                  Change_One (LAL.Sloc_Range
                              (Grand_Child.Parent.Next_Sibling));
               end if;
            when others =>
               Paren_To_Square;
         end case;

      end Modify_Aggregate;
   begin
      for Unit of Unit_Array loop
         Unit.Root.Traverse (Find_Array_Def'Access);
      end loop;
      for Unit of Unit_Array loop
         Unit.Root.Traverse (Find_Aggregate'Access);
      end loop;
      return Edit_Texts;
   end Find_Arrays_To_Aggregate;

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
      Edit_Info : Aggregates_To_Edit_Text.Map;
   begin
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
         Source_Files : Libadalang.Project_Provider.Filename_Vectors.Vector;
         Stream : aliased VSS.Text_Streams.Memory_UTF8_Output
           .Memory_UTF8_Output_Stream;
      begin
         if Source.Get /= Ada.Strings.Unbounded.Null_Unbounded_String then
            Source_Files := Libadalang.Project_Provider.Source_Files
              (Project.all, Libadalang.Project_Provider.Root_Project);
         else
            Source_Files := Libadalang.Project_Provider.Source_Files
               (Project.all, Libadalang.Project_Provider.Whole_Project);
         end if;
         declare
            AUA : LAL.Analysis_Unit_Array (Source_Files.First_Index
                                           .. Source_Files.Last_Index);
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
            Edit_Info := Find_Arrays_To_Aggregate (AUA);
            Output.JSON_Serialize (Edit_Info, Stream);
            Put_Line (VSS.Stream_Element_Vectors.Conversions
                      .Unchecked_To_String (Stream.Buffer));
         end;
      end;
   end Run;

end Tools.Array_Aggregates_Tool;
