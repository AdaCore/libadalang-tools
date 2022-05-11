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

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Common; use Libadalang.Common;

with Laltools.Common; use Laltools.Common;
with Laltools.Refactor.Pull_Up_Declaration;
with Laltools.Refactor.Subprogram_Signature;

package body Laltools.Refactor.Introduce_Parameter is

   -------------------------
   -- Introduce_Parameter --
   -------------------------

   overriding
   function Introduce_Parameter
     (Self           : Parameter_From_Object_Decl_Introducer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      use Laltools.Refactor.Subprogram_Signature;
      use Laltools.Refactor.Pull_Up_Declaration;

      Object_Decl         : constant Libadalang.Analysis.Object_Decl :=
        Self.Definition.P_Basic_Decl.As_Object_Decl;
      Enclosing_Subp_Body : constant Subp_Body :=
        Object_Decl.P_Parent_Basic_Decl.As_Subp_Body;

      Parameter_Adder        :
        constant Laltools.Refactor.Subprogram_Signature.Parameter_Adder :=
          Create
            (Unit          => Enclosing_Subp_Body.Unit,
             Location      =>
               End_Sloc (Enclosing_Subp_Body.F_Subp_Spec.Sloc_Range),
             New_Parameter =>
               To_Unbounded_String
                 (To_UTF8
                    (Self.Definition.Text
                     & " : "
                     & Object_Decl.F_Type_Expr.Text
                     & " := "
                     & Object_Decl.F_Default_Expr.Text)));
      Declaration_Pull_Upper : constant Declaration_Extractor :=
        Create_Declaration_Pull_Upper
          (Unit                     => Object_Decl.Unit,
           Declaration_SLOC         =>
             Start_Sloc (Object_Decl.Sloc_Range),
           Indentation              => 3,
           Only_Dependencies        => True,
           Try_Subp_Insertion_Point => True);

      Text_Edits : Text_Edit_Map;

   begin
      Merge
        (Source => Text_Edits,
         Target => Parameter_Adder.Refactor (Analysis_Units).Text_Edits);
      Merge
        (Source => Text_Edits,
         Target =>
           Declaration_Pull_Upper.Refactor (Analysis_Units).Text_Edits);

      Safe_Insert
        (Edits     => Text_Edits,
         File_Name => Object_Decl.Unit.Get_Filename,
         Edit      =>
           Text_Edit'
             (Location => Expand_SLOC_Range (Object_Decl),
              Text     => Null_Unbounded_String));

      return
        Refactoring_Edits'
          (Text_Edits     => Text_Edits,
           File_Creations => File_Creation_Ordered_Sets.Empty_Set,
           File_Deletions => Unbounded_String_Ordered_Sets.Empty_Set,
           File_Renames   => File_Rename_Ordered_Sets.Empty_Set,
           Diagnostics    => Refactoring_Diagnotic_Vectors.Empty_Vector);
   end Introduce_Parameter;

   -------------------------
   -- Introduce_Parameter --
   -------------------------

   overriding
   function Introduce_Parameter
     (Self           : Parameter_From_Expr_Introducer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      use Laltools.Refactor.Subprogram_Signature;

      Target_Expression_Expected_Type : constant Base_Type_Decl :=
        Self.Expr.P_Expected_Expression_Type;
      Target_Expression_Type          : constant Base_Type_Decl :=
        (if Target_Expression_Expected_Type.Is_Null then
           Self.Expr.P_Expression_Type
         else
           Target_Expression_Expected_Type);

      Parent_Subp : constant Subp_Body :=
        Self.Expr.P_Semantic_Parent.As_Subp_Body;

      Introduced_Parameter_Name : constant Unbounded_String :=
        To_Unbounded_String ("Introduced_Parameter");
      Introduced_Parameter_Type : constant Unbounded_String :=
        To_Unbounded_String
          (To_UTF8 (Target_Expression_Type.F_Name.Text));
      Introduced_Parameter_Spec : constant Unbounded_String :=
        Introduced_Parameter_Name & " : " & Introduced_Parameter_Type;

      Parameter_Adder :
        constant Laltools.Refactor.Subprogram_Signature.Parameter_Adder :=
          Create
            (Unit          => Parent_Subp.Unit,
             Location      => End_Sloc (Parent_Subp.F_Subp_Spec.Sloc_Range),
             New_Parameter => Introduced_Parameter_Spec);

      Text_Edits : Text_Edit_Map;

      procedure Process_Base_Id
        (Base_Id : Libadalang.Analysis.Base_Id'Class)
        with Pre => not Base_Id.Is_Null;
      --  Finds all references of Base_Id and replaces them by the
      --  introduced parameter.

      procedure Process_Dotted_Name
        (Dotted_Name : Libadalang.Analysis.Dotted_Name'Class)
        with Pre => not Dotted_Name.Is_Null;
      --  Finds all references of Dotted_Name and replaces them by the
      --  introduced parameter.

      procedure Process_Attribute_Ref
        (Attribute_Ref : Libadalang.Analysis.Attribute_Ref'Class)
        with Pre => not Attribute_Ref.Is_Null;
      --  An Attribute_Ref is written as "Some_Name'Some_Attribute". This
      --  procedure will find all references of "Some_Name" in its enclosing
      --  subprogram, and check if it as the same "Some_Attribute" as
      --  Self.Expr. If so, replaces this reference by the introduced
      --  parameter.

      procedure Process_Other_Expressions
        (Expr : Libadalang.Analysis.Expr'Class)
        with Pre => not Expr.Is_Null;
      --  For any kind of Self.Expr that is not one of the above, simply
      --  replace Self.Expr by the introduced parameter.

      procedure Process_Expr_References;
      --  Processes Self.Expr references according to its type

      ---------------------
      -- Process_Base_Id --
      ---------------------

      procedure Process_Base_Id
        (Base_Id : Libadalang.Analysis.Base_Id'Class)
      is
         Base_Id_Parent : constant Ada_Node := Base_Id.Parent;

      begin
         if Base_Id_Parent.Kind in Ada_Dotted_Name_Range
           and then Base_Id_Parent.As_Dotted_Name.F_Suffix = Base_Id.As_Name
         then
            Process_Dotted_Name (Base_Id_Parent.As_Dotted_Name);

         else
            declare
               Base_Id_Definition : constant Defining_Name :=
                 Resolve_Name_Precisely (Base_Id.As_Name);

               References : constant Ref_Result_Array :=
                 (if Base_Id_Definition.Is_Null then []
                  else Base_Id_Definition.P_Find_Refs (Parent_Subp));

            begin
               for Reference of References loop
                  Safe_Insert
                    (Edits     => Text_Edits,
                     File_Name => Self.Expr.Unit.Get_Filename,
                     Edit      =>
                       Text_Edit'
                         (Location => Ref (Reference).Sloc_Range,
                          Text     => Introduced_Parameter_Name));
               end loop;
            end;
         end if;
      end Process_Base_Id;

      -------------------------
      -- Process_Dotted_Name --
      -------------------------

      procedure Process_Dotted_Name
        (Dotted_Name : Libadalang.Analysis.Dotted_Name'Class)
      is
         Dotted_Name_Definition  : constant Defining_Name :=
           Dotted_Name.P_Referenced_Defining_Name;
         Dotted_Name_Definitions : constant Defining_Name_Array :=
           Get_Dotted_Name_Definitions (Dotted_Name);

         References : constant Ref_Result_Array :=
           (if Dotted_Name_Definition.Is_Null
              or else (for some Definition of Dotted_Name_Definitions
                       => Definition.Is_Null)
            then
              []
            else
              Dotted_Name_Definition.P_Find_Refs (Parent_Subp));

      begin
         for Reference of References loop
            declare
               Reference_Base_Id     : constant Base_Id'Class :=
                 Ref (Reference);
               Reference_Definitions : constant Defining_Name_Array :=
                 (if Reference_Base_Id.Parent.Kind in Ada_Dotted_Name then
                    Get_Dotted_Name_Definitions
                      (Reference_Base_Id.Parent.As_Dotted_Name)
                  else
                    []);

            begin
               if Dotted_Name_Definitions = Reference_Definitions then
                  Safe_Insert
                    (Edits     => Text_Edits,
                     File_Name => Self.Expr.Unit.Get_Filename,
                     Edit      =>
                       Text_Edit'
                         (Location => Ref (Reference).Parent.Sloc_Range,
                          Text     => Introduced_Parameter_Name));
               end if;
            end;
         end loop;
      end Process_Dotted_Name;

      ---------------------------
      -- Process_Attribute_Ref --
      ---------------------------

      procedure Process_Attribute_Ref
        (Attribute_Ref : Libadalang.Analysis.Attribute_Ref'Class) is
      begin
         case Attribute_Ref.F_Prefix.Kind is
            when Ada_Base_Id  =>
               declare
                  Base_Id_Definition : constant Defining_Name :=
                    Attribute_Ref.F_Prefix.P_Referenced_Defining_Name;

                  References : constant Ref_Result_Array :=
                    (if Base_Id_Definition.Is_Null then []
                     else Base_Id_Definition.P_Find_Refs (Parent_Subp));

               begin
                  for Reference of References loop
                     if Ref (Reference).Parent.Kind in Ada_Attribute_Ref
                       and then Ref (Reference).Parent.As_Attribute_Ref.
                                  F_Attribute.Text =
                                    Attribute_Ref.F_Attribute.Text
                     then
                        Safe_Insert
                          (Edits     => Text_Edits,
                           File_Name => Self.Expr.Unit.Get_Filename,
                           Edit      =>
                             Text_Edit'
                               (Location => Ref (Reference).Parent.Sloc_Range,
                                Text     => Introduced_Parameter_Name));
                     end if;
                  end loop;
               end;

            when Ada_Dotted_Name =>
               declare
                  Dotted_Name_Definition  : constant Defining_Name :=
                    Attribute_Ref.F_Prefix.P_Referenced_Defining_Name;
                  Dotted_Name_Definitions : constant Defining_Name_Array :=
                    Get_Dotted_Name_Definitions
                      (Attribute_Ref.F_Prefix.As_Dotted_Name);

                  References : constant Ref_Result_Array :=
                    (if Dotted_Name_Definition.Is_Null
                       or else (for some Definition of Dotted_Name_Definitions
                                => Definition.Is_Null)
                     then
                       []
                     else
                       Dotted_Name_Definition.P_Find_Refs (Parent_Subp));

               begin
                  for Reference of References loop
                     declare
                        Reference_Base_Id     : constant Base_Id'Class :=
                            Ref (Reference);
                        Reference_Definitions : constant Defining_Name_Array :=
                          (if Reference_Base_Id.Parent.Kind in
                                Ada_Dotted_Name
                             and then Reference_Base_Id.Parent.Parent.Kind in
                                        Ada_Attribute_Ref
                             and then Reference_Base_Id.Parent.Parent.
                                        As_Attribute_Ref.F_Attribute.Text =
                                          Attribute_Ref.F_Attribute.Text
                           then
                             Get_Dotted_Name_Definitions
                               (Reference_Base_Id.Parent.As_Dotted_Name)
                           else
                              []);

                     begin
                        if Dotted_Name_Definitions = Reference_Definitions then
                           Safe_Insert
                             (Edits     => Text_Edits,
                              File_Name => Self.Expr.Unit.Get_Filename,
                              Edit      =>
                                Text_Edit'
                                  (Location =>
                                     Ref (Reference).Parent.Parent.Sloc_Range,
                                   Text     => Introduced_Parameter_Name));
                        end if;
                     end;
                  end loop;
               end;

            when others =>
               null;
         end case;
      end Process_Attribute_Ref;

      -------------------------------
      -- Process_Other_Expressions --
      -------------------------------

      procedure Process_Other_Expressions
        (Expr : Libadalang.Analysis.Expr'Class) is
      begin
         Safe_Insert
           (Edits     => Text_Edits,
            File_Name => Expr.Unit.Get_Filename,
            Edit      =>
              Text_Edit'
                (Location => Expr.Sloc_Range,
                 Text     => Introduced_Parameter_Name));
      end Process_Other_Expressions;

      -----------------------------
      -- Process_Expr_References --
      -----------------------------

      procedure Process_Expr_References is
      begin
         case Self.Expr.Kind is
            when Ada_Base_Id =>
               Process_Base_Id (Self.Expr.As_Base_Id);

            when Ada_Dotted_Name_Range =>
               Process_Dotted_Name (Self.Expr.As_Dotted_Name);

            when Ada_Attribute_Ref_Range =>
               Process_Attribute_Ref (Self.Expr.As_Attribute_Ref);

            when others =>
               Process_Other_Expressions (Self.Expr);
         end case;
      end Process_Expr_References;

   begin
      Merge
        (Source => Text_Edits,
         Target => Parameter_Adder.Refactor (Analysis_Units).Text_Edits);

      Process_Expr_References;

      return
        Refactoring_Edits'
          (Text_Edits     => Text_Edits,
           File_Creations => File_Creation_Ordered_Sets.Empty_Set,
           File_Deletions => Unbounded_String_Ordered_Sets.Empty_Set,
           File_Renames   => File_Rename_Ordered_Sets.Empty_Set,
           Diagnostics    => Refactoring_Diagnotic_Vectors.Empty_Vector);
   end Introduce_Parameter;

   ---------------------------------------------
   -- Is_Object_Decl_With_Enclosing_Subp_Body --
   ---------------------------------------------

   function Is_Object_Decl_With_Enclosing_Subp_Body
     (Node : Ada_Node'Class)
      return Boolean
   is (not Node.Is_Null
       and then Node.Kind in Ada_Name
       and then not Node.As_Name.P_Enclosing_Defining_Name.Is_Null
       and then not Node.As_Name.P_Enclosing_Defining_Name.P_Basic_Decl.
                      Is_Null
       and then Node.As_Name.P_Enclosing_Defining_Name.P_Basic_Decl.Kind in
                  Ada_Object_Decl_Range
       and then not Node.As_Name.P_Enclosing_Defining_Name.P_Basic_Decl.
                      P_Parent_Basic_Decl.Is_Null
       and then Node.As_Name.P_Enclosing_Defining_Name.P_Basic_Decl.
                  P_Parent_Basic_Decl.Kind in Ada_Subp_Body_Range);

   --------------------------------------------------------
   -- Is_Expr_With_Non_Null_Type_And_Enclosing_Subp_Body --
   --------------------------------------------------------

   function Is_Expr_With_Non_Null_Type_And_Enclosing_Subp_Body
     (Node : Ada_Node'Class)
      return Boolean
   is (not Node.Is_Null
       and then Node.Kind in Ada_Expr
       and then (not Node.As_Expr.P_Expected_Expression_Type.Is_Null
                       or else not Node.As_Expr.P_Expression_Type.Is_Null)
       and then not Node.P_Semantic_Parent.Is_Null
       and then Node.P_Semantic_Parent.Kind in Ada_Subp_Body);

   --------------------------------------
   -- Is_Introduce_Parameter_Available --
   --------------------------------------

   function Is_Introduce_Parameter_Available
     (Unit       : Analysis_Unit;
      SLOC_Range : Source_Location_Range)
      return Boolean
   is
      Start_Node : constant Ada_Node :=
        Unit.Root.Lookup (Start_Sloc (SLOC_Range));
      End_Node   : constant Ada_Node :=
        Unit.Root.Lookup (End_Sloc (SLOC_Range));

      Enclosing_Parent : constant Ada_Node :=
        Find_First_Common_Parent (Start_Node, End_Node);

   begin
      return Is_Object_Decl_With_Enclosing_Subp_Body (Enclosing_Parent)
               or else Is_Expr_With_Non_Null_Type_And_Enclosing_Subp_Body
                         (Enclosing_Parent);
   end Is_Introduce_Parameter_Available;

   ---------------------------------
   -- Create_Parameter_Introducer --
   ---------------------------------

   function Create_Parameter_Introducer
     (Unit       : Analysis_Unit;
      SLOC_Range : Source_Location_Range)
      return Parameter_Introducer
   is
      Start_Node : constant Ada_Node :=
        Unit.Root.Lookup (Start_Sloc (SLOC_Range));
      End_Node   : constant Ada_Node :=
        Unit.Root.Lookup (End_Sloc (SLOC_Range));

      Enclosing_Parent : constant Ada_Node :=
        Find_First_Common_Parent (Start_Node, End_Node);

   begin
      return Parameter_Introducer'(Target => Enclosing_Parent);
   end Create_Parameter_Introducer;

   ---------------------
   -- Define_Strategy --
   ---------------------

   function Define_Strategy
     (Self : Parameter_Introducer)
      return Introduction_Strategy'Class is
   begin
      if Is_Object_Decl_With_Enclosing_Subp_Body (Self.Target) then
         return Parameter_From_Object_Decl_Introducer'
                 (Definition =>
                    Self.Target.As_Name.P_Enclosing_Defining_Name);
      elsif Is_Expr_With_Non_Null_Type_And_Enclosing_Subp_Body
              (Self.Target)
      then
         return Parameter_From_Expr_Introducer'(Expr => Self.Target.As_Expr);
      else
         raise Program_Error with "Failed to define introduction strategy";
      end if;
   end Define_Strategy;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Parameter_Introducer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is (Self.Define_Strategy.Introduce_Parameter (Analysis_Units));

end Laltools.Refactor.Introduce_Parameter;
