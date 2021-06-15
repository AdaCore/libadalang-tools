------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Assertions; use Ada.Assertions;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Common; use Libadalang.Common;

with Laltools.Subprogram_Hierarchy; use Laltools.Subprogram_Hierarchy;

package body Laltools.Refactor.Safe_Rename is

   function Check_Rename_Conflict
     (New_Name : Unbounded_Text_Type;
      Target   : Defining_Name'Class)
      return Boolean is (Target.F_Name.Text = To_Text (New_Name))
     with Pre => Target /= No_Defining_Name;
   --  Checks if Target's name is equal to New_Name
   --  FIXME: Do a case insensitive comparison

   function Check_Subp_Rename_Conflict
     (Subp_A   : Subp_Spec;
      New_Name : Unbounded_Text_Type;
      Subp_B   : Subp_Spec)
      return Boolean;
   --  Checks if renaming Subp_A to New_Name causes a conflict with Subp_B.
   --  This includes checking if both subprograms are type conformant.

   procedure Initialize_Unit_Slocs_Maps
     (Unit_References      : out Unit_Slocs_Maps.Map;
      Canonical_Definition : Defining_Name;
      References           : Base_Id_Vectors.Vector);
   --  Unitializes Unit_References with Canonical_Definition own reference
   --  and with all its references given by References.

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Source_Location_Range)
                 return Boolean is
   begin
      if Left.Start_Line = Right.Start_Line then
         return Left.Start_Column < Right.Start_Column;
      else
         return Left.Start_Line < Right.Start_Line;
      end if;
   end "<";

   --------------------------------
   -- Check_Subp_Rename_Conflict --
   --------------------------------

   function Check_Subp_Rename_Conflict
     (Subp_A   : Subp_Spec;
      New_Name : Unbounded_Text_Type;
      Subp_B   : Subp_Spec)
      return Boolean is
   begin
      if Subp_A = Subp_B
        or else Text (Subp_B.F_Subp_Name.F_Name) /= To_Text (New_Name)
      then
         return False;
      end if;

      --  Subp_B name is the name as New_Name, therefore, we need to check if
      --  both subprograms are type conformant.

      return Are_Subprograms_Type_Conformant (Subp_A, Subp_B, False);
   end Check_Subp_Rename_Conflict;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Self : in out Reference_Mapper)
   is
      Before : Unit_Slocs_Maps.Map renames Self.Original_References;
      After  : Unit_Slocs_Maps.Map renames Self.New_References;

      N_Char_Diff : constant Integer :=
        Length (Self.New_Name) - Length (Self.Original_Name);

      --  This map will contain the the references that were lost, i.e.,
      --  references that are found in Before but not in After.

      Minus : Unit_Slocs_Maps.Map := Before;

      --  This map will contain the the references that were gained, i.e.,
      --  references that are found in After but not in Before.

      Plus  : Unit_Slocs_Maps.Map := After;

      --  The top datastructure is a Unit_Slocs_Maps.Map which contains a
      --  Slocs_Maps.Map which contains a Slocs_Sets.Set. Therefore, three
      --  cursors are needed to navigate through the datastructure.

      C1 : Unit_Slocs_Maps.Cursor;
      C2 : Slocs_Maps.Cursor;
      C3 : Slocs_Sets.Cursor;

      --  The following three "Process" procedures navigate through Before
      --  and After looking for common Slocs. If a common Slocs is found then
      --  it is removed both from Minus and Plus. Since Minus was initialized
      --  as Before and Plus as After, Minus will contain the lost Slocs and
      --  Plus the gained ones.

      procedure Process_Slocs_Maps;
      --  Iterates through a Slocs_Maps.Map

      procedure Process_Slocs_Sets;
      --  Iterates through a Slocs_Sets.Set

      procedure Process_Slocs (Sloc_Index : Integer);
      --  Checks if a given Sloc of Before is found in After and if so, removes
      --  it from both Minus and Plus. Note that if the same line has multiple
      --  references, then they can have an offset between their Before Sloc
      --  and their After Sloc. This offset is cause because the length of
      --  Self.New_Name can be differente from the length of
      --  Self.Original_Name.

      -----------------------
      -- Process_Slocs_Map --
      -----------------------

      procedure Process_Slocs_Maps is
         use Unit_Slocs_Maps;
         use Slocs_Maps;

      begin
         C2 := Before.Constant_Reference (C1).First;

         while Has_Element (C2) loop
            if After.Constant_Reference (Key (C1)).Contains (Key (C2)) then
               Process_Slocs_Sets;

               --  If the element of the key given by C2 is empty (because all
               --  its Slocs were found in After) then delete this key.

               if Minus.Constant_Reference (Key (C1)).
                 Constant_Reference (Key (C2)).Length = 0
               then
                  Minus.Reference (Key (C1)).Delete (Key (C2));
               end if;

               --  If the element of the key given by C2 is empty (because
               --  Begore also has all its Slocs) then delete this key.

               if Plus.Constant_Reference (Key (C1)).
                 Constant_Reference (Key (C2)).Length = 0
               then
                  Plus.Reference (Key (C1)).Delete (Key (C2));
               end if;
            end if;

            Next (C2);
         end loop;
      end Process_Slocs_Maps;

      ------------------------
      -- Process_Slocs_Sets --
      ------------------------

      procedure Process_Slocs_Sets
      is
         use Unit_Slocs_Maps;
         use Slocs_Maps;
         use Slocs_Sets;

         Sloc_Index : Integer := 0;

      begin
         C3 := Before.Constant_Reference (C1).Constant_Reference (C2).First;

         while Has_Element (C3) loop
            Process_Slocs (Sloc_Index);
            Sloc_Index := Sloc_Index + 1;
            Next (C3);
         end loop;
      end Process_Slocs_Sets;

      -------------------
      -- Process_Slocs --
      -------------------

      procedure Process_Slocs (Sloc_Index : Integer)
      is
         use Unit_Slocs_Maps;
         use Slocs_Maps;
         use Slocs_Sets;

         --  Apply the offset to the Beforre Slocs

         Target_Start_Colum : constant Integer :=
           Integer (Element (C3).Start_Column) +  Sloc_Index * N_Char_Diff;
         Target_End_Column  : constant Integer :=
           Integer (Element (C3).End_Column) + (Sloc_Index + 1) * N_Char_Diff;

         Target_Sloc : constant Source_Location_Range :=
           (Start_Line   => Element (C3).Start_Line,
            End_Line     => Element (C3).End_Line,
            Start_Column => Column_Number (Target_Start_Colum),
            End_Column   => Column_Number (Target_End_Column));
      begin
         if After.Constant_Reference (Key (C1)).Constant_Reference (Key (C2))
           .Contains (Target_Sloc)
         then
            Minus.Reference (Key (C1)).Reference (Key (C2)).
              Delete (Element (C3));
            Plus.Reference (Key (C1)).Reference (Key (C2)).
              Delete (Target_Sloc);
         end if;
      end Process_Slocs;

      use Unit_Slocs_Maps;
      use type Ada.Containers.Count_Type;

   begin
      C1 := Before.First;

      while Has_Element (C1) loop
         if After.Contains (Key (C1)) then
            Process_Slocs_Maps;

            --  If the element of the key given by C1 is empty (because all
            --  its Slocs were found in After) then delete this key.

            if Minus.Element (Key (C1)).Length = 0 then
               Minus.Delete (Key (C1));
            end if;

            --  If the element of the key given by C1 is empty (because
            --  Begore also has all its Slocs) then delete this key.

            if Plus.Element (Key (C1)).Length = 0 then
               Plus.Delete (Key (C1));
            end if;
         end if;

         Next (C1);
      end loop;

      Self.References_Diff := (Minus, Plus);
   end Diff;

   -----------
   --  Find --
   -----------

   overriding
   function Find
     (Self : in out AST_Analyser)
      return Rename_Problem_Vectors.Vector
   is
      Problem_Finders : Specific_Rename_Problem_Finder_Vectors.Vector;
      Problems : Rename_Problem_Vectors.Vector;

   begin
      --  These are commons checks that need to happens independently of the
      --  kind of construct that we are renaming.

      Problem_Finders.Append
        (Name_Collision_Finder'
           (Canonical_Definition => Self.Canonical_Definition,
            New_Name             => Self.New_Name));
      Problem_Finders.Append
        (Collision_With_Compilation_Unit_Finder'
           (Units_Length         => Self.Units'Length,
            Canonical_Definition => Self.Canonical_Definition,
            New_Name             => Self.New_Name,
            Units                => Self.Units));
      Problem_Finders.Append
        (Name_Hiding_Finder'
           (Canonical_Definition => Self.Canonical_Definition,
            New_Name             => Self.New_Name));

      --  If we're trying to rename a subprogram, then check if this subprogram
      --  will collide with a compilation unit or if it will override another
      --  one.

      if Self.Canonical_Definition.P_Basic_Decl.Kind in
        Ada_Subp_Body | Ada_Subp_Decl
      then
         Problem_Finders.Append
           (Compilation_Unit_Collision_Finder'
              (Units_Length         => Self.Units'Length,
               Canonical_Definition => Self.Canonical_Definition,
               New_Name             => Self.New_Name,
               Units                => Self.Units));
         Problem_Finders.Append
           (Subp_Overriding_Finder'
              (Canonical_Definition => Self.Canonical_Definition,
               New_Name             => Self.New_Name));

         --  If we're trying to rename a package, then check if this package
         --  defines a compilation unit, and if so, look for conflicts with
         --  other compilation units.

      elsif Self.Canonical_Definition.P_Basic_Decl.Kind in
        Ada_Package_Decl
      then
         Problem_Finders.Append
           (Compilation_Unit_Collision_Finder'
              (Units_Length         => Self.Units'Length,
               Canonical_Definition => Self.Canonical_Definition,
               New_Name             => Self.New_Name,
               Units                => Self.Units));

         --  If we're trying to rename a subprogram parameter, then check if
         --  its subtype indication has the same name.

      elsif Self.Canonical_Definition.P_Basic_Decl.Kind in Ada_Param_Spec then
         Problem_Finders.Append
           (Param_Spec_Collision_Finder'
              (Canonical_Definition => Self.Canonical_Definition,
               New_Name             => Self.New_Name,
               Reference            => No_Base_Id));
      end if;

      Problem_Finders.Append
        (Subtype_Indication_Collision_Finder'
           (Canonical_Definition => Self.Canonical_Definition,
            References           => Self.Original_References_Ids,
            New_Name             => Self.New_Name));
      Problem_Finders.Append
        (Name_Hidden_Finder'
           (Canonical_Definition => Self.Canonical_Definition,
            References           => Self.Original_References_Ids,
            New_Name             => Self.New_Name));

      for Finder of Problem_Finders loop
         declare
            Problem : constant Rename_Problem'Class := Finder.Find;
         begin
            if Problem /= No_Rename_Problem then
               Problems.Append (Problem);
            end if;
         end;
      end loop;

      return Problems;
   end Find;

   -----------
   --  Find --
   -----------

   overriding
   function Find
     (Self : in out Reference_Mapper)
      return Rename_Problem_Vectors.Vector
   is
      function Create_Problems return Rename_Problem_Vectors.Vector;
      --  For every Sloc found in Self.References_Diff create either a
      --  Missing_Reference or New_Reference object and add it to a vector.

      ---------------------
      -- Create_Problems --
      ---------------------

      function Create_Problems return Rename_Problem_Vectors.Vector is
         use Unit_Slocs_Maps;
         C : Cursor;
      begin
         return Result : Rename_Problem_Vectors.Vector do
            --  Self.References_Diff.Minus contains all the references that
            --  would be lost.

            C := Self.References_Diff.Minus.First;

            while Has_Element (C) loop
               for Slocs_Set of
                 Constant_Reference (Self.References_Diff.Minus, C)
               loop
                  for Sloc of Slocs_Set loop
                     Result.Append
                       (Missing_Reference'
                          (Canonical_Definition => Self.Canonical_Definition,
                           New_Name             => Self.New_Name,
                           Conflicting_Id       =>
                             Lookup
                               (Node => Key (C).Root,
                                Sloc => Source_Location'
                                  (Line   => Sloc.Start_Line,
                                   Column => Sloc.Start_Column)).As_Name));
                  end loop;
               end loop;

               Next (C);
            end loop;

            --  Self.References_Diff.Plus contains all the references that
            --  would be gained.

            C := Self.References_Diff.Plus.First;

            while Has_Element (C) loop
               for Slocs_Set of
                 Constant_Reference (Self.References_Diff.Plus, C)
               loop
                  for Sloc of Slocs_Set loop
                     Result.Append
                       (New_Reference'
                          (Canonical_Definition => Self.Canonical_Definition,
                           New_Name             => Self.New_Name,
                           Conflicting_Id       =>
                             Lookup
                               (Node => Key (C).Root,
                                Sloc => Source_Location'
                                  (Line   => Sloc.Start_Line,
                                   Column => Sloc.Start_Column)).As_Name));
                  end loop;
               end loop;

               Next (C);
            end loop;
         end return;
      end Create_Problems;

   begin
      Self.Parse_Temporary_Buffers;
      Initialize_Unit_Slocs_Maps
        (Self.New_References,
         Self.Canonical_Definition,
         Find_All_References_For_Renaming
           (Self.Canonical_Definition, Self.Units));
      Self.Diff;
      Self.Parse_Original_Buffers;

      return Create_Problems;
   end Find;

   -----------
   --  Find --
   -----------

   overriding
   function Find
     (Self : Name_Collision_Finder)
      return Rename_Problem'Class
   is
      Local_Scopes : Ada_Node_List_Vectors.Vector renames
        Find_Local_Scopes (Self.Canonical_Definition.P_Basic_Decl);

      Def_Basic_Decl : constant Basic_Decl :=
        Self.Canonical_Definition.P_Basic_Decl;

      --  If Self.Canonical_Definition is associated to a subprogram, then its
      --  spec is needed to check for collisions with other subprograms.

      Original_Subp_Spec : Subp_Spec := No_Subp_Spec;

      function Check_Rename_Conflicts (Scope : Ada_Node_List)
                                       return Defining_Name;
      --  For every declaration of Scope, checks if it has the same name
      --  as Self.New_Name. If so, return a the Defining_Name of the
      --  conflicting declaration.

      function Check_Subp_Rename_Conflicts (Scope : Ada_Node_List)
                                            return Defining_Name;
      --  For every declaration of Scope, checks if it has the same
      --  name as Self.New_Name. Also checks if such declaration is a
      --  subprogram as if so, calls Check_Subp_Rename_Conflict to check
      --  if both are type conformant (if they have the same signature).

      function Process_Scope (Scope : Ada_Node_List)
                              return Defining_Name;
      --  If Self.Canonical_Definition's declaration is a subprogram,
      --  then delegates to Check_Subp_Rename_Conflicts, otherwise, to
      --  Check_Rename_Conflicts.

      ----------------------------
      -- Check_Rename_Conflicts --
      ----------------------------

      function Check_Rename_Conflicts (Scope : Ada_Node_List)
                                       return Defining_Name is
      begin
         for Declaration of Scope loop
            if Declaration.Kind in Ada_Basic_Decl then
               for Definition of
                 Declaration.As_Basic_Decl.P_Defining_Names
               loop
                  if Check_Rename_Conflict (Self.New_Name, Definition) then
                     return Definition;
                  end if;
               end loop;
            end if;
         end loop;
         return No_Defining_Name;
      end Check_Rename_Conflicts;

      ---------------------------------
      -- Check_Subp_Rename_Conflicts --
      ---------------------------------

      function Check_Subp_Rename_Conflicts
        (Scope : Ada_Node_List)
         return Defining_Name is
      begin
         Assert (Original_Subp_Spec /= No_Subp_Spec);

         for Decl of Scope loop
            if Decl.Kind in Ada_Basic_Decl then
               --  Filter the nodes that are not Basic_Decl

               if Decl.As_Basic_Decl.P_Is_Subprogram
                 or else Decl.Kind in Ada_Generic_Subp_Decl_Range
               then
                  --  If Decl is a subprogram, then not only check the name but
                  --  also its signature.

                  if Check_Subp_Rename_Conflict
                    (Original_Subp_Spec,
                     Self.New_Name,
                     Get_Subp_Spec (Decl.As_Basic_Decl))
                  then
                     return Decl.As_Basic_Decl.P_Defining_Name;
                  end if;

               else
                  --  Otherwise just check the name

                  for Definition of Decl.As_Basic_Decl.P_Defining_Names loop
                     if Check_Rename_Conflict (Self.New_Name, Definition) then
                        return Definition;
                     end if;
                  end loop;
               end if;
            end if;
         end loop;

         return No_Defining_Name;
      end Check_Subp_Rename_Conflicts;

      -------------------
      -- Process_Scope --
      -------------------

      function Process_Scope (Scope : Ada_Node_List) return Defining_Name is
      begin
         if Scope = No_Ada_Node_List then
            return No_Defining_Name;
         end if;

         if Def_Basic_Decl.P_Is_Subprogram then
            return Check_Subp_Rename_Conflicts (Scope);

         else
            return Check_Rename_Conflicts (Scope);
         end if;
      end Process_Scope;

   begin
      if Def_Basic_Decl.P_Is_Subprogram
        or else Def_Basic_Decl.Kind in Ada_Generic_Subp_Decl_Range
      then
         Original_Subp_Spec := Get_Subp_Spec (Def_Basic_Decl);
      end if;

      for Scope of Local_Scopes loop
         declare
            Conflicting_Definition : constant Defining_Name
              := Process_Scope (Scope);

         begin
            if Conflicting_Definition /= No_Defining_Name then
               return Name_Collision'
                 (Canonical_Definition => Self.Canonical_Definition,
                  New_Name             => Self.New_Name,
                  Conflicting_Id       => Conflicting_Definition.F_Name);
            end if;
         end;
      end loop;

      return No_Rename_Problem;
   end Find;

   ----------
   -- Find --
   ----------

   overriding
   function Find
     (Self : Collision_With_Compilation_Unit_Finder)
      return Rename_Problem'Class
   is
      Parent_Package : Package_Decl := No_Package_Decl;

      package Compilation_Unit_Vectors is new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Natural,
         Element_Type => Compilation_Unit'Class,
         "="          => "=");

      subtype Compilation_Unit_Vector is Compilation_Unit_Vectors.Vector;

      Compilation_Units : Compilation_Unit_Vector;

   begin
      Find_Parent_Package :
      for Parent of Self.Canonical_Definition.P_Basic_Decl.Parent.Parents loop
         case Parent.Kind is
            when Ada_Package_Body =>
               Parent_Package :=
                 Parent.As_Package_Body.P_Canonical_Part.As_Package_Decl;
               exit Find_Parent_Package;

            when Ada_Package_Decl =>
               Parent_Package := Parent.As_Package_Decl;
               exit Find_Parent_Package;

            when Ada_Subp_Body | Ada_Task_Body | Ada_Decl_Block =>
               return No_Rename_Problem;

            when others =>
               null;
         end case;
      end loop Find_Parent_Package;

      if Parent_Package = No_Package_Decl then
         return No_Rename_Problem;
      end if;

      if Parent_Package.Unit.Root.As_Compilation_Unit.P_Decl /=
        Parent_Package.As_Basic_Decl
      then
         return No_Rename_Problem;
      end if;

      for Unit of Self.Units loop
         if not Unit.Root.Is_Null then
            if Unit.Root.Kind in Ada_Compilation_Unit then
               Compilation_Units.Append (Unit.Root.As_Compilation_Unit);
            elsif Unit.Root.Kind in Ada_Compilation_Unit_List then
               for Comp_Unit of Unit.Root.As_Compilation_Unit_List loop
                  if not Comp_Unit.Is_Null then
                     Compilation_Units.Append (Comp_Unit);
                  end if;
               end loop;
            end if;
         end if;
      end loop;

      for Comp_Unit of Compilation_Units loop
         declare
            Unit_Decl : constant Basic_Decl := Comp_Unit.P_Decl;

         begin
            if Unit_Decl.P_Canonical_Part /= Parent_Package.As_Basic_Decl
              and then Unit_Decl.P_Parent_Basic_Decl.Unit = Parent_Package.Unit
            then
               declare
                  Unit_Decl_Identifier : constant Identifier :=
                    Get_Defining_Name_Id (Unit_Decl.P_Defining_Name);

               begin
                  --  Check if the new name is already used by other unit
                  --  FIXME: Do a case insensitive comparison

                  if Unit_Decl_Identifier.Text = To_Text (Self.New_Name) then
                     return Name_Collision'
                       (Canonical_Definition => Self.Canonical_Definition,
                        New_Name             => Self.New_Name,
                        Conflicting_Id       =>
                          Unit_Decl.P_Defining_Name.F_Name);
                  end if;
               end;
            end if;
         end;
      end loop;

      return No_Rename_Problem;
   end Find;

   ----------
   -- Find --
   ----------

   overriding
   function Find
     (Self : Compilation_Unit_Collision_Finder)
      return Rename_Problem'Class
   is
      Parent_Unit : constant Analysis_Unit :=
        Self.Canonical_Definition.P_Basic_Decl.P_Parent_Basic_Decl.Unit;
      Parent_Package : Package_Decl := No_Package_Decl;

   begin
      if Self.Canonical_Definition.Unit.Root.As_Compilation_Unit.P_Decl
        /= Self.Canonical_Definition.P_Basic_Decl
      then
         return No_Rename_Problem;
      end if;

      Parent_Package :=
        Parent_Unit.Root.As_Compilation_Unit.P_Decl.As_Package_Decl;

      --  There are two kinds of conflicts:
      --
      --  1) Package/Subprogram that defines a compilation unit is renamed to
      --  an already existing declaration in the spec of the parent package.

      if Parent_Unit /= Self.Canonical_Definition.P_Standard_Unit then
         for Node of Get_Package_Decl_Public_Decls (Parent_Package) loop
            if Node.Kind in Ada_Basic_Decl
              and then Check_Rename_Conflict
                (Self.New_Name,
                 Node.As_Basic_Decl.P_Defining_Name)
            then
               return Name_Collision'
                 (Canonical_Definition => Self.Canonical_Definition,
                  New_Name             => Self.New_Name,
                  Conflicting_Id       =>
                    Node.As_Basic_Decl.P_Defining_Name.F_Name);
            end if;
         end loop;

         declare
            Private_Decls : constant Ada_Node_List :=
              Get_Package_Decl_Private_Decls (Parent_Package);
         begin
            if Private_Decls /= No_Ada_Node_List then
               for Node of Private_Decls loop
                  if Node.Kind in Ada_Basic_Decl
                    and then Check_Rename_Conflict
                      (Self.New_Name,
                       Node.As_Basic_Decl.P_Defining_Name)
                  then
                     return Name_Collision'
                       (Canonical_Definition => Self.Canonical_Definition,
                        New_Name             => Self.New_Name,
                        Conflicting_Id       =>
                          Node.As_Basic_Decl.P_Defining_Name.F_Name);
                  end if;
               end loop;
            end if;
         end;
      end if;

      --  2) Package/Subprogram that defines a compilation unit is renamed to
      --  an already existing Package/Subprogram that also defines
      --  a compilation unit and both share the same parent package.

      for Unit of Self.Units loop
         if Unit.Root.As_Compilation_Unit.P_Decl.P_Parent_Basic_Decl.Unit =
           Parent_Unit
         then
            declare
               --  Get the suffix of the declaration of this unit and check.

               Unit_Decl_Identifier : constant Identifier :=
                 Get_Defining_Name_Id
                   (Unit.Root.As_Compilation_Unit.P_Decl.P_Defining_Name);

            begin
               --  Check if Self.New_Name is already used by this unit.

               if Unit_Decl_Identifier.Text = To_Text (Self.New_Name) then
                  return Name_Collision'
                    (Canonical_Definition => Self.Canonical_Definition,
                     New_Name             => Self.New_Name,
                     Conflicting_Id       =>
                       Unit.Root.As_Compilation_Unit.P_Decl.
                         P_Defining_Name.F_Name);
               end if;
            end;
         end if;
      end loop;
      return No_Rename_Problem;
   end Find;

   ----------
   -- Find --
   ----------

   overriding
   function Find
     (Self : Subp_Overriding_Finder)
      return Rename_Problem'Class
   is
      function Check_Subp_Overriding_By_Rename_Conflict
        (Derived_Type : Type_Decl;
         Base_Type    : Type_Decl;
         Subp_Spec_A  : Subp_Spec;
         Subp_Spec_B  : Subp_Spec)
         return Boolean;
      --  Given the following three assumptions:
      --  1) Derived_Type is a tagged typed derived from Base_Type
      --  2) Subp_Spec_A is a spec of a primitive of Derived_Type
      --  3) Subp_Spec_B is a spec of a primitive of Base_Type (and therefore
      --  has been inherited by Derived_Type
      --
      --  Checks is renaming Subp_Spec_A to Self.New_Name will override
      --  Subp_Spec_B.

      ----------------------------------------------
      -- Check_Subp_Overriding_By_Rename_Conflict --
      ----------------------------------------------

      function Check_Subp_Overriding_By_Rename_Conflict
        (Derived_Type : Type_Decl;
         Base_Type    : Type_Decl;
         Subp_Spec_A  : Subp_Spec;
         Subp_Spec_B  : Subp_Spec)
         return Boolean
      is
         Subp_A_Params : Param_Data_Vectors.Vector;
         Subp_B_Params : Param_Data_Vectors.Vector;

         use type Param_Data_Vectors.Vector;

      begin
         if Subp_Spec_A.F_Subp_Kind /= Subp_Spec_B.F_Subp_Kind then
            return False;
         end if;

         Subp_A_Params :=
           Create_Param_Data_Vector (Subp_Spec_A.F_Subp_Params);
         Subp_B_Params :=
           Create_Param_Data_Vector (Subp_Spec_B.F_Subp_Params);

         case Subp_Spec_A.F_Subp_Kind is
            when Ada_Subp_Kind_Procedure =>
               --  This is a procedure, so the base and derived type must
               --  be the type of the first parameter. Therefore, simply
               --  compare the remaining ones.

               Assert (Subp_A_Params.Length > 0);
               Assert (Subp_B_Params.Length > 0);

               Subp_A_Params.Delete_First;
               Subp_B_Params.Delete_First;

               return Subp_A_Params = Subp_B_Params;

            when Ada_Subp_Kind_Function =>
               --  This is a function, so the types can either be in the first
               --  parameter or the the return type. Both the derived and the
               --  base type must be in the same position.

               declare
                  Return_Type_A : constant Basic_Decl :=
                    Subp_Spec_A.P_Return_Type.P_Canonical_Part;
                  Return_Type_B : constant Basic_Decl :=
                    Subp_Spec_B.P_Return_Type.P_Canonical_Part;

               begin
                  --  First check if the return type of subprogram A is the
                  --  the derived type. If so, the return type of subprogram
                  --  B must be the base type. Otherwise, there is no
                  --  overwrite.

                  if Return_Type_A = Derived_Type.P_Canonical_Part then
                     if Return_Type_B /= Base_Type.P_Canonical_Part then
                        return False;
                     else
                        return Subp_A_Params = Subp_B_Params;
                     end if;

                  --  Otherwise check if the first type of subprogram A is the
                  --  derived type. If so, the first type of subprogram B must
                  --  be the base type. Otherwise, there is no overwrite.

                  else
                     if Subp_B_Params.Is_Empty or else
                       Subp_B_Params.First_Element.Param_Type /=
                         Base_Type.P_Canonical_Part
                     then
                        return False;
                     else
                        if not Subp_A_Params.Is_Empty then
                           Subp_A_Params.Delete_First;
                        end if;

                        if not Subp_B_Params.Is_Empty then
                           Subp_B_Params.Delete_First;
                        end if;

                        return Subp_A_Params = Subp_B_Params;
                     end if;
                  end if;
               end;
         end case;
      end Check_Subp_Overriding_By_Rename_Conflict;

      Subprogram_Spec : Subp_Spec := No_Subp_Spec;
      First_Type      : Type_Decl := No_Type_Decl;
      Base_Type       : Base_Type_Decl := No_Base_Type_Decl;

      use type Ada_Node_Kind_Type;

   begin
      if Self.Canonical_Definition.P_Basic_Decl.Kind = Ada_Subp_Decl then
         Subprogram_Spec :=
           Self.Canonical_Definition.P_Basic_Decl.As_Subp_Decl.F_Subp_Spec;
      else
         return No_Rename_Problem;
      end if;

      --  Is Subp_Decl a primitive?

      First_Type :=
        Subprogram_Spec.P_Primitive_Subp_First_Type
          (Imprecise_Fallback => False).As_Type_Decl;

      if First_Type = No_Type_Decl
        or else First_Type.Kind /= Ada_Type_Decl
      then
         return No_Rename_Problem;
      end if;

      --  Is First_Type a derived type?

      Base_Type := First_Type.P_Base_Type;

      if Base_Type = No_Base_Type_Decl
        or else Base_Type.Kind /= Ada_Type_Decl
      then
         return No_Rename_Problem;
      end if;

      --  Does Base_Type's list of all primitives include one that is named
      --  Self.New_Name?

      declare
         Primitives_List : constant Basic_Decl_Array :=
           Base_Type.As_Type_Decl.P_Get_Primitives (Only_Inherited => False);
      begin
         Look_For_Possible_Conflict :
         for Primitive of Primitives_List loop
            if Primitive.P_Defining_Name.F_Name.Text =
              To_Text (Self.New_Name)
            then
               if Check_Subp_Overriding_By_Rename_Conflict
                 (First_Type,
                  Base_Type.As_Type_Decl,
                  Subprogram_Spec,
                  Primitive.P_Canonical_Part.As_Subp_Decl.F_Subp_Spec)
               then
                  return Overriding_Subprogram'
                    (Canonical_Definition => Self.Canonical_Definition,
                     New_Name             => Self.New_Name,
                     Conflicting_Id       =>
                       Primitive.P_Canonical_Part.P_Defining_Name.F_Name);
               end if;
            end if;
         end loop Look_For_Possible_Conflict;
         return No_Rename_Problem;
      end;

   end Find;

   ----------
   -- Find --
   ----------

   overriding
   function Find (Self : Subtype_Indication_Collision_Finder)
                  return Rename_Problem'Class is
   begin
      for Reference of Self.References loop
         if Reference.Parent.Kind = Ada_Subtype_Indication
           and then Reference.Parent.Parent.Kind = Ada_Param_Spec
         then
            for Definition of Reference.Parent.Parent.As_Param_Spec.F_Ids loop
               if Definition.F_Name.Text = To_Text (Self.New_Name) then
                  return Name_Collision'
                    (Canonical_Definition => Self.Canonical_Definition,
                     New_Name             => Self.New_Name,
                     Conflicting_Id       =>
                       Definition.As_Defining_Name.P_Canonical_Part.F_Name);
               end if;
            end loop;
         end if;
      end loop;

      return No_Rename_Problem;
   end Find;

   ----------
   -- Find --
   ----------

   overriding
   function Find
     (Self : Name_Hiding_Finder)
      return Rename_Problem'Class
   is
      Canonical_Decl : constant Basic_Decl :=
        Self.Canonical_Definition.P_Basic_Decl;
      Is_Subprogram  : constant Boolean :=
        Canonical_Decl.P_Is_Subprogram
        or else Self.Canonical_Definition.P_Basic_Decl.Kind
          in Ada_Generic_Subp_Decl_Range;

      Possible_Problem : Hiding_Name;
      Found_Problem    : Boolean := False;

      Visible_Declarative_Parts   : constant Declarative_Part_Vector :=
        Get_CU_Visible_Declarative_Parts
          (Node       => Self.Canonical_Definition.P_Basic_Decl,
           Skip_First => True);
      Use_Units_Public_Parts : constant Declarative_Part_Vector :=
        Get_Use_Units_Public_Parts (Self.Canonical_Definition);

      procedure Check_Declarative_Part
        (Decl_Part : Declarative_Part'Class);
      --  Checks if Decl_Part contains any declaration that can be hidden by
      --  Canonical_Decl. If so, Possible_Problem is filled with the
      --  appropriate information and Found_Problem is set to True.

      procedure Check_Declarative_Part
        (Decl_Part : Declarative_Part'Class) is
      begin
         if Is_Subprogram then
            for Decl of Decl_Part.F_Decls loop
               --  Conflicts can only exist with subprograms and not with other
               --  kind of declarations.

               if Decl.Kind in Ada_Basic_Decl
                 and then (Decl.As_Basic_Decl.P_Is_Subprogram
                           or else Decl.Kind in Ada_Generic_Subp_Decl_Range)
               then
                  if Decl.Kind in Ada_Subp_Renaming_Decl_Range then
                     declare
                        Unwinded_Decl : constant Basic_Decl :=
                          Final_Renamed_Subp
                            (Decl.As_Subp_Renaming_Decl);

                     begin
                        if not Unwinded_Decl.Is_Null
                          and then Check_Subp_Rename_Conflict
                            (Subp_A   => Get_Subp_Spec (Canonical_Decl),
                             New_Name => Self.New_Name,
                             Subp_B   => Get_Subp_Spec (Unwinded_Decl))
                        then
                           Possible_Problem :=
                             (Canonical_Definition =>
                                Self.Canonical_Definition,
                              New_Name             => Self.New_Name,
                              Conflicting_Id       =>
                                Decl.As_Basic_Decl.P_Canonical_Part.
                                  P_Defining_Name.F_Name);
                           Found_Problem := True;
                           return;
                        end if;
                     end;
                  else
                     if Check_Subp_Rename_Conflict
                       (Subp_A   => Get_Subp_Spec (Canonical_Decl),
                        New_Name => Self.New_Name,
                        Subp_B   => Get_Subp_Spec (Decl.As_Basic_Decl))
                     then
                        Possible_Problem :=
                          (Canonical_Definition => Self.Canonical_Definition,
                           New_Name             => Self.New_Name,
                           Conflicting_Id       =>
                             Decl.As_Basic_Decl.P_Canonical_Part.
                               P_Defining_Name.F_Name);
                        Found_Problem := True;
                        return;
                     end if;
                  end if;
               end if;
            end loop;

         else
            for Decl of Decl_Part.F_Decls loop
               --  Conflicts can exists with any kind of declaration except
               --  subprograms.

               if Decl.Kind in Ada_Basic_Decl
                 and then (not (Decl.As_Basic_Decl.P_Is_Subprogram
                                or else Decl.Kind in
                                  Ada_Generic_Subp_Decl_Range))
               then
                  if Check_Rename_Conflict
                    (Self.New_Name,
                     Decl.As_Basic_Decl.P_Canonical_Part.P_Defining_Name)
                  then
                     Possible_Problem :=
                       (Canonical_Definition => Self.Canonical_Definition,
                        New_Name             => Self.New_Name,
                        Conflicting_Id       =>
                          Decl.As_Basic_Decl.P_Canonical_Part.
                            P_Defining_Name.F_Name);
                     Found_Problem := True;
                     return;
                  end if;
               end if;
            end loop;
         end if;
      end Check_Declarative_Part;

   begin
      for Decl_Part of Visible_Declarative_Parts loop
         Check_Declarative_Part (Decl_Part);
         exit when Found_Problem;
      end loop;

      if Found_Problem then
         return Possible_Problem;
      end if;

      for Decl_Part of Use_Units_Public_Parts loop
         Check_Declarative_Part (Decl_Part);
         exit when Found_Problem;
      end loop;

      if Found_Problem then
         return Possible_Problem;
      end if;

      return No_Rename_Problem;
   end Find;

   ----------
   -- Find --
   ----------

   overriding
   function Find (Self : Name_Hidden_Finder) return Rename_Problem'Class
   is
      Canonical_Decl : constant Basic_Decl :=
        Self.Canonical_Definition.P_Basic_Decl;

      Nested_Declarative_Parts : constant Declarative_Part_Vector :=
        Find_Nested_Scopes (Canonical_Decl);
      Own_Declarative_Part     : constant Declarative_Part_Vector :=
        (if Is_Declarative_Part_Owner (Canonical_Decl) then
            Get_Declarative_Parts (Canonical_Decl)
         else
            Declarative_Part_Vectors.Empty_Vector);

      Stop_Node : Ada_Node := No_Ada_Node;
      Dec_Visible_Declarative_Parts : constant Declarative_Part_Vectors.Vector
        := Find_Local_Scopes (Self.Canonical_Definition.P_Basic_Decl);
      Ref_Visible_Declarative_Parts : Declarative_Part_Vectors.Vector;

      --  If we are renaming a subprogram, it only becomes hidden by another
      --  subprogram that is type conformant.

      Is_Subp : constant Boolean :=
        Canonical_Decl.P_Is_Subprogram
        or else Canonical_Decl.Kind in Ada_Generic_Subp_Decl_Range;
      Spec    : constant Subp_Spec :=
        (if Is_Subp then
            Get_Subp_Spec (Canonical_Decl)
         else
            No_Subp_Spec);

      function Check_Conflict (Definition : Defining_Name) return Boolean;
      --  Delegates to Check_Subp_Rename_Conflict after doing necessary
      --  convertions between node types.

      --------------------
      -- Check_Conflict --
      --------------------

      function Check_Conflict (Definition : Defining_Name) return Boolean is
      begin
         if Is_Subp then
            if Definition.P_Basic_Decl.Kind = Ada_Subp_Body then
               return Check_Subp_Rename_Conflict
                 (Subp_A   => Spec,
                  New_Name => Self.New_Name,
                  Subp_B   =>
                    Definition.P_Basic_Decl.As_Subp_Body.F_Subp_Spec);

            elsif Definition.P_Basic_Decl.Kind = Ada_Subp_Decl then
               return Check_Subp_Rename_Conflict
                 (Subp_A   => Spec,
                  New_Name => Self.New_Name,
                  Subp_B   =>
                    Definition.P_Basic_Decl.As_Subp_Decl.F_Subp_Spec);
            end if;
         end if;

         return Check_Rename_Conflict
           (New_Name => Self.New_Name,
            Target   => Definition);
      end Check_Conflict;

   begin
      --  First: Check if there is a declaration with the same name as
      --  Self.New_Name in the nested declarative parts that have visibility of
      --  Self.Canonical_Definition.
      --  Example: a procedure Foo that declares, in a declarative part inside
      --  its Handled_Stmts, a nested procedure also called Foo.

      for Declarative_Part of Nested_Declarative_Parts loop
         for Declaration of Declarative_Part.F_Decls loop
            if Declaration.Kind in Ada_Basic_Decl then
               for Definition of
                 Declaration.As_Basic_Decl.P_Defining_Names
               loop
                  if Check_Conflict (Definition) then
                     return Hidden_Name'
                       (Canonical_Definition => Self.Canonical_Definition,
                        New_Name             => Self.New_Name,
                        Conflicting_Id       => Definition.F_Name);
                  end if;
               end loop;
            end if;
         end loop;
      end loop;

      --  Second: If Self.Canonical_Definition.P_Basic_Decl is a
      --  declarative part owner, then check if there are any declarations with
      --  the same name as Self.New_Name in its own declarative parts.
      --  Example: a procedure Foo that declares, in its own declarative part,
      --  a nested procedure also called Foo.

      for Declarative_Part of Own_Declarative_Part loop
         for Declaration of Declarative_Part.F_Decls loop
            if Declaration.Kind in Ada_Basic_Decl then
               for Definition of
                 Declaration.As_Basic_Decl.P_Defining_Names
               loop
                  if Check_Conflict (Definition) then
                     return Hidden_Name'
                       (Canonical_Definition => Self.Canonical_Definition,
                        New_Name             => Self.New_Name,
                        Conflicting_Id       => Definition.F_Name);
                  end if;
               end loop;
            end if;
         end loop;
      end loop;

      --  Third: Check if Self.Canonical_Definition references will be hidden
      --  by another definition, i.e., if they will become references of
      --  another definition.

      for Reference of Self.References loop
         if Reference.Parent.Parent.Parent.Kind /= Ada_Param_Spec then
            Find_Stop_Node :
            for Parent of Reference.Parents loop
               if Parent.Kind in
                 Ada_Basic_Decl | Ada_Declarative_Part | Ada_Handled_Stmts
               then
                  Stop_Node := Parent;
                  exit Find_Stop_Node;
               end if;
            end loop Find_Stop_Node;

            if Stop_Node /= No_Ada_Node
              and then Stop_Node.Kind = Ada_Param_Spec
            then
               Stop_Node := Stop_Node.Parent.Parent.Parent.Parent;
            end if;

            Ref_Visible_Declarative_Parts := Find_Local_Scopes (Stop_Node);

            for Declarative_Part of Ref_Visible_Declarative_Parts loop
               --  Do not look for conflicts in Self.Canonical_Part own
               --  declarative part, since this would be a name collision
               --  conflict already detected by the Name_Collision_Finder.

               if not Declarative_Part_Vectors.Has_Element
                 (Declarative_Part_Vectors.Find
                    (Dec_Visible_Declarative_Parts, Declarative_Part))
               then
                  Look_For_Conflicts :
                  for Declaration of Declarative_Part.F_Decls loop
                     if Declaration.Kind in Ada_Basic_Decl then
                        --  If Self.Canonical_Definition is found, then it
                        --  can't be hidden, so stop the search.

                        if Declaration.As_Basic_Decl.P_Canonical_Part =
                          Self.Canonical_Definition.P_Basic_Decl
                          or else Declaration = Stop_Node
                        then
                           exit Look_For_Conflicts;
                        end if;

                        for Definition of
                          Declaration.As_Basic_Decl.P_Defining_Names
                        loop
                           if Check_Conflict (Definition) then
                              return Hidden_Name'
                                (Canonical_Definition =>
                                   Self.Canonical_Definition,
                                 New_Name             => Self.New_Name,
                                 Conflicting_Id       => Definition.F_Name);
                           end if;
                        end loop;
                     end if;
                  end loop Look_For_Conflicts;
               else
                  null;
               end if;
            end loop;
         end if;
      end loop;

      return No_Rename_Problem;
   end Find;

   ----------
   -- Find --
   ----------

   overriding
   function Find
     (Self : Param_Spec_Collision_Finder)
      return Rename_Problem'Class
   is
      use type Ada_Node_Kind_Type;

      Param_Spec : Libadalang.Analysis.Param_Spec renames
        Self.Canonical_Definition.P_Basic_Decl.As_Param_Spec;
      Subtype_Indication : Libadalang.Analysis.Subtype_Indication renames
        Param_Spec.F_Type_Expr.As_Subtype_Indication;
      Param_Spec_List : Libadalang.Analysis.Param_Spec_List renames
        Self.Canonical_Definition.P_Basic_Decl.Parent.As_Param_Spec_List;

   begin
      if Self.Canonical_Definition.P_Basic_Decl.Kind = Ada_Param_Spec then
         --  Possible problem 1: Renaming a parameter to the same name as its
         --  subtype indication

         if Param_Spec.F_Type_Expr.Kind = Ada_Subtype_Indication
           and then Subtype_Indication.F_Name.Kind = Ada_Identifier
           and then Subtype_Indication.F_Name.Text = To_Text (Self.New_Name)
         then
            return Name_Collision'
              (Canonical_Definition => Self.Canonical_Definition,
               New_Name             => Self.New_Name,
               Conflicting_Id        => Param_Spec.F_Type_Expr.
                 As_Subtype_Indication.F_Name.P_Referenced_Decl.
                   P_Defining_Name.F_Name);
         end if;

         --  Possible problem 2: Renaming a parameter to the same name as
         --  another parameter

         for Spec of Param_Spec_List loop
            for Spec_Definition of Spec.F_Ids loop
               if Spec_Definition.F_Name.Text = To_Text (Self.New_Name) then
                  return Name_Collision'
                    (Canonical_Definition => Self.Canonical_Definition,
                     New_Name             => Self.New_Name,
                     Conflicting_Id       =>
                       Spec_Definition.As_Defining_Name.F_Name);
               end if;
            end loop;
         end loop;
      end if;

      return No_Rename_Problem;
   end Find;

   -----------------------------------
   -- Find_All_Renamable_References --
   -----------------------------------

   function Find_All_Renamable_References
     (Node           : Ada_Node'Class;
      New_Name       : Unbounded_Text_Type;
      Units          : Analysis_Unit_Array;
      Algorithm_Kind : Problem_Finder_Algorithm_Kind)
      return Renamable_References
   is
      Canonical_Definition : Defining_Name := No_Defining_Name;

      function Initialize_Algorithm return Problem_Finder_Algorithm'Class;
      --  Returns an initialized Problem_Finder_Algorithm depending on
      --  Algorithm_Kind.

      --------------------------
      -- Initialize_Algorithm --
      --------------------------

      function Initialize_Algorithm return Problem_Finder_Algorithm'Class is
      begin
         case Algorithm_Kind is
         when Map_References =>
            return Algorithm : Reference_Mapper
              (Units_Length => Units'Length)
            do
               Algorithm.Initialize
                 (Canonical_Definition => Canonical_Definition,
                  New_Name             => New_Name,
                  Units                => Units);
            end return;

         when Analyse_AST =>
            return Algorithm : AST_Analyser (Units_Length => Units'Length) do
               Algorithm.Initialize
                 (Canonical_Definition => Canonical_Definition,
                  New_Name             => New_Name,
                  Units                => Units);
            end return;
         end case;
      end Initialize_Algorithm;

   begin
      if not Is_Renamable (Node) then
         return References : Renamable_References;
      end if;

      Canonical_Definition :=
        Resolve_Name_Precisely (Get_Node_As_Name (Node.As_Ada_Node));

      declare
         Algorithm : Problem_Finder_Algorithm'Class := Initialize_Algorithm;
         Problems  : constant Rename_Problem_Vectors.Vector := Algorithm.Find;

      begin
         return Renamable_References'
           (References => Algorithm.Get_Original_References,
            Problems   => Problems);
      end;
   end Find_All_Renamable_References;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                 : out AST_Analyser;
      Canonical_Definition : Defining_Name;
      New_Name             : Unbounded_Text_Type;
      Units                : Analysis_Unit_Array) is
   begin
      Self.Canonical_Definition    := Canonical_Definition;
      Self.New_Name                := New_Name;
      Self.Units                   := Units;
      Self.Original_References_Ids :=
        Find_All_References_For_Renaming (Canonical_Definition, Units);
      Initialize_Unit_Slocs_Maps
        (Unit_References      => Self.Original_References,
         Canonical_Definition => Canonical_Definition,
         References           => Self.Original_References_Ids);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                 : out Reference_Mapper;
      Canonical_Definition : Defining_Name;
      New_Name             : Unbounded_Text_Type;
      Units                : Analysis_Unit_Array)
   is
      procedure Initialize_Temporary_Buffers;
      --  For every unit of Self.Original_References initializes a
      --  Self.Temporary_Buffers with a copy of the original buffers but with
      --  the references renamed to New_Name.

      ----------------------------------
      -- Initialize_Temporary_Buffers --
      ----------------------------------

      procedure Initialize_Temporary_Buffers is
         --  Self.Original_References already has all the units that contain
         --  a reference of Canonical_Definition.

         Units_Cursor : Unit_Slocs_Maps.Cursor :=
           Self.Original_References.First;
         Current_Unit : Analysis_Unit;

      begin
         while Unit_Slocs_Maps.Has_Element (Units_Cursor) loop
            Current_Unit := Unit_Slocs_Maps.Key (Units_Cursor);
            Self.Temporary_Buffers.Insert
              (Key      => Current_Unit,
               New_Item => Null_Unbounded_String);

            declare
               Unit_File : File_Type;
               Unit_Filename : constant String :=
                 Unit_Slocs_Maps.Key (Units_Cursor).Get_Filename;
               Slocs : constant Unit_Slocs_Maps.Constant_Reference_Type :=
                 Unit_Slocs_Maps.Constant_Reference
                   (Container => Self.Original_References,
                    Position  => Units_Cursor);
               Slocs_Cursor : Slocs_Maps.Cursor := Slocs.First;
               Line_Number : Positive := 1;
               Line : Unbounded_String := Null_Unbounded_String;

            begin
               --  Open the file associated to the Current_Unit, read every
               --  line replacing all references of Canonical_Definition
               --  by the New_Name. Then update Self.Temporary_Buffers.

               Open (File => Unit_File,
                     Mode => In_File,
                     Name => Unit_Filename);

               while not End_Of_File (Unit_File) loop
                  Line := Get_Line (Unit_File);

                  if Slocs.Contains (Line_Number) then
                     declare
                        --  Cursor needs to be the last Sloc of this line so
                        --  that the start columns of the others slocs are
                        --  not affected.

                        Cursor : Slocs_Sets.Cursor :=
                          Slocs.Constant_Reference (Line_Number).Last;
                     begin
                        while Slocs_Sets.Has_Element (Cursor) loop
                           Replace_Slice
                             (Source => Line,
                              Low    => Positive
                                (Slocs_Sets.Element (Cursor).Start_Column),
                              High   => Natural
                                (Slocs_Sets.Element (Cursor).End_Column - 1),
                              By     => Image (To_Text (Self.New_Name)));
                           Slocs_Sets.Previous (Cursor);
                        end loop;
                     end;

                     Slocs_Maps.Next (Slocs_Cursor);
                  end if;

                  Append (Source   =>
                            Self.Temporary_Buffers.Reference (Current_Unit),
                          New_Item => Line);
                  Append (Source   =>
                            Self.Temporary_Buffers.Reference (Current_Unit),
                          New_Item => Ada.Characters.Latin_1.LF);
                  Line_Number := Line_Number + 1;
               end loop;

               Close (File => Unit_File);

               --  Every reference on this line was replaced.
               Assert (not Slocs_Maps.Has_Element (Slocs_Cursor));
            end;

            Unit_Slocs_Maps.Next (Units_Cursor);
         end loop;
      end Initialize_Temporary_Buffers;

   begin
      Self.Canonical_Definition      := Canonical_Definition;
      Self.Canonical_Definition_Unit := Canonical_Definition.Unit;
      Self.Canonical_Definition_Sloc := Canonical_Definition.Sloc_Range;
      Self.Units                     := Units;
      Self.Original_Name             :=
        To_Unbounded_Text (Canonical_Definition.F_Name.Text);
      Self.New_Name                  := New_Name;
      Initialize_Unit_Slocs_Maps
        (Unit_References      => Self.Original_References,
         Canonical_Definition => Canonical_Definition,
         References           =>
           Find_All_References_For_Renaming (Canonical_Definition, Units));
      Initialize_Temporary_Buffers;
   end Initialize;

   --------------------------------
   -- Initialize_Unit_Slocs_Maps --
   --------------------------------

   procedure Initialize_Unit_Slocs_Maps
     (Unit_References      : out Unit_Slocs_Maps.Map;
      Canonical_Definition : Defining_Name;
      References           : Base_Id_Vectors.Vector)
   is
      procedure Add_Node (Node : Ada_Node'Class);
      --  Add Node to Unit_References.

      --------------
      -- Add_Node --
      --------------

      procedure Add_Node (Node : Ada_Node'Class) is
         Unit : constant Analysis_Unit := Node.Unit;
         New_Sloc : constant Source_Location_Range := Node.Sloc_Range;
         Start_Line : constant Positive := Positive (New_Sloc.Start_Line);
      begin
         Assert (New_Sloc.Start_Line = New_Sloc.End_Line);

         if Unit_References.Contains (Unit) then
            declare
               References : constant Unit_Slocs_Maps.Reference_Type :=
                 Unit_References.Reference (Unit);

            begin
               if References.Contains (Start_Line) then
                  if not
                    References.Reference (Start_Line).Contains (New_Sloc)
                  then
                     References.Reference (Start_Line).Insert (New_Sloc);
                  end if;

               else
                  declare
                     New_Set : Slocs_Sets.Set;

                  begin
                     New_Set.Insert (New_Sloc);
                     References.Insert (Start_Line, New_Set);
                  end;
               end if;
            end;

         else
            declare
               New_Set : Slocs_Sets.Set;
               New_Map : Slocs_Maps.Map;

            begin
               New_Set.Insert (New_Sloc);
               New_Map.Insert (Start_Line, New_Set);
               Unit_References.Insert (Unit, New_Map);
            end;
         end if;
      end Add_Node;
   begin
      --  P_Find_All_References does not include Canonical_Definition own
      --  reference, so add it here.

      Add_Node (Canonical_Definition);

      for Reference of References loop
         Add_Node (Reference);
      end loop;
   end Initialize_Unit_Slocs_Maps;

   ----------
   -- Info --
   ----------

   overriding
   function Info (Self : Hidden_Name) return String is
   begin
      return "Renaming " & Image (Self.Canonical_Definition.F_Name.Text)
        & " to " & Image (To_Text (Self.New_Name))
        & " will hide it by "
        & Self.Conflicting_Id.Image;
   end Info;

   ----------
   -- Info --
   ----------

   overriding
   function Info (Self : Hiding_Name) return String is
   begin
      return "Renaming " & Image (Self.Canonical_Definition.F_Name.Text)
        & " to " & Image (To_Text (Self.New_Name))
        & " hides "
        & Self.Conflicting_Id.Image;
   end Info;

   ----------
   -- Info --
   ----------

   overriding
   function Info (Self : Missing_Reference) return String is
   begin
      return "Renaming " & Image (Self.Canonical_Definition.F_Name.Text)
        & " to " & Image (To_Text (Self.New_Name)) & " loses reference "
        & Self.Conflicting_Id.Image;
   end Info;

   ----------
   -- Info --
   ----------

   overriding
   function Info (Self : Name_Collision) return String is
   begin
      return "Renaming " & Image (Self.Canonical_Definition.F_Name.Text)
        & " to " & Image (To_Text (Self.New_Name))
        & " creates a name collision with "
        & Self.Conflicting_Id.Image;
   end Info;

   ----------
   -- Info --
   ----------

   overriding
   function Info (Self : New_Reference) return String is
   begin
      return "Renaming " & Image (Self.Canonical_Definition.F_Name.Text)
        & " to " & Image (To_Text (Self.New_Name))
        & " creates a new reference "
        & Self.Conflicting_Id.Image;
   end Info;

   ----------
   -- Info --
   ----------

   overriding
   function Info (Self : Overriding_Subprogram) return String is
   begin
      return "Renaming " & Image (Self.Canonical_Definition.F_Name.Text)
        & " to " & Image (To_Text (Self.New_Name))
        & " will override "
        & Self.Conflicting_Id.Image;
   end Info;

   ----------------------------
   -- Parse_Original_Buffers --
   ----------------------------

   procedure Parse_Original_Buffers
     (Self : in out Reference_Mapper) is
      use Unit_Buffers;

      Units_Cursor : Cursor := Self.Temporary_Buffers.First;

   begin
      while Has_Element (Units_Cursor) loop
         Key (Units_Cursor).Reparse;
         Next (Units_Cursor);
      end loop;

      Self.Update_Canonical_Definition;
   end Parse_Original_Buffers;

   -----------------------------
   -- Parse_Temporary_Buffers --
   -----------------------------

   procedure Parse_Temporary_Buffers
     (Self : in out Reference_Mapper) is
      use Unit_Buffers;

      Units_Cursor : Cursor := Self.Temporary_Buffers.First;

   begin
      while Has_Element (Units_Cursor) loop
         Key (Units_Cursor).Reparse
           (Buffer =>
              To_String (Self.Temporary_Buffers.Reference (Units_Cursor)));
         Next (Units_Cursor);
      end loop;

      Self.Update_Canonical_Definition;
   end Parse_Temporary_Buffers;

   ---------------------------------
   -- Update_Canonical_Definition --
   ---------------------------------

   procedure Update_Canonical_Definition
     (Self : in out Reference_Mapper)
   is
      Node : constant Ada_Node := Self.Canonical_Definition_Unit.Root.Lookup
        (Source_Location'
           (Line   => Self.Canonical_Definition_Sloc.Start_Line,
            Column => Self.Canonical_Definition_Sloc.Start_Column));
      Name_Node : constant Libadalang.Analysis.Name := Get_Node_As_Name (Node);

   begin
      if Name_Node = No_Name then
         raise Program_Error;
      end if;

      Self.Canonical_Definition := Resolve_Name_Precisely (Name_Node);

      --  If the canonical definition was lost then the rename created
      --  and Self cannot continue.

      if Self.Canonical_Definition = No_Defining_Name then
         raise Program_Error;
      end if;
   end Update_Canonical_Definition;

end Laltools.Refactor.Safe_Rename;
