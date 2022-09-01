------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;
with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Common; use Libadalang.Common;

package body Laltools.Refactor.Safe_Rename is

   function "+"
     (T : Text_Type)
      return Unbounded_Text_Type renames To_Unbounded_Text;
   function "+" (Text : Text_Type) return UTF_8_String renames To_UTF8;
   function "+" (UT : Unbounded_Text_Type) return Text_Type renames To_Text;
   function "+"
     (Source : String)
      return Unbounded_String renames To_Unbounded_String;

   --------------------------
   -- Create_Naming_Scheme --
   --------------------------

   function Create_Naming_Scheme
     (Attribute_Value_Provider : not null Attribute_Value_Provider_Access)
      return Naming_Scheme_Type
   is
      use GNATCOLL.Projects;

      Casing          : constant String :=
        Attribute_Value_Provider.all (Casing_Attribute);
      Dot_Replacement : constant String :=
        Attribute_Value_Provider.all (Dot_Replacement_Attribute);
      Spec_Suffix     : constant String :=
        Attribute_Value_Provider.all (Spec_Suffix_Attribute, "Ada");
      Body_Suffix     : constant String :=
        Attribute_Value_Provider.all (Impl_Suffix_Attribute, "Ada");

   begin
      return
        Naming_Scheme_Type'
          (Casing          =>
             (if Casing = "" then lowercase
              else Casing_Type'Value (To_Lower (Casing))),
           Dot_Replacement =>
             (if Dot_Replacement = "" then +"-"
              else +Dot_Replacement),
           Spec_Suffix     =>
             (if Spec_Suffix = "" then +".ads"
              else +Spec_Suffix),
           Body_Suffix     =>
             (if Body_Suffix = "" then +".adb"
              else +Body_Suffix));

   exception
      when others => return Invalid_Naming_Scheme;
   end Create_Naming_Scheme;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : Naming_Scheme_Type) return Boolean is
     (Self /= Invalid_Naming_Scheme);

   function Equivalent_Parameter_Mode (L, R : Ada_Mode) return Boolean is
      (L = R
       or else (L = Ada_Mode_In and then R = Ada_Mode_Default)
       or else (L = Ada_Mode_Default and then R = Ada_Mode_In));
   --  Returns True if L and R are the same or if both are either Ada_Mode_In
   --  or Ada_Mode_Default.

   --  Data type used to compared subprogram signatures
   --  The type definition of a parameter cannot be stored as an Ada_Node
   --  since the equality comparision between the same node with and
   --  without generic context will result False. Their have however, is
   --  the same.

   type Parameter_Data is
      record
         Has_Aliased          : Boolean;
         Mode                 : Ada_Mode;
         Type_Definition_Hash : Ada.Containers.Hash_Type;
      end record;

   function Equivalent_Parameter_Data
     (L, R : Parameter_Data)
      return Boolean;
   --  Returns True if L and R are the same, using the
   --  Equivalent_Parameter_Mode function to evaluate the equality of the Mode
   --  component.

   package Parameter_Data_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Parameter_Data,
      "="          => Equivalent_Parameter_Data);

   subtype Parameter_Data_Vector is Parameter_Data_Vectors.Vector;

   function Create_Parameter_Data_Vector
     (Parameters : Params)
      return Parameter_Data_Vector;
   --  Creates a vector with the Parameter_Data of each parameter of
   --  Parameters.

   function Are_Subprograms_Type_Conformant
     (Subp_A      : Basic_Decl'Class;
      Subp_B      : Basic_Decl'Class;
      Check_Modes : Boolean := False)
      return Boolean;
   --  Checks if Subp_A and Subp_B are type conformant. Check_Modes is a flag
   --  that defines in the mode of each parameter is also checked. In such
   --  check, In and Default mode are considered the same.

   function Check_Rename_Conflict
     (New_Name : Unbounded_Text_Type;
      Target   : Defining_Name'Class)
      return Boolean is (Target.F_Name.Text = To_Text (New_Name))
     with Pre => Target /= No_Defining_Name;
   --  Checks if Target's name is equal to New_Name
   --  FIXME: Do a case insensitive comparison

   function Check_Subp_Rename_Conflict
     (Subp_A   : Basic_Decl'Class;
      New_Name : Unbounded_Text_Type;
      Subp_B   : Basic_Decl'Class)
      return Boolean
     with Pre => (Is_Subprogram (Subp_A) or else Subp_A.Kind
                    in Ada_Generic_Subp_Instantiation_Range)
                 and then (Is_Subprogram (Subp_B) or else
                             Subp_B.Kind in
                               Ada_Generic_Subp_Instantiation_Range);
   --  Checks if renaming Subp_A to New_Name causes a conflict with Subp_B.
   --  This includes checking if both subprograms are type conformant.
   --  FIXME: Do a case insensitive comparison.

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

   -------------------------------------
   -- Are_Subprograms_Type_Conformant --
   -------------------------------------

   function Are_Subprograms_Type_Conformant
     (Subp_A      : Basic_Decl'Class;
      Subp_B      : Basic_Decl'Class;
      Check_Modes : Boolean := False)
      return Boolean
   is
      Subp_A_Spec : constant Base_Subp_Spec := Get_Subp_Spec (Subp_A);
      Subp_B_Spec : constant Base_Subp_Spec := Get_Subp_Spec (Subp_B);

      Subp_A_Params : constant Params :=
        (if not Subp_A_Spec.Is_Null then Get_Subp_Spec_Params (Subp_A_Spec)
         else No_Params);
      Subp_B_Params : constant Params :=
        (if not Subp_A_Spec.Is_Null then Get_Subp_Spec_Params (Subp_B_Spec)
         else No_Params);

      Both_Specs_Not_Null : constant Boolean :=
        not Subp_A_Spec.Is_Null and then not Subp_B_Spec.Is_Null;

      Both_Entry_Decls : constant Boolean :=
        (Both_Specs_Not_Null
         and then Subp_A_Spec.Kind in Ada_Entry_Spec_Range
         and then Subp_B_Spec.Kind in Ada_Entry_Spec_Range);

      Both_Normal_Subps_Decls : constant Boolean :=
        (not Both_Entry_Decls
         and then Both_Specs_Not_Null
         and then Subp_A_Spec.Kind in Ada_Subp_Spec_Range
         and then Subp_B_Spec.Kind in Ada_Subp_Spec_Range);

      Both_Procedures : constant Boolean :=
        (Both_Normal_Subps_Decls
         and then Subp_A_Spec.As_Subp_Spec.F_Subp_Kind in
           Ada_Subp_Kind_Procedure_Range
         and then Subp_B_Spec.As_Subp_Spec.F_Subp_Kind in
           Ada_Subp_Kind_Procedure_Range);

      Both_Functions : constant Boolean :=
        (Both_Normal_Subps_Decls
         and then Subp_A_Spec.As_Subp_Spec.F_Subp_Kind in
           Ada_Subp_Kind_Function_Range
         and then Subp_B_Spec.As_Subp_Spec.F_Subp_Kind in
           Ada_Subp_Kind_Function_Range);

      Both_Params_Null : constant Boolean :=
        Subp_A_Params.Is_Null and then Subp_B_Params.Is_Null;

      Only_One_Params_Null : constant Boolean :=
        Subp_A_Params.Is_Null xor Subp_B_Params.Is_Null;

      package Hash_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Ada.Containers.Hash_Type);

      subtype Hash_Vector is Hash_Vectors.Vector;

      function Create_Hash_Vector
        (Parameters : Params)
         return Hash_Vector;
      --  Creates a vector with the hash of the type declaration of each
      --  parameter of Parameters.

      ------------------------
      -- Create_Hash_Vector --
      ------------------------

      function Create_Hash_Vector (Parameters : Params)
                                   return Hash_Vector
      is
         Param_Type_Hash : Hash_Type;

      begin
         return Hashes : Hash_Vector do
            if not Parameters.Is_Null then
               for Param_Spec of Parameters.F_Params loop
                  Param_Type_Hash :=
                    Hash (Param_Spec.F_Type_Expr.P_Designated_Type_Decl.
                            As_Ada_Node);

                  for Unsued_Id of Param_Spec.F_Ids loop
                     Hashes.Append (Param_Type_Hash);
                  end loop;
               end loop;
            end if;
         end return;
      end Create_Hash_Vector;

      use type Hash_Vectors.Vector;
      use type Parameter_Data_Vectors.Vector;

   begin
      --  If any of the subprograms does not have a spec, then this a malformed
      --  tree. Do not detect collisions on this case, hence, return False.
      --  Consider two subprograms of different kinds as non type conformant.

      if not Both_Specs_Not_Null
        or else (not Both_Entry_Decls
                 and then not Both_Procedures
                 and then not Both_Functions)
      then
         return False;
      end if;

      --  At this point we know that:
      --    * both subprogram specs are not null
      --    * both subprograms are of the same kind
      --  If they do not have parameters, then:
      --    * if they're both procedures or entry decls, then they're type
      --      conformant
      --    * otherwise, they must both be functions and if they have different
      --      return types, then they're not type conformant
      --  If only one has parameters, then they're not type conformant.

      if Both_Params_Null then
         if Both_Procedures or else Both_Entry_Decls then
            return True;
         else
            Assert (Both_Functions);
            if Hash (Subp_A_Spec.As_Subp_Spec.P_Return_Type.As_Ada_Node) /=
                 Hash (Subp_B_Spec.As_Subp_Spec.P_Return_Type.As_Ada_Node)
            then
               return False;
            end if;
         end if;
      elsif Only_One_Params_Null then
         return False;
      end if;

      --  At this point we know that:
      --    * both subprogram specs are not null
      --    * both subprograms are of the same kind
      --    * both subprograms have parameters
      --    * if both subprogram are functions, they have the same return type
      --  The only thing left to check are their parameters.

      case Check_Modes is
         when True
            => return Create_Parameter_Data_Vector (Subp_A_Params) =
                        Create_Parameter_Data_Vector (Subp_B_Params);

         when False
            => return Create_Hash_Vector (Subp_A_Params) =
                        Create_Hash_Vector (Subp_B_Params);
      end case;
   end Are_Subprograms_Type_Conformant;

   --------------------------------
   -- Check_Subp_Rename_Conflict --
   --------------------------------

   function Check_Subp_Rename_Conflict
     (Subp_A   : Basic_Decl'Class;
      New_Name : Unbounded_Text_Type;
      Subp_B   : Basic_Decl'Class)
      return Boolean
   is
      Subp_B_Name : constant Text_Type := Subp_B.P_Defining_Name.F_Name.Text;

   begin
      if Subp_A = Subp_B or else Subp_B_Name /= To_Text (New_Name) then
         return False;
      end if;

      --  Subp_B name is the name as New_Name, therefore, we need to check if
      --  both subprograms are type conformant.
      return Are_Subprograms_Type_Conformant (Subp_A, Subp_B, False);
   end Check_Subp_Rename_Conflict;

   ----------------------------------
   -- Create_Parameter_Data_Vector --
   ----------------------------------

   function Create_Parameter_Data_Vector
     (Parameters : Params)
      return Parameter_Data_Vector is
   begin
      return Parameters_Data : Parameter_Data_Vector do
         if not Parameters.Is_Null then
            for Param_Spec of Parameters.F_Params loop
               for Parameter of Param_Spec.F_Ids loop
                  Parameters_Data.Append
                    (Parameter_Data'
                     (Has_Aliased          => Param_Spec.F_Has_Aliased,
                      Mode                 => Param_Spec.F_Mode.Kind,
                      Type_Definition_Hash => Hash
                       (Param_Spec.F_Type_Expr.P_Designated_Type_Decl.
                          As_Ada_Node)));
               end loop;
            end loop;
         end if;
      end return;
   end Create_Parameter_Data_Vector;

   -------------------------------
   -- Equivalent_Parameter_Data --
   -------------------------------

   function Equivalent_Parameter_Data
     (L, R : Parameter_Data)
      return Boolean is
   begin
      return L.Has_Aliased = R.Has_Aliased
        and then Equivalent_Parameter_Mode (L.Mode, R.Mode)
        and then L.Type_Definition_Hash = R.Type_Definition_Hash;
   end Equivalent_Parameter_Data;

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
      return Refactoring_Diagnotic_Vector
   is
      Problem_Finders : Specific_Rename_Problem_Finder_Vectors.Vector;
      Problems : Refactoring_Diagnotic_Vector;

   begin
      --  If we're renaming an enum literal, just check for collisions within
      --  the Enum_Literal_Decl_List.

      if Self.Canonical_Definition.Parent.Kind in Ada_Enum_Literal_Decl then
         Problem_Finders.Append
           (Enum_Name_Collision_Finder'
              (Canonical_Definition => Self.Canonical_Definition,
               New_Name             => Self.New_Name));

      else

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

         --  If we're trying to rename a subprogram, then check if this
         --  subprogram will collide with a compilation unit or if it will
         --  override another one.

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

         elsif Self.Canonical_Definition.P_Basic_Decl.Kind in
           Ada_Param_Spec
         then
            Problem_Finders.Append
              (Param_Spec_Collision_Finder'
                 (Canonical_Definition => Self.Canonical_Definition,
                  New_Name             => Self.New_Name,
                  Reference            => No_Base_Id));
         end if;

         Problem_Finders.Append
           (Subtype_Indication_Collision_Finder'
              (Canonical_Definition => Self.Canonical_Definition,
               References           => Self.References,
               New_Name             => Self.New_Name));
         Problem_Finders.Append
           (Name_Hidden_Finder'
              (Canonical_Definition => Self.Canonical_Definition,
               References           => Self.References,
               New_Name             => Self.New_Name));
      end if;

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
      return Refactoring_Diagnotic_Vector
   is
      function Create_Problems return Refactoring_Diagnotic_Vector;
      --  For every Sloc found in Self.References_Diff create either a
      --  Missing_Reference or New_Reference object and add it to a vector.

      ---------------------
      -- Create_Problems --
      ---------------------

      function Create_Problems return Refactoring_Diagnotic_Vector is
         use Unit_Slocs_Maps;
         C : Cursor;
      begin
         return Result : Refactoring_Diagnotic_Vector do
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
      Canonical_Decl : constant Basic_Decl :=
        Self.Canonical_Definition.P_Basic_Decl;

      --  If Canonical_Decl.Kind in Ada_Param_Spec_Range, then do nothing.
      --  In this case, there can only be collision with other parameters
      --  or with its type indication. Such collision are detected by the
      --  Param_Spec_Collision_Finder.

      Local_Scopes : constant Ada_List_Hashed_Set :=
        (if Canonical_Decl.Kind in Ada_Param_Spec_Range then
            Ada_List_Hashed_Sets.Empty_Set
         else
            Find_Enclosing_Declarative_Parts (Canonical_Decl));

      --  If Self.Canonical_Decl is a subprogram, then its Spec node is needed
      --  check for collisions with other subprograms.

      Is_Subp : constant Boolean :=
        Is_Subprogram (Canonical_Decl)
        or else Canonical_Decl.Kind in Ada_Generic_Subp_Instantiation_Range;

      Canonical_Subp_Spec : constant Base_Subp_Spec :=
        (if Is_Subp then Get_Subp_Spec (Canonical_Decl)
         else No_Base_Subp_Spec);

      function Check_Rename_Conflicts
        (Scope : Ada_List'Class)
         return Defining_Name
        with Pre => not Scope.Is_Null
                    and then Scope.Kind in Ada_Ada_Node_List
                                             | Ada_Basic_Decl_List_Range
                                             | Ada_Param_Spec_List_Range;
      --  For every declaration of Scope, checks if it has the same name
      --  as Self.New_Name. If so, return a the Defining_Name of the
      --  conflicting declaration.

      function Check_Subp_Rename_Conflicts
        (Scope : Ada_List'Class)
         return Defining_Name;
      --  For every declaration of Scope, checks if it has the same
      --  name as Self.New_Name. Also checks if such declaration is a
      --  subprogram as if so, calls Check_Subp_Rename_Conflict to check
      --  if both are type conformant (i.e., if they have the same signature).

      function Process_Scope
        (Scope : Ada_List'Class)
         return Defining_Name;
      --  If Canonical_Decl is a subprogram, then delegates to
      --  Check_Subp_Rename_Conflicts, otherwise, to Check_Rename_Conflicts.

      ----------------------------
      -- Check_Rename_Conflicts --
      ----------------------------

      function Check_Rename_Conflicts
        (Scope : Ada_List'Class)
         return Defining_Name
      is
         Result : Defining_Name := No_Defining_Name;

         function Visit (Node : Ada_Node'Class) return Visit_Status;
         --  Checks if Node is a conflict, and if so, sets Result to it,
         --  stopping the iterative process.

         -----------
         -- Visit --
         -----------

         function Visit (Node : Ada_Node'Class) return Visit_Status is
         begin
            if Node.Kind in Ada_Basic_Decl then
               for Definition of Node.As_Basic_Decl.P_Defining_Names loop
                  if Check_Rename_Conflict (Self.New_Name, Definition) then
                     Result := Definition;
                     return Stop;
                  end if;
               end loop;
            end if;

            return (if Node = Scope then Into else Over);
         end Visit;

      begin
         Scope.Traverse (Visit'Access);

         return Result;
      end Check_Rename_Conflicts;

      ---------------------------------
      -- Check_Subp_Rename_Conflicts --
      ---------------------------------

      function Check_Subp_Rename_Conflicts
        (Scope : Ada_List'Class)
         return Defining_Name
      is
         Result : Defining_Name := No_Defining_Name;

         function Visit (Node : Ada_Node'Class) return Visit_Status;
         --  Checks if Node is a conflict, and if so, sets Result to it,
         --  stopping the iterative process.

         -----------
         -- Visit --
         -----------

         function Visit (Node : Ada_Node'Class) return Visit_Status is
         begin
            if Node.Kind in Ada_Basic_Decl then
               --  Filter the nodes that are not Basic_Decl

               if Is_Subprogram (Node.As_Basic_Decl)
                 or else Node.Kind in Ada_Generic_Subp_Instantiation
               then
                  --  If Decl is a subprogram, then not only check the name
                  --  but also its signature.

                  if Check_Subp_Rename_Conflict
                    (Canonical_Decl,
                     Self.New_Name,
                     Node.As_Basic_Decl)
                  then
                     Result := Node.As_Basic_Decl.P_Defining_Name;
                     return Stop;
                  end if;

               else
                  for Definition of Node.As_Basic_Decl.P_Defining_Names loop
                     if Check_Rename_Conflict
                       (Self.New_Name, Definition)
                     then
                        Result := Node.As_Basic_Decl.P_Defining_Name;
                        return Stop;
                     end if;
                  end loop;
               end if;
            end if;

            return (if Node = Scope then Into else Over);
         end Visit;

      begin
         Scope.Traverse (Visit'Access);

         return Result;
      end Check_Subp_Rename_Conflicts;

      -------------------
      -- Process_Scope --
      -------------------

      function Process_Scope (Scope : Ada_List'Class) return Defining_Name is
      begin
         if Scope.Is_Null then
            return No_Defining_Name;
         end if;

         if Is_Subp then
            Assert (not Canonical_Subp_Spec.Is_Null);
            return Check_Subp_Rename_Conflicts (Scope);

         else
            return Check_Rename_Conflicts (Scope);
         end if;
      end Process_Scope;

   begin
      --  If we're renaming a Generic_Subp_Instantiation whose
      --  Generic_Subp_Decl does not exist, then Original_Subp_Spec will be
      --  null. In that case, there isn't enough information to check for
      --  conflicts.

      if Is_Subp and then Canonical_Subp_Spec.Is_Null then
         return No_Rename_Problem;
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
     (Self : Enum_Name_Collision_Finder)
      return Rename_Problem'Class is
   begin
      for Enum_Literal of
        Self.Canonical_Definition.Parent.Parent.As_Enum_Literal_Decl_List
      loop
         if Enum_Literal.P_Defining_Name /= Self.Canonical_Definition then
            if Check_Rename_Conflict
              (New_Name => Self.New_Name,
               Target   => Enum_Literal.P_Defining_Name)
            then
               return Name_Collision'
                 (Canonical_Definition => Self.Canonical_Definition,
                  New_Name             => Self.New_Name,
                  Conflicting_Id       => Enum_Literal.P_Defining_Name.F_Name);
            end if;
         end if;
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
      Parent_Package : Base_Package_Decl := No_Base_Package_Decl;

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
                 Parent.As_Package_Body.P_Canonical_Part.As_Base_Package_Decl;
               exit Find_Parent_Package;

            when Ada_Base_Package_Decl =>
               Parent_Package := Parent.As_Base_Package_Decl;
               exit Find_Parent_Package;

            when Ada_Subp_Body | Ada_Task_Body | Ada_Decl_Block =>
               return No_Rename_Problem;

            when others =>
               null;
         end case;
      end loop Find_Parent_Package;

      if Parent_Package.Is_Null then
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
            Unit_Decl : constant Basic_Decl :=
              (if not Comp_Unit.P_Decl.Is_Null then
                  Comp_Unit.P_Decl.P_Canonical_Part
               else
                  No_Basic_Decl);
            Unit_Decl_Parent : constant Basic_Decl :=
              (if not Unit_Decl.Is_Null then
                  Unit_Decl.P_Parent_Basic_Decl
               else
                  No_Basic_Decl);
         begin
            if not Unit_Decl.Is_Null
              and then not Unit_Decl_Parent.Is_Null
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

      function Process_Compilation_Unit
        (Compilation_Unit : Libadalang.Analysis.Compilation_Unit)
         return Rename_Problem'Class;
      --  Checks if there are collisions with the top level declaration of
      --  Compilation_Unit.

      ------------------------------
      -- Process_Compilation_Unit --
      ------------------------------

      function Process_Compilation_Unit
        (Compilation_Unit : Libadalang.Analysis.Compilation_Unit)
         return Rename_Problem'Class
      is
         Parent_Package : Package_Decl := No_Package_Decl;

      begin
         if Compilation_Unit.P_Decl /=
           Self.Canonical_Definition.P_Basic_Decl
         then
            return No_Rename_Problem;
         end if;

         Parent_Package :=
           Parent_Unit.Root.As_Compilation_Unit.P_Decl.As_Package_Decl;

         --  There are two kinds of conflicts:
         --
         --  1) Package/Subprogram that defines a compilation unit is renamed
         --  to an already existing declaration in the spec of the parent
         --  package.

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

         --  2) Package/Subprogram that defines a compilation unit is renamed
         --  to an already existing Package/Subprogram that also defines
         --  a compilation unit and both share the same parent package.

         for Analysis_Unit of Self.Units loop
            for Compilation_Unit of Get_Compilation_Units (Analysis_Unit) loop
               if Compilation_Unit.P_Decl.P_Parent_Basic_Decl.Unit =
                 Parent_Unit
               then
                  declare
                     --  Get the suffix of the declaration of this unit and
                     --  check.

                     Unit_Decl_Identifier : constant Identifier :=
                       Get_Defining_Name_Id
                         (Compilation_Unit.P_Decl.P_Defining_Name);

                  begin
                     --  Check if Self.New_Name is already used by this unit

                     if Unit_Decl_Identifier.Text =
                          To_Text (Self.New_Name)
                     then
                        return Name_Collision'
                          (Canonical_Definition => Self.Canonical_Definition,
                           New_Name             => Self.New_Name,
                           Conflicting_Id       =>
                             Compilation_Unit.P_Decl.P_Defining_Name.F_Name);
                     end if;
                  end;
               end if;
            end loop;
         end loop;
         return No_Rename_Problem;
      end Process_Compilation_Unit;

   begin
      case Self.Canonical_Definition.Unit.Root.Kind is
         when Ada_Compilation_Unit_List_Range =>
            for Compilaton_Unit of
              Self.Canonical_Definition.Unit.Root.As_Compilation_Unit_List
            loop
               declare
                  Problem : constant Rename_Problem'Class :=
                    Process_Compilation_Unit
                      (Compilaton_Unit.As_Compilation_Unit);

               begin
                  if Problem /= No_Rename_Problem then
                     return Problem;
                  end if;
               end;
            end loop;

            return No_Rename_Problem;

         when Ada_Compilation_Unit_Range =>
            return
              Process_Compilation_Unit
                (Self.Canonical_Definition.Unit.Root.As_Compilation_Unit);

         when others =>
            return No_Rename_Problem;
      end case;
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
         Subp_A_Params : Parameter_Data_Vector;
         Subp_B_Params : Parameter_Data_Vector;

         use type Parameter_Data_Vectors.Vector;

      begin
         if Subp_Spec_A.F_Subp_Kind.Kind /= Subp_Spec_B.F_Subp_Kind.Kind then
            return False;
         end if;

         Subp_A_Params :=
           Create_Parameter_Data_Vector (Subp_Spec_A.F_Subp_Params);
         Subp_B_Params :=
           Create_Parameter_Data_Vector (Subp_Spec_B.F_Subp_Params);

         case Ada_Subp_Kind'(Subp_Spec_A.F_Subp_Kind) is
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
                       Subp_B_Params.First_Element.Type_Definition_Hash /=
                         Hash (Base_Type.P_Canonical_Part.As_Ada_Node)
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
        or else First_Type.Kind not in Ada_Type_Decl
      then
         return No_Rename_Problem;
      end if;

      --  Is First_Type a derived type?

      Base_Type := First_Type.P_Base_Type;

      if Base_Type = No_Base_Type_Decl
        or else Base_Type.Kind not in Ada_Type_Decl
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
      use Ada_List_Hashed_Sets;

      Canonical_Decl : constant Basic_Decl :=
        Self.Canonical_Definition.P_Basic_Decl;

      --  If we are renaming a subprogram, it only hides another subprogram
      --  that is type conformant.

      Is_Subp : constant Boolean :=
        Is_Subprogram (Canonical_Decl)
        or else Canonical_Decl.Kind in Ada_Generic_Subp_Instantiation_Range;

      Canonical_Subp_Spec : constant Base_Subp_Spec :=
        (if Is_Subp then Get_Subp_Spec (Canonical_Decl)
         else No_Base_Subp_Spec);

      Possible_Problem : Hiding_Name;
      Found_Problem    : Boolean := False;

      --  These are the scopes where declarations can get hidden by
      --  Canonical_Decl.
      Parent_Scopes : constant Ada_List_Hashed_Set :=
        Difference
          (Find_Visible_Scopes (Canonical_Decl),
           Find_Enclosing_Scopes (Canonical_Decl));

      Stop_Node : constant Basic_Decl := Canonical_Decl.P_Parent_Basic_Decl;
      Use_Units_Public_Parts : constant Declarative_Part_Vector :=
        Get_Use_Units_Public_Parts (Self.Canonical_Definition);

      procedure Check_Scope
        (Scope : Ada_List'Class);
      --  Checks if Decl_Part contains any declaration that can be hidden by
      --  Canonical_Decl. If so, Possible_Problem is filled with the
      --  appropriate information and Found_Problem is set to True.

      procedure Check_Scope
        (Scope : Ada_List'Class) is
      begin
         if Is_Subp then
            Assert (not Canonical_Subp_Spec.Is_Null);

            for Decl of Scope.Children loop
               exit when Decl = Stop_Node;

               --  Conflicts can only exist with subprograms and not with other
               --  kind of declarations.

               if Decl.Kind in Ada_Basic_Decl
                 and then (Is_Subprogram (Decl.As_Basic_Decl)
                           or else Decl.Kind in
                             Ada_Generic_Subp_Instantiation_Range)
               then
                  if Check_Subp_Rename_Conflict
                    (Subp_A   => Canonical_Decl,
                     New_Name => Self.New_Name,
                     Subp_B   => Decl.As_Basic_Decl)
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

         else
            for Decl of Scope.Children loop
               exit when Decl = Stop_Node;

               --  Conflicts can exists with any kind of declaration except
               --  subprograms

               if Decl.Kind in Ada_Basic_Decl
                 and then not (Is_Subprogram (Decl.As_Basic_Decl)
                               or else Decl.Kind in
                                 Ada_Generic_Subp_Instantiation_Range)
               then
                  for Definition of Decl.As_Basic_Decl.P_Defining_Names loop
                     if Check_Rename_Conflict (Self.New_Name, Definition) then
                        Possible_Problem :=
                          (Canonical_Definition => Self.Canonical_Definition,
                           New_Name             => Self.New_Name,
                           Conflicting_Id       => Definition.F_Name);
                        Found_Problem := True;
                        return;
                     end if;
                  end loop;
               end if;
            end loop;
         end if;
      end Check_Scope;

   begin
      for Scope of Parent_Scopes loop
         Check_Scope (Scope);
         exit when Found_Problem;
      end loop;

      if Found_Problem then
         return Possible_Problem;
      end if;

      for Decl_Part of Use_Units_Public_Parts loop
         Check_Scope (Decl_Part.F_Decls);
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
      use Ada_List_Hashed_Sets;

      Canonical_Decl : constant Basic_Decl :=
        Self.Canonical_Definition.P_Basic_Decl;

      --  If we are renaming a subprogram, it only becomes hidden by another
      --  subprogram that is type conformant.

      Is_Subp : constant Boolean :=
        (Is_Subprogram (Canonical_Decl)
         or else Canonical_Decl.Kind in Ada_Generic_Subp_Instantiation_Range);

      Canonical_Subp_Spec : constant Base_Subp_Spec :=
        (if Is_Subp then Get_Subp_Spec (Canonical_Decl)
         else No_Base_Subp_Spec);

      Nested_Declarative_Parts : constant Declarative_Part_Vector :=
        Find_Nested_Scopes (Canonical_Decl);
      Own_Declarative_Part     : constant Declarative_Part_Vector :=
        (if Is_Declarative_Part_Owner (Canonical_Decl) then
            Get_Declarative_Parts (Canonical_Decl)
         else
            Declarative_Part_Vectors.Empty_Vector);

      Stop_Node : Ada_Node := No_Ada_Node;
      Canonical_Definition_Local_Scopes : constant Ada_List_Hashed_Set :=
        Find_Visible_Scopes (Canonical_Decl);
      Reference_Local_Scopes : Ada_List_Hashed_Set;

      function Check_Conflict (Definition : Defining_Name'Class)
                               return Boolean;
      --  Delegates to Check_Subp_Rename_Conflict after doing necessary
      --  convertions between node types.

      --------------------
      -- Check_Conflict --
      --------------------

      function Check_Conflict (Definition : Defining_Name'Class) return Boolean
      is
         Definition_Decl    : constant Basic_Decl :=
           Definition.P_Basic_Decl;
         Are_Both_Subps     : constant Boolean :=
           Is_Subp
           and then (Is_Subprogram (Definition_Decl)
                     or else Definition_Decl.Kind in
                       Ada_Generic_Subp_Instantiation_Range);
         Are_Both_Not_Subps : constant Boolean :=
           not Is_Subp
           and then not (Is_Subprogram (Definition_Decl)
                         or else Definition_Decl.Kind in
                           Ada_Generic_Subp_Instantiation_Range);
      begin
         if Are_Both_Subps then
            return Check_Subp_Rename_Conflict
              (Subp_A   => Canonical_Decl,
               New_Name => Self.New_Name,
               Subp_B   => Definition_Decl);

         elsif Are_Both_Not_Subps then
            return Check_Rename_Conflict
              (New_Name => Self.New_Name,
               Target   => Definition);
         end if;

         return False;
      end Check_Conflict;

   begin
      --  If we're renaming a Generic_Subp_Instantiation whose
      --  Generic_Subp_Decl does not exist, then Original_Subp_Spec will be
      --  null. In that case, there isn't enough information to check for
      --  conflicts.

      if Is_Subp and then Canonical_Subp_Spec.Is_Null then
         return No_Rename_Problem;
      end if;

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
         Reference_Local_Scopes :=
           Difference
             (Find_Visible_Scopes (Reference),
              Find_Enclosing_Scopes (Reference));

         for Scope of
           Difference
             (Reference_Local_Scopes, Canonical_Definition_Local_Scopes)
         loop
            --  Do not look for conflicts in Self.Canonical_Part own
            --  local scopes, since this would be a name collision
            --  conflict already detected by the Name_Collision_Finder.

            --  Reference is only hidden by a declaration that is declared
            --  before it.
            Find_Stop_Node :
            for Parent of Reference.Parents loop
               if Parent.Kind in
                 Ada_Basic_Decl | Ada_Declarative_Part | Ada_Handled_Stmts
               then
                  Stop_Node := Parent;
                  exit Find_Stop_Node;
               end if;
            end loop Find_Stop_Node;

            declare
               Conflicting_Definition : Defining_Name := No_Defining_Name;

               function Visit
                 (Node : Ada_Node'Class)
                     return Visit_Status;
               --  Checks if Node is a conflict, and if so, sets Result
               --  to it, stopping the iterative process.

               -----------
               -- Visit --
               -----------

               function Visit
                 (Node : Ada_Node'Class)
                     return Visit_Status is
               begin
                  if Node.Kind in Ada_Basic_Decl then
                     --  If Self.Canonical_Definition is found, then it
                     --  can't be hidden, so stop the search.

                     if Node.As_Basic_Decl.P_Canonical_Part =
                       Self.Canonical_Definition.P_Basic_Decl
                       or else Node = Stop_Node
                     then
                        return Stop;
                     end if;

                     for Definition of
                       Node.As_Basic_Decl.P_Defining_Names
                     loop
                        if Check_Conflict (Definition) then
                           Conflicting_Definition := Definition;
                           return Stop;
                        end if;
                     end loop;
                  end if;

                  return (if Node = Scope then Into else Over);
               end Visit;

            begin
               Scope.Traverse (Visit'Access);

               if not Conflicting_Definition.Is_Null then
                  return Hidden_Name'
                    (Canonical_Definition => Self.Canonical_Definition,
                     New_Name             => Self.New_Name,
                     Conflicting_Id       => Conflicting_Definition.F_Name);
               end if;
            end;
         end loop;
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
      Param_Spec : Libadalang.Analysis.Param_Spec renames
        Self.Canonical_Definition.P_Basic_Decl.As_Param_Spec;
      Type_Expr  : Libadalang.Analysis.Type_Expr renames
        Param_Spec.F_Type_Expr;
      Subtype_Indication : constant Libadalang.Analysis.Subtype_Indication :=
        (if Type_Expr.Kind in Ada_Subtype_Indication
         then Type_Expr.As_Subtype_Indication
         else No_Subtype_Indication);
      Param_Spec_List : Libadalang.Analysis.Param_Spec_List renames
        Self.Canonical_Definition.P_Basic_Decl.Parent.As_Param_Spec_List;
      Declarative_Part : constant Libadalang.Analysis.Declarative_Part :=
        (if Is_Declarative_Part_Owner (Param_Spec.P_Parent_Basic_Decl) then
            Get_Declarative_Part (Param_Spec.P_Parent_Basic_Decl)
         elsif Is_Declarative_Part_Owner
           (Param_Spec.P_Parent_Basic_Decl.P_Next_Part_For_Decl)
         then
            Get_Declarative_Part
              (Param_Spec.P_Parent_Basic_Decl.P_Next_Part_For_Decl)
         else
            No_Declarative_Part);

   begin
      if Self.Canonical_Definition.P_Basic_Decl.Kind = Ada_Param_Spec then
         --  Possible problem 1: Renaming a parameter to the same name as its
         --  subtype indication

         if Param_Spec.F_Type_Expr.Kind = Ada_Subtype_Indication
           and then not Subtype_Indication.Is_Null
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

         --  Possible problem 3: Renaming a parameter to the same name of
         --  a declaration of its subprogram declarative part

         if not Declarative_Part.Is_Null then
            for Decl of Declarative_Part.F_Decls loop
               if Decl.Kind in Ada_Basic_Decl then
                  for Defining_Name
                    of Decl.As_Basic_Decl.P_Defining_Names
                  loop
                     if Defining_Name.F_Name.Text =
                       To_Text (Self.New_Name)
                     then
                        return Name_Collision'
                          (Canonical_Definition => Self.Canonical_Definition,
                           New_Name             => Self.New_Name,
                           Conflicting_Id       => Defining_Name.F_Name);
                     end if;
                  end loop;
               end if;
            end loop;
         end if;

         --  Possible problem 4: Renaming a parameter to the same name of
         --  a declaration of its subprogram declarative part
         --  Example:
         --
         --  function ComputeSquarePlusCube (X : Integer) return Integer is
         --    (declare T : constant Integer := X;
         --     begin T * T * (1 + T));

         declare
            Subp_Canonical_Part : constant Basic_Decl :=
              Param_Spec.P_Parent_Basic_Decl;
            Expression_Function : constant Expr_Function :=
              (if Subp_Canonical_Part.Kind in Ada_Expr_Function_Range then
                  Subp_Canonical_Part.As_Expr_Function
               elsif not Subp_Canonical_Part.P_Next_Part_For_Decl.Is_Null
                      and then Subp_Canonical_Part.P_Next_Part_For_Decl.Kind in
                                 Ada_Expr_Function_Range
               then
                  Subp_Canonical_Part.P_Next_Part_For_Decl.As_Expr_Function
               else No_Expr_Function);
         begin
            if not Expression_Function.Is_Null
              and then Is_Decl_Expr_Owner (Expression_Function)
            then
               for Decl of
                 Expression_Function.F_Expr.As_Paren_Expr.F_Expr.As_Decl_Expr.
                   F_Decls
               loop
                  for Defining_Name
                    of Decl.As_Basic_Decl.P_Defining_Names
                  loop
                     if Defining_Name.F_Name.Text =
                       To_Text (Self.New_Name)
                     then
                        return Name_Collision'
                          (Canonical_Definition => Self.Canonical_Definition,
                           New_Name             => Self.New_Name,
                           Conflicting_Id       => Defining_Name.F_Name);
                     end if;
                  end loop;
               end loop;
            end if;
         end;
      end if;

      return No_Rename_Problem;
   end Find;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                 : out AST_Analyser;
      Canonical_Definition : Defining_Name;
      New_Name             : Unbounded_Text_Type;
      References           : Base_Id_Vectors.Vector;
      Units                : Analysis_Unit_Array) is
   begin
      Self.Canonical_Definition := Canonical_Definition;
      Self.New_Name             := New_Name;
      Self.Units                := Units;
      Self.References           := References;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                 : out Reference_Mapper;
      Canonical_Definition : Defining_Name;
      New_Name             : Unbounded_Text_Type;
      Original_References  : Unit_Slocs_Maps.Map;
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
      Self.Original_References       := Original_References;
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
      --  If Canonical_Definition is a dotted name, this means that we're
      --  trying to rename its suffix.

      if Canonical_Definition.F_Name.Kind in Ada_Dotted_Name_Range then
         Add_Node (Canonical_Definition.F_Name.As_Dotted_Name.F_Suffix);
      else
         Add_Node (Canonical_Definition);
      end if;

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

   -------------------------
   -- Create_Safe_Renamer --
   -------------------------

   function Create_Safe_Renamer
     (Definition               : Defining_Name'Class;
      New_Name                 : Unbounded_Text_Type;
      Algorithm                : Problem_Finder_Algorithm_Kind;
      Attribute_Value_Provider : Attribute_Value_Provider_Access := null)
      return Safe_Renamer
   is (((Definition.P_Canonical_Part,
          New_Name,
          Algorithm,
          (if Attribute_Value_Provider = null then
            Default_Naming_Scheme
          else
            Create_Naming_Scheme (Attribute_Value_Provider)),
          Attribute_Value_Provider)));

   --------------
   -- Refactor --
   --------------

   function Refactor
     (Self           : Safe_Renamer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      Edits : Refactoring_Edits;

      Units : constant Analysis_Unit_Array := Analysis_Units.all;

      References : constant Base_Id_Vectors.Vector :=
        Find_All_References_For_Renaming (Self.Canonical_Definition, Units);

      function Initialize_Algorithm return Problem_Finder_Algorithm'Class;
      --  Returns an initialized Problem_Finder_Algorithm depending on
      --  Self.Algorithm.

      --------------------------
      -- Initialize_Algorithm --
      --------------------------

      function Initialize_Algorithm return Problem_Finder_Algorithm'Class
      is
         References_Map : Unit_Slocs_Maps.Map;

      begin
         case Self.Algorithm is
            when Map_References =>
               return Algorithm : Reference_Mapper
                 (Units_Length => Units'Length)
               do
                  Initialize_Unit_Slocs_Maps
                    (Unit_References      => References_Map,
                     Canonical_Definition => Self.Canonical_Definition,
                     References           => References);

                  Algorithm.Initialize
                    (Canonical_Definition => Self.Canonical_Definition,
                     New_Name             => Self.New_Name,
                     Original_References  => References_Map,
                     Units                => Units);
               end return;

            when Analyse_AST =>
               return Algorithm : AST_Analyser (Units'Length) do
                  Algorithm.Initialize
                    (Canonical_Definition => Self.Canonical_Definition,
                     New_Name             => Self.New_Name,
                     References           => References,
                     Units                => Units);
               end return;
         end case;
      end Initialize_Algorithm;

      Algorithm : Problem_Finder_Algorithm'Class := Initialize_Algorithm;

   begin
      Self.Add_References_To_Edits (References, Edits);

      --  If Self.Canonical_Definition is a top level declaration then its
      --  file name and all other file names of its references that are
      --  top level declartions need to be renamed.

      if Self.Is_Top_Level_Decl (Self.Canonical_Definition.P_Basic_Decl) then
         Self.Add_Files_Rename_To_Edits (References, Edits);
      end if;

      Edits.Diagnostics := Algorithm.Find;

      return Edits;
   end Refactor;

   -----------------------------
   -- Add_References_To_Edits --
   -----------------------------

   procedure Add_References_To_Edits
     (Self       : Safe_Renamer;
      References : Base_Id_Vectors.Vector;
      Edits      : in out Refactoring_Edits)
   is
      New_Name : constant Unbounded_String := +(+(+Self.New_Name));

   begin
      for Reference of References loop
         Safe_Insert
           (Edits     => Edits.Text_Edits,
            File_Name => Reference.Unit.Get_Filename,
            Edit      => Text_Edit'
              (Location => Reference.Sloc_Range,
               Text     => New_Name));
      end loop;
   end Add_References_To_Edits;

   -----------------------
   -- Is_Top_Level_Decl --
   -----------------------

   function Is_Top_Level_Decl
     (Self : Safe_Renamer;
      Decl : Basic_Decl'Class)
      return Boolean
   is
      Top_Level_Decl : constant Basic_Decl :=
        (if not Decl.Is_Null
         and then Decl.P_Enclosing_Compilation_Unit.F_Body.Kind in
                    Ada_Library_Item
         then
           Decl.P_Enclosing_Compilation_Unit.F_Body.As_Library_Item.F_Item
         else No_Basic_Decl);

   begin
      --  References of Self.Canonical_Definition can include
      --  Generic_Package_Internal and Generic_Subp_Internal nodes,
      --  whose parents (Generic_Package_Decl and Generic_Subp_Decl
      --  respectively) might be top level declarations.

      return not Top_Level_Decl.Is_Null
        and then (Decl = Top_Level_Decl
                  or else (Decl.Kind in
                             Ada_Generic_Package_Internal_Range
                             | Ada_Generic_Subp_Internal_Range
                           and then not Decl.Parent.Is_Null
                           and then Decl.Parent.Kind in
                             Ada_Generic_Decl
                           and then Decl.Parent.As_Basic_Decl =
                             Top_Level_Decl));
   end Is_Top_Level_Decl;

   -------------------------------
   -- Add_Files_Rename_To_Edits --
   -------------------------------

   procedure Add_Files_Rename_To_Edits
     (Self       : Safe_Renamer;
      References : Base_Id_Vectors.Vector;
      Edits      : in out Refactoring_Edits)
   is
      File_Rename : Laltools.Refactor.File_Rename;

      function New_File_Name (Reference : Base_Id) return String;
      --  Computes the new file name based on the old one, on the
      --  `Self.Canonical_Definition` text and on `Self.New_Name`.
      --  Example:
      --  `Self.Canonical_Definition.F_Name.Text`: Foo.Bar_Baz
      --  `Self.New_Name`: Qux
      --  Old filename: foo-bar_baz.ads
      --  New filename: foo-qux.ads

      -------------------
      -- New_File_Name --
      -------------------

      function New_File_Name (Reference : Base_Id) return String
      is
         Unit_Old_Filename : constant String :=
           Reference.Unit.Get_Filename;
         Directory_Name    : constant String :=
           Containing_Directory (Unit_Old_Filename);
         File_Extension    : constant String :=
           (if Reference.P_Top_Level_Decl (Reference.Unit).Kind in
              Ada_Body_Node
            then
              To_String (Self.Naming_Scheme.Body_Suffix)
            else
              To_String (Self.Naming_Scheme.Spec_Suffix));

         New_Definition_Name : Unbounded_Text_Type;

         Parent : Ada_Node;

         Parent_Dotted_Name : Dotted_Name;

         function Transform (Old_Definition_Name : String) return String;
         --  Transforms all characters to lower case and replaces all `.` by
         --  `-`.

         function Compose
           (Containing_Directory : String;
            Name                 : String;
            Extension            : String)
            return String;
         --  Returns the name of the file with the specified
         --  Containing_Directory, Name, and Extension.
         --  Similar to Ada.Directories.Compose but it won't add '.' between
         --  Name and Extension.

         ---------------
         -- Transform --
         ---------------

         function Transform (Old_Definition_Name : String) return String
         is
            New_Definition_Name    : Unbounded_String;
            Next_May_Be_Upper_Case : Boolean := True;

         begin
            for J in Old_Definition_Name'Range loop
               if Old_Definition_Name (J) = '.' then
                  Append
                    (New_Definition_Name,
                     Self.Naming_Scheme.Dot_Replacement);
                  Next_May_Be_Upper_Case := True;

               else
                  case Self.Naming_Scheme.Casing is
                     when mixedcase =>
                        if Next_May_Be_Upper_Case then
                           Append
                             (New_Definition_Name,
                              To_Upper (Old_Definition_Name (J)));
                        else
                           Append
                             (New_Definition_Name,
                              To_Lower (Old_Definition_Name (J)));
                        end if;
                     when lowercase =>
                        Append
                          (New_Definition_Name,
                           To_Lower (Old_Definition_Name (J)));
                     when uppercase =>
                           Append
                             (New_Definition_Name,
                              To_Upper (Old_Definition_Name (J)));
                  end case;
                  Next_May_Be_Upper_Case := False;
               end if;
            end loop;

            return To_String (New_Definition_Name);
         end Transform;

         -------------
         -- Compose --
         -------------

         function Compose
           (Containing_Directory : String;
            Name                 : String;
            Extension            : String)
            return String
         is
            Dir_Separator : constant Character;
            pragma Import (C, Dir_Separator, "__gnat_dir_separator");
            --  Running system default directory separator

         begin
            return Containing_Directory & Dir_Separator & Name & Extension;
         end Compose;

      begin
         if Reference.Parent.Kind in Ada_Dotted_Name then
            Parent_Dotted_Name := Reference.Parent.As_Dotted_Name;

            --  There are three cases:

            --  1) Foo.Bar - We want to rename Foo and it is the first name

            if Parent_Dotted_Name.F_Prefix = Reference then
               New_Definition_Name := Self.New_Name;

               --  Each parent of `Parent_Dotted_Name` that is an
               --  `Ada_Dotted_Name` has a suffix.

               Parent := Reference.Parent;

               while Parent.Kind in Ada_Dotted_Name_Range loop
                  Append (New_Definition_Name, ".");
                  Append
                    (New_Definition_Name,
                     +Parent.As_Dotted_Name.F_Suffix.Text);

                  Parent := Parent.Parent;
               end loop;

            --  2) Bar.Foo.Baz - We want to rename Foo and it is in the middle

            elsif Parent_Dotted_Name.F_Suffix = Reference
              and then Parent_Dotted_Name.Parent.Kind in Ada_Dotted_Name
            then
               --  `Parent_Dotted_Name` already has all the prefixes needed

               New_Definition_Name :=
                 +Parent_Dotted_Name.F_Prefix.Text & "." & Self.New_Name;
               Parent := Reference.Parent;

               --  Each parent of `Parent_Dotted_Name` that is an
               --  `Ada_Dotted_Name` has a suffix.

               Parent := Parent.Parent;

               while Parent.Kind in Ada_Dotted_Name_Range loop
                  Append (New_Definition_Name, ".");
                  Append
                    (New_Definition_Name,
                     +Parent.As_Dotted_Name.F_Suffix.Text);

                  Parent := Parent.Parent;
               end loop;

            --  3) Bar.Foo - We want to rename Foo and it is the last name

            elsif Parent_Dotted_Name.F_Suffix = Reference
              and then not (Parent_Dotted_Name.Parent.Kind in Ada_Dotted_Name)
            then
               --  `Parent_Dotted_Name` already has all the prefixes needed

               New_Definition_Name :=
                 +Parent_Dotted_Name.F_Prefix.Text & "." & Self.New_Name;

            --  4) Logic error in this algorithm.

            else
               raise Program_Error;
            end if;

            return Compose
              (Directory_Name,
               Transform (+(+New_Definition_Name)),
               File_Extension);

         else
            return Compose
              (Directory_Name,
               Transform (+(+Self.New_Name)),
               File_Extension);
         end if;
      end New_File_Name;

   begin
      if not Is_Valid (Self.Naming_Scheme) then
         Refactor_Trace.Trace
           ("Invalid naming scheme. Safe Rename refactoring will not try to "
            & "rename any source files.");
         return;
      end if;

      for Reference of References loop
         declare
            Enclosing_Defining_Name : constant Defining_Name :=
              Reference.P_Enclosing_Defining_Name;
            Enclosing_Basic_Decl    : constant Basic_Decl :=
              (if Enclosing_Defining_Name.Is_Null then No_Basic_Decl
               else Enclosing_Defining_Name.P_Basic_Decl);

         begin
            --  Only rename sources if Enclosing_Basic_Decl is a top level
            --  declaration of its compilation unit, and its analysis unit
            --  only has one compilation unit.
            if Self.Is_Top_Level_Decl (Enclosing_Basic_Decl)
              and then Enclosing_Basic_Decl.Unit.Root.Kind in
                Ada_Compilation_Unit_Range
              and then
                (Self.Attribute_Value_Provider = null
                 or else Self.Attribute_Value_Provider.all
                          (GNATCOLL.Projects.Spec_Attribute,
                           To_UTF8
                             (Enclosing_Basic_Decl.P_Defining_Name.Text)) =
                           "")
            then
               File_Rename.Filepath :=
                 To_Unbounded_String (Reference.Unit.Get_Filename);
               File_Rename.New_Name :=
                 To_Unbounded_String (New_File_Name (Reference));

               if not Edits.File_Renames.Contains (File_Rename) then
                  Edits.File_Renames.Insert (File_Rename);
               end if;
            end if;
         end;
      end loop;
   end Add_Files_Rename_To_Edits;

end Laltools.Refactor.Safe_Rename;
