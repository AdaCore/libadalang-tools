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
--  This package contains refactoring tools that allow pulling up declarations
--  to an outer scope

with Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Fixed;

with GNAT.String_Split;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Common; use Libadalang.Common;

with Laltools.Common; use Laltools.Common;

package body Laltools.Refactor.Pull_Up_Declaration is

   Tool_Name : constant String := "Pull Up Declaration";

   function "+" (T : Text_Type) return String renames To_UTF8;

   function "+"
     (S : String)
      return Unbounded_String
      renames To_Unbounded_String;

   function "+"
     (US : Unbounded_String)
      return String
      renames To_String;

   function "<" (L, R : Ada_Node) return Boolean is
     (if L = R then
        False
      else
        (if L.Unit = R.Unit then
           Compare (L.Sloc_Range, Start_Sloc (R.Sloc_Range)) in Inside | After
         else
           L.Unit.Get_Filename < R.Unit.Get_Filename));
   --  Compares two Ada_Node, first by their Unit filename, and then by their
   --  Source_Location_Range.

   function "<" (L, R : Basic_Decl) return Boolean is
     (L.As_Ada_Node < R.As_Ada_Node);
   --  Compares two Basic_Decl, first by their Unit filename, and then by their
   --  Source_Location_Range.

   package Basic_Decl_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Basic_Decl,
      "<"          => "<",
      "="          => "=");

   subtype Basic_Decl_Ordered_Set is Basic_Decl_Ordered_Sets.Set;

   function "<" (L, R : Defining_Name) return Boolean is
     (L.As_Ada_Node < R.As_Ada_Node);
   --  Compares two Defining_Name, first by their Unit filename, and then by
   --  their Source_Location_Range.

   package Defining_Name_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Defining_Name,
      "<"          => "<",
      "="          => "=");

   subtype Defining_Name_Ordered_Set is Defining_Name_Ordered_Sets.Set;

   function Get_Local_Declarations
     (Decl : Basic_Decl'Class)
      return Basic_Decl_Ordered_Set;
   --  Gets all Basic_Decl nodes on the same Declarative_Part as Decl

   function Get_Dependencies
     (Definition : Libadalang.Analysis.Defining_Name'Class;
      Recursive  : Boolean := True)
      return Defining_Name_Ordered_Set;
   --  Get the canonical part of all the Defining_Name nodes needed to
   --  correctly define Definition.
   --  If Recursive, then also returns dependencies of the dependencies.

   function Get_Local_Dependencies
     (Definition : Libadalang.Analysis.Defining_Name'Class;
      Recursive  : Boolean := True)
      return Defining_Name_Ordered_Set;
   --  Gets the canonical part of all Defining_Name nodes needed to correctly
   --  define Definition, that are also declared in the same Declarative_Part
   --  as Definition.
   --  If Recursive, then also returns local dependencies of the dependencies.

   type Comments_Position_Type is (Before, After, Both);

   function Get_Declaration_SLOC_Range_With_Comments
     (Decl              : Basic_Decl'Class;
      Comments_Position : Comments_Position_Type := Both)
      return Source_Location_Range;
   --  Gets the Sloc_Range of Decl including any adjacent comments.
   --  Which adjacent comments are included is controlled by Comments_Position.

   type Insertion_Point_Type is
      record
         Filename : Unbounded_String;
         Location : Source_Location;
      end record;

   function Get_Insertion_Point
     (Decl                     : Basic_Decl'Class;
      Try_Subp_Insertion_Point : Boolean := False)
      return Insertion_Point_Type;
   --  Assuming that Decl is the Basic_Decl that will be pulled up, computes
   --  the Insertion_Point_Type where the pulled up declarations should be
   --  inserted. If Try_Subp_Insertion_Point is True, and if Decl is
   --  begin pulled up from a subprogram body, then Decl and its dependencies
   --  are inserted above the subprogram's canonical part.

   function Line_Distance
     (From, To : Token_Reference)
      return Integer
     with Pre => From /= No_Token and then To /= No_Token;
   --  Calculates the difference between From's and To's Start_Line

   function Merge_Intersecting_SLOC_Ranges
     (Unit        : Analysis_Unit;
      SLOC_Ranges : Source_Location_Range_Ordered_Set)
      return Source_Location_Range_Ordered_Set;
   --  Merges the elements of SLOC_Ranges that intersect each other

   procedure Compute_Subp_Dependencies_To_Pull_Up
     (Subp                    : Basic_Decl;
      Declarations_To_Pull_Up : out Basic_Decl_Ordered_Set;
      Parameters_To_Pull_Up   : out Basic_Decl_Ordered_Set)
     with Pre => Is_Subprogram (Subp);
   --  Compute all declarations that need to be pulled up and
   --  parameters that need to be added to Subp.

   package Basic_Decl_To_Ada_Mode_Ordered_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Basic_Decl,
        Element_Type => Ada_Mode,
        "<"          => "<",
        "="          => "=");

   subtype Basic_Decl_To_Ada_Mode_Ordered_Map is
     Basic_Decl_To_Ada_Mode_Ordered_Maps.Map;

   function Compute_Parameters_Mode
     (Subp         : Basic_Decl;
      Object_Decls : Basic_Decl_Ordered_Set)
      return Basic_Decl_To_Ada_Mode_Ordered_Map;
   --  Compute the mode of each parameter that need to be added do Subp

   function Get_Subp_Headers
     (Subps : Basic_Decl_Ordered_Set)
      return Source_Location_Range_Ordered_Set;
   --  For each Basic_Decl of Subps that is a subprogram, gets the
   --  Source_Location_Range of the subprogram header.
   --  TODO: Consider moving this to Get_Declaration_SLOC_Range_With_Comments

   package Source_Location_To_Unbounded_String_Ordered_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Source_Location,
        Element_Type => Unbounded_String,
        "<"          => "<");

   subtype Source_Location_To_Unbounded_String_Ordered_Map is
     Source_Location_To_Unbounded_String_Ordered_Maps.Map;

   function Compute_Parameter_Insertions
     (Subp            : Basic_Decl;
      Parameters_Mode : Basic_Decl_To_Ada_Mode_Ordered_Map)
      return Source_Location_To_Unbounded_String_Ordered_Map;
   --  Computes a map where the keys are Source_Locations in the original
   --  source where the parameters needs to be added and the the elements are
   --  Unbounded_Strings with the content of such parameters.

   function Compute_Actual_Parameter_Insertions
     (Subp            : Basic_Decl;
      Analysis_Units  : Analysis_Unit_Array;
      Parameters_Mode : Basic_Decl_To_Ada_Mode_Ordered_Map)
      return Source_Location_To_Unbounded_String_Ordered_Map;
   --  Computes a map where the keys are Source_Locations in the original
   --  source where the actual parameters needs to be added and the the
   --  elements are Unbounded_Strings with the content of such
   --  actual parameters.

   function Apply_Insertions
     (Unit        : Analysis_Unit;
      SLOC_Ranges : Source_Location_Range_Ordered_Set;
      Insertions  : Source_Location_To_Unbounded_String_Ordered_Map;
      Padding     : Natural := 3)
      return Unbounded_String;
   --  Returns an Unbounded_String with the concatenation of the text of each
   --  Source_Location_Range of SLOC_Range with any insertion found in
   --  Insertions.

   function Remove_Padding
     (This    : String;
      Padding : Natural := 3)
      return String;
   --  Removes exactly the amount of leading whitespaces given by
   --  Padding, if possible.

   -----------------------------
   --  Get_Local_Declarations --
   -----------------------------

   function Get_Local_Declarations
     (Decl : Basic_Decl'Class)
      return Basic_Decl_Ordered_Set
   is
      Enclosing_Declarative_Part : Declarative_Part;

   begin
      return Local_Basic_Decls : Basic_Decl_Ordered_Set do
         for Decl_Part of Decl.P_All_Parts loop
            Enclosing_Declarative_Part :=
              Get_Enclosing_Declarative_Part (Decl_Part);

            for Node of Enclosing_Declarative_Part.F_Decls loop
               if Node.Kind in Ada_Basic_Decl
                 and then Node.As_Basic_Decl.P_Canonical_Part /=
                   Decl.P_Canonical_Part
               then
                  Local_Basic_Decls.Include
                    (Node.As_Basic_Decl.P_Canonical_Part);
               end if;
            end loop;

            if Enclosing_Declarative_Part.Parent.Kind in
                 Ada_Subp_Body_Range
            then
               declare
                  Parent_Subp_Body : constant Basic_Decl :=
                    Enclosing_Declarative_Part.Parent.As_Subp_Body.
                      P_Canonical_Part;
                  Subp_Params      : constant Params :=
                    Get_Subp_Params (Parent_Subp_Body);

               begin
                  if not Subp_Params.Is_Null then
                     for Param_Spec of Subp_Params.F_Params loop
                        Local_Basic_Decls.Include (Param_Spec.As_Basic_Decl);
                     end loop;
                  end if;
               end;
            end if;
         end loop;
      end return;
   end Get_Local_Declarations;

   -----------------------
   --  Get_Dependencies --
   -----------------------

   function Get_Dependencies
     (Definition : Libadalang.Analysis.Defining_Name'Class;
      Recursive  : Boolean := True)
      return Defining_Name_Ordered_Set
   is
      Dependencies : Defining_Name_Ordered_Set;

      function Get_Dependencies_Recursive_Helper
        (Node : Ada_Node'Class)
         return Visit_Status;
      --  Checks if Node is a Name. If so, tries to resolve it precisely and
      --  and adds the referenced Defining_Name to Dependencies.
      --  Then, recursively calls Get_Dependencies on this added referenced
      --  Defining_Name.

      ---------------------------------------
      -- Get_Dependencies_Recursive_Helper --
      ---------------------------------------

      function Get_Dependencies_Recursive_Helper
        (Node : Ada_Node'Class)
         return Visit_Status
      is
         Referenced_Defining_Name : Libadalang.Analysis.Defining_Name;

      begin
         if Node.Kind in Ada_Name then
            Referenced_Defining_Name :=
              Node.As_Name.P_Referenced_Defining_Name;

            if not Referenced_Defining_Name.Is_Null
              and then not Referenced_Defining_Name.Is_Synthetic
              and then Referenced_Defining_Name /= Definition
            then
               Dependencies.Include (Referenced_Defining_Name);

               if Recursive then
                  Dependencies.Union
                    (Get_Dependencies (Referenced_Defining_Name));
               end if;

               return Over;
            end if;
         end if;

         return Into;
      end Get_Dependencies_Recursive_Helper;

   begin
      Definition.P_Basic_Decl.Traverse
        (Get_Dependencies_Recursive_Helper'Access);

      return Dependencies;
   end Get_Dependencies;

   -----------------------------
   --  Get_Local_Dependencies --
   -----------------------------

   function Get_Local_Dependencies
     (Definition : Libadalang.Analysis.Defining_Name'Class;
      Recursive  : Boolean := True)
      return Defining_Name_Ordered_Set
   is
      use Defining_Name_Ordered_Sets;
      Local_Declarations : constant Basic_Decl_Ordered_Set :=
        Get_Local_Declarations (Definition.P_Basic_Decl);

      All_Parts_Local_Dependencies : Defining_Name_Ordered_Set;

   begin
      for Local_Declaration of Local_Declarations loop
         for Local_Defining_Name of Local_Declaration.P_Defining_Names loop
            for Defining_Name_Decl_Part of
                  Definition.P_Basic_Decl.P_All_Parts
            loop
               if Local_Defining_Name.P_Find_Refs
                    (Defining_Name_Decl_Part)'Length > 0
               then
                  All_Parts_Local_Dependencies.Include
                    (Local_Defining_Name.P_Canonical_Part);

                  if Local_Declaration.Kind not in Ada_Param_Spec
                    and then Recursive
                  then
                     Union
                       (All_Parts_Local_Dependencies,
                        Get_Local_Dependencies (Local_Defining_Name));
                  end if;
               end if;
            end loop;
         end loop;
      end loop;

      return All_Parts_Local_Dependencies;
   end Get_Local_Dependencies;

   ----------------------------------------------
   -- Get_Declaration_SLOC_Range_With_Comments --
   ----------------------------------------------

   function Get_Declaration_SLOC_Range_With_Comments
     (Decl              : Basic_Decl'Class;
      Comments_Position : Comments_Position_Type := Both)
      return Source_Location_Range
   is
      type Direction  is (Forward, Backward);

      function Next_Non_Whitespace_Or_Comment
        (Token : Token_Reference;
         Going : Direction)
         return Token_Reference;
      --  Finds the next Token that is not a Whitespace or Comment in the
      --  direction defined by Going.

      ------------------------------------
      -- Next_Non_Whitespace_Or_Comment --
      ------------------------------------

      function Next_Non_Whitespace_Or_Comment
        (Token : Token_Reference;
         Going : Direction)
         return Token_Reference
      is
         Last_Comment  : Token_Reference := No_Token;
         Last_Token    : Token_Reference := No_Token;
         Current_Token : Token_Reference := Token;

      begin
         Last_Token := Current_Token;
         loop
            case Going is
               when Forward => Current_Token := Next (Current_Token);
               when Backward => Current_Token := Previous (Current_Token);
            end case;

            --  Exits when:
            --  - there are no more tokens to process
            --  - there is an empty line between two tokens
            --  - when the last token is not a whole line comment nor a
            --    whitespace
            exit when Current_Token = No_Token
              or else abs (Line_Distance (Current_Token, Last_Token)) > 1
              or else (Kind (Data (Current_Token)) not in
                         Ada_Whitespace | Ada_Comment
                       and then not Is_Whole_Line_Comment (Current_Token));

            if Kind (Data (Current_Token)) in Ada_Comment then
               Last_Comment := Current_Token;
            end if;
            Last_Token := Current_Token;
         end loop;

         return Last_Comment;
      end Next_Non_Whitespace_Or_Comment;

   begin
      case Comments_Position is
         when Before =>
            declare
               Start_Token            : constant Token_Reference :=
                 Next_Non_Whitespace_Or_Comment (Decl.Token_Start, Backward);
               Start_Token_SLOC_Range : constant Source_Location_Range :=
                 (if Start_Token /= No_Token then
                    Sloc_Range (Data (Start_Token))
                  else
                    No_Source_Location_Range);

            begin
               return
                 (if Start_Token_SLOC_Range /= No_Source_Location_Range then
                    (Start_Token_SLOC_Range.Start_Line,
                     Decl.Sloc_Range.End_Line,
                     Start_Token_SLOC_Range.Start_Column,
                     Decl.Sloc_Range.End_Column)
                  else
                    Decl.Sloc_Range);
            end;

         when After =>
            declare
               End_Token            : constant Token_Reference :=
                 Next_Non_Whitespace_Or_Comment (Decl.Token_Start, Forward);
               End_Token_SLOC_Range : constant Source_Location_Range :=
                 (if End_Token /= No_Token then
                    Sloc_Range (Data (End_Token))
                  else
                    No_Source_Location_Range);

            begin
               return
                 (if End_Token_SLOC_Range /= No_Source_Location_Range then
                    (Decl.Sloc_Range.Start_Line,
                     End_Token_SLOC_Range.End_Line,
                     Decl.Sloc_Range.Start_Column,
                     End_Token_SLOC_Range.End_Column)
                  else
                    Decl.Sloc_Range);
            end;

         when Both =>
            declare
               Start_Token            : constant Token_Reference :=
                 Next_Non_Whitespace_Or_Comment (Decl.Token_Start, Backward);
               Start_Token_SLOC_Range : constant Source_Location_Range :=
                 (if Start_Token /= No_Token then
                    Sloc_Range (Data (Start_Token))
                  else
                    Decl.Sloc_Range);
               End_Token            : constant Token_Reference :=
                 Next_Non_Whitespace_Or_Comment (Decl.Token_End, Forward);
               End_Token_SLOC_Range : constant Source_Location_Range :=
                 (if End_Token /= No_Token then
                    Sloc_Range (Data (End_Token))
                  else
                    Decl.Sloc_Range);

            begin
               return
                 (Start_Token_SLOC_Range.Start_Line,
                  End_Token_SLOC_Range.End_Line,
                  Start_Token_SLOC_Range.Start_Column,
                  End_Token_SLOC_Range.End_Column);
            end;
      end case;
   end Get_Declaration_SLOC_Range_With_Comments;

   -------------------------
   -- Get_Insertion_Point --
   -------------------------

   function Get_Insertion_Point
     (Decl                     : Basic_Decl'Class;
      Try_Subp_Insertion_Point : Boolean := False)
      return Insertion_Point_Type
   is
      First_Enclosing_Declarative_Part : constant Declarative_Part :=
        Get_Enclosing_Declarative_Part (Decl);

      Owner_Is_Decl_Block : constant Boolean :=
        First_Enclosing_Declarative_Part.Parent.Kind in Ada_Decl_Block_Range;

      Second_Enclosing_Declarative_Part : constant Declarative_Part :=
        (if Owner_Is_Decl_Block then
           Get_Enclosing_Declarative_Part
             (First_Enclosing_Declarative_Part.Parent)
         else
           No_Declarative_Part);

      Header_SLOC_Range_IGNORE : constant Source_Location_Range :=
        Get_Basic_Decl_Header_SLOC_Range
          (First_Enclosing_Declarative_Part.P_Parent_Basic_Decl);

   begin
      if Try_Subp_Insertion_Point then
         declare
            Subp_Body : Libadalang.Analysis.Subp_Body := No_Subp_Body;

         begin
            for Parent of Decl.Parents (With_Self => False) loop
               if Parent.Kind in Ada_Subp_Body then
                  Subp_Body := Parent.As_Subp_Body;
                  exit;
               end if;
            end loop;

            declare
               Subp_Body_Canonical_Part : constant Basic_Decl :=
                 (if Subp_Body.Is_Null then No_Basic_Decl
                  else Subp_Body.P_Canonical_Part);
               Subp_Header_SLOC_Range   : constant Source_Location_Range :=
                 (if Subp_Body_Canonical_Part.Is_Null then
                    No_Source_Location_Range
                  else
                    Get_Basic_Decl_Header_SLOC_Range
                      (Subp_Body_Canonical_Part));

            begin
               if Subp_Header_SLOC_Range /= No_Source_Location_Range then
                  --  A Subp_Body was found and it's canonical part is
                  --  itself. It also has a header.
                  return
                    Insertion_Point_Type'
                      (To_Unbounded_String
                         (Subp_Body_Canonical_Part.Unit.Get_Filename),
                       Start_Sloc (Subp_Header_SLOC_Range));
               elsif not Subp_Body_Canonical_Part.Is_Null then
                  --  A Subp_Body was found but it does not have a header
                  return
                    Insertion_Point_Type'
                      (To_Unbounded_String
                         (Subp_Body_Canonical_Part.Unit.Get_Filename),
                       (Subp_Body_Canonical_Part.Sloc_Range.Start_Line, 1));
               end if;
            end;
         end;
      end if;

      --  If Try_Subp_Insertion_Point is True and this line was reached, then
      --  we failed to attempt to compute an insertion point right before a
      --  subprogram. Therefore, proced with the default strategy.

      return
        (if Owner_Is_Decl_Block then
            Insertion_Point_Type'
              (To_Unbounded_String
                 (Second_Enclosing_Declarative_Part.Unit.Get_Filename),
               Source_Location'
                 (Second_Enclosing_Declarative_Part.Sloc_Range.End_Line, 1))
         else
           Insertion_Point_Type'
              (To_Unbounded_String
                 (First_Enclosing_Declarative_Part.Unit.Get_Filename),
               Source_Location'
                 (First_Enclosing_Declarative_Part.P_Parent_Basic_Decl.
                    P_Canonical_Part.Sloc_Range.Start_Line,
                  1)));
   end Get_Insertion_Point;

   -------------------
   -- Line_Distance --
   -------------------

   function Line_Distance
     (From, To : Token_Reference)
      return Integer
   is (Integer (Sloc_Range (Data (From)).Start_Line)
       - Integer (Sloc_Range (Data (To)).Start_Line));

   ------------------------------------
   -- Merge_Intersecting_SLOC_Ranges --
   ------------------------------------

   function Merge_Intersecting_SLOC_Ranges
     (Unit        : Analysis_Unit;
      SLOC_Ranges : Source_Location_Range_Ordered_Set)
         return Source_Location_Range_Ordered_Set
   is
      use Source_Location_Range_Ordered_Sets;

      Reduced_SLOC_Ranges : Source_Location_Range_Ordered_Set;
      SLOC_Ranges_Cursor  : Cursor := SLOC_Ranges.First;

   begin
      if Has_Element (SLOC_Ranges_Cursor) then
         Reduced_SLOC_Ranges.Include (SLOC_Ranges.First_Element);
         Next (SLOC_Ranges_Cursor);
      end if;

      while Has_Element (SLOC_Ranges_Cursor) loop
         if Compare
              (Reduced_SLOC_Ranges.Last_Element,
               Start_Sloc (Element (SLOC_Ranges_Cursor)))
            = Inside
         then
            Reduced_SLOC_Ranges.Replace_Element
              (Reduced_SLOC_Ranges.Last,
               Make_Range
                 (Start_Sloc (Reduced_SLOC_Ranges.Last_Element),
                  End_Sloc (Element (SLOC_Ranges_Cursor))));

         elsif Start_Sloc (Element (SLOC_Ranges_Cursor)).Line =
                 Reduced_SLOC_Ranges.Last_Element.End_Line + 1
               and then Start_Sloc (Element (SLOC_Ranges_Cursor)).Column = 1
         then
            declare
               Previous_Line : constant Text_Type :=
                 Unit.Get_Line
                   (Integer (Reduced_SLOC_Ranges.Last_Element.End_Line));

            begin
               if Previous_Line'Length + 1 =
                 Reduced_SLOC_Ranges.Last_Element.End_Column
               then
                  Reduced_SLOC_Ranges.Replace_Element
                    (Reduced_SLOC_Ranges.Last,
                     Make_Range
                       (Start_Sloc (Reduced_SLOC_Ranges.Last_Element),
                        End_Sloc (Element (SLOC_Ranges_Cursor))));

               else
                  Reduced_SLOC_Ranges.Include
                    (Element (SLOC_Ranges_Cursor));
               end if;
            end;

         else
            Reduced_SLOC_Ranges.Include
              (Element (SLOC_Ranges_Cursor));
         end if;
         Next (SLOC_Ranges_Cursor);
      end loop;

      return Reduced_SLOC_Ranges;
   end Merge_Intersecting_SLOC_Ranges;

   ------------------------------------------
   -- Compute_Subp_Dependencies_To_Pull_Up --
   ------------------------------------------

   procedure Compute_Subp_Dependencies_To_Pull_Up
     (Subp                    : Basic_Decl;
      Declarations_To_Pull_Up : out Basic_Decl_Ordered_Set;
      Parameters_To_Pull_Up   : out Basic_Decl_Ordered_Set)
   is
      use Basic_Decl_Ordered_Sets;

      Object_Decl_Dependencies       : Basic_Decl_Ordered_Set;
      Final_Object_Decl_Dependencies : Basic_Decl_Ordered_Set;

   begin
      for Local_Dependency of
            Get_Local_Dependencies (Subp.P_Defining_Name)
      loop
         Declarations_To_Pull_Up.Include
           (Local_Dependency.P_Basic_Decl);
      end loop;

      for Declaration of Declarations_To_Pull_Up loop
         if Declaration.Kind in Ada_Object_Decl | Ada_Param_Spec then
            Object_Decl_Dependencies.Include (Declaration);
         end if;
      end loop;

      for Declaration of Declarations_To_Pull_Up loop
         if Is_Subprogram (Declaration) then
            declare
               Dependencies_Of_Subp_Dependency :
                 constant Defining_Name_Ordered_Set :=
                   Get_Local_Dependencies (Declaration.P_Defining_Name);

            begin
               for Dependency of Dependencies_Of_Subp_Dependency loop
                  if Dependency.P_Basic_Decl.Kind in Ada_Object_Decl then
                     Final_Object_Decl_Dependencies.Include
                       (Dependency.P_Basic_Decl);
                  end if;
               end loop;
            end;
         end if;
      end loop;

      Parameters_To_Pull_Up :=
        Object_Decl_Dependencies - Final_Object_Decl_Dependencies;

      Declarations_To_Pull_Up.Union (Final_Object_Decl_Dependencies);
      Declarations_To_Pull_Up.Difference (Parameters_To_Pull_Up);
   end Compute_Subp_Dependencies_To_Pull_Up;

   -----------------------------
   -- Compute_Parameters_Mode --
   -----------------------------

   function Compute_Parameters_Mode
     (Subp         : Basic_Decl;
      Object_Decls : Basic_Decl_Ordered_Set)
      return Basic_Decl_To_Ada_Mode_Ordered_Map
   is
      Parameters_Mode_Map : Basic_Decl_To_Ada_Mode_Ordered_Map;

   begin
      for Object_Decl of Object_Decls loop
         for Declaration_Part of Subp.P_All_Parts loop
            declare
               References               : constant Ref_Result_Array :=
                 Object_Decl.P_Defining_Name.P_Find_Refs (Declaration_Part);
               First_Is_Write_Reference : Boolean;
               Any_Write_References     : Boolean;

            begin
               if References'Length /= 0 then
                  First_Is_Write_Reference :=
                    Ref (References (References'First)).P_Is_Write_Reference;
                  Any_Write_References :=
                    (for some Reference of
                       References (References'First + 1 .. References'Last)
                     => Ref (Reference).P_Is_Write_Reference);

                  if First_Is_Write_Reference then
                     --  First reference is a write reference so this
                     --  parameter must be an `out` parameter.
                     Parameters_Mode_Map.Include (Object_Decl, Ada_Mode_Out);

                  else
                     --  First reference is a read reference.
                     --  This parameter must be either `in` or `in out`.
                     if Any_Write_References then
                        --  Must be `in out`
                        Parameters_Mode_Map.Include
                          (Object_Decl, Ada_Mode_In_Out);
                     else
                        --  Must be `in`
                        Parameters_Mode_Map.Include (Object_Decl, Ada_Mode_In);
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end loop;

      return Parameters_Mode_Map;
   end Compute_Parameters_Mode;

   ----------------------
   -- Get_Subp_Headers --
   ----------------------

   function Get_Subp_Headers
     (Subps : Basic_Decl_Ordered_Set)
      return Source_Location_Range_Ordered_Set
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      --  ---------------------  -> This is a header edge
      --  -- Subprogram_Name --  -> This is the header body
      --  ---------------------  -> This is a header edge

      function Is_Header_Edge
        (Token           : Token_Reference;
         Subprogram_Name : String)
         return Boolean
      is (Trim (+Text (Token), Both) =
            String'((Subprogram_Name'Length + 6) * "-"));
      --  Checks if Token is the header edge

      function Is_Header_Body
        (Token : Token_Reference;
         Subprogram_Name : String)
         return Boolean
      is (Trim (+Text (Token), Both) =
            "-- " & Subprogram_Name & " --");
      --  Checks if Token is the header body

      Token_0 : Token_Reference;
      Token_1 : Token_Reference;
      Token_2 : Token_Reference;
      Token_3 : Token_Reference;

      Subprogram_Name : Unbounded_String;

      Headers : Source_Location_Range_Ordered_Set;

   begin
      for Subp of Subps loop
         if Is_Subprogram (Subp) then
            for Subprogram_Part of Subp.P_All_Parts loop
               Token_0 := Subprogram_Part.Token_Start;
               Token_1 := Token_0;
               loop
                  Token_1 := Previous (Token_1);
                  exit when Token_1 = No_Token
                    or else Kind (Data (Token_1)) not in
                      Ada_Whitespace;
               end loop;
               Token_2 := Token_1;
               loop
                  Token_2 := Previous (Token_2);
                  exit when Token_2 = No_Token
                    or else Kind (Data (Token_2)) not in
                      Ada_Whitespace;
               end loop;
               Token_3 := Token_2;
               loop
                  Token_3 := Previous (Token_3);
                  exit when Token_3 = No_Token
                    or else Kind (Data (Token_3)) not in
                      Ada_Whitespace;
               end loop;

               Subprogram_Name :=
                 +(+(Subprogram_Part.P_Defining_Name.Text));

               if Is_Whole_Line_Comment (Token_1)
                 and then Is_Whole_Line_Comment (Token_2)
                 and then Is_Whole_Line_Comment (Token_3)
                 and then abs (Line_Distance (Token_2, Token_1)) = 1
                 and then abs (Line_Distance (Token_3, Token_2)) = 1
                 and then Is_Header_Edge (Token_1, +Subprogram_Name)
                 and then Is_Header_Body (Token_2, +Subprogram_Name)
                 and then Is_Header_Edge (Token_3, +Subprogram_Name)
               then
                  Headers.Include
                    ((Sloc_Range (Data (Token_3)).Start_Line,
                      Sloc_Range (Data (Token_1)).Start_Line,
                      Sloc_Range (Data (Token_3)).Start_Column,
                      Sloc_Range (Data (Token_3)).End_Column));
               end if;
            end loop;
         end if;
      end loop;
      return Headers;
   end Get_Subp_Headers;

   ----------------------------------
   -- Compute_Parameter_Insertions --
   ----------------------------------

   function Compute_Parameter_Insertions
     (Subp            : Basic_Decl;
      Parameters_Mode : Basic_Decl_To_Ada_Mode_Ordered_Map)
      return Source_Location_To_Unbounded_String_Ordered_Map
   is
      use Ada.Characters.Latin_1;
      use Basic_Decl_To_Ada_Mode_Ordered_Maps;

      Other_Insertion_Point : Source_Location;
      New_Parameters        : Unbounded_String;

      Subp_Params           : Params;
      Subp_Param_Specs      : Param_Spec_List;
      Subp_First_Param_Spec : Param_Spec;
      Subp_Last_Param_Spec  : Param_Spec;
      Indentation           : Natural;

      Parameters_Cursor     :
        Basic_Decl_To_Ada_Mode_Ordered_Maps.Cursor :=
          Parameters_Mode.First;
      Parameter_Declaration : Basic_Decl;
      Parameter_Definition  : Defining_Name;
      Parameter_Type        : Type_Expr;

      Parameters_Insertions :
        Source_Location_To_Unbounded_String_Ordered_Map;

   begin
      if Has_Element (Parameters_Cursor) then
         for Declaration_Part of Subp.P_All_Parts loop
            New_Parameters := Null_Unbounded_String;
            Parameters_Cursor :=
              Parameters_Mode.First;
            Subp_Params := Get_Subp_Params (Declaration_Part);
            if not Subp_Params.Is_Null then
               Subp_Param_Specs := Subp_Params.F_Params;
               Subp_First_Param_Spec :=
                 Param_Spec_List_Element
                   (Subp_Param_Specs,
                    Param_Spec_List_First (Subp_Param_Specs)).As_Param_Spec;
               Subp_Last_Param_Spec :=
                 Param_Spec_List_Element
                   (Subp_Param_Specs, Length (Subp_Param_Specs)).As_Param_Spec;
               Indentation :=
                 Natural (Subp_First_Param_Spec.Sloc_Range.Start_Column - 1);
               Other_Insertion_Point :=
                 End_Sloc (Subp_Last_Param_Spec.Sloc_Range);
               while Has_Element (Parameters_Cursor) loop
                  Parameter_Declaration := Key (Parameters_Cursor);
                  Parameter_Definition :=
                    Parameter_Declaration.P_Defining_Name;
                  Parameter_Type :=
                    Parameter_Declaration.P_Type_Expression;
                  Append (New_Parameters, ";" & LF);
                  Append (New_Parameters, Indentation * " ");
                  Append
                    (New_Parameters,
                     +Parameter_Definition.Text
                     & " : "
                     & (case Element (Parameters_Cursor) is
                          when Ada_Mode_Out_Range    => "out ",
                          when Ada_Mode_In_Out_Range => "in out ",
                          when others                => "")
                     & (+Parameter_Type.Text));

                  Next (Parameters_Cursor);
               end loop;
               Parameters_Insertions.Insert
                 (Other_Insertion_Point, New_Parameters);

            else
               Other_Insertion_Point :=
                 End_Sloc (Declaration_Part.P_Defining_Name.Sloc_Range);
               Indentation :=
                 Natural
                   (Declaration_Part.P_Defining_Name.Sloc_Range.End_Column)
                 + 1;
               Append (New_Parameters, " (");
               Parameter_Declaration :=
                 Key (Parameters_Cursor);
               Parameter_Definition :=
                 Parameter_Declaration.P_Defining_Name;
               Parameter_Type :=
                 Parameter_Declaration.P_Type_Expression;
               Append
                 (New_Parameters,
                  (+Parameter_Definition.Text)
                  & " : "
                  & (case Element (Parameters_Cursor) is
                       when Ada_Mode_Out_Range    => "out ",
                       when Ada_Mode_In_Out_Range => "in out ",
                       when others                => "")
                  & (+Parameter_Type.Text));
               Next (Parameters_Cursor);
               while Has_Element (Parameters_Cursor) loop
                  Parameter_Declaration := Key (Parameters_Cursor);
                  Parameter_Definition :=
                    Parameter_Declaration.P_Defining_Name;
                  Parameter_Type :=
                    Parameter_Declaration.P_Type_Expression;
                  Append (New_Parameters, ";" & LF);
                  Append (New_Parameters, Indentation * " ");
                  Append
                    (New_Parameters,
                     (+Parameter_Definition.Text)
                     & " : "
                     & (case Element (Parameters_Cursor) is
                          when Ada_Mode_Out_Range    => "out ",
                          when Ada_Mode_In_Out_Range => "in out ",
                          when others                => "")
                     & (+Parameter_Type.Text));
                  Next (Parameters_Cursor);
               end loop;
               Append (New_Parameters, ")");
               Parameters_Insertions.Insert
                 (Other_Insertion_Point, New_Parameters);
            end if;
         end loop;
      end if;

      return Parameters_Insertions;
   end Compute_Parameter_Insertions;

   -----------------------------------------
   -- Compute_Actual_Parameter_Insertions --
   -----------------------------------------

   function Compute_Actual_Parameter_Insertions
     (Subp            : Basic_Decl;
      Analysis_Units  : Analysis_Unit_Array;
      Parameters_Mode : Basic_Decl_To_Ada_Mode_Ordered_Map)
      return Source_Location_To_Unbounded_String_Ordered_Map
   is
      use Ada.Characters.Latin_1;
      use Basic_Decl_To_Ada_Mode_Ordered_Maps;

      Calls_References  : constant Ref_Result_Array :=
        P_Find_All_Calls (Subp.P_Defining_Name, Analysis_Units);
      Call              : Call_Stmt;
      Call_Expr         : Libadalang.Analysis.Call_Expr;
      Call_Kind         : Ada_Node_Kind_Type;
      Param_Assoc_List  : Assoc_List;
      First_Param_Assoc : Param_Assoc;
      Has_Designators   : Boolean;
      Parameters_Cursor : Basic_Decl_To_Ada_Mode_Ordered_Maps.Cursor;
      Object_Decl       : Basic_Decl;
      Actual_Parameters : Unbounded_String;
      Indentation       : Natural;

      Insertions : Source_Location_To_Unbounded_String_Ordered_Map;

   begin
      if Parameters_Mode.Is_Empty then
         return Insertions;
      end if;

      for Call_Reference of Calls_References loop
         Parameters_Cursor := Parameters_Mode.First;
         Actual_Parameters := Null_Unbounded_String;

         if Ref (Call_Reference).Parent.Kind in Ada_Call_Stmt then
            Call := Ref (Call_Reference).Parent.As_Call_Stmt;
            Indentation := Natural (Call.Sloc_Range.End_Column) + 2 - 1;
            Object_Decl := Key (Parameters_Cursor);
            Append (Actual_Parameters, " (");
            Append
              (Actual_Parameters,
               (if Object_Decl.Kind in Ada_Object_Decl then
                  +Object_Decl.As_Object_Decl.F_Ids.Text
                else
                  +Object_Decl.As_Param_Spec.F_Ids.Text));
            Append (Actual_Parameters, " => ");
            Append
              (Actual_Parameters,
               (if Object_Decl.Kind in Ada_Object_Decl then
                  +Object_Decl.As_Object_Decl.F_Ids.Text
                else
                  +Object_Decl.As_Param_Spec.F_Ids.Text));
            Next (Parameters_Cursor);
            while Has_Element (Parameters_Cursor) loop
               Object_Decl := Key (Parameters_Cursor);
               Append (Actual_Parameters, "," & LF);
               Append (Actual_Parameters, Indentation * " ");
               Append
                 (Actual_Parameters,
                  (if Object_Decl.Kind in Ada_Object_Decl then
                     +Object_Decl.As_Object_Decl.F_Ids.Text
                   else
                     +Object_Decl.As_Param_Spec.F_Ids.Text));
               Append (Actual_Parameters, " => ");
               Append
                 (Actual_Parameters,
                  (if Object_Decl.Kind in Ada_Object_Decl then
                     +Object_Decl.As_Object_Decl.F_Ids.Text
                   else
                     +Object_Decl.As_Param_Spec.F_Ids.Text));
               Next (Parameters_Cursor);
            end loop;
            Append (Actual_Parameters, ")");
            Insertions.Include
              ((Call.F_Call.Sloc_Range.End_Line,
                Call.F_Call.Sloc_Range.End_Column),
               Actual_Parameters);

         elsif Ref (Call_Reference).Parent.Kind in Ada_Call_Expr then
            Call_Expr := Ref (Call_Reference).Parent.As_Call_Expr;
            Call_Kind := Call_Expr.F_Suffix.Kind;
            case Call_Kind is
               when Ada_Assoc_List_Range =>
                  Param_Assoc_List :=
                    Call_Expr.F_Suffix.As_Assoc_List;
                  for Param_Assoc of Param_Assoc_List loop
                     First_Param_Assoc := Param_Assoc.As_Param_Assoc;
                     Has_Designators :=
                       not First_Param_Assoc.F_Designator.Is_Null;
                     exit;
                  end loop;
                  Indentation :=
                    Natural (First_Param_Assoc.Sloc_Range.Start_Column) - 1;
                  if Has_Designators then
                     while Has_Element (Parameters_Cursor) loop
                        Object_Decl := Key (Parameters_Cursor);
                        Append (Actual_Parameters, "," & LF);
                        Append (Actual_Parameters, Indentation * " ");
                        Append
                          (Actual_Parameters,
                           +Object_Decl.As_Object_Decl.F_Ids.Text);
                        Append (Actual_Parameters, " => ");
                        Append
                          (Actual_Parameters,
                           +Object_Decl.As_Object_Decl.F_Ids.Text);
                        Next (Parameters_Cursor);
                     end loop;
                  else
                     while Has_Element (Parameters_Cursor) loop
                        Object_Decl := Key (Parameters_Cursor);
                        Append (Actual_Parameters, "," & LF);
                        Append (Actual_Parameters, Indentation * " ");
                        Append
                          (Actual_Parameters,
                           (if Object_Decl.Kind in Ada_Object_Decl then
                              +Object_Decl.As_Object_Decl.F_Ids.Text
                            else
                              +Object_Decl.As_Param_Spec.F_Ids.Text));
                        Next (Parameters_Cursor);
                     end loop;
                  end if;

                  Insertions.Include
                    ((Param_Assoc_List.Sloc_Range.End_Line,
                     Param_Assoc_List.Sloc_Range.End_Column),
                     Actual_Parameters);
               when others =>
                  null;
            end case;
         end if;
      end loop;
      return Insertions;
   end Compute_Actual_Parameter_Insertions;

   ----------------------
   -- Apply_Insertions --
   ----------------------

   function Apply_Insertions
     (Unit        : Analysis_Unit;
      SLOC_Ranges : Source_Location_Range_Ordered_Set;
      Insertions  : Source_Location_To_Unbounded_String_Ordered_Map;
      Padding     : Natural := 3)
      return Unbounded_String
   is
      use Ada.Characters.Latin_1;
      use Source_Location_To_Unbounded_String_Ordered_Maps;

      Insertions_Cursor :
        Source_Location_To_Unbounded_String_Ordered_Maps.Cursor :=
          Insertions.Last;
      Insertion_Point   : Source_Location :=
        (if Has_Element (Insertions_Cursor) then  Key (Insertions_Cursor)
         else No_Source_Location);
      Insertion_Text    : Unbounded_String :=
        (if Has_Element (Insertions_Cursor) then Element (Insertions_Cursor)
         else Null_Unbounded_String);

      Extracted_Text : Unbounded_String;

   begin
      for SLOC_Range of reverse SLOC_Ranges loop
         if SLOC_Range.Start_Line = SLOC_Range.End_Line then
            --  Only one line to pull up
            if Has_Element (Insertions_Cursor)
              and then SLOC_Range.Start_Line = Insertion_Point.Line
            then
               declare
                  Line        : constant Text_Type :=
                    Unit.Get_Line (Integer (SLOC_Range.Start_Line));
                  Start_Index : Integer :=
                    Line'First + Integer (Insertion_Point.Column) - 1;
                  End_Index   : Integer :=
                    Line'First + Integer (SLOC_Range.End_Column) - 2;

                  Slice           : Unbounded_String;
                  Text_To_Extract : Unbounded_String;

               begin
                  --  Add the intertions while they belong to this
                  --  line.
                  loop
                     Slice := +(+Line (Start_Index .. End_Index));
                     Text_To_Extract :=
                       Insertion_Text & Slice & Text_To_Extract;

                     --  Go to the next insertion and update the
                     --  intersion data if it exists. Otherwise,
                     --  Append the last slice and exit.
                     Previous (Insertions_Cursor);
                     if Has_Element (Insertions_Cursor) then
                        Insertion_Point := Key (Insertions_Cursor);
                        Insertion_Text := Element (Insertions_Cursor);

                     else
                        Start_Index :=
                          Line'First + Integer (SLOC_Range.Start_Column) - 1;
                        End_Index := Start_Index - 1;

                        Slice :=
                          (Start_Index - Line'First) * " "
                          & (+(+Line (Start_Index .. End_Index)));
                        Text_To_Extract :=
                          LF & Slice & Text_To_Extract;
                        Extracted_Text :=
                          Text_To_Extract & Extracted_Text;
                        exit;
                     end if;

                     --  Update Start/End_Index taking into account if
                     --  the next intersection is in this line.
                     --  Also append the last slice and exit if it is
                     --  not.
                     if SLOC_Range.Start_Line /=
                       Insertion_Point.Line
                     then
                        Start_Index :=
                          Line'First
                            + Integer (SLOC_Range.Start_Column) - 1;
                        End_Index := Start_Index - 1;

                        Slice :=
                          (Start_Index - Line'First) * " "
                          & (+(+Line (Start_Index .. End_Index)));
                        Text_To_Extract :=
                          LF & Slice & Text_To_Extract;
                        Extracted_Text :=
                          Text_To_Extract & Extracted_Text;
                        exit;

                     else
                        Start_Index :=
                          Line'First
                            + Integer (Insertion_Point.Column)
                          - 1;
                        End_Index := Start_Index - 1;
                     end if;
                  end loop;
               end;

            else
               declare
                  Line        : constant Text_Type :=
                    Unit.Get_Line (Integer (SLOC_Range.Start_Line));
                  Start_Index : constant Natural :=
                    Line'First + Integer (SLOC_Range.Start_Column) - 1;
                  End_Index   : constant Natural :=
                    Line'First + Integer (SLOC_Range.End_Column) - 2;
                  Line_Text   : constant Unbounded_String :=
                    (Start_Index - Line'First) * " "
                    & (+(+(Line (Start_Index .. End_Index))));

               begin
                  Extracted_Text := LF & Line_Text & Extracted_Text;
               end;
            end if;

         else
            --  Multiples lines to extract
            for Line_Number in reverse
              SLOC_Range.Start_Line .. SLOC_Range.End_Line
            loop
               if Has_Element (Insertions_Cursor)
                 and then Line_Number = Insertion_Point.Line
               then
                  --  There is a least one insertion point in this
                  --  line.
                  declare
                     Line        : constant Text_Type :=
                       Unit.Get_Line (Integer (Line_Number));
                     Start_Index : Integer :=
                       Line'First + Integer (Insertion_Point.Column) - 1;
                     End_Index   : Integer :=
                       (if Line_Number = SLOC_Range.End_Line then
                           Line'First + Integer (SLOC_Range.End_Column) - 2
                        else
                           Line'Last);

                     Slice           : Unbounded_String;
                     Text_To_Extract : Unbounded_String;

                  begin
                     --  Add the intertions while they belong to this
                     --  line.
                     loop
                        Slice := +(+(Line (Start_Index .. End_Index)));
                        Text_To_Extract :=
                          Insertion_Text & Slice & Text_To_Extract;

                        --  Go to the next insertion and update the
                        --  insertion data if it exists. Otherwise,
                        --  Append the last slice and exit.
                        Previous (Insertions_Cursor);
                        if Has_Element (Insertions_Cursor) then
                           Insertion_Point :=
                             Key (Insertions_Cursor);
                           Insertion_Text :=
                             Element (Insertions_Cursor);
                        else
                           End_Index := Start_Index - 1;
                           Start_Index :=
                             (if Line_Number =
                                SLOC_Range.Start_Line
                              then
                                Line'First
                                + Integer (SLOC_Range.Start_Column)
                                - 1
                              else
                                Line'First);

                           Slice :=
                             (Start_Index - Line'First) * " "
                             & (+(+Line (Start_Index .. End_Index)));
                           Text_To_Extract := Slice & Text_To_Extract;
                           Extracted_Text :=
                             LF & Text_To_Extract & Extracted_Text;
                           exit;
                        end if;

                        --  Update Start/End_Index taking into account
                        --  if the next interstion is in this line.
                        --  Also append the last slice and exit if it
                        --  is not.
                        if Line_Number /=
                          Insertion_Point.Line
                        then
                           End_Index := Start_Index - 1;
                           Start_Index :=
                             (if Line_Number =  SLOC_Range.Start_Line then
                                Line'First
                                + Integer (SLOC_Range.Start_Column)
                                - 1
                              else
                                Line'First);

                           Slice :=
                             (Start_Index - Line'First) * " "
                             & (+(+(Line (Start_Index .. End_Index))));
                           Text_To_Extract :=
                             Slice & Text_To_Extract;
                           Extracted_Text :=
                             LF & Text_To_Extract & Extracted_Text;
                           exit;
                        else
                           End_Index := Start_Index - 1;
                           Start_Index :=
                             Line'First + Integer (Insertion_Point.Column) - 1;
                        end if;
                     end loop;
                  end;
               else
                  declare
                     Line            : constant Text_Type :=
                       Unit.Get_Line
                         (Integer (Line_Number));
                     Start_Index     : constant Integer :=
                       (if Line_Number =
                          SLOC_Range.Start_Line
                        then
                          Line'First
                          + Integer (SLOC_Range.Start_Column)
                          - 1
                        else Line'First);
                     End_Index       : constant Integer :=
                       (if Line_Number =
                          SLOC_Range.End_Line
                        then
                          Line'First
                          + Integer (SLOC_Range.End_Column)
                          - 2
                        else
                           Line'Last);
                     Line_Text   : constant Unbounded_String :=
                       (Start_Index - Line'First) * " "
                       & (+(+(Line (Start_Index .. End_Index))));

                  begin
                     Extracted_Text :=
                       LF & Line_Text & Extracted_Text;
                  end;
               end if;
            end loop;
         end if;
      end loop;

      --  Extracted_Text now has all the declarations that need to be
      --  pulled up, with the original indentation.
      --  When possible, remove the ammount of indentation given by
      --  Padding.

      declare
         use GNAT.String_Split;

         Extracted_Text_Lines : constant Slice_Set :=
           Create (+Extracted_Text, "" & LF);
         Cursor : Slice_Number := First_Cursor (Extracted_Text_Lines);

      begin
         Extracted_Text := Null_Unbounded_String;
         while Has_Element (Extracted_Text_Lines, Cursor) loop
            Append
              (Extracted_Text,
               (if Cursor = Slice_Count (Extracted_Text_Lines) then
                  Remove_Padding
                    (Slice (Extracted_Text_Lines, Cursor), Padding)
                else
                  Remove_Padding
                  (Slice (Extracted_Text_Lines, Cursor) & LF, Padding)));
            Cursor := Advance (Extracted_Text_Lines, Cursor);
         end loop;
      end;

      return Extracted_Text;
   end Apply_Insertions;

   --------------------
   -- Remove_Padding --
   --------------------

   function Remove_Padding
     (This    : String;
      Padding : Natural := 3)
      return String
   is
      Padding_Whitespaces : constant String := +(Padding * " ");

   begin
      return
        (if This'Length >= Padding
           and then This (This'First .. This'First + Padding - 1) =
                      Padding_Whitespaces
         then
            This (This'First + Padding .. This'Last)
         else
            This);
   end Remove_Padding;

   --------------------------------------
   -- Is_Pull_Up_Declaration_Available --
   --------------------------------------

   function Is_Pull_Up_Declaration_Available
     (Unit      : Analysis_Unit;
      Node_SLOC : Source_Location)
      return Boolean
   is
      Node                              : constant Ada_Node :=
        Unit.Root.Lookup (Node_SLOC);
      Enclosing_Declaration             : constant Basic_Decl :=
        (if not Node.Is_Null
         and then Node.Kind in Ada_Name
         and then not Node.As_Name.P_Enclosing_Defining_Name.Is_Null
         and then not Node.As_Name.P_Enclosing_Defining_Name.P_Basic_Decl.
           Is_Null
         then
            Node.As_Name.P_Enclosing_Defining_Name.P_Basic_Decl
         else
            No_Basic_Decl);

      --  Enclosing_Declaration must be in a declarative part that has a parent
      --  declarative part.
      Enclosing_Declarative_Part        : Declarative_Part;
      Second_Enclosing_Declarative_Part : Declarative_Part;

   begin
      --  If Enclosing_Declaration is a top level declaration, it can't be
      --  extracted.
      if Enclosing_Declaration.Is_Null
        or else Enclosing_Declaration.Parent.Is_Null
        or else Enclosing_Declaration.Parent.Parent.Is_Null
        or else Enclosing_Declaration.Parent.Parent.Kind not in
          Ada_Declarative_Part_Range
      then
         return False;
      end if;
      Enclosing_Declarative_Part  :=
        Enclosing_Declaration.Parent.Parent.As_Declarative_Part;

      --  Do not pull up declarations inside package's declarative part.
      if Enclosing_Declarative_Part.Kind in
           Ada_Public_Part_Range | Ada_Private_Part_Range
        or else Enclosing_Declarative_Part.Parent.Kind in
                  Ada_Package_Body_Range
      then
         return False;
      end if;

      --  There must be a higher level declarative part where
      --  Enclosing_Declaration can be extracted. This means that a declaration
      --  declared in the declarative part of a top level declaration cannot
      --  be extracted.

      Second_Enclosing_Declarative_Part :=
        Get_Enclosing_Declarative_Part (Enclosing_Declarative_Part.Parent);
      if Second_Enclosing_Declarative_Part.Is_Null then
         return False;
      end if;

      return True;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Is_Refactoring_Tool_Available_Default_Error_Message (Tool_Name));
         return False;
   end Is_Pull_Up_Declaration_Available;

   -----------------------------------
   -- Create_Declaration_Pull_Upper --
   -----------------------------------

   function Create_Declaration_Pull_Upper
     (Unit              : Analysis_Unit;
      Declaration_SLOC  : Source_Location;
      Indentation       : Natural := 3;
      Only_Dependencies : Boolean := False;
      Try_Subp_Insertion_Point : Boolean := False)
      return Declaration_Extractor
   is ((Declaration                     =>
          (Unit.Root.Lookup (Declaration_SLOC).As_Name.
             P_Enclosing_Defining_Name.P_Basic_Decl.P_Canonical_Part),
        Indentation                     => Indentation,
        Only_Dependencies               => Only_Dependencies,
        Try_Subp_Insertion_Point => Try_Subp_Insertion_Point));

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Declaration_Extractor;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      Unit : constant Analysis_Unit := Self.Declaration.Unit;

      Insertion_Point : constant Insertion_Point_Type :=
        Get_Insertion_Point
          (Self.Declaration, Self.Try_Subp_Insertion_Point);

      Declarations_To_Pull_Up : Unbounded_String;

      Text_Edits : Text_Edit_Map;
      Edits      : Refactoring_Edits;

      procedure Append
        (Text       : in out Unbounded_String;
         Unit       : Analysis_Unit;
         SLOC_Range : Source_Location_Range);
      --  Adds to Text the text in the SLOC_Range of Unit, removing leading
      --  whitespaces (the amount of whitespaces is given by Padding).

      procedure Process_Non_Subprogram;
      --  Pull up a declaration that is not a subprogram

      procedure Process_Subprogram;
      --  Pull up a declaration that is a subprogram

      ------------
      -- Append --
      ------------

      procedure Append
        (Text       : in out Unbounded_String;
         Unit       : Analysis_Unit;
         SLOC_Range : Source_Location_Range)
      is
         use Ada.Characters.Latin_1;

      begin
         if SLOC_Range.Start_Line = SLOC_Range.End_Line then
            declare
               Start_Line       : constant Text_Type :=
                 Unit.Get_Line (Positive (SLOC_Range.Start_Line));
               Start_Line_Slice : constant String :=
                 +(Start_Line
                     (Start_Line'First + Positive (SLOC_Range.Start_Column) - 1
                      .. Start_Line'First + Positive (SLOC_Range.End_Column)
                         - 2));

            begin
               Append (Text, Remove_Padding (Start_Line_Slice) & LF);
            end;

         elsif SLOC_Range.End_Line = SLOC_Range.Start_Line + 1 then
            declare
               Start_Line       : constant Text_Type :=
                 Unit.Get_Line (Positive (SLOC_Range.Start_Line));
               Start_Line_Slice : constant String :=
                 +(Start_Line
                     (Start_Line'First + Positive (SLOC_Range.Start_Column) - 1
                      .. Start_Line'Last));

               End_Line       : constant Text_Type :=
                 Unit.Get_Line (Positive (SLOC_Range.End_Line));
               End_Line_Slice : constant String :=
                 +End_Line
                   (End_Line'First ..
                    End_Line'First + Positive (SLOC_Range.End_Column) - 2);

            begin
               Append (Text, Remove_Padding (Start_Line_Slice) & LF);
               Append (Text, Remove_Padding (End_Line_Slice) & LF);
            end;
         else
            declare
               Start_Line       : constant Text_Type :=
                 Unit.Get_Line (Positive (SLOC_Range.Start_Line));
               Start_Line_Slice : constant String :=
                 +Start_Line
                    (Start_Line'First + Positive (SLOC_Range.Start_Column) - 1
                     .. Start_Line'Last);

               End_Line       : constant Text_Type :=
                 Unit.Get_Line (Positive (SLOC_Range.End_Line));
               End_Line_Slice : constant String :=
                 +End_Line
                    (End_Line'First
                     .. End_Line'First + Positive (SLOC_Range.End_Column) - 2);

            begin
               Append (Text, Remove_Padding (Start_Line_Slice) & LF);
               for Line_Number in
                 SLOC_Range.Start_Line + 1 .. SLOC_Range.End_Line - 1
               loop
                  Append
                    (Text,
                     Remove_Padding (+Unit.Get_Line (Positive (Line_Number)))
                     & LF);
               end loop;
               Append
                 (Text,
                  Remove_Padding (End_Line_Slice)
                  & LF);
            end;
         end if;
      end Append;

      ----------------------------
      -- Process_Non_Subprogram --
      ----------------------------

      procedure Process_Non_Subprogram is
         SLOC_Ranges_To_Pull_Up : Source_Location_Range_Ordered_Set;

      begin
         for Local_Dependency of
           Get_Local_Dependencies (Self.Declaration.P_Defining_Name)
         loop
            for Local_Dependency_Part of
                  Local_Dependency.P_Basic_Decl.P_All_Parts
            loop
               SLOC_Ranges_To_Pull_Up.Include
                 (Get_Declaration_SLOC_Range_With_Comments
                    (Local_Dependency_Part));
            end loop;
         end loop;

         if not Self.Only_Dependencies then
            for Declaration_Part of Self.Declaration.P_All_Parts loop
               SLOC_Ranges_To_Pull_Up.Include
                 (Get_Declaration_SLOC_Range_With_Comments (Declaration_Part));
            end loop;
         end if;

         SLOC_Ranges_To_Pull_Up :=
           Expand_SLOC_Ranges (Unit, SLOC_Ranges_To_Pull_Up);
         SLOC_Ranges_To_Pull_Up :=
           Merge_Intersecting_SLOC_Ranges (Unit, SLOC_Ranges_To_Pull_Up);

         for SLOC_Range of SLOC_Ranges_To_Pull_Up loop
            Safe_Insert
              (Edits     => Text_Edits,
               File_Name => Unit.Get_Filename,
               Edit      => Text_Edit'(SLOC_Range, Null_Unbounded_String));
            Append
              (Declarations_To_Pull_Up,
               Unit,
               SLOC_Range);
         end loop;

         Safe_Insert
           (Edits     => Text_Edits,
            File_Name => To_String (Insertion_Point.Filename),
            Edit      =>
              Text_Edit'
                (Make_Range
                   (Insertion_Point.Location,
                    Insertion_Point.Location),
                 Declarations_To_Pull_Up));
      end Process_Non_Subprogram;

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram is
         Declarations_To_Pull_Up       : Basic_Decl_Ordered_Set;
         Parameters_To_Pull_Up         : Basic_Decl_Ordered_Set;
         Subprogram_Headers            : Source_Location_Range_Ordered_Set;
         Parameters_Mode               : Basic_Decl_To_Ada_Mode_Ordered_Map;
         Parameter_Insertions          :
           Source_Location_To_Unbounded_String_Ordered_Map;
         Actual_Parameter_Insertions   :
           Source_Location_To_Unbounded_String_Ordered_Map;
         Declarations_To_Pull_Up_SLOCs : Source_Location_Range_Ordered_Set;
         Declarations_To_Pull_Up_Text  : Unbounded_String;

      begin
         --  For a subprogram, some object declarations can now be passed as a
         --  new parameter. So compute which declarations need to be pulled up
         --  and which object declarations need to be added as parameters.
         Compute_Subp_Dependencies_To_Pull_Up
           (Subp                    => Self.Declaration,
            Declarations_To_Pull_Up => Declarations_To_Pull_Up,
            Parameters_To_Pull_Up   => Parameters_To_Pull_Up);

         if not Self.Only_Dependencies then
            Declarations_To_Pull_Up.Include (Self.Declaration);
         end if;

         --  For any dependency in Declarations_To_Pull_Up that is a
         --  subprogram, including Self.Declaration, find their headers.
         Subprogram_Headers := Get_Subp_Headers (Declarations_To_Pull_Up);

         --  For each declaration that needs to be added as a parameter,
         --  compute which parameter mode it needs to have.
         Parameters_Mode :=
           Compute_Parameters_Mode
             (Subp         => Self.Declaration,
              Object_Decls => Parameters_To_Pull_Up);

         --  Formal parameters need to be added to the spec of
         --  Self.Declaration. Compute the Source_Location where they need to
         --  be added in the spec.
         Parameter_Insertions :=
           Compute_Parameter_Insertions
             (Subp            => Self.Declaration,
              Parameters_Mode => Parameters_Mode);

         --  Actual parameters need to be added to Self.Declaration's calls.
         --  Compute the Source_Location where they need to be added in
         --  such calls.
         Actual_Parameter_Insertions :=
           Compute_Actual_Parameter_Insertions
             (Subp            => Self.Declaration,
              Analysis_Units  => Analysis_Units.all,
              Parameters_Mode => Parameters_Mode);

         --  Declarations_To_Pull_Up now have at least the canonical part
         --  of each dependency. For each dependency, and for each part of
         --  the dependency, compute it's Source_Location_Range, including
         --  leading whitespaces, adjacent comments and blank lines that
         --  follow immidiatelly after.
         for Declaration of Declarations_To_Pull_Up loop
            for Declaration_Part of Declaration.P_All_Parts loop
               Declarations_To_Pull_Up_SLOCs.Include
                 (Get_Declaration_SLOC_Range_With_Comments
                    (Declaration_Part));
            end loop;
         end loop;
         Declarations_To_Pull_Up_SLOCs.Union (Subprogram_Headers);
         Declarations_To_Pull_Up_SLOCs :=
           Expand_SLOC_Ranges (Unit, Declarations_To_Pull_Up_SLOCs);
         Declarations_To_Pull_Up_SLOCs := Merge_Intersecting_SLOC_Ranges
           (Unit, Declarations_To_Pull_Up_SLOCs);

         --  With the Source_Location_Ranges that were computed above,
         --  we now know what text needs to be extracted. That text
         --  includes the spec and body of Self.Declaration. So extract
         --  all that text while inserting the formal parameters in
         --  Self.Declaration's spec and body.
         Declarations_To_Pull_Up_Text :=
           Apply_Insertions
             (Unit, Declarations_To_Pull_Up_SLOCs, Parameter_Insertions);

         --  Replace the Source_Location_Ranges of the declarations that
         --  need to be pulled up by an empty string.
         for SLOC_Range of Declarations_To_Pull_Up_SLOCs loop
            Safe_Insert
              (Edits     => Text_Edits,
               File_Name => Self.Declaration.Unit.Get_Filename,
               Edit      =>
                 Text_Edit'
                   (SLOC_Range,
                    Null_Unbounded_String));
         end loop;

         --  Insert the pulled up declarations in the parent Declarative_Part
         Safe_Insert
           (Edits     => Text_Edits,
            File_Name => To_String (Insertion_Point.Filename),
            Edit      =>
               Text_Edit'
                 (Make_Range
                    (Insertion_Point.Location,
                     Insertion_Point.Location),
                  Declarations_To_Pull_Up_Text));

         --  Insert the actual parameters in Self.Declaration's calls
         declare
            use Source_Location_To_Unbounded_String_Ordered_Maps;

            Actual_Parameter_Insertion :
              Source_Location_To_Unbounded_String_Ordered_Maps.Cursor :=
                Actual_Parameter_Insertions.First;

         begin
            while Has_Element (Actual_Parameter_Insertion) loop
               Safe_Insert
                 (Edits     => Text_Edits,
                  File_Name => Unit.Get_Filename,
                  Edit      =>
                    Text_Edit'
                      (Make_Range
                         (Key (Actual_Parameter_Insertion),
                          Key (Actual_Parameter_Insertion)),
                       Element (Actual_Parameter_Insertion)));
               Next (Actual_Parameter_Insertion);
            end loop;
         end;
      end Process_Subprogram;

   begin
      if not Is_Subprogram (Self.Declaration) then
         Process_Non_Subprogram;
      else
         Process_Subprogram;
      end if;

      Edits.Text_Edits := Text_Edits;
      return Edits;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));
         return No_Refactoring_Edits;
   end Refactor;

end Laltools.Refactor.Pull_Up_Declaration;
