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

with Ada.Assertions;
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

   function Get_Text
     (Unit       : Analysis_Unit;
      SLOC_Range : Source_Location_Range;
      Prepend_Spaces : Boolean := False)
      return Unbounded_String;
   --  Gets Unit's text delimited by SLOC_Range

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
     (Definition               : Defining_Name'Class;
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

   procedure Find_Subp_Dependencies_To_Pull_Up
     (Subp                                   : Basic_Decl'Class;
      Definitions_To_Pull_Up_As_Declarations : out Defining_Name_Ordered_Set;
      Definitions_To_Pull_Up_As_Parameters   : out Defining_Name_Ordered_Set)
     with Pre => not Subp.Is_Null and then Is_Subprogram (Subp);
   --  Compute all declarations that need to be pulled up and
   --  parameters that need to be added to Subp.

   package Defining_Name_To_Ada_Mode_Ordered_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Defining_Name,
        Element_Type => Ada_Mode,
        "<"          => "<",
        "="          => "=");

   subtype Defining_Name_To_Ada_Mode_Ordered_Map is
     Defining_Name_To_Ada_Mode_Ordered_Maps.Map;

   function Compute_Parameters_Mode
     (Subp         : Basic_Decl;
      Object_Decls : Defining_Name_Ordered_Set)
      return Defining_Name_To_Ada_Mode_Ordered_Map;
   --  Compute the mode of each parameter that need to be added do Subp

   function Get_Subp_Headers
     (Subps : Defining_Name_Ordered_Set)
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

   function "<" (L, R : Source_Location_Range) return Boolean is
     (Compare (L, Start_Sloc (R)) = After);

   package Source_Location_Range_To_Unbounded_String_Ordered_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Source_Location_Range,
        Element_Type => Unbounded_String,
        "<"          => "<");

   subtype Source_Location_Range_To_Unbounded_String_Ordered_Map is
     Source_Location_Range_To_Unbounded_String_Ordered_Maps.Map;

   type Extraction_Edit is record
      SLOC_Range       : Source_Location_Range;
      Replacement_Text : Unbounded_String;
      Extracted_Text   : Unbounded_String;
   end record;

   No_Extraction_Edit : constant Extraction_Edit :=
     (SLOC_Range       => No_Source_Location_Range,
      Replacement_Text => Null_Unbounded_String,
      Extracted_Text   => Null_Unbounded_String);

   function "<" (L, R : Extraction_Edit) return Boolean is
     (Compare (L.SLOC_Range, Start_Sloc (R.SLOC_Range)) = After);

   package Extraction_Edit_Ordered_Sets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Extraction_Edit,
        "<"          => "<",
        "="          => "=");

   subtype Extraction_Edit_Ordered_Set is Extraction_Edit_Ordered_Sets.Set;

   function Compute_Parameter_Insertions
     (Subp            : Basic_Decl;
      Parameters_Mode : Defining_Name_To_Ada_Mode_Ordered_Map)
      return Extraction_Edit_Ordered_Set;
   --  Computes a map where the keys are Source_Locations in the original
   --  source where the parameters needs to be added and the the elements are
   --  Unbounded_Strings with the content of such parameters.

   function Compute_Actual_Parameter_Insertions
     (Subp            : Basic_Decl;
      Analysis_Units  : Analysis_Unit_Array;
      Parameters_Mode : Defining_Name_To_Ada_Mode_Ordered_Map)
      return Source_Location_To_Unbounded_String_Ordered_Map;
   --  Computes a map where the keys are Source_Locations in the original
   --  source where the actual parameters needs to be added and the the
   --  elements are Unbounded_Strings with the content of such
   --  actual parameters.

   function Apply_Extraction_Edits
     (Unit                 : Analysis_Unit;
      Original_SLOC_Ranges : Source_Location_Range_Ordered_Set;
      Extraction_Edits     : Extraction_Edit_Ordered_Set;
      Output_Text_Edits    :
        out Source_Location_Range_To_Unbounded_String_Ordered_Map;
      Extracted_Text      :
        out Unbounded_String)
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
         for Definition of Decl.P_Defining_Names loop
            for Decl_Part of Definition.P_All_Parts loop
               Enclosing_Declarative_Part :=
                 Get_Enclosing_Declarative_Part (Decl_Part.P_Basic_Decl);

               for Node of Enclosing_Declarative_Part.F_Decls
                 when Node.Kind in Ada_Basic_Decl
               loop
                  for Node_Defining_Name of
                    Node.As_Basic_Decl.P_Defining_Names
                  loop
                     Local_Basic_Decls.Include
                       (Node_Defining_Name.P_Canonical_Part.P_Basic_Decl);
                  end loop;
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
                           Local_Basic_Decls.Include
                             (Param_Spec.As_Basic_Decl);
                        end loop;
                     end if;
                  end;
               end if;
            end loop;
         end loop;

         Local_Basic_Decls.Exclude (Decl.As_Basic_Decl);
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
            for Defining_Name_Decl_Part of Definition.P_All_Parts  loop
               if Local_Defining_Name.P_Find_Refs
                 (Defining_Name_Decl_Part.P_Basic_Decl)'Length > 0
               then
                  All_Parts_Local_Dependencies.Include
                    (Local_Defining_Name.P_Canonical_Part);

                  if Local_Defining_Name.P_Basic_Decl.Kind
                       not in Ada_Param_Spec
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

      for Defition_Part of Definition.P_All_Parts loop
         All_Parts_Local_Dependencies.Exclude (Defition_Part);
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
     (Definition               : Defining_Name'Class;
      Try_Subp_Insertion_Point : Boolean := False)
      return Insertion_Point_Type
   is
      First_Enclosing_Declarative_Part : constant Declarative_Part :=
        Get_Enclosing_Declarative_Part (Definition.P_Basic_Decl);

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
            for Parent of
              Definition.P_Basic_Decl.Parents (With_Self => False)
            loop
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
                    Sloc_Range.Start_Line,
                  1)));
   end Get_Insertion_Point;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Unit           : Analysis_Unit;
      SLOC_Range     : Source_Location_Range;
      Prepend_Spaces : Boolean := False)
      return Unbounded_String
   is
      use Ada.Characters.Latin_1;

   begin
      return Extracted_Text : Unbounded_String do
         if SLOC_Range.Start_Line = SLOC_Range.End_Line then
            declare
               Line        : constant Text_Type :=
                 Unit.Get_Line (Integer (SLOC_Range.Start_Line));
               Start_Index : constant Natural :=
                 Line'First + Integer (SLOC_Range.Start_Column) - 1;
               End_Index   : constant Natural :=
                 Line'First + Integer (SLOC_Range.End_Column) - 2;

            begin
               Extracted_Text :=
                 (if Prepend_Spaces then
                    Natural ((SLOC_Range.Start_Column - 1)) * " "
                    & (+(+(Line (Start_Index .. End_Index))))
                  else
                     +(+(Line (Start_Index .. End_Index))));
            end;

         else
            for Line_Number in
              SLOC_Range.Start_Line .. SLOC_Range.End_Line
            loop
               declare
                  Line            : constant Text_Type :=
                    Unit.Get_Line (Integer (Line_Number));
                  Start_Index     : constant Integer :=
                    (if Line_Number = SLOC_Range.Start_Line then
                        Line'First
                     + Integer (SLOC_Range.Start_Column)
                     - 1
                     else Line'First);
                  End_Index       : constant Integer :=
                    (if Line_Number = SLOC_Range.End_Line then
                        Line'First
                     + Integer (SLOC_Range.End_Column)
                     - 2
                     else
                        Line'Last);

               begin
                  if Prepend_Spaces
                    and then Line_Number = SLOC_Range.Start_Line
                  then
                     Append
                       (Extracted_Text,
                        Natural ((SLOC_Range.Start_Column - 1)) * " "
                        & (+(+(Line (Start_Index .. End_Index)))));

                  else
                     Append
                       (Extracted_Text,
                        +(+(Line (Start_Index .. End_Index))));
                  end if;

                  if Line_Number /= SLOC_Range.End_Line then
                     Append (Extracted_Text, LF);
                  end if;
               end;
            end loop;
         end if;
      end return;
   end Get_Text;

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

   ---------------------------------------
   -- Find_Subp_Dependencies_To_Pull_Up --
   ---------------------------------------

   procedure Find_Subp_Dependencies_To_Pull_Up
     (Subp                                   : Basic_Decl'Class;
      Definitions_To_Pull_Up_As_Declarations : out Defining_Name_Ordered_Set;
      Definitions_To_Pull_Up_As_Parameters   : out Defining_Name_Ordered_Set)
   is
      use Defining_Name_Ordered_Sets;

      Object_Decl_Dependencies       : Defining_Name_Ordered_Set;
      Final_Object_Decl_Dependencies : Defining_Name_Ordered_Set;

   begin
      Definitions_To_Pull_Up_As_Declarations :=
        Get_Local_Dependencies (Subp.P_Defining_Name);

      for Dependency of Definitions_To_Pull_Up_As_Declarations loop
         if Dependency.P_Basic_Decl.Kind in
           Ada_Object_Decl | Ada_Param_Spec
         then
            Object_Decl_Dependencies.Include (Dependency);
         end if;
      end loop;

      for Dependency of Definitions_To_Pull_Up_As_Declarations loop
         if Is_Subprogram (Dependency.P_Basic_Decl) then
            declare
               Dependencies_Of_Subp_Dependency :
                 constant Defining_Name_Ordered_Set :=
                   Get_Local_Dependencies (Dependency);

            begin
               for Dependency of Dependencies_Of_Subp_Dependency loop
                  if Dependency.P_Basic_Decl.Kind in Ada_Object_Decl then
                     Final_Object_Decl_Dependencies.Include (Dependency);
                  end if;
               end loop;
            end;
         end if;
      end loop;

      Definitions_To_Pull_Up_As_Parameters :=
        Object_Decl_Dependencies - Final_Object_Decl_Dependencies;

      Definitions_To_Pull_Up_As_Declarations.Union
        (Final_Object_Decl_Dependencies);
      Definitions_To_Pull_Up_As_Declarations.Difference
        (Definitions_To_Pull_Up_As_Parameters);
   end Find_Subp_Dependencies_To_Pull_Up;

   -----------------------------
   -- Compute_Parameters_Mode --
   -----------------------------

   function Compute_Parameters_Mode
     (Subp         : Basic_Decl;
      Object_Decls : Defining_Name_Ordered_Set)
      return Defining_Name_To_Ada_Mode_Ordered_Map
   is
      Parameters_Mode_Map : Defining_Name_To_Ada_Mode_Ordered_Map;

   begin
      for Definition of Object_Decls loop
         for Declaration_Part of Subp.P_All_Parts loop
            declare
               References               : constant Ref_Result_Array :=
                 Definition.P_Find_Refs (Declaration_Part);
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
                     Parameters_Mode_Map.Include (Definition, Ada_Mode_Out);

                  else
                     --  First reference is a read reference.
                     --  This parameter must be either `in` or `in out`.
                     if Any_Write_References then
                        --  Must be `in out`
                        Parameters_Mode_Map.Include
                          (Definition, Ada_Mode_In_Out);
                     else
                        --  Must be `in`
                        Parameters_Mode_Map.Include (Definition, Ada_Mode_In);
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
     (Subps : Defining_Name_Ordered_Set)
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
         if Is_Subprogram (Subp.P_Basic_Decl) then
            for Subprogram_Part of Subp.P_Basic_Decl.P_All_Parts loop
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
      Parameters_Mode : Defining_Name_To_Ada_Mode_Ordered_Map)
      return Extraction_Edit_Ordered_Set
   is
      use Ada.Characters.Latin_1;
      use Defining_Name_To_Ada_Mode_Ordered_Maps;

      Other_Insertion_Point : Source_Location;
      New_Parameters        : Unbounded_String;

      Subp_Params           : Params;
      Subp_Param_Specs      : Param_Spec_List;
      Subp_First_Param_Spec : Param_Spec;
      Subp_Last_Param_Spec  : Param_Spec;
      Indentation           : Natural;

      Parameters_Cursor     :
        Defining_Name_To_Ada_Mode_Ordered_Maps.Cursor :=
          Parameters_Mode.First;
      Parameter_Definition  : Defining_Name;
      Parameter_Type        : Type_Expr;

      Parameters_Insertions : Extraction_Edit_Ordered_Set;

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
                  Parameter_Definition := Key (Parameters_Cursor);
                  Parameter_Type :=
                    Parameter_Definition.P_Basic_Decl.P_Type_Expression;
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
                 (Extraction_Edit'
                    (SLOC_Range       =>
                       Make_Range
                         (Other_Insertion_Point, Other_Insertion_Point),
                     Replacement_Text => Null_Unbounded_String,
                     Extracted_Text   => New_Parameters));

            else
               Other_Insertion_Point :=
                 End_Sloc (Declaration_Part.P_Defining_Name.Sloc_Range);
               Indentation :=
                 Natural
                   (Declaration_Part.P_Defining_Name.Sloc_Range.End_Column)
                 + 1;
               Append (New_Parameters, " (");
               Parameter_Definition := Key (Parameters_Cursor);
               Parameter_Type :=
                 Parameter_Definition.P_Basic_Decl.P_Type_Expression;
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
                  Parameter_Definition := Key (Parameters_Cursor);
                  Parameter_Type :=
                    Parameter_Definition.P_Basic_Decl.P_Type_Expression;
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
                 (Extraction_Edit'
                    (SLOC_Range       =>
                       Make_Range
                         (Other_Insertion_Point, Other_Insertion_Point),
                     Replacement_Text => Null_Unbounded_String,
                     Extracted_Text   => New_Parameters));
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
      Parameters_Mode : Defining_Name_To_Ada_Mode_Ordered_Map)
      return Source_Location_To_Unbounded_String_Ordered_Map
   is
      use Ada.Characters.Latin_1;
      use Defining_Name_To_Ada_Mode_Ordered_Maps;

      Calls_References  : constant Ref_Result_Array :=
        P_Find_All_Calls (Subp.P_Defining_Name, Analysis_Units);
      Call              : Call_Stmt;
      Call_Expr         : Libadalang.Analysis.Call_Expr;
      Call_Kind         : Ada_Node_Kind_Type;
      Param_Assoc_List  : Assoc_List;
      First_Param_Assoc : Param_Assoc;
      Has_Designators   : Boolean;
      Parameters_Cursor : Defining_Name_To_Ada_Mode_Ordered_Maps.Cursor;
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
            Append (Actual_Parameters, " (");
            Append
              (Actual_Parameters,
               +(Key (Parameters_Cursor).F_Name.Text));
            Append (Actual_Parameters, " => ");
            Append
              (Actual_Parameters,
               +(Key (Parameters_Cursor).F_Name.Text));
            Next (Parameters_Cursor);
            while Has_Element (Parameters_Cursor) loop
               Append (Actual_Parameters, "," & LF);
               Append (Actual_Parameters, Indentation * " ");
               Append
                 (Actual_Parameters,
                  +(Key (Parameters_Cursor).F_Name.Text));
               Append (Actual_Parameters, " => ");
               Append
                 (Actual_Parameters,
                  +(Key (Parameters_Cursor).F_Name.Text));
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
                        Append (Actual_Parameters, "," & LF);
                        Append (Actual_Parameters, Indentation * " ");
                        Append
                          (Actual_Parameters,
                           +(Key (Parameters_Cursor).F_Name.Text));
                        Append (Actual_Parameters, " => ");
                        Append
                          (Actual_Parameters,
                           +(Key (Parameters_Cursor).F_Name.Text));
                        Next (Parameters_Cursor);
                     end loop;
                  else
                     while Has_Element (Parameters_Cursor) loop
                        Append (Actual_Parameters, "," & LF);
                        Append (Actual_Parameters, Indentation * " ");
                        Append
                          (Actual_Parameters,
                           +(Key (Parameters_Cursor).F_Name.Text));
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

   -------------------------------
   -- Apply_Extraction_Edits --
   -------------------------------

   function Apply_Extraction_Edits
     (Unit                 : Analysis_Unit;
      Original_SLOC_Ranges : Source_Location_Range_Ordered_Set;
      Extraction_Edits     : Extraction_Edit_Ordered_Set;
      Output_Text_Edits    :
        out Source_Location_Range_To_Unbounded_String_Ordered_Map;
      Extracted_Text      :
        out Unbounded_String)
      return Unbounded_String
   is
      use Ada.Characters.Latin_1;
      use Extraction_Edit_Ordered_Sets;

      Extraction_Edits_Cursor : Cursor :=
        Extraction_Edits.First;
      Current_Extraction_Edit  : Extraction_Edit :=
        (if Has_Element (Extraction_Edits_Cursor) then
           Element (Extraction_Edits_Cursor)
         else
            No_Extraction_Edit);
      Previous_Extraction_Edit : Extraction_Edit;

   begin
      --  Default the out parameters
      Output_Text_Edits :=
        Source_Location_Range_To_Unbounded_String_Ordered_Maps.Empty_Map;
      Extracted_Text := Null_Unbounded_String;

      for Original_SLOC_Range of Original_SLOC_Ranges loop
         if Current_Extraction_Edit = No_Extraction_Edit then
            Append
              (Extracted_Text,
               Get_Text (Unit, Original_SLOC_Range, False));
            Append (Extracted_Text, LF);
            Output_Text_Edits.Insert
              (Original_SLOC_Range,
               Null_Unbounded_String);

         else
            if Current_Extraction_Edit.SLOC_Range = Original_SLOC_Range then
               Append
                 (Extracted_Text,
                  Current_Extraction_Edit.Extracted_Text & LF);
               Output_Text_Edits.Insert
                 (Original_SLOC_Range,
                  Current_Extraction_Edit.Replacement_Text);

               Next (Extraction_Edits_Cursor);
               Current_Extraction_Edit :=
                 (if Has_Element (Extraction_Edits_Cursor) then
                    Element (Extraction_Edits_Cursor)
                  else
                    No_Extraction_Edit);

            elsif Compare
                   (Original_SLOC_Range,
                    Start_Sloc (Current_Extraction_Edit.SLOC_Range)) = Inside
            then
               Ada.Assertions.Assert
                 ("<="
                    (End_Sloc (Current_Extraction_Edit.SLOC_Range),
                     End_Sloc (Original_SLOC_Range)));

               declare
                  Pre_Extraction_Edit_SLOC_Range : Source_Location_Range :=
                    Source_Location_Range'
                      (Start_Line   => Original_SLOC_Range.Start_Line,
                       End_Line     =>
                         Current_Extraction_Edit.SLOC_Range.Start_Line,
                       Start_Column => Original_SLOC_Range.Start_Column,
                       End_Column   =>
                         Current_Extraction_Edit.SLOC_Range.Start_Column);
                  Pre_Extraction_Edit_Text       : Unbounded_String :=
                    (if Start_Sloc (Pre_Extraction_Edit_SLOC_Range) /=
                       End_Sloc (Pre_Extraction_Edit_SLOC_Range)
                     then
                       Get_Text
                         (Unit           => Unit,
                          SLOC_Range     =>
                            Pre_Extraction_Edit_SLOC_Range,
                          Prepend_Spaces => True)
                     else
                       Null_Unbounded_String);

                  Edited_Extraction_Text : Unbounded_String;
                  Edited_Output_Text     : Unbounded_String;

               begin
                  Extraction_Edit_Loop :
                  loop
                     Append
                       (Edited_Extraction_Text, Pre_Extraction_Edit_Text);
                     Append
                       (Edited_Extraction_Text,
                        Current_Extraction_Edit.Extracted_Text);
                     if Current_Extraction_Edit.Replacement_Text /=
                          ""
                     then
                        Append
                          (Edited_Output_Text,
                           Current_Extraction_Edit.Replacement_Text);
                        Append (Edited_Output_Text, LF);
                     end if;

                     Previous_Extraction_Edit := Current_Extraction_Edit;

                     Next (Extraction_Edits_Cursor);
                     Current_Extraction_Edit :=
                       (if Has_Element (Extraction_Edits_Cursor) then
                          Element (Extraction_Edits_Cursor)
                        else
                          No_Extraction_Edit);

                     exit Extraction_Edit_Loop
                       when Current_Extraction_Edit = No_Extraction_Edit
                         or else Compare
                         (Original_SLOC_Range,
                          Start_Sloc (Current_Extraction_Edit.SLOC_Range))
                           /= Inside;

                     Pre_Extraction_Edit_SLOC_Range :=
                       Source_Location_Range'
                         (Start_Line   =>
                            Previous_Extraction_Edit.SLOC_Range.End_Line,
                          End_Line     =>
                            Current_Extraction_Edit.SLOC_Range.Start_Line,
                          Start_Column =>
                            Previous_Extraction_Edit.SLOC_Range.End_Column,
                          End_Column   =>
                            Current_Extraction_Edit.SLOC_Range.Start_Column);
                     Pre_Extraction_Edit_Text :=
                       (if Start_Sloc (Pre_Extraction_Edit_SLOC_Range) /=
                            End_Sloc (Pre_Extraction_Edit_SLOC_Range)
                        then
                          Get_Text
                            (Unit           => Unit,
                             SLOC_Range     =>
                               Pre_Extraction_Edit_SLOC_Range,
                             Prepend_Spaces => False)
                        else
                           Null_Unbounded_String);
                  end loop Extraction_Edit_Loop;

                  declare
                     Post_Extraction_Edit_SLOC_Range :
                       constant Source_Location_Range :=
                         Source_Location_Range'
                           (Start_Line   =>
                              Previous_Extraction_Edit.SLOC_Range.End_Line,
                            End_Line     => Original_SLOC_Range.End_Line,
                            Start_Column =>
                              Previous_Extraction_Edit.SLOC_Range.End_Column,
                            End_Column   => Original_SLOC_Range.End_Column);
                     Post_Extraction_Edit_Text       :
                       constant Unbounded_String :=
                         (if Start_Sloc (Post_Extraction_Edit_SLOC_Range) /=
                            End_Sloc (Post_Extraction_Edit_SLOC_Range)
                          then
                            Get_Text
                              (Unit           => Unit,
                               SLOC_Range     =>
                                 Post_Extraction_Edit_SLOC_Range,
                               Prepend_Spaces => False)
                          else
                            Null_Unbounded_String);

                  begin
                     Append
                       (Edited_Extraction_Text, Post_Extraction_Edit_Text);
                     Append (Edited_Extraction_Text, LF);
                  end;

                  Append (Extracted_Text, Edited_Extraction_Text);
                  Output_Text_Edits.Insert
                    (Original_SLOC_Range, Edited_Output_Text);
               end;

            else
               Append
                 (Extracted_Text,
                  Get_Text (Unit, Original_SLOC_Range, True));
               Append (Extracted_Text, LF);

               Output_Text_Edits.Insert
                 (Original_SLOC_Range,
                  Null_Unbounded_String);
            end if;
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
                    (Slice (Extracted_Text_Lines, Cursor), 3)
                else
                  Remove_Padding
                    (Slice (Extracted_Text_Lines, Cursor) & LF, 3)));
            Cursor := Advance (Extracted_Text_Lines, Cursor);
         end loop;
      end;

      return Extracted_Text;
   end Apply_Extraction_Edits;

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
      Definition_SLOC   : Source_Location;
      Indentation       : Natural := 3;
      Only_Dependencies : Boolean := False;
      Try_Subp_Insertion_Point : Boolean := False)
      return Declaration_Extractor
   is ((Definition                      =>
          (Unit.Root.Lookup (Definition_SLOC).As_Name.
             P_Enclosing_Defining_Name.P_Canonical_Part),
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
      Unit : constant Analysis_Unit := Self.Definition.Unit;

      Insertion_Point : constant Insertion_Point_Type :=
        Get_Insertion_Point
          (Self.Definition, Self.Try_Subp_Insertion_Point);

      Text_Edits : Text_Edit_Map;
      Edits      : Refactoring_Edits;

      procedure Process_Non_Subprogram;
      --  Pull up a declaration that is not a subprogram

      procedure Process_Subprogram;
      --  Pull up a declaration that is a subprogram

      package Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Maps is new
        Ordered_Maps
          (Key_Type     => Basic_Decl,
           Element_Type => Defining_Name_Ordered_Set,
           "<"          => "<",
           "="          => Defining_Name_Ordered_Sets."=");

      subtype Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Map is
        Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Maps.Map;

      function Ordered_Keys
        (Map : Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Map)
         return Basic_Decl_Ordered_Set;
      --  Returns a ordered set with Map's keys

      ------------------
      -- Ordered_Keys --
      ------------------

      function Ordered_Keys
        (Map : Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Map)
            return Basic_Decl_Ordered_Set
      is
         use Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Maps;

         C : Cursor := Map.First;

      begin
         return Keys : Basic_Decl_Ordered_Set do
            while Has_Element (C) loop
               Keys.Insert (Key (C));
               Next (C);
            end loop;
         end return;
      end Ordered_Keys;

      procedure Split_Definitions_To_Pull_Up
        (Definitions_To_Pull_Up_As_Declarations  :
         Defining_Name_Ordered_Set;
         Declarations_To_Pull_Up_As_Declarations :
         out Basic_Decl_Ordered_Set;
         Declarations_To_Pull_Up_Incompletelly   :
         out Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Map);
      --  Splits Definitions_To_Pull_Up_As_Declarations in two groups:
      --  1) A set with all the declarations that need to be fully pulled up
      --  2) A map where the keys are the declarations that need to be
      --     partially pulled up and the values are the defining names that
      --     need to be pulled up.

      ----------------------------------
      -- Split_Definitions_To_Pull_Up --
      ----------------------------------

      procedure Split_Definitions_To_Pull_Up
        (Definitions_To_Pull_Up_As_Declarations  :
         Defining_Name_Ordered_Set;
         Declarations_To_Pull_Up_As_Declarations :
         out Basic_Decl_Ordered_Set;
         Declarations_To_Pull_Up_Incompletelly   :
         out Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Map) is
      begin
         for Definition of Definitions_To_Pull_Up_As_Declarations loop
            if Definition.Parent.Kind in Ada_Defining_Name_List_Range
              and then Definition.Parent.Children_Count > 1
            then
               if Declarations_To_Pull_Up_Incompletelly.Contains
                 (Definition.P_Basic_Decl)
               then
                  Declarations_To_Pull_Up_Incompletelly.Reference
                    (Definition.P_Basic_Decl).Insert (Definition);
               else
                  declare
                     Group : Defining_Name_Ordered_Set;

                  begin
                     Group.Insert (Definition);
                     Declarations_To_Pull_Up_Incompletelly.Insert
                       (Definition.P_Basic_Decl, Group);
                  end;
               end if;

            else
               Declarations_To_Pull_Up_As_Declarations.Insert
                 (Definition.P_Basic_Decl);
            end if;
         end loop;

         declare
            use Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Maps;

            Complete_Basic_Decls : Basic_Decl_Ordered_Set;

            C : Cursor := Declarations_To_Pull_Up_Incompletelly.First;

         begin
            while Has_Element (C) loop
               if (for all Definition of Key (C).P_Defining_Names
                   => Definitions_To_Pull_Up_As_Declarations.Contains
                     (Definition))
               then
                  Complete_Basic_Decls.Insert (Key (C));
               end if;
               Next (C);
            end loop;

            for Basic_Decl of Complete_Basic_Decls loop
               Declarations_To_Pull_Up_Incompletelly.Delete (Basic_Decl);
            end loop;
         end;
      end Split_Definitions_To_Pull_Up;

      function Compute_Declarations_To_Pull_Up_Incompletelly_Extraction_Edits
        (Declarations_To_Pull_Up_Incompletelly :
           Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Map)
         return Extraction_Edit_Ordered_Set;
      --  Object declarations that need to be pulled up incompletelly
      --  need a special handling. The pulled up text can only contain the
      --  declarations that must be pulled up and the original list of
      --  object should be replaced by the ones that will not be pulled up.

      --------------------------------------------------------------------
      -- Compute_Declarations_To_Pull_Up_Incompletelly_Extraction_Edits --
      --------------------------------------------------------------------

      function Compute_Declarations_To_Pull_Up_Incompletelly_Extraction_Edits
        (Declarations_To_Pull_Up_Incompletelly :
         Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Map)
         return Extraction_Edit_Ordered_Set
      is
         use Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Maps;

         C : Cursor := Declarations_To_Pull_Up_Incompletelly.First;

         Result : Extraction_Edit_Ordered_Set;

      begin
         while Has_Element (C) loop
            declare
               First_Extracted   : Boolean := True;
               First_Replacement : Boolean := True;

               Declaration : constant Object_Decl :=
                 Key (C).As_Object_Decl;
               Definitions : constant Defining_Name_List :=
                 Declaration.F_Ids;

               Decl_Text_WO_Definitions : constant Unbounded_String :=
                 Get_Text
                   (Definitions.Unit,
                    Make_Range
                      (End_Sloc (Definitions.Sloc_Range),
                       End_Sloc (Declaration.Sloc_Range)));

               Declaration_Extraction_Edit : Extraction_Edit;

            begin
               Declaration_Extraction_Edit.SLOC_Range :=
                 Declaration.Sloc_Range;

               for Definition of Definitions loop
                  if Element (C).Contains (Definition.As_Defining_Name) then
                     --  This definition needs to be pulled up
                     if not First_Extracted then
                        Append
                          (Declaration_Extraction_Edit.Extracted_Text,
                           ", ");
                     end if;
                     First_Extracted := True;

                     Append
                       (Declaration_Extraction_Edit.Extracted_Text,
                        +(+Definition.Text));

                  else
                     --  This definition cannot be pulled up
                     if not First_Replacement then
                        Append
                          (Declaration_Extraction_Edit.Replacement_Text,
                           ", ");
                     end if;
                     First_Replacement := False;

                     Append
                       (Declaration_Extraction_Edit.Replacement_Text,
                        +(+Definition.Text));

                  end if;
               end loop;
               Append
                 (Declaration_Extraction_Edit.Extracted_Text,
                  Decl_Text_WO_Definitions);
               Append
                 (Declaration_Extraction_Edit.Replacement_Text,
                  Decl_Text_WO_Definitions);

               Result.Insert (Declaration_Extraction_Edit);
            end;

            Next (C);
         end loop;

         return Result;
      end Compute_Declarations_To_Pull_Up_Incompletelly_Extraction_Edits;

      ----------------------------
      -- Process_Non_Subprogram --
      ----------------------------

      procedure Process_Non_Subprogram is
         Declarations_To_Pull_Up_SLOCs : Source_Location_Range_Ordered_Set;

         Definitions_To_Pull_Up : Defining_Name_Ordered_Set;

         Declarations_To_Pull_Up_As_Declarations : Basic_Decl_Ordered_Set;
         Declarations_To_Pull_Up_Incompletelly   :
           Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Map;

         Declarations_To_Pull_Up_Incompletelly_Extraction_Edits :
           Extraction_Edit_Ordered_Set;

         Output_Text_Edits :
           Source_Location_Range_To_Unbounded_String_Ordered_Map;
         Declarations_To_Pull_Up_Text : Unbounded_String;

      begin
         for Local_Dependency of Get_Local_Dependencies (Self.Definition)
           when Local_Dependency.P_Basic_Decl.Kind not in Ada_Param_Spec_Range
         loop
            Definitions_To_Pull_Up.Include (Local_Dependency);

            for Local_Dependency_Part of
                  Local_Dependency.P_All_Parts
            loop
               Declarations_To_Pull_Up_SLOCs.Include
                 (Get_Declaration_SLOC_Range_With_Comments
                    (Local_Dependency_Part.P_Basic_Decl));
            end loop;
         end loop;

         if not Self.Only_Dependencies then
            Definitions_To_Pull_Up.Include (Self.Definition);
            for Declaration_Part of Self.Definition.P_All_Parts loop
               Declarations_To_Pull_Up_SLOCs.Include
                 (Get_Declaration_SLOC_Range_With_Comments
                    (Declaration_Part.P_Basic_Decl));
            end loop;
         end if;

         Split_Definitions_To_Pull_Up
           (Definitions_To_Pull_Up,
            Declarations_To_Pull_Up_As_Declarations,
            Declarations_To_Pull_Up_Incompletelly);

         Declarations_To_Pull_Up_Incompletelly_Extraction_Edits :=
           Compute_Declarations_To_Pull_Up_Incompletelly_Extraction_Edits
             (Declarations_To_Pull_Up_Incompletelly);

         Declarations_To_Pull_Up_SLOCs :=
           Expand_SLOC_Ranges (Unit, Declarations_To_Pull_Up_SLOCs);
         Declarations_To_Pull_Up_SLOCs :=
           Merge_Intersecting_SLOC_Ranges
             (Unit, Declarations_To_Pull_Up_SLOCs);

         --  for SLOC_Range of Declarations_To_Pull_Up_SLOCs loop
         --     Safe_Insert
         --       (Edits     => Text_Edits,
         --        File_Name => Unit.Get_Filename,
         --        Edit      => Text_Edit'(SLOC_Range, Null_Unbounded_String));
         --     Append
         --       (Declarations_To_Pull_Up,
         --        Unit,
         --        SLOC_Range);
         --  end loop;

         Declarations_To_Pull_Up_Text :=
           Apply_Extraction_Edits
             (Unit                 => Unit,
              Original_SLOC_Ranges => Declarations_To_Pull_Up_SLOCs,
              Extraction_Edits     =>
                Declarations_To_Pull_Up_Incompletelly_Extraction_Edits,
              Output_Text_Edits    => Output_Text_Edits,
              Extracted_Text       => Declarations_To_Pull_Up_Text);

         --  Replace the Source_Location_Ranges of the declarations that
         --  need to be pulled up by an empty string.
         declare
            use Source_Location_Range_To_Unbounded_String_Ordered_Maps;
            C : Cursor := Output_Text_Edits.First;

         begin
            while Has_Element (C) loop
               Safe_Insert
                 (Edits     => Text_Edits,
                  File_Name => Self.Definition.Unit.Get_Filename,
                  Edit      =>
                    Text_Edit'
                      (Key (C),
                       Element (C)));
               Next (C);
            end loop;
         end;

         Safe_Insert
           (Edits     => Text_Edits,
            File_Name => To_String (Insertion_Point.Filename),
            Edit      =>
              Text_Edit'
                (Make_Range
                   (Insertion_Point.Location,
                    Insertion_Point.Location),
                 Declarations_To_Pull_Up_Text));
      end Process_Non_Subprogram;

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram is
         All_Definitions_To_Pull_Up_As_Declarations :
           Defining_Name_Ordered_Set;
         All_Definitions_To_Pull_Up_As_Parameters   :
           Defining_Name_Ordered_Set;

         Declarations_To_Pull_Up_As_Declarations : Basic_Decl_Ordered_Set;
         Declarations_To_Pull_Up_Incompletelly   :
           Basic_Decl_To_Defining_Name_Ordered_Set_Ordered_Map;

         Subprogram_Headers             : Source_Location_Range_Ordered_Set;
         Parameters_Mode                :
           Defining_Name_To_Ada_Mode_Ordered_Map;
         Parameter_Insertions           :
           Extraction_Edit_Ordered_Set;
         Actual_Parameter_Insertions    :
           Source_Location_To_Unbounded_String_Ordered_Map;
         Declarations_To_Pull_Up_Incompletelly_Extraction_Edits :
           Extraction_Edit_Ordered_Set;

         Declarations_To_Pull_Up_SLOCs  : Source_Location_Range_Ordered_Set;
         Declarations_To_Pull_Up_Text   : Unbounded_String;

         Output_Text_Edits :
           Source_Location_Range_To_Unbounded_String_Ordered_Map;

      begin
         --  For a subprogram, some object declarations can now be passed as a
         --  new parameter. So compute which declarations need to be pulled up
         --  and which object declarations need to be added as parameters.
         Find_Subp_Dependencies_To_Pull_Up
           (Subp                    => Self.Definition.P_Basic_Decl,
            Definitions_To_Pull_Up_As_Declarations =>
              All_Definitions_To_Pull_Up_As_Declarations,
            Definitions_To_Pull_Up_As_Parameters   =>
              All_Definitions_To_Pull_Up_As_Parameters);

         if not Self.Only_Dependencies then
            All_Definitions_To_Pull_Up_As_Declarations.Include
              (Self.Definition);
         end if;

         Split_Definitions_To_Pull_Up
           (All_Definitions_To_Pull_Up_As_Declarations,
            Declarations_To_Pull_Up_As_Declarations,
            Declarations_To_Pull_Up_Incompletelly);

         Declarations_To_Pull_Up_Incompletelly_Extraction_Edits :=
           Compute_Declarations_To_Pull_Up_Incompletelly_Extraction_Edits
             (Declarations_To_Pull_Up_Incompletelly);

         --  For each declaration that needs to be added as a parameter,
         --  compute which parameter mode it needs to have.
         Parameters_Mode :=
           Compute_Parameters_Mode
             (Subp         => Self.Definition.P_Basic_Decl,
              Object_Decls => All_Definitions_To_Pull_Up_As_Parameters);

         --  For any dependency in Declarations_To_Pull_Up that is a
         --  subprogram, including Self.Declaration, find their headers.
         Subprogram_Headers :=
           Get_Subp_Headers (All_Definitions_To_Pull_Up_As_Declarations);

         --  Formal parameters need to be added to the spec of
         --  Self.Declaration. Compute the Source_Location where they need to
         --  be added in the spec.
         Parameter_Insertions :=
           Compute_Parameter_Insertions
             (Subp            => Self.Definition.P_Basic_Decl,
              Parameters_Mode => Parameters_Mode);

         --  Actual parameters need to be added to Self.Declaration's calls.
         --  Compute the Source_Location where they need to be added in
         --  such calls.
         Actual_Parameter_Insertions :=
           Compute_Actual_Parameter_Insertions
             (Subp            => Self.Definition.P_Basic_Decl,
              Analysis_Units  => Analysis_Units.all,
              Parameters_Mode => Parameters_Mode);

         --  Declarations_To_Pull_Up now have at least the canonical part
         --  of each dependency. For each dependency, and for each part of
         --  the dependency, compute it's Source_Location_Range, including
         --  leading whitespaces, adjacent comments and blank lines that
         --  follow immidiatelly after.
         declare
            use Basic_Decl_Ordered_Sets;

            All_Declarations : constant Basic_Decl_Ordered_Set :=
              Union
                (Declarations_To_Pull_Up_As_Declarations,
                 Ordered_Keys (Declarations_To_Pull_Up_Incompletelly));

         begin
            for Declaration of All_Declarations loop
               for Defining_Name of Declaration.P_Defining_Names loop
                  for Definition_Part of Defining_Name.P_All_Parts loop
                     Declarations_To_Pull_Up_SLOCs.Include
                       (Get_Declaration_SLOC_Range_With_Comments
                          (Definition_Part.P_Basic_Decl));
                  end loop;
               end loop;
            end loop;
         end;
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
           Apply_Extraction_Edits
             (Unit                 => Unit,
              Original_SLOC_Ranges => Declarations_To_Pull_Up_SLOCs,
              Extraction_Edits     =>
                Extraction_Edit_Ordered_Sets.Union
                  (Declarations_To_Pull_Up_Incompletelly_Extraction_Edits,
                   Parameter_Insertions),
              Output_Text_Edits    => Output_Text_Edits,
              Extracted_Text       => Declarations_To_Pull_Up_Text);

         --  Replace the Source_Location_Ranges of the declarations that
         --  need to be pulled up by an empty string.
         declare
            use Source_Location_Range_To_Unbounded_String_Ordered_Maps;
            C : Cursor := Output_Text_Edits.First;

         begin
            while Has_Element (C) loop
               Safe_Insert
                 (Edits     => Text_Edits,
                  File_Name => Self.Definition.Unit.Get_Filename,
                  Edit      =>
                    Text_Edit'
                      (Key (C),
                       Element (C)));
               Next (C);
            end loop;
         end;

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
      if not Is_Subprogram (Self.Definition.P_Basic_Decl) then
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
