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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;
with GNATCOLL.VFS;
with GNAT.Regpat;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Common; use Libadalang.Common;

with Lint.Command_Line;
with Lint.File_Edits;
with Lint.Utils;

with VSS.Strings.Conversions;

package body Lint.Tools.Array_Aggregates_Tool is
   use Libadalang.Analysis;

   function To_Text_Edit_Map
     (Map : Aggregate_Edits)
      return Laltools.Refactor.Text_Edit_Map;
   --  Converts an Aggregates_To_Text_Edit_Ordered_Set_Map into a Text_Edit_Map

   procedure Run_Normal_Mode;
   --  Run the tool using the project file or sources list

   procedure Run_GNAT_Warnings_Mode;
   --  Run the tool using GNAT output with obsolete array syntax warnings

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

   ----------------------
   -- To_Text_Edit_Map --
   ----------------------

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

   procedure Compute_Aggregate_Edits
     (Aggregate  : Libadalang.Analysis.Aggregate;
      Edit_Texts : in out Aggregate_Edits);
   --  Computes the text edits for Aggregate

   procedure Safe_Insert
     (Edits : in out Laltools.Refactor.Text_Edit_Ordered_Set;
      Edit  : Laltools.Refactor.Text_Edit);
   --  Safely insert Edit in Edits

   procedure Safe_Insert
     (Edits     : in out Aggregates_To_Text_Edit_Ordered_Set_Maps.Map;
      Aggregate : Libadalang.Analysis.Aggregate;
      Edit      : Laltools.Refactor.Text_Edit);
   --  Safely inserts an Edit associated to Aggregate on Edits

   -----------------------------
   -- Compute_Aggregate_Edits --
   -----------------------------

   procedure Compute_Aggregate_Edits
     (Aggregate  : Libadalang.Analysis.Aggregate;
      Edit_Texts : in out Aggregate_Edits)
   is
      Aggregate_Assoc_List  : constant Assoc_List :=
        Aggregate.As_Base_Aggregate.F_Assocs;
      Aggregate_Sloc_Range  : constant Source_Location_Range :=
        Sloc_Range (Aggregate);

      procedure Process_One_Element_Aggregate;
      --  Replace one element aggregates of the form
      --  (<first_index> => <value>) by [Value]

      procedure Process_Empty_Aggregate;
      --  Replace empty aggregates of the form
      --  (<some_index>..<some_lower_index> => <value>) by []

      procedure Process_Multi_Element_Aggregate;
      --  Replace (<array_aggregate_contents>) by
      --  [<array_aggregate_contents>]

      -------------------------------------
      -- Process_Multi_Element_Aggregate --
      -------------------------------------

      procedure Process_Multi_Element_Aggregate is
         Edit : Laltools.Refactor.Text_Edit;

      begin
         Edit.Location.Start_Column := Aggregate_Sloc_Range.Start_Column;
         Edit.Location.End_Column := Aggregate_Sloc_Range.Start_Column + 1;
         Edit.Location.Start_Line := Aggregate_Sloc_Range.Start_Line;
         Edit.Location.End_Line := Aggregate_Sloc_Range.Start_Line;
         Edit.Text := Ada.Strings.Unbounded.To_Unbounded_String ("[");
         Safe_Insert (Edit_Texts, Aggregate, Edit);

         Edit.Location.Start_Column := Aggregate_Sloc_Range.End_Column - 1;
         Edit.Location.End_Column := Aggregate_Sloc_Range.End_Column;
         Edit.Location.Start_Line := Aggregate_Sloc_Range.End_Line;
         Edit.Location.End_Line := Aggregate_Sloc_Range.End_Line;
         Edit.Text := Ada.Strings.Unbounded.To_Unbounded_String ("]");
         Safe_Insert (Edit_Texts, Aggregate, Edit);
      end Process_Multi_Element_Aggregate;

      -----------------------------------
      -- Process_One_Element_Aggregate --
      -----------------------------------

      procedure Process_One_Element_Aggregate is
         Value            : constant Expr :=
           Aggregate.F_Assocs.First_Child.As_Aggregate_Assoc.F_R_Expr;
         Value_Sloc_Range : constant Source_Location_Range :=
           Value.Sloc_Range;
         Edit             : Laltools.Refactor.Text_Edit;

      begin
         Edit.Location.Start_Column := Aggregate_Sloc_Range.Start_Column;
         Edit.Location.End_Column := Value_Sloc_Range.Start_Column;
         Edit.Location.Start_Line := Aggregate_Sloc_Range.Start_Line;
         Edit.Location.End_Line := Value_Sloc_Range.Start_Line;
         Edit.Text := Ada.Strings.Unbounded.To_Unbounded_String ("[");
         Safe_Insert (Edit_Texts, Aggregate, Edit);

         Edit.Location.Start_Column := Aggregate_Sloc_Range.End_Column - 1;
         Edit.Location.End_Column := Aggregate_Sloc_Range.End_Column;
         Edit.Location.Start_Line := Aggregate_Sloc_Range.End_Line;
         Edit.Location.End_Line := Aggregate_Sloc_Range.End_Line;
         Edit.Text := Ada.Strings.Unbounded.To_Unbounded_String ("]");
         Safe_Insert (Edit_Texts, Aggregate, Edit);
      end Process_One_Element_Aggregate;

      -----------------------------
      -- Process_Empty_Aggregate --
      -----------------------------

      procedure Process_Empty_Aggregate is
      begin
         Safe_Insert
           (Edits     => Edit_Texts,
            Aggregate => Aggregate,
            Edit      =>
              Laltools.Refactor.Text_Edit'
                (Location => Aggregate_Sloc_Range,
                 Text     =>
                   Ada.Strings.Unbounded.To_Unbounded_String ("[]")));
      end Process_Empty_Aggregate;

   begin
      case Aggregate_Assoc_List.Children'Length is
         when 1 =>
            declare
               Designator : constant Ada_Node :=
                 Aggregate.F_Assocs.First_Child.As_Aggregate_Assoc.
                   F_Designators.First_Child;

            begin
               if Designator.Kind in Ada_Bin_Op_Range then
                  declare
                     Start_Val : constant Expr :=
                       Designator.As_Bin_Op.F_Left;
                     End_Val   : constant Expr :=
                       Designator.As_Bin_Op.F_Right;

                  begin
                     if Start_Val.Kind in Ada_Int_Literal_Range
                       and End_Val.Kind in Ada_Int_Literal_Range
                     then
                        if Start_Val.P_Eval_As_Int
                          > End_Val.P_Eval_As_Int
                        then
                           Process_Empty_Aggregate;
                        elsif Start_Val.P_Eval_As_Int
                          < End_Val.P_Eval_As_Int
                        then
                           Process_Multi_Element_Aggregate;
                        else
                           Process_One_Element_Aggregate;
                        end if;
                     else
                        Process_Multi_Element_Aggregate;
                     end if;
                  end;

               elsif Designator.Kind in Ada_Others_Designator then
                  Process_Multi_Element_Aggregate;

               elsif Designator.Kind in
                 Ada_Identifier | Ada_Dotted_Name_Range
               then
                  --  The designator can either be an index or the entire
                  --  index type.
                  --  Try to resolve it and check if it resolved to a
                  --  Base_Type_Decl. If it was not able to resolve, play
                  --  safe and simply change to square brackets but keep the
                  --  index.
                  declare
                     Index_Decl : constant Basic_Decl :=
                       Designator.As_Name.P_Referenced_Decl;

                  begin
                     if Index_Decl.Is_Null
                       or else Index_Decl.Kind in Ada_Base_Type_Decl
                     then
                        Process_Multi_Element_Aggregate;

                     else
                        Process_One_Element_Aggregate;
                     end if;
                  end;

               else
                  Process_One_Element_Aggregate;
               end if;

            exception
               when E : others =>
                  Logger.Trace
                    (E,
                     "Failed to process array with a single association."
                     & "Falling back to processing it as a multi element "
                     & "array: ");
                  Process_Multi_Element_Aggregate;
            end;

         when others =>
            Process_Multi_Element_Aggregate;
      end case;
   end Compute_Aggregate_Edits;

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
     (Edits     : in out Aggregates_To_Text_Edit_Ordered_Set_Maps.Map;
      Aggregate : Libadalang.Analysis.Aggregate;
      Edit      : Laltools.Refactor.Text_Edit)
   is
      Edits_Set : Laltools.Refactor.Text_Edit_Ordered_Set;

   begin
      if Edits.Contains (Aggregate) then
         Safe_Insert (Edits.Reference (Aggregate), Edit);

      else
         Edits_Set.Insert (Edit);
         Edits.Insert (Aggregate, Edits_Set);
      end if;
   end Safe_Insert;

   ------------------------------
   -- Upgrade_Array_Aggregates --
   ------------------------------

   function Upgrade_Array_Aggregates
     (Units : Analysis_Unit_Array)
      return Aggregate_Edits
   is
      Edit_Texts : Aggregate_Edits;

      function Find_And_Process_Aggregates
        (Node : Ada_Node'Class)
         return Visit_Status;
      --  Finds all aggregates, checks if they're arrays and if so calls
      --  Compute_Aggregate_Edits on them.

      ---------------------------------
      -- Find_And_Process_Aggregates --
      ---------------------------------

      function Find_And_Process_Aggregates
        (Node : Ada_Node'Class)
         return Visit_Status
      is
         function Is_Array
           (Aggregate : Libadalang.Analysis.Aggregate)
            return Boolean;
         --  Check if this Aggregate is an array

         --------------
         -- Is_Array --
         --------------

         function Is_Array
           (Aggregate : Libadalang.Analysis.Aggregate)
            return Boolean
         is
            Expression_Type : constant Base_Type_Decl :=
              (declare Temp : constant Base_Type_Decl :=
                 Aggregate.P_Expression_Type;
               begin (if Temp.Is_Null then Aggregate.P_Expected_Expression_Type
                      else Temp));
            Full_View       : constant Base_Type_Decl :=
              (if Expression_Type.Is_Null then
                  No_Base_Type_Decl
               else
                  Expression_Type.P_Full_View);

         begin
            if Expression_Type.Is_Null then
               Logger.Trace
                 ("WARNING: Failed to get the type of the aggregate "
                  & Aggregate.Image);
               return False;
            elsif Full_View.Is_Null then
               Logger.Trace
                 ("WARNING: Failed to get the full view of the aggregate type "
                  & Expression_Type.Image);
               return Expression_Type.P_Is_Array_Type;
            else
               return Full_View.P_Is_Array_Type;
            end if;

         exception
            when E : others =>
               if Aggregate.Is_Null then
                  Logger.Trace (E, "Unexpected null Aggregate node: ");

               else
                  Logger.Trace
                    (E,
                     "Unexpected exception when checking if "
                     & Aggregate.Image
                     & "is an array: ");
               end if;
               return False;
         end Is_Array;

      begin
         if Node.Kind = Ada_Aggregate then
            if Is_Array (Node.As_Aggregate)
              or else Node.As_Aggregate.P_Is_Subaggregate
            then
               Compute_Aggregate_Edits (Node.As_Aggregate, Edit_Texts);
            end if;
         end if;

         return Into;
      exception
         when E : others =>
            if Node.Is_Null then
               Logger.Trace (E, "Unexpected null node: ");

            else
               Logger.Trace
                 (E,
                  "Unexpected exception when processing " & Node.Image & ": ");
            end if;
            return Over;
      end Find_And_Process_Aggregates;

      use Ada.Strings;
      use Ada.Strings.Fixed;

      Units_Count : constant String  := Trim (Units'Length'Image, Both);

   begin
      if Units'Length > 0 then
         Lint.Logger.Trace ("Finding all array objects");
         for J in Units'Range loop
            Log_Progress
              (J, Units_Count, "Processing " & Units (J).Get_Filename);
            Units (J).Root.Traverse (Find_And_Process_Aggregates'Access);
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

   ---------------------
   -- Run_Normal_Mode --
   ---------------------

   procedure Run_Normal_Mode is
      use Lint.File_Edits;
      use type Lint.Command_Line.Sources.Result_Array;

      Sources : constant Lint.Command_Line.Sources.Result_Array :=
        Lint.Command_Line.Sources.Get;

      Units : constant Analysis_Unit_Array :=
        (if Sources = Lint.Command_Line.Sources.No_Results then
           Lint.Utils.Get_Project_Analysis_Units
             (Ada.Strings.Unbounded.To_String
                (Lint.Command_Line.Project.Get))
         else
           Lint.Utils.Get_Analysis_Units_From_Sources_List
             (Lint.Utils.Sources_List (Sources),
              Ada.Strings.Unbounded.To_String
                (Lint.Command_Line.Project.Get)));

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
   end Run_Normal_Mode;

   ----------------------------
   -- Run_GNAT_Warnings_Mode --
   ----------------------------

   procedure Run_GNAT_Warnings_Mode is
      Regexp  : constant String :=
        "^(.*):(\d+):(\d+): warning: array aggregate using \(\) is an "
        & "obsolescent syntax, use \[\] instead.*$";
      Matcher : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile (Regexp);
      Matches : GNAT.Regpat.Match_Array (0 .. 3);

      Warnings_File : Ada.Text_IO.File_Type;

      Edits      : Laltools.Refactor.Text_Edit_Map;

      use Lint.File_Edits;

   begin
      Ada.Text_IO.Open
        (Warnings_File,
         Ada.Text_IO.In_File,
         GNATCOLL.VFS."+"
           (Lint.Command_Line.From_GNAT_Warnings.Get.Full_Name));
      while not Ada.Text_IO.End_Of_File (Warnings_File) loop
         declare
            Line    : constant String := Ada.Text_IO.Get_Line (Warnings_File);
            use type GNAT.Regpat.Match_Location;

         begin
            GNAT.Regpat.Match (Matcher, Line, Matches);
            if (for all Match of Matches => Match /= GNAT.Regpat.No_Match) then
               declare
                  Source        : constant String :=
                    Line (Matches (1).First .. Matches (1).Last);
                  Line_Number   : constant Positive :=
                    Positive'Value
                      (Line (Matches (2).First .. Matches (2).Last));
                  Column_Number : constant Positive :=
                    Positive'Value
                      (Line (Matches (3).First .. Matches (3).Last));

                  Context    : constant Analysis_Context := Create_Context;
                  Unit       : constant Analysis_Unit :=
                    Context.Get_From_File (Source);
                  Aggregate : constant Libadalang.Analysis.Aggregate :=
                    Unit.Root.Lookup
                      (Langkit_Support.Slocs.Source_Location'
                         (Langkit_Support.Slocs.Line_Number (Line_Number),
                          Langkit_Support.Slocs.Column_Number
                            (Column_Number))).As_Aggregate;

                  Edit_Texts : Aggregate_Edits;

               begin
                  Compute_Aggregate_Edits (Aggregate, Edit_Texts);

                  for Map_Entry in Edit_Texts.Iterate loop
                     for Text_Edit of Map_Entry.Element loop
                        Laltools.Refactor.Safe_Insert
                          (Edits, Map_Entry.Key.Unit.Get_Filename, Text_Edit);
                     end loop;
                  end loop;
               end;
            end if;
         exception
            when E : others =>
               Logger.Trace
                 (E, "Unexpected exception when processing line " & Line);
         end;
      end loop;
      Ada.Text_IO.Close (Warnings_File);

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
   end Run_GNAT_Warnings_Mode;

   ---------
   -- Run --
   ---------

   procedure Run is
      use type GNATCOLL.VFS.Virtual_File;

   begin
      if Lint.Command_Line.From_GNAT_Warnings.Get /= GNATCOLL.VFS.No_File then
         Run_GNAT_Warnings_Mode;

      else
         Run_Normal_Mode;
      end if;
   end Run;

end Lint.Tools.Array_Aggregates_Tool;
