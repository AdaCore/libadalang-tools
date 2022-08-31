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
--  Common GNATpp partial selection utilities

with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Characters.Latin_1;

with GNAT.Strings;

with Libadalang.Common; use Libadalang.Common;
with Laltools.Common;   use Laltools.Common;
with Langkit_Support.Text; use Langkit_Support.Text;
with Utils.Command_Lines;

with Pp.Actions;

package body Laltools.Partial_GNATPP is

   -----------
   -- Print --
   -----------

   procedure Print (E : Partial_Select_Edits) is
      use Ada.Directories;
   begin
      Ada.Text_IO.Put_Line ("*************************************");
      Ada.Text_IO.Put_Line
        (Simple_Name (E.Unit.Get_Filename) & "(" & E.Node.Image & ") - " &
         Image (E.Edit.Location));
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (E.Edit.Text));
      Ada.Text_IO.Put_Line ("*************************************");
   end Print;

   type Search_Direction is (Forward, Backward);

   function Lookup
     (Unit : Analysis_Unit; Token : Token_Reference; Look : Search_Direction)
      return Ada_Node;
   --  Finds the next Ada_Node relative to Token. Look param controls the
   --  search direction. If Token already belongs to an Ada_Node, that node is
   --  returned. Returns No_Ada_Node if no node is found or if
   --  Token = No_Token.

   function Next_Non_Whitespace
     (Token  : Token_Reference;
      Search : Search_Direction)
      return Token_Reference;
   --  Finds the next non white Token_Reference relative to Token. Search
   --  controls the lookup direction. Returns No_Token if no whitespace
   --  is found or if Token = No_Token.

   function Get_Selection
     (Unit : Analysis_Unit; Node : Ada_Node;
      Start_Tok, End_Tok : Token_Reference)
      return Utils.Char_Vectors.Char_Vector;
   --  The returned value represents the text selection that will be passed
   --  as Input parameter of Format_Vector to be reformatted

   function Get_Common_Enclosing_Parent_Node
     (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node;
   --  Starting from 2 given nodes, get the first enclosing common parent node

   ----------------------------------------
   --  Get_Common_Enclosing_Parent_Node  --
   ----------------------------------------

   function Get_Common_Enclosing_Parent_Node
     (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node
   is
      pragma Assert
        (Start_Node /= No_Ada_Node and then End_Node /= No_Ada_Node);

      --  Start of Get_Common_Enclosing_Parent_Node
   begin

      if Start_Node = End_Node then
         return Start_Node;
      else
         declare
            Start_Parents : constant Ada_Node_Array := Start_Node.Parents;
            End_Parents   : constant Ada_Node_Array := End_Node.Parents;
         begin
            for Idx in Start_Parents'First .. Start_Parents'Last - 1 loop
               for I of End_Parents loop
                  if Start_Parents (Idx) = I then
                     return I.As_Ada_Node;
                  end if;
               end loop;
            end loop;
         end;
      end if;

      return No_Ada_Node;
   end Get_Common_Enclosing_Parent_Node;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Unit : Analysis_Unit;
      Token : Token_Reference;
      Look : Search_Direction)
      return Ada_Node
   is
      Crt_Token      : Token_Reference              := Token;
      Crt_Token_Kind : Libadalang.Common.Token_Kind := Kind (Data (Crt_Token));
   begin
      --  Nothing to do if Aux_Token <=> Token is a No_Token or already
      --  belongs to an Ada_Node.
      while not (Crt_Token = No_Token)
        and then Crt_Token_Kind in Ada_Comment | Ada_Whitespace
      loop
         case Look is
            when Forward =>
               Crt_Token := Next (Crt_Token);

            when Backward =>
               Crt_Token := Previous (Crt_Token);
         end case;
         Crt_Token_Kind := Kind (Data (Crt_Token));
      end loop;

      if Crt_Token = No_Token then
         return No_Ada_Node;
      end if;

      return  Unit.Root.Lookup
        (Start_Sloc (Sloc_Range (Data (Crt_Token)))).As_Ada_Node;
   end Lookup;

   -------------------------
   -- Next_Non_Whitespace --
   -------------------------

   function Next_Non_Whitespace
     (Token  : Token_Reference;
      Search : Search_Direction)
      return Token_Reference
   is
      Crt_Tok : Token_Reference := Token;
   begin
      while Crt_Tok /= No_Token loop
         case Search is
            when Forward  => Crt_Tok := Next (Crt_Tok);
            when Backward => Crt_Tok := Previous (Crt_Tok);
         end case;
         exit when Kind (Data (Crt_Tok)) /= Ada_Whitespace;
      end loop;

      if Crt_Tok /= No_Token and then Kind (Data (Crt_Tok)) /= Ada_Whitespace
      then
         return Crt_Tok;
      end if;

      return No_Token;
   end Next_Non_Whitespace;

   ---------------------
   --  Get_Selection  --
   ---------------------

   function Get_Selection
     (Unit               : Analysis_Unit;
      Node               : Ada_Node;
      Start_Tok, End_Tok : Token_Reference)
      return Utils.Char_Vectors.Char_Vector
   is
      use Ada.Strings.Wide_Wide_Fixed;
      use Ada.Characters.Latin_1;

      function Get_Minimum_Indentation_Level
        (Unit       : Analysis_Unit;
         Start_Line : Line_Number;
         Start_Tok  : Token_Reference)
         return Natural;
      --  Computes the indentation level of the least indented node or comment
      --  that will be used as an offset to format the selection

      -------------------------------------
      --  Get_Minimum_Indentation_Level  --
      -------------------------------------

      function Get_Minimum_Indentation_Level
        (Unit       : Analysis_Unit;
         Start_Line : Line_Number;
         Start_Tok  : Token_Reference)
         return Natural
      is
         SL_First_Non_Blank : constant Positive :=
           Index_Non_Blank (Unit.Get_Line (Positive (Start_Line)));
         Include_SL : constant Boolean :=
           (SL_First_Non_Blank = Text (Start_Tok)'First);
         Crt_Indent : Natural := 0;
         Min_Indent : Natural := Natural'Last;
      begin
         for Line_Nb in
           (if Include_SL then Start_Line else Start_Line + 1)
           .. Node.Sloc_Range.End_Line
         loop
            declare
               L : constant Text_Type := Unit.Get_Line (Positive (Line_Nb));
            begin
               if L /= ""
                 and then Index_Non_Blank (L) > 0
                 and then Index_Non_Blank (L) >= L'First
               then
                  Crt_Indent := Index_Non_Blank (L) - L'First;
                  if Crt_Indent < Min_Indent then
                     Min_Indent := Crt_Indent;
                  end if;
               end if;
            end;
         end loop;
         return Min_Indent;
      end Get_Minimum_Indentation_Level;

      Selection  : Utils.Char_Vectors.Char_Vector;
      --  variable to store the initial selected text as Char_Vector

      Start_Line : constant Line_Number :=
        Sloc_Range (Data (Start_Tok)).Start_Line;
      End_Line   : constant Line_Number :=
        Sloc_Range (Data (End_Tok)).End_Line;

      Lines_Number : constant Positive :=
        1 + Positive (End_Line) - Positive (Start_Line);
      --  Stores the number of lines contained by the selection.

      SL_First_Non_Blank_Idx : constant Positive :=
        Index_Non_Blank (Unit.Get_Line (Positive (Start_Line)));
      --  Stores the Start_Line first index which is not a whitespace.

      Include_SL : constant Boolean :=
        (SL_First_Non_Blank_Idx = Text (Start_Tok)'First);
      --  Stores the decision about if start line should be or not included
      --  in the selection.

      EL_First_Non_Blank_Idx : constant Positive :=
        Index_Non_Blank (Unit.Get_Line (Positive (End_Line)));
      --  Stores the End_Line first index which is not a whitespace.

      Include_EL : constant Boolean :=
        (EL_First_Non_Blank_Idx = Text (End_Tok)'First);
      --  Stores the decision about if end line should be or not included
      --  in the selection.

      Sel_Strt_Idx : array (Start_Line .. End_Line) of Natural :=
        [others => 0];
      --  For each line stores the corresponding start index of the line
      Sel_End_Idx  : array (Start_Line .. End_Line) of Natural :=
        [others => 0];
      --  For each line stores the corresponding end index of the line

      Indent       : Natural := 0;
      --  Indentation value that is computed in order to be used in the
      --  selection formatting, being considered as an offset for the returned
      --  selection formatting.

   begin
      if Lines_Number = 1 then
         --  If the selection contains one line store the selected line
         --  starting end ending index
         Sel_Strt_Idx (Start_Line) := Text (Start_Tok)'First;
         Sel_End_Idx (Start_Line) := Text (End_Tok)'Last;
      else
         --  If multiple line selection then compute the indentation offset
         --  and fill the start/end selection index arrays
         Indent := Get_Minimum_Indentation_Level
           (Unit, Start_Line, Start_Tok);

         --  Handle selection first line
         if Include_SL then
            Sel_Strt_Idx (Start_Line) :=
              Unit.Get_Line (Integer (Start_Line))'First + Indent;
            Sel_End_Idx (Start_Line) :=
              Unit.Get_Line (Integer (Start_Line))'Last;
         else
            Sel_Strt_Idx (Start_Line) := Text (Start_Tok)'First;
            Sel_End_Idx (Start_Line) :=
              Unit.Get_Line (Integer (Start_Line))'Last;
         end if;

         --  Loop through the selection lines and fill the arrays containing
         --  corresponding line start and end indexes between the first and
         --  the last line. The Indent value is used as an offset for the
         --  start index computation.
         for Line_Nb in Start_Line + 1 .. End_Line - 1 loop
            Sel_Strt_Idx (Line_Nb) :=
              Unit.Get_Line (Integer (Line_Nb))'First + Indent;
            Sel_End_Idx (Line_Nb) := Unit.Get_Line (Integer (Line_Nb))'Last;
         end loop;

         --  Handle selection end line
         Sel_Strt_Idx (End_Line) :=
              Unit.Get_Line (Integer (End_Line))'First + Indent;
         Sel_End_Idx (End_Line) :=
           (if Include_EL then Text (End_Tok)'Last
            else Unit.Get_Line (Integer (End_Line))'Last);
      end if;

      --  Create the returned Selection based on the selection start and end
      --  index arrays
      for L_Nb in Start_Line .. End_Line loop

         declare
            Crt_Line : constant Text_Type := Unit.Get_Line (Integer (L_Nb));
            S : GNAT.Strings.String_Access :=
              new String'(To_UTF8
                          (Crt_Line
                             (Sel_Strt_Idx (L_Nb) .. Sel_End_Idx (L_Nb))));
         begin
            --  Add a LF for each line of the selection except for the last one
            if L_Nb /= End_Line then
               Selection.Append (S.all & LF);
            else
               Selection.Append (S.all);
            end if;

            GNAT.Strings.Free (S);
         end;
      end loop;

      return Selection;
   end Get_Selection;

   ------------------------------------------
   --  Get_Selected_Region_Enclosing_Node  --
   ------------------------------------------

   procedure Get_Selected_Region_Enclosing_Node
     (Unit             :     Analysis_Unit;
      SL_Range         : Source_Location_Range;
      Start_Node       : out Ada_Node; End_Node : out Ada_Node;
      Enclosing_Node   : out Ada_Node;
      Input_Sel        : out Utils.Char_Vectors.Char_Vector;
      Output_Sel_Range : out Source_Location_Range)

   is
      Crt_Start_Tok : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'(SL_Range.Start_Line, SL_Range.Start_Column));
      Crt_End_Tok : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'(SL_Range.End_Line, SL_Range.End_Column));

      Crt_Start_Node : constant Ada_Node :=
        Lookup (Unit, Crt_Start_Tok, Forward);
      Crt_End_Node : constant Ada_Node := Lookup (Unit, Crt_End_Tok, Backward);

      --  This is a variable used to find the first relevant parent of
      --  Crt_Start_Node and Crt_End_Node
      Parent_Node : Ada_Node := No_Ada_Node;

      function Is_Relevant_Parent_Kind (Kind : Ada_Node_Kind_Type)
                                        return Boolean
      is
        (Kind in Ada_Decl_Block |
         Ada_Package_Body | Ada_Package_Decl |
         Ada_Subp_Body | Ada_Subp_Decl |
         Ada_Type_Decl | Ada_Object_Decl |
         Ada_Entry_Decl |   --  Ada_Entry_Body |
         Ada_Task_Body | Ada_Single_Task_Decl |
         Ada_Compilation_Unit |
         Ada_Stmt);

      function Is_Relevant_Parent_Node
        (Node : Ada_Node'Class) return Boolean is
        (not Node.Is_Null and then Is_Relevant_Parent_Kind (Node.Kind));

      procedure Is_Relevant_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean);
      --  When Parent is a relevant node stop the search and set Parent_Node.

      function Are_Overlapping_Nodes
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Boolean;
      --  Returns True if one of these nodes is already overlapping the other.

      function Get_Overlapping_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node;
      --  Returns the overlapping node.

      --------------------------------------
      -- Is_Relevant_Parent_Node_Callback --
      --------------------------------------

      procedure Is_Relevant_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean)
      is
      begin
         Stop        := True;
         Parent_Node := Parent;
      end Is_Relevant_Parent_Node_Callback;

      -------------------------------
      -- Are_Overlapping_Nodes --
      -------------------------------

      function Are_Overlapping_Nodes
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Boolean
      is
      begin
         pragma Assert (Start_Node /= End_Node);

         return
           (Start_Node.Sloc_Range.Start_Line > End_Node.Sloc_Range.Start_Line
            and then
            Start_Node.Sloc_Range.End_Line < End_Node.Sloc_Range.End_Line)
           or else
             (Start_Node.Sloc_Range.Start_Line < End_Node.Sloc_Range.Start_Line
              and then
              Start_Node.Sloc_Range.End_Line > End_Node.Sloc_Range.End_Line);
      end Are_Overlapping_Nodes;

      --------------------------
      -- Get_Overlapping_Node --
      --------------------------

      function Get_Overlapping_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node
      is
      begin
         pragma Assert (Start_Node /= End_Node
                        and then Are_Overlapping_Nodes (Start_Node, End_Node));

         if Start_Node.Sloc_Range.Start_Line > End_Node.Sloc_Range.Start_Line
            and then
             Start_Node.Sloc_Range.End_Line < End_Node.Sloc_Range.End_Line
         then
            return End_Node;

         elsif
           Start_Node.Sloc_Range.Start_Line < End_Node.Sloc_Range.Start_Line
           and then
             Start_Node.Sloc_Range.End_Line > End_Node.Sloc_Range.End_Line
         then
            return Start_Node;
         end if;

         return No_Ada_Node;
      end Get_Overlapping_Node;

      --  Start of Get_Selected_Region_Enclosing_Node
   begin
      Enclosing_Node := No_Ada_Node;
      Parent_Node    := Crt_Start_Node;

      --  Find the first relevant parent of Crt_Start_Node
      if not Is_Relevant_Parent_Kind (Kind (Crt_Start_Node)) then
         Find_Matching_Parents
           (Crt_Start_Node, Is_Relevant_Parent_Node'Access,
            Is_Relevant_Parent_Node_Callback'Access);
      end if;
      Start_Node := Parent_Node.As_Ada_Node;

      --  Find the first relevant parent of Crt_End_Node
      Parent_Node := Crt_End_Node.As_Ada_Node;

      if not Is_Relevant_Parent_Kind (Kind (Crt_End_Node)) then
         Find_Matching_Parents
           (Crt_End_Node, Is_Relevant_Parent_Node'Access,
            Is_Relevant_Parent_Node_Callback'Access);
      end if;
      End_Node := Parent_Node.As_Ada_Node;

      --  When the selection contains different parts of different nodes,
      --  find the first encolsing parent node, otherwise the Enclosing_Node
      --  will be equal to Start_Node or End_Node in some situations.
      if Start_Node /= End_Node then
         if Are_Overlapping_Nodes (Start_Node, End_Node) then
            Enclosing_Node := Get_Overlapping_Node (Start_Node, End_Node);

         else
            Enclosing_Node :=
              Get_Common_Enclosing_Parent_Node (Start_Node, End_Node);

            Parent_Node := Enclosing_Node;
            if Enclosing_Node /= No_Ada_Node
              and then not Is_Relevant_Parent_Kind (Kind (Enclosing_Node))
            then
               Find_Matching_Parents
                 (Enclosing_Node, Is_Relevant_Parent_Node'Access,
                  Is_Relevant_Parent_Node_Callback'Access);
               Enclosing_Node := Parent_Node.As_Ada_Node;
            end if;
         end if;

         pragma Assert (Enclosing_Node /= No_Ada_Node);

      else
         Enclosing_Node := Start_Node;
      end if;

      ---------------------------------------------------------------------
      --  Compute the input selection to be reformatted:
      --  * find true start and end token to define selection margins
      --  * get the selected region

      pragma Assert (Enclosing_Node /= No_Ada_Node);

      declare
         --  If Crt_Start_Token is an Ada_Whitespace, then find the next token
         --  not an Ada_Whitespace. That will be the first guess for the
         --  True_Start_Token.
         --  Same fo Crt_End_Token
         True_Start_Tok : Token_Reference :=
           (if Kind (Data (Crt_Start_Tok)) = Ada_Whitespace then
               Next_Non_Whitespace (Crt_Start_Tok, Forward)
            else Crt_Start_Tok);

         True_End_Tok : Token_Reference :=
           (if Kind (Data (Crt_End_Tok)) = Ada_Whitespace then
               Next_Non_Whitespace (Crt_End_Tok, Backward)
            else Crt_End_Tok);

         Start_Line, End_Line : Line_Number;
         Start_Col, End_Col   : Column_Number;
      begin

         --  If True_Start_Tok comes after the first token of Enclosing_Node
         --  then update it.
         True_Start_Tok :=
           (if Enclosing_Node.Compare
              (Start_Sloc (Sloc_Range (Data (True_Start_Tok)))) = Before
            then True_Start_Tok
            else Unit.Lookup_Token
              (Source_Location'
                   (Enclosing_Node.Sloc_Range.Start_Line,
                    Enclosing_Node.Sloc_Range.Start_Column)));

         --  If True_End_Tok comes before the last token of Enclosing_Node
         --  then update it.
         True_End_Tok :=
           (if Enclosing_Node.Compare
              (End_Sloc (Sloc_Range (Data (True_End_Tok)))) = After
            then True_End_Tok
            else Unit.Lookup_Token
              (Source_Location'
                   (Enclosing_Node.Sloc_Range.End_Line,
                    Enclosing_Node.Sloc_Range.End_Column - 1)));

         Input_Sel := Get_Selection (Unit,
                                     Enclosing_Node,
                                     True_Start_Tok, True_End_Tok);

         --  Start_Line/Start_Col stores the starting line/column information
         --  for the current selection.
         Start_Line := Sloc_Range (Data (True_Start_Tok)).Start_Line;
         --  Indentation is added by the formatting, so include indentation of
         --  the first line (if any) in the selected region. This only affects
         --  the range which is replaced by IDEs with the formatted code.
         declare
            use Ada.Strings.Wide_Wide_Fixed;

            Line : constant Text_Type :=
              Unit.Get_Line (Positive (Start_Line));
            First_Non_Blank_Index  : constant Natural :=
              Index_Non_Blank (Line);
            First_Non_Blank_Column : constant Natural :=
              (if First_Non_Blank_Index = 0 then
                 0
               else
                 Index_Non_Blank (Line) - Line'First + 1);

         begin
            Start_Col :=
              (if First_Non_Blank_Column /= 0
                 and then First_Non_Blank_Column =
                            Integer
                              (Sloc_Range (Data (True_Start_Tok)).Start_Column)
               then
                 1
               else
                 Sloc_Range (Data (True_Start_Tok)).Start_Column);
         end;

         --  End_Line/End_Col stores the ending line/column information for
         --  the current selection.
         End_Line := Sloc_Range (Data (True_End_Tok)).End_Line;
         End_Col := Sloc_Range (Data (True_End_Tok)).End_Column;

         --  The selection real margins to be rewritten
         Output_Sel_Range := Source_Location_Range'
           (Start_Line, End_Line, Start_Col, End_Col);
      end;

   end Get_Selected_Region_Enclosing_Node;

   ----------------------------
   --  Get_Previous_Sibling  --
   ----------------------------

   function Get_Previous_Sibling (Node : Ada_Node) return Ada_Node
   is
   begin
      return (if Node /= No_Ada_Node then Node.Previous_Sibling.As_Ada_Node
              else No_Ada_Node);
   end Get_Previous_Sibling;

   ------------------------
   --  Get_Next_Sibling  --
   ------------------------

   function Get_Next_Sibling (Node : Ada_Node) return Ada_Node
   is
   begin
      return (if Node /= No_Ada_Node then Node.Next_Sibling.As_Ada_Node
              else No_Ada_Node);
   end Get_Next_Sibling;

   ---------------------------
   --  Get_Starting_Offset  --
   ---------------------------

   function Get_Starting_Offset
     (Node                   : Ada_Node;
      PP_Indent              : Natural;
      PP_Indent_Continuation : Natural) return Natural
   is
      pragma Unreferenced (PP_Indent_Continuation);
      Parent_Node : Ada_Node := No_Ada_Node;

      function Is_Expected_Parent_Kind (Kind : Ada_Node_Kind_Type)
                                        return Boolean
      is
        (Kind in Ada_Package_Body | Ada_Package_Decl |
         Ada_Library_Item | Ada_Subp_Body | Ada_Task_Body | Ada_Decl_Block |
         Ada_For_Loop_Stmt | Ada_Loop_Stmt | Ada_While_Loop_Stmt);

      function Is_Expected_Parent_Node
        (Node : Ada_Node'Class) return Boolean is
        (not Node.Is_Null and then Is_Expected_Parent_Kind (Node.Kind));

      procedure Is_Expected_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean);
      --  When Parent is a relevant node stop the search and set Parent_Node

      --------------------------------------
      -- Is_Expected_Parent_Node_Callback --
      --------------------------------------

      procedure Is_Expected_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean)
      is
      begin
         Stop        := True;
         Parent_Node := Parent;
      end Is_Expected_Parent_Node_Callback;

      function Get_Parent_Offset (Node : Ada_Node) return Natural;
      --  Returns the parent indentation related to the current Node.

      function Get_Parent_Offset (Node : Ada_Node) return Natural
      is
         Offset : Natural := 0;
      begin
         Parent_Node := Node;
         Find_Matching_Parents
           (Node, Is_Expected_Parent_Node'Access,
            Is_Expected_Parent_Node_Callback'Access);

         if Kind (Parent_Node) = Ada_Library_Item
           and then Natural (Parent_Node.Sloc_Range.Start_Line) = 1
           and then Natural (Parent_Node.Sloc_Range.Start_Column) > 0
         then
            Offset := 0;
         else
            Offset := Natural (Parent_Node.Sloc_Range.Start_Column);
         end if;

         case Kind (Parent_Node) is
            when Ada_Package_Body | Ada_Package_Decl
               | Ada_Task_Body | Ada_Subp_Body | Ada_Decl_Block
               | Ada_For_Loop_Stmt | Ada_Loop_Stmt | Ada_While_Loop_Stmt

               => Offset := Offset + PP_Indent;

            when others => null;
         end case;

         return Offset;
      end Get_Parent_Offset;

      Prev_Sibling : constant Ada_Node := Get_Previous_Sibling (Node);
      Next_Sibling : constant Ada_Node := Get_Next_Sibling (Node);
      Offset       : Natural := 0;
   begin
      if Prev_Sibling /= No_Ada_Node and then Next_Sibling /= No_Ada_Node
        and then Prev_Sibling.Sloc_Range.Start_Column =
          Next_Sibling.Sloc_Range.Start_Column
      then
         Offset := Natural (Prev_Sibling.Sloc_Range.Start_Column);

      elsif Prev_Sibling /= No_Ada_Node then
         if Kind (Node) in Ada_Subp_Body | Ada_Package_Body then
            if Prev_Sibling /= No_Ada_Node
              and then Kind (Prev_Sibling) = Ada_Private_Absent
              and then Next_Sibling = No_Ada_Node
            then
               --  Get the parent node which should be a Library_Item which
               --  will give us the offset to use for the reformatting
               Offset := Get_Parent_Offset (Node);
            else
               Offset := Natural (Prev_Sibling.Sloc_Range.Start_Column);
            end if;
         else
            Offset := Natural (Prev_Sibling.Sloc_Range.Start_Column);
         end if;

      elsif Next_Sibling /= No_Ada_Node then
         Offset := Natural (Next_Sibling.Sloc_Range.Start_Column);

      elsif Prev_Sibling = No_Ada_Node and then Next_Sibling = No_Ada_Node
      then
         --  We should look backward for the Node parent to find the offset
         --  of the parent and compute the one related to the reformatted node
         --  based on gnatpp indentation and indent continuation parameters
         Offset := Get_Parent_Offset (Node);
      end if;

      return Offset;
   end Get_Starting_Offset;

   ---------------------------------------------------
   --  Filter_Initially_Selected_Lines_From_Output  --
   ---------------------------------------------------

   procedure Filter_Initially_Selected_Lines_From_Output
     (Unit              : Analysis_Unit;
      Initial_SL_Range  : Source_Location_Range;
      --  Enclosing_Node    : Ada_Node;
      --  Input_Sel         : Utils.Char_Vectors.Char_Vector;
      Output            : Utils.Char_Vectors.Char_Vector;
      Output_SL_Range   : Source_Location_Range;
      New_Output        : out Utils.Char_Vectors.Char_Vector;
      New_SL_Range      : out Source_Location_Range)
   is
      use Utils.Char_Vectors;
      use Ada.Characters.Latin_1;
      use Ada.Strings.Unbounded;
      --
      --  Output_Str : constant String :=
      --    Char_Vectors.Elems (Output)
      --    (1 .. Char_Vectors.Last_Index (Output));

      type Selected_Line_Record is
         record
            Line_Nb : Line_Number;
            Line    : Unbounded_String;
            SLOC    : Source_Location_Range;
         end record;
      No_Line : constant Selected_Line_Record := Selected_Line_Record'
        (Line_Nb => 0,
         Line =>  Null_Unbounded_String,
         SLOC => No_Source_Location_Range);
      type Selected_Lines_Arr is
        array (Natural range <>) of Selected_Line_Record;

      function Get_Initial_Selection
        (Unit     : Analysis_Unit;
         SL_Range : Source_Location_Range)
      return Utils.Char_Vectors.Char_Vector;
      --  The returned value is the initial text selection corresponding
      --  to the given SL_Range in the given Unit.

      procedure Split_Lines (Buffer        : Utils.Char_Vectors.Char_Vector;
                             SL_Range      : Source_Location_Range;
                             Split_Char    : Character;
                             Sel_Lines_Arr : out Selected_Lines_Arr);
      --  Split a selection buffer based on the split character that is passed
      --  into lines and return them as an array of lines.

      procedure Extract_Original_Selection_From_Output
        (Original_Arr  : Selected_Lines_Arr;
         Formatted_Arr : Selected_Lines_Arr;
         Filtered_Arr  : out Selected_Lines_Arr);
      --  Given the original and the formatted arrays retrieves the reformatted
      --  original lines and returns them as filtered array elements

      function Create_Filtered_Output (Filtered_Arr : Selected_Lines_Arr)
                                       return Utils.Char_Vectors.Char_Vector;
      --  Creates the regenerated and filtred output selection

      -----------------------------
      --  Get_Initial_Selection  --
      -----------------------------

      function Get_Initial_Selection
        (Unit     : Analysis_Unit;
         SL_Range : Source_Location_Range)
      return Utils.Char_Vectors.Char_Vector
      is
         Sel : Utils.Char_Vectors.Char_Vector;

         Start_Line : constant Line_Number := SL_Range.Start_Line;
         End_Line   : constant Line_Number := SL_Range.End_Line;
      begin
         for L_Nb in Start_Line .. End_Line loop
            declare
               Crt_Line : constant Text_Type := Unit.Get_Line (Integer (L_Nb));
               S        : GNAT.Strings.String_Access :=
                 new String'(To_UTF8 (Crt_Line));
            begin
               Sel.Append (S.all & LF);
               GNAT.Strings.Free (S);
            end;
         end loop;
         return Sel;
      end Get_Initial_Selection;

      ------------------
      --  Split_Lines --
      ------------------

      procedure Split_Lines (Buffer        : Utils.Char_Vectors.Char_Vector;
                             SL_Range      : Source_Location_Range;
                             Split_Char    : Character;
                             Sel_Lines_Arr : out Selected_Lines_Arr)
      is
         Str : constant String :=
           Char_Vectors.Elems (Buffer)
           (1 .. Char_Vectors.Last_Index (Buffer));
         Crt_Line : Unbounded_String := Null_Unbounded_String;

         Start_Line   : constant Line_Number := SL_Range.Start_Line;
         Line_Nb      : Natural := 0;
         Crt_Line_Nb  : Line_Number := 0;
      begin
         for Idx in Str'Range loop
            Append (Crt_Line, Str (Idx));
            if Str (Idx) = Split_Char then
               Line_Nb := Line_Nb + 1;
               Crt_Line_Nb := Line_Number (Line_Nb + Natural (Start_Line) - 1);
               Sel_Lines_Arr (Line_Nb) := Selected_Line_Record'
                 (Line_Nb => Crt_Line_Nb,
                  Line    => Crt_Line,
                  SLOC    => Source_Location_Range'
                    (Start_Line   => Crt_Line_Nb,
                     End_Line     => Crt_Line_Nb,
                     Start_Column => 1,
                     End_Column   => Column_Number (Length (Crt_Line) - 1)));
               Crt_Line := Null_Unbounded_String;
            end if;
         end loop;

      end Split_Lines;

      ----------------------------------------------
      --  Extract_Original_Selection_From_Output  --
      ----------------------------------------------

      procedure Extract_Original_Selection_From_Output
        (Original_Arr  : Selected_Lines_Arr;
         Formatted_Arr : Selected_Lines_Arr;
         Filtered_Arr  : out Selected_Lines_Arr)
      is
         pragma Assert (Original_Arr'Size > 0
                        and then Formatted_Arr'Size > 0
                        and then Original_Arr'Size <= Formatted_Arr'Size);

         Orig_Line_Nb  : constant Line_Number := Original_Arr (1).Line_Nb;
         Count         : Natural              := 0;
      begin
         for Idx in Formatted_Arr'Range loop
            if Formatted_Arr (Idx).Line_Nb >= Orig_Line_Nb then
               Count := Count + 1;
               if Count <= Original_Arr'Last then
                  Filtered_Arr (Count) := Formatted_Arr (Idx);
               else
                  exit;
               end if;
            end if;
         end loop;
      end Extract_Original_Selection_From_Output;

      ------------------------------
      --  Create_Filtered_Output  --
      ------------------------------

      function Create_Filtered_Output (Filtered_Arr : Selected_Lines_Arr)
                                       return Utils.Char_Vectors.Char_Vector
      is
         Sel      : Utils.Char_Vectors.Char_Vector;
         Crt_Line : Unbounded_String := Null_Unbounded_String;
      begin
         for Idx in Filtered_Arr'Range loop
            Crt_Line := Filtered_Arr (Idx).Line;
            declare
               S : GNAT.Strings.String_Access :=
                 new String'(To_String (Crt_Line));
            begin
               if Idx = Filtered_Arr'Last
                 and then S'Last > S'First
               then
                  Sel.Append (S.all (S'First .. S'Last - 1));
                  GNAT.Strings.Free (S);
               else
                  Sel.Append (S.all);
                  GNAT.Strings.Free (S);
               end if;
            end;
         end loop;
         return Sel;
      end Create_Filtered_Output;

      ---------------------------------------
      --  Create_Filtered_Selection_Range  --
      ---------------------------------------

      function Create_Filtered_Selection_Range
        (Filtered_Arr : Selected_Lines_Arr) return Source_Location_Range
      is
        (Source_Location_Range'
           (Start_Line =>
                 Filtered_Arr (Filtered_Arr'First).SLOC.Start_Line,
            Start_Column =>
               Filtered_Arr (Filtered_Arr'First).SLOC.Start_Column,
            End_Line =>
               Filtered_Arr (Filtered_Arr'Last).SLOC.Start_Line,
            End_Column =>
               Filtered_Arr (Filtered_Arr'Last).SLOC.End_Column + 1));
      --  Creates the new filtered and reformatted text source location range

      --  ORIGINAL SELECTION specifics
      Orig_Sel : constant Utils.Char_Vectors.Char_Vector :=
        Get_Initial_Selection (Unit, Initial_SL_Range);
      --  Orig_Sel_Str : constant String :=
      --    Char_Vectors.Elems (Orig_Sel)
      --    (1 .. Char_Vectors.Last_Index (Orig_Sel));

      Orig_Start_Line : constant Line_Number := Initial_SL_Range.Start_Line;
      Orig_End_Line   : constant Line_Number := Initial_SL_Range.End_Line;

      Orig_Lines_Number : constant Positive :=
        1 + Positive (Orig_End_Line) - Positive (Orig_Start_Line);
      --  Stores the number of lines contained by the selection

      Orig_Lines_Arr : Selected_Lines_Arr (1 .. Orig_Lines_Number) :=
        [others => No_Line];

      --  OUTPUT specifics
      Output_Start_Line : constant Line_Number := Output_SL_Range.Start_Line;
      Output_End_Line   : constant Line_Number := Output_SL_Range.End_Line;

      Output_Lines_Number : constant Positive :=
        1 + Positive (Output_End_Line) - Positive (Output_Start_Line);
      --  Stores the number of lines contained by the generated output

      Output_Lines_Arr : Selected_Lines_Arr (1 .. Output_Lines_Number) :=
        [others => No_Line];
      --  Stores the reformatted output lines obtained from the initial
      --  selection

      Filtered_Sel_Arr : Selected_Lines_Arr (1 .. Orig_Lines_Number) :=
        [others => No_Line];
      --  Stores the reformatted lines matching the initial selection

   begin
      --  Create lines arrays related to the origial selected lines
      --  and the ones reformatted by the partial gnatpp engine
      Split_Lines (Orig_Sel, Initial_SL_Range, LF, Orig_Lines_Arr);
      Split_Lines (Output, Output_SL_Range, LF, Output_Lines_Arr);

      --  Extract the significant part of the generated output matching the
      --  original selection and update the output selection.
      Extract_Original_Selection_From_Output
        (Orig_Lines_Arr, Output_Lines_Arr, Filtered_Sel_Arr);

      --  Create the new filtered and formatted selection related informations
      --  to be used by the IDE
      New_Output := Create_Filtered_Output (Filtered_Sel_Arr);
      New_SL_Range := Create_Filtered_Selection_Range (Orig_Lines_Arr);

   end Filter_Initially_Selected_Lines_From_Output;

   ------------------------
   --  Format_Selection  --
   ------------------------

   procedure Format_Selection
     (Main_Unit                : Analysis_Unit;
      Input_Selection_Range    : Source_Location_Range;
      Output                   : out Utils.Char_Vectors.Char_Vector;
      Output_Selection_Range   : out Source_Location_Range;
      PP_Messages              : out Pp.Scanner.Source_Message_Vector;
      Formatted_Node           : out Ada_Node;
      PP_Options               : Pp.Command_Lines.Cmd_Line;
      Force_Source_Line_Breaks : Boolean := True)
   is
      use Pp.Actions;
      use Pp.Command_Lines;
      use Utils.Char_Vectors;
      use Utils.Command_Lines;

      Source_Line_Breaks : constant Boolean :=
        Force_Source_Line_Breaks
        or else Pp_Boolean_Switches.Arg
                  (PP_Options, Pp.Command_Lines.Source_Line_Breaks);

      Final_PP_Options : Pp.Command_Lines.Cmd_Line :=
        Copy_Command_Line (PP_Options);

      procedure Set_Source_Line_Breaks_Switches
        (PP_Options : in out Command_Line);
      --  This procedure will updates the gnatpp command line switches
      --  if the flag --source-line-breaks is passed to patial_gnatpp.
      --  The gnatpp command line will get --source-line-breaks switch and
      --  order gnatpp switches, that might be used and potentially
      --  incompatibles with this one, in order to get the expected gnatpp
      --  behavior.

      procedure Set_Source_Line_Breaks_Switches
        (PP_Options : in out Command_Line)
      is
         use Pp.Command_Lines.Pp_Boolean_Switches;
         use Pp.Command_Lines.Pp_Flag_Switches;
         use Pp.Command_Lines.Pp_Nat_Switches;

      begin
         Set_Arg (PP_Options, Pp.Command_Lines.Source_Line_Breaks, True);
         Set_Arg (PP_Options, Comments_Fill, False);
         Set_Arg (PP_Options, Separate_Loop_Then, False);
         Set_Arg (PP_Options, Separate_Then, False);
         Set_Arg (PP_Options, Separate_Loop, False);
         Set_Arg (PP_Options, No_Separate_Loop, False);
         Set_Arg (PP_Options, No_Separate_Then, False);
         Set_Arg (PP_Options, No_Separate_Loop_Then, False);
         Set_Arg (PP_Options, Separate_Label, False);
         Set_Arg (PP_Options, Separate_Stmt_Name, False);
         Set_Arg (PP_Options, Separate_Is, False);
         Set_Arg (PP_Options, Use_On_New_Line, False);
         Set_Arg (PP_Options, Split_Line_Before_Op, False);
         Set_Arg (PP_Options, Split_Line_Before_Record, False);
         Set_Arg (PP_Options, Insert_Blank_Lines, False);
         Set_Arg (PP_Options, Preserve_Blank_Lines, False);
         Set_Arg (PP_Options, Preserve_Line_Breaks, False);
         Set_Arg (PP_Options, Vertical_Enum_Types, False);
         Set_Arg (PP_Options, Vertical_Array_Types, False);
         Set_Arg (PP_Options, Vertical_Named_Aggregates, False);
         Set_Arg (PP_Options, Vertical_Case_Alternatives, False);
         Set_Arg (PP_Options, Call_Threshold, Natural'Last);
         Set_Arg (PP_Options, Par_Threshold, Natural'Last);
         Set_Arg (PP_Options, Case_Threshold, Natural'Last);
      end Set_Source_Line_Breaks_Switches;

      Start_Node, End_Node : Ada_Node;
      Offset               : Natural := 0;
      Input_Sel            : Char_Vector;

   begin

      --  Pass --source-line-breaks to gnatpp and update other potentially
      --  used switches if this is needed based on the partial gnatpp
      --  command line flag
      if Source_Line_Breaks then
         Set_Source_Line_Breaks_Switches (Final_PP_Options);
      end if;

      --  Find the corresponding Start_Node and End_Node given the initial

      --  selection range
      Get_Selected_Region_Enclosing_Node (Main_Unit,
                                          Input_Selection_Range,
                                          Start_Node,
                                          End_Node,
                                          Formatted_Node,
                                          Input_Sel,
                                          Output_Selection_Range);
      --  Ada.Text_IO.Put_Line
      --    ("MKU Enclosing_Node = " & Enclosing_Node.Image);
      --
      --  Ada.Text_IO.Put_Line
      --    ("MKU Enclosing_Node START SLOC = ("
      --     & Enclosing_Node.Sloc_Range.Start_Line'Img & ","
      --     & Enclosing_Node.Sloc_Range.Start_Column'Img
      --     & ")");
      --
      --  Ada.Text_IO.Put_Line
      --    ("MKU Enclosing_Node END SLOC = ("
      --     & Enclosing_Node.Sloc_Range.End_Line'Img & ","
      --     & Enclosing_Node.Sloc_Range.End_Column'Img
      --     & ")");

      pragma Assert (Formatted_Node /= No_Ada_Node);

      --  Determine the offset for the indentation of the enclosing node
      --  based on the previous or next sibling starting column position
      --  and set this value for further usage by Insert_Indentation in
      --  the post phases processing of the tree.

      Offset := Get_Starting_Offset (Formatted_Node,
                                     PP_Indentation (PP_Options),
                                     PP_Indent_Continuation (PP_Options));

      if Offset /= 0 then
         Set_Partial_Gnatpp_Offset (Offset - 1);
      end if;

      --  Format_Vector will rewrite the input selection and returns the
      --  formatted text corresponding to the Enclosing_Node. The output
      --  contains more than the initial selected text since it is based
      --  on the closest enclosing parent of the initial selection.

      Format_Vector
        (Cmd            => Final_PP_Options,
         Input          => Input_Sel,
         Node           => Formatted_Node,
         Output         => Output,
         Messages       => PP_Messages,
         Partial_Gnatpp => True);

      --  In the case of preserving source line breaks switch usage, get the
      --  filtered output of the significant lines based on the initial
      --  selection to make text edits only on that part of the code.

      if Source_Line_Breaks then
         declare
            New_Output          : Char_Vector;
            New_Output_SL_Range : Source_Location_Range;
         begin
            Filter_Initially_Selected_Lines_From_Output
              (Unit             => Main_Unit,
               Initial_SL_Range => Input_Selection_Range,
               Output           => Output,
               Output_SL_Range  => Output_Selection_Range,
               New_Output       => New_Output,
               New_SL_Range     => New_Output_SL_Range);

            Output := New_Output;
            Output_Selection_Range := New_Output_SL_Range;
         end;
      end if;

   end Format_Selection;

end Laltools.Partial_GNATPP;
