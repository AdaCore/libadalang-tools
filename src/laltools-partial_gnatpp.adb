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

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Characters.Latin_1;

with GNAT.Strings;

with Libadalang.Common; use Libadalang.Common;
with Laltools.Common;   use Laltools.Common;
with Langkit_Support.Text; use Langkit_Support.Text;

package body Laltools.Partial_GNATPP is

   -----------
   -- Print --
   -----------

   procedure Print (E : Partial_Select_Edits) is
   begin
      Ada.Text_IO.Put_Line ("*************************************");
      Ada.Text_IO.Put_Line
        (E.Unit.Get_Filename & "(" & E.Node.Image & ") - " &
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
     (Unit : Analysis_Unit; Token : Token_Reference; Look : Search_Direction)
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
     (Unit : Analysis_Unit; Node : Ada_Node;
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
         Crt_Indent : Natural;
         Min_Indent : Natural := Natural'Last;
      begin
         for Line_Nb in
           (if Include_SL then Start_Line else Start_Line + 1)
           .. Node.Sloc_Range.Start_Line
         loop
            declare
               L : constant Text_Type := Unit.Get_Line (Positive (Line_Nb));
            begin
               if L /= "" then
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
        Line_Number'Min (Sloc_Range (Data (Start_Tok)).Start_Line,
                         Node.Sloc_Range.Start_Line);
      Start_Col  : constant Column_Number :=
        Column_Number'Min (Sloc_Range (Data (Start_Tok)).Start_Column,
                           Node.Sloc_Range.Start_Column);
      --  Start_Line/Start_Col stores the starting line/column information for
      --  the current selection. The computation of these values are based
      --  on the provided Start_Line/Start_Column associated with the Start_Tok
      --  input and the Node SLOC, Node that represents the Ada_Node to be
      --  rewritten.
      --  In order to make sure that the selection is complete, the minimal
      --  value of the SLOCs will be used as selection starting point.

      End_Line   : constant Line_Number :=
        Line_Number'Max (Sloc_Range (Data (End_Tok)).End_Line,
                         Node.Sloc_Range.End_Line);
      End_Col    : constant Column_Number :=
        Column_Number'Max (Sloc_Range (Data (End_Tok)).End_Column,
                           Node.Sloc_Range.End_Column) - 1;
      --  End_Line/End_Col stores the ending line/column information for
      --  the current selection. The computation of these values are based
      --  on the provided End_Line/End_Column associated with the Start_Tok
      --  input and the Node SLOC, Node that represents the Ada_Node to be
      --  rewritten.
      --  In order to make sure that the selection is complete, the maximal
      --  value of the SLOCs will be used as selection ending point.

      Real_Start_Tok : constant Token_Reference :=
        Unit.Lookup_Token (Source_Location'(Start_Line, Start_Col));
      Real_End_Tok   : constant Token_Reference :=
        Unit.Lookup_Token (Source_Location'(End_Line, End_Col));
      --  In case of partial selection of the line, the values stored by the
      --  Real_Start_Tok/Real_End_Tok variables will determine the real tokens
      --  to be used for the text selection computation.

      Lines_Number : constant Positive :=
        1 + Positive (End_Line) - Positive (Start_Line);
      --  Stores the number of lines contained by the selection.

      SL_First_Non_Blank_Idx : constant Positive :=
        Index_Non_Blank (Unit.Get_Line (Positive (Start_Line)));
      --  Stores the Start_Line first index which is not a whitespace.

      Include_SL : constant Boolean :=
        (SL_First_Non_Blank_Idx = Text (Real_Start_Tok)'First);
      --  Stores the decision about if start line should be or not included
      --  in the selection.

      EL_First_Non_Blank_Idx : constant Positive :=
        Index_Non_Blank (Unit.Get_Line (Positive (End_Line)));
      --  Stores the End_Line first index which is not a whitespace.

      Include_EL : constant Boolean :=
        (EL_First_Non_Blank_Idx = Text (Real_End_Tok)'First);
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
         Sel_Strt_Idx (Start_Line) := Text (Real_Start_Tok)'First;
         Sel_End_Idx (Start_Line) := Text (Real_End_Tok)'Last;
      else
         --  If multiple line selection then compute the indentation offset
         --  and fill the start/end selection index arrays
         Indent := Get_Minimum_Indentation_Level
           (Unit, Start_Line, Real_Start_Tok);

         --  Handle selection first line
         if Include_SL then
            Sel_Strt_Idx (Start_Line) :=
              Unit.Get_Line (Integer (Start_Line))'First + Indent;
            Sel_End_Idx (Start_Line) :=
              Unit.Get_Line (Integer (Start_Line))'Last;
         else
            Sel_Strt_Idx (Start_Line) := Text (Real_Start_Tok)'First;
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
           (if Include_EL then Text (Real_End_Tok)'Last
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
     (Unit           :     Analysis_Unit; SL_Range : Source_Location_Range;
      Start_Node     : out Ada_Node; End_Node : out Ada_Node;
      Enclosing_Node : out Ada_Node;
      Input_Sel      : out Utils.Char_Vectors.Char_Vector)

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
         Ada_Type_Decl |
         Ada_Object_Decl |
         --  Ada_Entry_Body |
         --  Ada_Task_Body |
         --  Ada_Begin_Block |
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
         Ada_Library_Item | Ada_Subp_Body);

         --  Ada_Task_Body |
         --  Ada_Begin_Block |
         --  Ada_Extended_Return_Stmt |
         --  Ada_Accept_Stmt_With_Stmts |
         --  Ada_Type_Decl |
         --  Ada_Compilation_Unit);

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
               Offset :=
                 Natural (Node.Parent.As_Ada_Node.Sloc_Range.Start_Column);
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
         Parent_Node := Node;
         Find_Matching_Parents
           (Node, Is_Expected_Parent_Node'Access,
            Is_Expected_Parent_Node_Callback'Access);

         Offset := Natural (Parent_Node.Sloc_Range.Start_Column);

         case Kind (Parent_Node) is
            when Ada_Package_Body =>
               Offset := Offset + PP_Indent;

            when others => null;
         end case;

      end if;

      return Offset;
   end Get_Starting_Offset;

end Laltools.Partial_GNATPP;
