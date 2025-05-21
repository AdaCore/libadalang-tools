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

with Ada.Assertions;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Text_IO;

with GNAT.Strings;

with Laltools.Common; use Laltools.Common;

with Langkit_Support.Text; use Langkit_Support.Text;

with Pp.Actions;

with Utils.Command_Lines;

package body Laltools.Partial_GNATPP is

   function Copy_Slice
     (Source : Utils.Char_Vectors.Char_Vector; Offset : Natural := 0)
      return Ada.Strings.Unbounded.Unbounded_String
   is (Ada.Strings.Unbounded.To_Unbounded_String
         (Utils.Char_Vectors.Char_Vectors.Elems (Source)
            (1
             + Offset
             .. Utils.Char_Vectors.Char_Vectors.Last_Index (Source))));
   --  Converts a Char_Vector into an Unbounded_String

   type Partial_Select_Edits is record
      Unit : Analysis_Unit;
      Node : Ada_Node;
      Edit : Text_Edit;
   end record;
   --  Stores the selected region related information

   procedure Print (E : Partial_Select_Edits);
   pragma Unreferenced (Print);
   --  Print an E in an human readable format to the standard output

   procedure Get_Selected_Region_Enclosing_Node
     (Unit             : Analysis_Unit;
      SL_Range         : Source_Location_Range;
      Start_Node       : out Ada_Node;
      End_Node         : out Ada_Node;
      Enclosing_Node   : out Ada_Node;
      Input_Sel        : out Utils.Char_Vectors.Char_Vector;
      Output_Sel_Range : out Source_Location_Range);
   --  Retrieves the first and the last Ada node of a given selection range.
   --  These might be the same relevant node or different nodes depending on
   --  the initial text selection.
   --  The closest enclosing parent is also computed. It will be the start or
   --  end node when these are identical and the first common parent when
   --  these are different.
   --  Input_Sel will contain the selected region of the file to be rewritten.
   --  Output_Sel_Range contains the Source_Location_Range to be rewritten.

   function Get_Previous_Sibling (Node : Ada_Node) return Ada_Node;
   --  Returns the node's previous sibling or No_Ada_Node if no sibling found

   function Get_Next_Sibling (Node : Ada_Node) return Ada_Node;
   --  Returns the node's next sibling or No_Ada_Node if no sibling found

   function Get_Initial_Indentation
     (Node : Ada_Node; PP_Indentation : Natural) return Natural;
   --  Returns the initial indentation that needs to be used for the selected
   --  Node formatting

   function Get_First_Line_Offset (Node : Ada_Node) return Natural;
   --  Returns an offset that needs to be applied to the first line.
   --  This is for cases like 'overriding procedure', where 'overriding' is
   --  not in the same node as 'procedure'.

   procedure Filter_Initially_Selected_Lines_From_Output
     (Unit             : Analysis_Unit;
      Initial_SL_Range : Source_Location_Range;
      Output           : Utils.Char_Vectors.Char_Vector;
      Output_SL_Range  : Source_Location_Range;
      New_Output       : out Utils.Char_Vectors.Char_Vector;
      New_SL_Range     : out Source_Location_Range);
   --  Retrieves the initial selected line(s) from the Output and returns
   --  the related Char_Vector with the associated new selection range.
   --  This will be used only for the case when the initial sourece line breaks
   --  are preserved.
   --  The Initial_SL_Range contains the initial source location range selected
   --  in the source file related to the given Unit.
   --  The Enclosing_Node is the enclosing parent that was reformatted.
   --  Input_Sel will contain the initial text selection of the enclosing node.
   --  Output and Output_SL_Range contains the results of Format_Vector.
   --  New_Output and New_SL_Range will contain the filtered lines of the
   --  reformatted selection.

   -----------
   -- Print --
   -----------

   procedure Print (E : Partial_Select_Edits) is
      use Ada.Directories;
   begin
      Ada.Text_IO.Put_Line ("*************************************");
      Ada.Text_IO.Put_Line
        (Simple_Name (E.Unit.Get_Filename)
         & "("
         & E.Node.Image
         & ") - "
         & Image (E.Edit.Location));
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (E.Edit.Text));
      Ada.Text_IO.Put_Line ("*************************************");
   end Print;

   type Search_Direction is (Forward, Backward);

   function Lookup
     (Unit  : Analysis_Unit;
      Token : Libadalang.Common.Token_Reference;
      Look  : Search_Direction) return Ada_Node;
   --  Finds the next Ada_Node relative to Token. Look param controls the
   --  search direction. If Token already belongs to an Ada_Node, that node is
   --  returned. Returns No_Ada_Node if no node is found or if
   --  Token = No_Token.

   function Next_Non_Whitespace
     (Token : Libadalang.Common.Token_Reference; Search : Search_Direction)
      return Libadalang.Common.Token_Reference;
   --  Finds the next non white Token_Reference relative to Token. Search
   --  controls the lookup direction. Returns No_Token if no whitespace
   --  is found or if Token = No_Token.

   function Get_Selection_Text
     (Unit               : Analysis_Unit;
      Node               : Ada_Node;
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
      pragma
        Assert (Start_Node /= No_Ada_Node and then End_Node /= No_Ada_Node);

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
     (Unit  : Analysis_Unit;
      Token : Libadalang.Common.Token_Reference;
      Look  : Search_Direction) return Ada_Node
   is
      Crt_Token      : Token_Reference := Token;
      Crt_Token_Kind : Libadalang.Common.Token_Kind :=
        Kind (Libadalang.Common.Data (Crt_Token));
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

      return
        Unit.Root.Lookup (Start_Sloc (Sloc_Range (Data (Crt_Token))))
          .As_Ada_Node;
   end Lookup;

   -------------------------
   -- Next_Non_Whitespace --
   -------------------------

   function Next_Non_Whitespace
     (Token : Token_Reference; Search : Search_Direction)
      return Token_Reference
   is
      Crt_Tok : Token_Reference := Token;
   begin
      while Crt_Tok /= No_Token loop
         case Search is
            when Forward =>
               Crt_Tok := Next (Crt_Tok);

            when Backward =>
               Crt_Tok := Previous (Crt_Tok);
         end case;
         exit when Kind (Data (Crt_Tok)) /= Ada_Whitespace;
      end loop;

      if Crt_Tok /= No_Token and then Kind (Data (Crt_Tok)) /= Ada_Whitespace
      then
         return Crt_Tok;
      end if;

      return No_Token;
   end Next_Non_Whitespace;

   --------------------------
   --  Get_Selection_Text  --
   --------------------------

   function Get_Selection_Text
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
         Start_Tok  : Token_Reference) return Natural;
      --  Computes the indentation level of the least indented node or comment
      --  that will be used as an offset to format the selection

      -------------------------------------
      --  Get_Minimum_Indentation_Level  --
      -------------------------------------

      function Get_Minimum_Indentation_Level
        (Unit       : Analysis_Unit;
         Start_Line : Line_Number;
         Start_Tok  : Token_Reference) return Natural
      is
         SL_First_Non_Blank : constant Positive :=
           Index_Non_Blank (Unit.Get_Line (Positive (Start_Line)));
         Include_SL         : constant Boolean :=
           (SL_First_Non_Blank = Text (Start_Tok)'First);
         Crt_Indent         : Natural := 0;
         Min_Indent         : Natural := Natural'Last;
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

      Selection : Utils.Char_Vectors.Char_Vector;
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

      Indent : Natural := 0;
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
         Indent := Get_Minimum_Indentation_Level (Unit, Start_Line, Start_Tok);

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
           (if Include_EL
            then Text (End_Tok)'Last
            else Unit.Get_Line (Integer (End_Line))'Last);
      end if;

      --  Create the returned Selection based on the selection start and end
      --  index arrays
      for L_Nb in Start_Line .. End_Line loop

         declare
            Crt_Line : constant Text_Type := Unit.Get_Line (Integer (L_Nb));
            S        : GNAT.Strings.String_Access :=
              new String'
                (To_UTF8
                   (Crt_Line (Sel_Strt_Idx (L_Nb) .. Sel_End_Idx (L_Nb))));
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
   end Get_Selection_Text;

   ------------------------------------------
   --  Get_Selected_Region_Enclosing_Node  --
   ------------------------------------------

   procedure Get_Selected_Region_Enclosing_Node
     (Unit             : Analysis_Unit;
      SL_Range         : Source_Location_Range;
      Start_Node       : out Ada_Node;
      End_Node         : out Ada_Node;
      Enclosing_Node   : out Ada_Node;
      Input_Sel        : out Utils.Char_Vectors.Char_Vector;
      Output_Sel_Range : out Source_Location_Range)

   is
      Crt_Start_Tok : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'(SL_Range.Start_Line, SL_Range.Start_Column));
      Crt_End_Tok   : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'(SL_Range.End_Line, SL_Range.End_Column));

      Crt_Start_Node : constant Ada_Node :=
        Lookup (Unit, Crt_Start_Tok, Forward);
      Crt_End_Node   : constant Ada_Node :=
        Lookup (Unit, Crt_End_Tok, Backward);

      --  This is a variable used to find the first relevant parent of
      --  Crt_Start_Node and Crt_End_Node
      Parent_Node : Ada_Node := No_Ada_Node;

      function Is_Relevant_Parent_Kind
        (Kind : Ada_Node_Kind_Type) return Boolean
      is (Kind
          in Ada_Decl_Block
           | Ada_Type_Decl
           | Ada_Compilation_Unit
           | Ada_Stmt
           | Ada_Basic_Decl);

      function Is_Relevant_Parent_Node (Node : Ada_Node'Class) return Boolean
      is (not Node.Is_Null and then Is_Relevant_Parent_Kind (Node.Kind));

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
        (Parent : Ada_Node; Stop : in out Boolean) is
      begin
         Stop := True;
         Parent_Node := Parent;
      end Is_Relevant_Parent_Node_Callback;

      -------------------------------
      -- Are_Overlapping_Nodes --
      -------------------------------

      function Are_Overlapping_Nodes
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Boolean is
      begin
         pragma Assert (Start_Node /= End_Node);

         return
           (Start_Node.Sloc_Range.Start_Line > End_Node.Sloc_Range.Start_Line
            and then Start_Node.Sloc_Range.End_Line
                     < End_Node.Sloc_Range.End_Line)
           or else (Start_Node.Sloc_Range.Start_Line
                    < End_Node.Sloc_Range.Start_Line
                    and then Start_Node.Sloc_Range.End_Line
                             > End_Node.Sloc_Range.End_Line);
      end Are_Overlapping_Nodes;

      --------------------------
      -- Get_Overlapping_Node --
      --------------------------

      function Get_Overlapping_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node is
      begin
         pragma
           Assert
             (Start_Node /= End_Node
                and then Are_Overlapping_Nodes (Start_Node, End_Node));

         if Start_Node.Sloc_Range.Start_Line > End_Node.Sloc_Range.Start_Line
           and then Start_Node.Sloc_Range.End_Line
                    < End_Node.Sloc_Range.End_Line
         then
            return End_Node;

         elsif Start_Node.Sloc_Range.Start_Line
           < End_Node.Sloc_Range.Start_Line
           and then Start_Node.Sloc_Range.End_Line
                    > End_Node.Sloc_Range.End_Line
         then
            return Start_Node;
         end if;

         return No_Ada_Node;
      end Get_Overlapping_Node;

      --  Start of Get_Selected_Region_Enclosing_Node
   begin
      Enclosing_Node := No_Ada_Node;
      Parent_Node := Crt_Start_Node;

      --  Find the first relevant parent of Crt_Start_Node
      if not Is_Relevant_Parent_Kind (Kind (Crt_Start_Node)) then
         Find_Matching_Parents
           (Crt_Start_Node,
            Is_Relevant_Parent_Node'Access,
            Is_Relevant_Parent_Node_Callback'Access);
      end if;
      Start_Node := Parent_Node.As_Ada_Node;

      --  Find the first relevant parent of Crt_End_Node
      Parent_Node := Crt_End_Node.As_Ada_Node;

      if not Is_Relevant_Parent_Kind (Kind (Crt_End_Node)) then
         Find_Matching_Parents
           (Crt_End_Node,
            Is_Relevant_Parent_Node'Access,
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
                 (Enclosing_Node,
                  Is_Relevant_Parent_Node'Access,
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
           (if Kind (Data (Crt_Start_Tok)) = Ada_Whitespace
            then Next_Non_Whitespace (Crt_Start_Tok, Forward)
            else Crt_Start_Tok);

         True_End_Tok : Token_Reference :=
           (if Kind (Data (Crt_End_Tok)) = Ada_Whitespace
            then Next_Non_Whitespace (Crt_End_Tok, Backward)
            else Crt_End_Tok);

         Start_Line, End_Line : Line_Number;
         Start_Col, End_Col   : Column_Number;
      begin

         --  If True_Start_Tok comes after the first token of Enclosing_Node
         --  then update it.
         True_Start_Tok :=
           (if Enclosing_Node.Compare
                 (Start_Sloc (Sloc_Range (Data (True_Start_Tok))))
              = Before
            then True_Start_Tok
            else
              Unit.Lookup_Token
                (Source_Location'
                   (Enclosing_Node.Sloc_Range.Start_Line,
                    Enclosing_Node.Sloc_Range.Start_Column)));

         --  If True_End_Tok comes before the last token of Enclosing_Node
         --  then update it.
         True_End_Tok :=
           (if Enclosing_Node.Compare
                 (End_Sloc (Sloc_Range (Data (True_End_Tok))))
              = After
            then True_End_Tok
            else
              Unit.Lookup_Token
                (Source_Location'
                   (Enclosing_Node.Sloc_Range.End_Line,
                    Enclosing_Node.Sloc_Range.End_Column - 1)));

         Input_Sel :=
           Get_Selection_Text
             (Unit, Enclosing_Node, True_Start_Tok, True_End_Tok);

         --  Start_Line/Start_Col stores the starting line/column information
         --  for the current selection.
         Start_Line := Sloc_Range (Data (True_Start_Tok)).Start_Line;
         --  Indentation is added by the formatting, so include indentation of
         --  the first line (if any) in the selected region. This only affects
         --  the range which is replaced by IDEs with the formatted code.
         declare
            use Ada.Strings.Wide_Wide_Fixed;

            Line                   : constant Text_Type :=
              Unit.Get_Line (Positive (Start_Line));
            First_Non_Blank_Index  : constant Natural :=
              Index_Non_Blank (Line);
            First_Non_Blank_Column : constant Natural :=
              (if First_Non_Blank_Index = 0
               then 0
               else Index_Non_Blank (Line) - Line'First + 1);

         begin
            Start_Col :=
              (if First_Non_Blank_Column /= 0
                 and then First_Non_Blank_Column
                          = Integer
                              (Sloc_Range (Data (True_Start_Tok)).Start_Column)
               then 1
               else Sloc_Range (Data (True_Start_Tok)).Start_Column);
         end;

         --  End_Line/End_Col stores the ending line/column information for
         --  the current selection.
         End_Line := Sloc_Range (Data (True_End_Tok)).End_Line;
         End_Col := Sloc_Range (Data (True_End_Tok)).End_Column;

         --  The selection real margins to be rewritten
         Output_Sel_Range :=
           Source_Location_Range'(Start_Line, End_Line, Start_Col, End_Col);
      end;

   end Get_Selected_Region_Enclosing_Node;

   ----------------------------
   --  Get_Previous_Sibling  --
   ----------------------------

   function Get_Previous_Sibling (Node : Ada_Node) return Ada_Node is
   begin
      return
        (if Node /= No_Ada_Node
         then Node.Previous_Sibling.As_Ada_Node
         else No_Ada_Node);
   end Get_Previous_Sibling;

   ------------------------
   --  Get_Next_Sibling  --
   ------------------------

   function Get_Next_Sibling (Node : Ada_Node) return Ada_Node is
   begin
      return
        (if Node /= No_Ada_Node
         then Node.Next_Sibling.As_Ada_Node
         else No_Ada_Node);
   end Get_Next_Sibling;

   -------------------------------
   --  Get_Initial_Indentation  --
   -------------------------------

   function Get_Initial_Indentation
     (Node : Ada_Node; PP_Indentation : Natural) return Natural
   is
      Parent_Node : Ada_Node := No_Ada_Node;

      function Get_Parent_Indentation (Node : Ada_Node) return Natural;
      --  Returns the Node's parent indentation

      function Is_Expected_Parent_Kind
        (Kind : Ada_Node_Kind_Type) return Boolean
      is (Kind
          in Ada_Package_Body
           | Ada_Package_Decl
           | Ada_Library_Item
           | Ada_Subp_Body
           | Ada_Task_Body
           | Ada_Decl_Block
           | Ada_For_Loop_Stmt
           | Ada_Loop_Stmt
           | Ada_While_Loop_Stmt
           | Ada_If_Stmt_Range
           | Ada_Case_Stmt_Range
           | Ada_Case_Stmt_Alternative_Range);

      function Is_Expected_Parent_Node (Node : Ada_Node'Class) return Boolean
      is (not Node.Is_Null and then Is_Expected_Parent_Kind (Node.Kind));

      procedure Is_Expected_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean);
      --  When Parent is a relevant node stop the search and set Parent_Node

      --------------------------------------
      -- Is_Expected_Parent_Node_Callback --
      --------------------------------------

      procedure Is_Expected_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean) is
      begin
         Stop := True;
         Parent_Node := Parent;
      end Is_Expected_Parent_Node_Callback;

      ---------------------------
      -- Get_Parent_Indenation --
      ---------------------------

      function Get_Parent_Indentation (Node : Ada_Node) return Natural is
         Offset : Natural := 0;

      begin
         Parent_Node := Node;
         Find_Matching_Parents
           (Node,
            Is_Expected_Parent_Node'Access,
            Is_Expected_Parent_Node_Callback'Access);

         if Kind (Parent_Node) = Ada_Library_Item
           and then Natural (Parent_Node.Sloc_Range.Start_Line) = 1
           and then Natural (Parent_Node.Sloc_Range.Start_Column) > 0
         then
            Offset := 0;
         else
            Offset := Natural (Parent_Node.Sloc_Range.Start_Column) - 1;
         end if;

         case Kind (Parent_Node) is
            when Ada_Package_Body
               | Ada_Package_Decl
               | Ada_Task_Body
               | Ada_Subp_Body
               | Ada_Decl_Block
               | Ada_For_Loop_Stmt
               | Ada_Loop_Stmt
               | Ada_While_Loop_Stmt
               | Ada_If_Stmt_Range
               | Ada_Case_Stmt_Range
               | Ada_Case_Stmt_Alternative_Range
            =>
               Offset := Offset + PP_Indentation;

            when others =>
               null;
         end case;

         return Offset;
      end Get_Parent_Indentation;

      Prev_Sibling : constant Ada_Node := Get_Previous_Sibling (Node);
      Next_Sibling : constant Ada_Node := Get_Next_Sibling (Node);
      Offset       : Natural := 0;

   begin
      if Node.Kind in Ada_Ada_List then
         Offset :=
           (if Node.Sloc_Range.Start_Column = 0
            then 0
            else Natural (Node.Sloc_Range.Start_Column) - 1);

      elsif Node.Kind in Ada_Subp_Spec_Range then
         --  Subp_Spec nodes can have an overriding node sibling. The correct
         --  offset is given by the enclosing declaration, which is the
         --  parent node.
         Offset :=
           Get_Initial_Indentation
             (Node.P_Parent_Basic_Decl.As_Ada_Node, PP_Indentation);

      elsif (not Prev_Sibling.Is_Null and not Next_Sibling.Is_Null)
        and then Prev_Sibling.Sloc_Range.Start_Column
                 = Next_Sibling.Sloc_Range.Start_Column
      then
         Offset :=
           (if Prev_Sibling.Sloc_Range.Start_Column = 0
            then 0
            else Natural (Prev_Sibling.Sloc_Range.Start_Column) - 1);

      elsif not Prev_Sibling.Is_Null then
         if Node.Kind
            in Ada_Subp_Body
             | Ada_Package_Body
             | Ada_Package_Decl
             | Ada_Generic_Package_Renaming_Decl
         then
            if Prev_Sibling.Kind = Ada_Private_Absent
              and then Next_Sibling.Is_Null
            then
               --  Get the parent node which should be a Library_Item which
               --  will give us the offset to use for the reformatting
               Offset := Get_Parent_Indentation (Node);
            else
               Offset :=
                 (if Prev_Sibling.Sloc_Range.Start_Column = 0
                  then 0
                  else Natural (Prev_Sibling.Sloc_Range.Start_Column) - 1);
            end if;
         else
            Offset :=
              (if Prev_Sibling.Sloc_Range.Start_Column = 0
               then 0
               else Natural (Prev_Sibling.Sloc_Range.Start_Column) - 1);
         end if;

      elsif not Next_Sibling.Is_Null then
         Offset :=
           (if Next_Sibling.Sloc_Range.Start_Column = 0
            then 0
            else Natural (Next_Sibling.Sloc_Range.Start_Column) - 1);

      elsif Prev_Sibling.Is_Null and Next_Sibling.Is_Null then
         --  We should look backward for the Node parent to find the offset
         --  of the parent and compute the one related to the reformatted node
         --  based on gnatpp indentation and indent continuation parameters
         Offset := Get_Parent_Indentation (Node);

      else
         Offset :=
           (if Node.Sloc_Range.Start_Column = 0
            then 0
            else Natural (Node.Sloc_Range.Start_Column) - 1);
      end if;

      return Offset;
   end Get_Initial_Indentation;

   ---------------------------
   -- Get_First_Line_Offset --
   ---------------------------

   function Get_First_Line_Offset (Node : Ada_Node) return Natural is
      Overriding_Text     : constant String := "overriding";
      Not_Overriding_Text : constant String := "not overriding";

   begin
      if Node.Kind in Ada_Subp_Spec_Range then
         if Node.Previous_Sibling.Kind in Ada_Overriding_Overriding_Range
           and then Node.Previous_Sibling.Sloc_Range.Start_Line
                    = Node.Sloc_Range.Start_Line
         then
            return Overriding_Text'Length;
         elsif Node.Previous_Sibling.Kind
               in Ada_Overriding_Not_Overriding_Range
           and then Node.Previous_Sibling.Sloc_Range.Start_Line
                    = Node.Sloc_Range.Start_Line
         then
            return Not_Overriding_Text'Length;
         else
            return 0;
         end if;

      else
         return 0;
      end if;
   end Get_First_Line_Offset;

   ---------------------------------------------------
   --  Filter_Initially_Selected_Lines_From_Output  --
   ---------------------------------------------------

   procedure Filter_Initially_Selected_Lines_From_Output
     (Unit             : Analysis_Unit;
      Initial_SL_Range : Source_Location_Range;
      Output           : Utils.Char_Vectors.Char_Vector;
      Output_SL_Range  : Source_Location_Range;
      New_Output       : out Utils.Char_Vectors.Char_Vector;
      New_SL_Range     : out Source_Location_Range)
   is
      use Utils.Char_Vectors;
      use Ada.Characters.Latin_1;

      type Selected_Line_Record is record
         Line_Nb : Line_Number;
         Line    : Ada.Strings.Unbounded.Unbounded_String;
         SLOC    : Source_Location_Range;
      end record;
      No_Line : constant Selected_Line_Record :=
        Selected_Line_Record'
          (Line_Nb => 0,
           Line    => Ada.Strings.Unbounded.Null_Unbounded_String,
           SLOC    => No_Source_Location_Range);
      type Selected_Lines_Arr is
        array (Natural range <>) of Selected_Line_Record;

      function Get_Initial_Selection
        (Unit : Analysis_Unit; SL_Range : Source_Location_Range)
         return Utils.Char_Vectors.Char_Vector;
      --  The returned value is the initial text selection corresponding
      --  to the given SL_Range in the given Unit.

      procedure Split_Lines
        (Buffer        : Utils.Char_Vectors.Char_Vector;
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

      function Create_Filtered_Output
        (Filtered_Arr : Selected_Lines_Arr)
         return Utils.Char_Vectors.Char_Vector;
      --  Creates the regenerated and filtred output selection

      -----------------------------
      --  Get_Initial_Selection  --
      -----------------------------

      function Get_Initial_Selection
        (Unit : Analysis_Unit; SL_Range : Source_Location_Range)
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

      procedure Split_Lines
        (Buffer        : Utils.Char_Vectors.Char_Vector;
         SL_Range      : Source_Location_Range;
         Split_Char    : Character;
         Sel_Lines_Arr : out Selected_Lines_Arr)
      is
         Str      : constant String :=
           Char_Vectors.Elems (Buffer) (1 .. Char_Vectors.Last_Index (Buffer));
         Crt_Line : Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Null_Unbounded_String;

         Start_Line  : constant Line_Number := SL_Range.Start_Line;
         Line_Nb     : Natural := 0;
         Crt_Line_Nb : Line_Number := 0;

         Last_Was_Split_Char : Boolean := False;

      begin
         for Idx in Str'Range loop
            Last_Was_Split_Char := False;
            Ada.Strings.Unbounded.Append (Crt_Line, Str (Idx));
            if Str (Idx) = Split_Char then
               Last_Was_Split_Char := True;
               Line_Nb := Line_Nb + 1;
               Crt_Line_Nb := Line_Number (Line_Nb + Natural (Start_Line) - 1);
               Sel_Lines_Arr (Line_Nb) :=
                 Selected_Line_Record'
                   (Line_Nb => Crt_Line_Nb,
                    Line    => Crt_Line,
                    SLOC    =>
                      Source_Location_Range'
                        (Start_Line   => Crt_Line_Nb,
                         End_Line     => Crt_Line_Nb,
                         Start_Column => 1,
                         End_Column   =>
                           Column_Number
                             (Ada.Strings.Unbounded.Length (Crt_Line) - 1)));
               Crt_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
            end if;
         end loop;

         if not Last_Was_Split_Char then
            Line_Nb := Line_Nb + 1;
            Crt_Line_Nb := Line_Number (Line_Nb + Natural (Start_Line) - 1);
            Sel_Lines_Arr (Line_Nb) :=
              Selected_Line_Record'
                (Line_Nb => Crt_Line_Nb,
                 Line    => Crt_Line,
                 SLOC    =>
                   Source_Location_Range'
                     (Start_Line   => Crt_Line_Nb,
                      End_Line     => Crt_Line_Nb,
                      Start_Column => 1,
                      End_Column   =>
                        Column_Number
                          (Ada.Strings.Unbounded.Length (Crt_Line) - 1)));
         end if;
      end Split_Lines;

      ----------------------------------------------
      --  Extract_Original_Selection_From_Output  --
      ----------------------------------------------

      procedure Extract_Original_Selection_From_Output
        (Original_Arr  : Selected_Lines_Arr;
         Formatted_Arr : Selected_Lines_Arr;
         Filtered_Arr  : out Selected_Lines_Arr)
      is
         pragma
           Assert
             (Original_Arr'Size > 0
                and then Formatted_Arr'Size > 0
                and then Original_Arr'Size <= Formatted_Arr'Size);

         Orig_Line_Nb : constant Line_Number := Original_Arr (1).Line_Nb;
         Count        : Natural := 0;
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

      function Create_Filtered_Output
        (Filtered_Arr : Selected_Lines_Arr)
         return Utils.Char_Vectors.Char_Vector
      is
         Sel      : Utils.Char_Vectors.Char_Vector;
         Crt_Line : Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Null_Unbounded_String;
      begin
         for Idx in Filtered_Arr'Range loop
            Crt_Line := Filtered_Arr (Idx).Line;
            declare
               S : GNAT.Strings.String_Access :=
                 new String'(Ada.Strings.Unbounded.To_String (Crt_Line));
            begin
               if Idx = Filtered_Arr'Last
                 and then S'Last > S'First
                 and then S.all (S'Last) = LF
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
      is (Source_Location_Range'
            (Start_Line   => Filtered_Arr (Filtered_Arr'First).SLOC.Start_Line,
             Start_Column =>
               Filtered_Arr (Filtered_Arr'First).SLOC.Start_Column,
             End_Line     => Filtered_Arr (Filtered_Arr'Last).SLOC.Start_Line,
             End_Column   =>
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

      Final_PP_Options : Pp.Command_Lines.Cmd_Line := PP_Options;

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
         Set_Arg (PP_Options, Vertical_Enum_Types, False);
         Set_Arg (PP_Options, Vertical_Array_Types, False);
         Set_Arg (PP_Options, Vertical_Named_Aggregates, False);
         Set_Arg (PP_Options, Vertical_Case_Alternatives, False);
         Set_Arg (PP_Options, Call_Threshold, Natural'Last);
         Set_Arg (PP_Options, Par_Threshold, Natural'Last);
         Set_Arg (PP_Options, Case_Threshold, Natural'Last);
      end Set_Source_Line_Breaks_Switches;

      Start_Node, End_Node : Ada_Node;
      Initial_Indentation  : Natural := 0;
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
      Get_Selected_Region_Enclosing_Node
        (Main_Unit,
         Input_Selection_Range,
         Start_Node,
         End_Node,
         Formatted_Node,
         Input_Sel,
         Output_Selection_Range);
      pragma Assert (Formatted_Node /= No_Ada_Node);

      --  Determine the offset for the indentation of the enclosing node
      --  based on the previous or next sibling starting column position
      --  and set this value for further usage by Insert_Indentation in
      --  the post phases processing of the tree.

      Initial_Indentation :=
        Get_Initial_Indentation (Formatted_Node, PP_Indentation (PP_Options));

      --  Format_Vector will rewrite the input selection and returns the
      --  formatted text corresponding to the Enclosing_Node. The output
      --  contains more than the initial selected text since it is based
      --  on the closest enclosing parent of the initial selection.

      Format_Vector
        (Cmd                 => Final_PP_Options,
         Input               => Input_Sel,
         Node                => Formatted_Node,
         Output              => Output,
         Messages            => PP_Messages,
         Initial_Indentation => Initial_Indentation,
         Partial_GNATPP      => True);

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

   -----------
   -- Image --
   -----------

   function Image (Edit : Partial_Formatting_Edit) return String is
      use Ada.Characters.Latin_1;
      use Ada.Directories;

   begin
      return
        "*************************************"
        & LF
        & Simple_Name (Edit.Formatted_Node.Unit.Get_Filename)
        & "("
        & Edit.Formatted_Node.Image
        & ") - "
        & Image (Edit.Edit.Location)
        & LF
        & '^'
        & LF
        & Ada.Strings.Unbounded.To_String (Edit.Edit.Text)
        & '$'
        & LF
        & "*************************************";
   end Image;

   subtype Relevant_Parent is Ada_Node_Kind_Type
   with
     Predicate =>
       Relevant_Parent
       in Ada_Compilation_Unit
        | Ada_Declarative_Part_Range
        | Ada_Handled_Stmts_Range
        | Ada_Stmt_List
        | Ada_Stmt
        | Ada_Decl_Block_Range
        | Ada_Package_Decl_Range
        | Ada_Package_Body_Range
        | Ada_Subp_Decl_Range
        | Ada_Subp_Body_Range
        | Ada_Type_Decl
        | Ada_Object_Decl_Range
        | Ada_Entry_Decl_Range
        | Ada_Entry_Body_Range
        | Ada_Task_Body_Range
        | Ada_Single_Task_Decl_Range
        | Ada_Generic_Package_Decl_Range
        | Ada_Generic_Package_Renaming_Decl_Range
        | Ada_Package_Renaming_Decl_Range
        | Ada_Exception_Decl_Range
        | Ada_Null_Subp_Decl_Range
        | Ada_Subp_Spec_Range;

   ---------------------------
   -- Get_Formatting_Region --
   ---------------------------

   function Get_Formatting_Region
     (Unit : Analysis_Unit; Input_Range : Source_Location_Range)
      return Formatting_Region_Type
   is
      function Is_Relevant_Parent_Node (Node : Ada_Node'Class) return Boolean
      is (not Node.Is_Null and then Node.Kind in Relevant_Parent);
      --  Checks if Node is not null and if its Kind is in Relevant_Parent

      procedure Get_First_Common_Relevant_Parent
        (Parents_A : Ada_Node_Array;
         Parents_B : Ada_Node_Array;
         Parent    : out Ada_Node;
         Index     : out Natural)
      with Post => (if Parent.Is_Null then Index = 0);
      --  Given Parents_A and Parents_B, sets Parent to the first common parent
      --  which has Is_Relevant_Parent_Node as True. Also sets Index to the
      --  array index which is the same in both Parents_A and Parents_B.
      --  If the first common relevant parent is not found, then Parent will
      --  be set to null and Index to 0.

      --------------------------------------
      -- Get_First_Common_Relevant_Parent --
      --------------------------------------

      procedure Get_First_Common_Relevant_Parent
        (Parents_A : Ada_Node_Array;
         Parents_B : Ada_Node_Array;
         Parent    : out Ada_Node;
         Index     : out Natural) is
      begin
         Parent := No_Ada_Node;
         Index := 0;

         declare
            Max_Length : constant Natural :=
              Positive'Min (Parents_A'Length, Parents_B'Length);
         begin
            if Max_Length = 0 then
               return;
            end if;

            for J in 0 .. Max_Length - 1 loop
               declare
                  Parent_A : Ada_Node renames Parents_A (Parents_A'Last - J);
                  Parent_B : Ada_Node renames Parents_B (Parents_B'Last - J);

               begin
                  exit when Parent_A /= Parent_B;

                  if Is_Relevant_Parent_Node (Parent_A) then
                     Parent := Parent_A;
                     Index := J;
                  end if;
               end;
            end loop;
         end;
      end Get_First_Common_Relevant_Parent;

      Start_Token_First_Estimate  : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'
             (Line   => Input_Range.Start_Line,
              Column => Input_Range.Start_Column));
      Start_Token_Second_Estimate : constant Token_Reference :=
        (if Kind (Data (Start_Token_First_Estimate)) = Ada_Whitespace
         then Next_Non_Whitespace (Start_Token_First_Estimate, Forward)
         else Start_Token_First_Estimate);

      End_Token_First_Estimate  : constant Token_Reference :=
        Unit.Lookup_Token
          (Sloc =>
             Source_Location'
               (Line   => Input_Range.End_Line,
                Column => Input_Range.End_Column));
      End_Token_Second_Estimate : constant Token_Reference :=
        (if Input_Range.Start_Line = Input_Range.End_Line
           and Input_Range.Start_Column = Input_Range.End_Column
         then Start_Token_Second_Estimate
         else
           (if Kind (Data (End_Token_First_Estimate)) = Ada_Whitespace
            then Next_Non_Whitespace (End_Token_First_Estimate, Backward)
            else End_Token_First_Estimate));

      Start_Node_Estimate         : constant Ada_Node :=
        Lookup (Unit, Start_Token_Second_Estimate, Forward);
      Start_Node_Estimate_Parents : constant Ada_Node_Array :=
        Start_Node_Estimate.Parents;
      End_Node_Estimate           : constant Ada_Node :=
        Lookup (Unit, End_Token_Second_Estimate, Backward);
      End_Node_Estimate_Parents   : constant Ada_Node_Array :=
        End_Node_Estimate.Parents;

      Enclosing_Parent       : Ada_Node := No_Ada_Node;
      Enclosing_Parent_Index : Natural := 0;

      Start_Token : Token_Reference;
      End_Token   : Token_Reference;

   begin
      Get_First_Common_Relevant_Parent
        (Parents_A => Start_Node_Estimate_Parents,
         Parents_B => End_Node_Estimate_Parents,
         Parent    => Enclosing_Parent,
         Index     => Enclosing_Parent_Index);

      Ada.Assertions.Assert (not Enclosing_Parent.Is_Null);

      if Enclosing_Parent.Kind in Ada_Declarative_Part_Range then
         --  This if statment can in the future be removed.
         --  This is to deal with Ada_Node_List nodes children of an
         --  Declarative_Part nodes. This rule can in the future be relaxed
         --  to simply Ada_Ada_List and this branch removed.

         declare
            Start_Node       : constant Ada_Node :=
              Start_Node_Estimate_Parents
                (Start_Node_Estimate_Parents'Last
                 - Enclosing_Parent_Index
                 - 2);
            Start_Node_Index : constant Positive := Start_Node.Child_Index + 1;
            End_Node         : constant Ada_Node :=
              End_Node_Estimate_Parents
                (End_Node_Estimate_Parents'Last - Enclosing_Parent_Index - 2);
            End_Node_Index   : constant Positive := End_Node.Child_Index + 1;

            Is_Slice : constant Boolean :=
              Start_Node_Index /= End_Node_Index
              and then (Start_Node_Index /= 1
                        or End_Node_Index /= End_Node.Parent.Last_Child_Index);

         begin
            Ada.Assertions.Assert (Start_Node.Parent = End_Node.Parent);
            Ada.Assertions.Assert (Start_Node.Parent.Kind in Ada_Ada_List);

            Start_Token :=
              (if Start_Node.Compare
                    (Start_Sloc
                       (Sloc_Range (Data (Start_Token_Second_Estimate))))
                 = Before
               then Start_Token_Second_Estimate
               else Start_Node.Token_Start);
            End_Token :=
              (if End_Node.Compare
                    (Start_Sloc
                       (Sloc_Range (Data (End_Token_Second_Estimate))))
                 = After
               then End_Token_Second_Estimate
               else
                 Unit.Lookup_Token
                   (Source_Location'
                      (End_Node.Sloc_Range.End_Line,
                       End_Node.Sloc_Range.End_Column - 1)));
            return
              (if Is_Slice
                 or else (Start_Node_Index = 1
                          and then End_Node_Index
                                   = End_Node.Parent.Last_Child_Index)
               then
                 Formatting_Region_Type'
                   (Start_Token       => Start_Token,
                    End_Token         => End_Token,
                    Enclosing_Node    => Start_Node.Parent,
                    List_Slice        => True,
                    Start_Child_Index => Start_Node_Index,
                    End_Child_Index   => End_Node_Index)
               else
                 Formatting_Region_Type'
                   (Start_Token    => Start_Token,
                    End_Token      => End_Token,
                    Enclosing_Node => Start_Node,
                    List_Slice     => False));
         end;

      elsif Enclosing_Parent.Kind in Ada_Stmt_List then
         declare
            Start_Node       : constant Ada_Node :=
              Start_Node_Estimate_Parents
                (Start_Node_Estimate_Parents'Last
                 - Enclosing_Parent_Index
                 - 1);
            Start_Node_Index : constant Positive := Start_Node.Child_Index + 1;
            End_Node         : constant Ada_Node :=
              End_Node_Estimate_Parents
                (End_Node_Estimate_Parents'Last - Enclosing_Parent_Index - 1);
            End_Node_Index   : constant Positive := End_Node.Child_Index + 1;

            Is_Slice : constant Boolean :=
              Start_Node_Index /= End_Node_Index
              and then (Start_Node_Index /= 1
                        or End_Node_Index /= End_Node.Parent.Last_Child_Index);

         begin
            Ada.Assertions.Assert (Start_Node.Parent = End_Node.Parent);
            Ada.Assertions.Assert (Start_Node.Parent.Kind in Ada_Ada_List);

            Start_Token :=
              (if Start_Node.Compare
                    (Start_Sloc
                       (Sloc_Range (Data (Start_Token_Second_Estimate))))
                 = Before
               then Start_Token_Second_Estimate
               else Start_Node.Token_Start);
            End_Token :=
              (if End_Node.Compare
                    (Start_Sloc
                       (Sloc_Range (Data (End_Token_Second_Estimate))))
                 = After
               then End_Token_Second_Estimate
               else
                 Unit.Lookup_Token
                   (Source_Location'
                      (End_Node.Sloc_Range.End_Line,
                       End_Node.Sloc_Range.End_Column - 1)));
            return
              (if Is_Slice
                 or else (Start_Node_Index = 1
                          and then End_Node_Index
                                   = End_Node.Parent.Last_Child_Index)
               then
                 Formatting_Region_Type'
                   (Start_Token       => Start_Token,
                    End_Token         => End_Token,
                    Enclosing_Node    => Start_Node.Parent,
                    List_Slice        => True,
                    Start_Child_Index => Start_Node_Index,
                    End_Child_Index   => End_Node_Index)
               else
                 Formatting_Region_Type'
                   (Start_Token    => Start_Token,
                    End_Token      => End_Token,
                    Enclosing_Node => Start_Node,
                    List_Slice     => False));
         end;
      else
         Start_Token :=
           (if Enclosing_Parent.Compare
                 (Start_Sloc (Sloc_Range (Data (Start_Token_Second_Estimate))))
              = Before
            then Start_Token_Second_Estimate
            else Enclosing_Parent.Token_Start);
         End_Token :=
           (if Enclosing_Parent.Compare
                 (Start_Sloc (Sloc_Range (Data (End_Token_Second_Estimate))))
              = After
            then End_Token_Second_Estimate
            else Enclosing_Parent.Token_End);

         return
           Formatting_Region_Type'
             (Start_Token    => Start_Token,
              End_Token      => End_Token,
              Enclosing_Node => Enclosing_Parent,
              List_Slice     => False);
      end if;
   end Get_Formatting_Region;

   ------------------------
   --  Format_Selection  --
   ------------------------

   function Format_Selection
     (Unit                  : Analysis_Unit;
      Input_Selection_Range : Source_Location_Range;
      PP_Options            : Pp.Command_Lines.Cmd_Line)
      return Partial_Formatting_Edit
   is
      use Pp.Actions;
      use Pp.Command_Lines;
      use Utils.Command_Lines;

      Formatting_Region   : constant Formatting_Region_Type :=
        Get_Formatting_Region (Unit, Input_Selection_Range);
      Initial_Indentation : Natural :=
        Get_Initial_Indentation
          (Formatting_Region.Enclosing_Node, PP_Indentation (PP_Options));
      Previous_Token      : constant Token_Reference :=
        Previous_Non_Whitespace_Non_Comment_Token
          (Formatting_Region.Enclosing_Node.Token_Start);
      Previous_Token_Node : constant Ada_Node :=
        (if Previous_Token = No_Token
         then No_Ada_Node
         else
           Formatting_Region.Enclosing_Node.Unit.Root.Lookup
             (Start_Sloc (Sloc_Range (Data (Previous_Token)))));
      Indentation_Guess   : constant Natural :=
        (if Previous_Token_Node.Is_Null
         then 0
         else Estimate_Indentation (Formatting_Region.Enclosing_Node));
      First_Line_Offset   : constant Natural :=
        Get_First_Line_Offset (Formatting_Region.Enclosing_Node);

      Formatted_Text_Ignore : Natural := 0;

   begin
      if Initial_Indentation /= Indentation_Guess then
         Initial_Indentation := Indentation_Guess;
      end if;

      declare
         Input_Text     : constant Utils.Char_Vectors.Char_Vector :=
           Get_Selection_Text
             (Formatting_Region.Enclosing_Node.Unit,
              Formatting_Region.Enclosing_Node,
              Formatting_Region.Start_Token,
              Formatting_Region.End_Token);
         Formatted_Text : Utils.Char_Vectors.Char_Vector;
         Diagnostics    : Pp.Scanner.Source_Message_Vector;

      begin
         if Formatting_Region.List_Slice then
            Format_Vector
              (Cmd                 => PP_Options,
               Input               => Input_Text,
               Node                => Formatting_Region.Enclosing_Node,
               Output              => Formatted_Text,
               Messages            => Diagnostics,
               Initial_Indentation => Initial_Indentation,
               Partial_GNATPP      => True,
               Start_Child_Index   => Formatting_Region.Start_Child_Index,
               End_Child_Index     => Formatting_Region.End_Child_Index);
         else
            Format_Vector
              (Cmd                 => PP_Options,
               Input               => Input_Text,
               Node                => Formatting_Region.Enclosing_Node,
               Output              => Formatted_Text,
               Messages            => Diagnostics,
               First_Line_Offset   => First_Line_Offset,
               Initial_Indentation => Initial_Indentation,
               Partial_GNATPP      => True);
         end if;

         declare
            Start_Line, End_Line : Line_Number;
            Start_Col, End_Col   : Column_Number;
            Formatted_Range      : Source_Location_Range;

         begin
            Start_Line :=
              Sloc_Range (Data (Formatting_Region.Start_Token)).Start_Line;

            declare
               use Ada.Strings.Wide_Wide_Fixed;

               Line                   : constant Text_Type :=
                 Formatting_Region.Enclosing_Node.Unit.Get_Line
                   (Positive (Start_Line));
               First_Non_Blank_Index  : constant Natural :=
                 Index_Non_Blank (Line);
               First_Non_Blank_Column : constant Natural :=
                 (if First_Non_Blank_Index = 0
                  then 0
                  else Index_Non_Blank (Line) - Line'First + 1);

            begin
               Start_Col :=
                 (if First_Non_Blank_Column /= 0
                    and then First_Non_Blank_Column
                             = Integer
                                 (Sloc_Range
                                    (Data (Formatting_Region.Start_Token))
                                    .Start_Column)
                  then 1
                  else
                    Sloc_Range (Data (Formatting_Region.Start_Token))
                      .Start_Column);

               if Start_Col /= 1 then
                  --  The formatted text has an Initial_Indentation in all
                  --  lines, including the first one. If Start_Col /= 1 it
                  --  means that we must trim the leading blanks of the
                  --  formatted text.

                  Formatted_Text_Ignore := @ + Initial_Indentation;
               end if;
            end;

            End_Line :=
              Sloc_Range (Data (Formatting_Region.End_Token)).End_Line;
            End_Col :=
              Sloc_Range (Data (Formatting_Region.End_Token)).End_Column;

            Formatted_Range :=
              Source_Location_Range'(Start_Line, End_Line, Start_Col, End_Col);

            return
              Partial_Formatting_Edit'
                (Edit           =>
                   Text_Edit'
                     (Formatted_Range,
                      Copy_Slice (Formatted_Text, Formatted_Text_Ignore)),
                 Formatted_Node => Formatting_Region.Enclosing_Node,
                 Indentation    => Initial_Indentation,
                 Diagnostics    => Diagnostics);
         end;
      end;
   end Format_Selection;

   -----------------------------------------------
   -- Previous_Non_Whitespace_Non_Comment_Token --
   -----------------------------------------------

   function Previous_Non_Whitespace_Non_Comment_Token
     (Token : Token_Reference) return Token_Reference is
   begin
      return
         Result : Token_Reference :=
           (if Token = No_Token then No_Token else Previous (Token))
      do
         while Result /= No_Token
           and then Kind (Data (Result)) in Ada_Whitespace | Ada_Comment
         loop
            Result := Previous (Result);
         end loop;
      end return;
   end Previous_Non_Whitespace_Non_Comment_Token;

   function Parent_Based_Indentation
     (Parents            : Ada_Node_Array;
      Indentation        : Positive := 3;
      Inline_Indentation : Positive := 2) return Natural;
   --  Computes Indentation starting at zero and incrementing based on the
   --  Parents kind or returning earlier if finds a parent that always sets
   --  indentation, for instance, a parameter list.

   ------------------------------
   -- Parent_Based_Indentation --
   ------------------------------

   function Parent_Based_Indentation
     (Parents            : Ada_Node_Array;
      Indentation        : Positive := 3;
      Inline_Indentation : Positive := 2) return Natural
   is
      Current_Indentation : Natural := 0;

   begin
      for Parent of Parents loop
         case Parent.Kind is
            when Ada_Loop_Stmt_Range
               | Ada_For_Loop_Stmt_Range
               | Ada_While_Loop_Stmt_Range
               | Ada_If_Stmt_Range
               | Ada_Case_Stmt_Range
               | Ada_Case_Stmt_Alternative_Range
               | Ada_Record_Type_Def_Range
               | Ada_Generic_Formal_Part_Range
               | Ada_Begin_Block_Range
               | Ada_Decl_Block_Range
            =>
               Current_Indentation := @ + Indentation;

            when Ada_Declarative_Part_Range =>
               --  When we type declare, a DeclBlock is created but not a
               --  DeclarativePart one. Only when you close the block with an
               --  end the node is created.
               --  DeclarativePart is a node that adds indentation.
               --  We cannot simply make DeclBlock also add indentation because
               --  it would double indent. So only add indentation to
               --  DeclarativeParts if their parent is not  DeclBlock.
               if Parent.Parent.Kind not in Ada_Decl_Block_Range then
                  Current_Indentation := @ + Indentation;
               end if;

            when Ada_Handled_Stmts_Range =>
               --  HandledStmts can be children of DeclBlock and BeginBlock.
               --  These two add indentation, so HandledStmts should not
               --  double add if its their child.
               if Parent.Parent.Kind
                  not in Ada_Begin_Block_Range | Ada_Decl_Block_Range
               then
                  Current_Indentation := @ + Indentation;
               end if;

            when Ada_Subp_Spec_Range | Ada_Assign_Stmt_Range =>
               Current_Indentation := @ + Inline_Indentation;

            when Ada_Dotted_Name_Range =>
               Current_Indentation :=
                 Natural (Parent.Sloc_Range.Start_Column)
                 - 1
                 + Inline_Indentation;
               exit;

            when Ada_Params_Range =>
               Current_Indentation :=
                 Natural (Parent.Sloc_Range.Start_Column) - 1 + 1;
               exit;

            when Ada_Assoc_List_Range | Ada_Component_List_Range =>
               Current_Indentation :=
                 Natural (Parent.Sloc_Range.Start_Column) - 1;
               exit;

            when others =>
               null;
         end case;
      end loop;

      return Current_Indentation;
   end Parent_Based_Indentation;

   --------------------------
   -- Estimate_Indentation --
   --------------------------

   function Estimate_Indentation
     (Unit : Analysis_Unit; Line_Number : Langkit_Support.Slocs.Line_Number)
      return Natural
   is
      Token : constant Token_Reference :=
        Unit.Lookup_Token (Source_Location'(Line_Number, 1));

      function Get_Relevant_Parents return Ada_Node_Array;
      --  TODO

      function Get_Relevant_Parents return Ada_Node_Array is
         Previous : Token_Reference :=
           (if Token = No_Token
            then No_Token
            else Previous_Non_Whitespace_Non_Comment_Token (Token));

      begin
         if Previous = No_Token then
            return [];
         end if;

         if Kind (Data (Previous)) in Ada_Comma | Ada_Dot then
            Previous := Previous_Non_Whitespace_Non_Comment_Token (Previous);
         end if;

         declare
            Node : constant Ada_Node :=
              Unit.Root.Lookup (Start_Sloc (Sloc_Range (Data (Previous))));

         begin
            if Node.Kind in Ada_Subp_Body_Range then
               if Kind (Data (Previous)) in Ada_Is then
                  return Node.As_Subp_Body.F_Decls.Parents;

               elsif Kind (Data (Previous)) in Ada_Begin then
                  return Node.As_Subp_Body.F_Stmts.Parents;
               end if;

            elsif Node.Kind in Ada_Package_Body_Range then
               if Kind (Data (Previous)) in Ada_Is then
                  return Node.As_Package_Body.F_Decls.Parents;

               elsif Kind (Data (Previous)) in Ada_Begin then
                  return Node.As_Package_Body.F_Stmts.Parents;
               end if;

            elsif Node.Kind in Ada_Package_Decl_Range then
               if Kind (Data (Previous)) in Ada_Is then
                  return Node.As_Package_Decl.F_Public_Part.Parents;

               elsif Kind (Data (Previous)) in Ada_Private then
                  return Node.As_Package_Decl.F_Private_Part.Parents;
               end if;

            elsif Node.Kind in Ada_Generic_Package_Internal_Range then
               if Kind (Data (Previous)) in Ada_Is then
                  return
                    Node.As_Generic_Package_Internal.F_Public_Part.Parents;

               elsif Kind (Data (Previous)) in Ada_Private then
                  return
                    Node.As_Generic_Package_Internal.F_Private_Part.Parents;
               end if;

            elsif Node.Kind in Ada_Generic_Formal_Part_Range then
               if Kind (Data (Previous)) in Ada_Generic then
                  return Node.As_Generic_Formal_Part.F_Decls.Parents;
               end if;
            end if;

            return Node.Parents;
         end;
      end Get_Relevant_Parents;

   begin

      return Parent_Based_Indentation (Get_Relevant_Parents);
   end Estimate_Indentation;

   --------------------------
   -- Estimate_Indentation --
   --------------------------

   function Estimate_Indentation
     (Node               : Ada_Node;
      Indentation        : Positive := 3;
      Inline_Indentation : Positive := 2) return Natural
   is
      Parents : constant Ada_Node_Array :=
        (if Node.Is_Null then [] else Node.Parents (False));

   begin
      return
        Parent_Based_Indentation (Parents, Indentation, Inline_Indentation);
   end Estimate_Indentation;

end Laltools.Partial_GNATPP;
