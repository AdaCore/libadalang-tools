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

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Common; use Libadalang.Common;

package body Laltools.Refactor.Sort_Dependencies is

   ------------------------------------
   -- Is_Sort_Dependencies_Available --
   ------------------------------------

   function Is_Sort_Dependencies_Available
     (Unit             : Analysis_Unit;
      Sloc             : Source_Location)
      return Boolean is
   begin
      if Unit = No_Analysis_Unit or else Unit.Root.Is_Null then
         return False;
      end if;

      declare
         Node                  : constant Ada_Node := Unit.Root.Lookup (Sloc);
         Node_Compilation_Unit : Libadalang.Analysis.Compilation_Unit;
         Node_Prelude          : Ada_Node_List;
      begin
         if Node.Is_Null then
            return False;
         end if;

         Node_Compilation_Unit := Node.P_Enclosing_Compilation_Unit;
         if Node_Compilation_Unit.Is_Null then
            return False;
         end if;

         Node_Prelude := Node_Compilation_Unit.F_Prelude;
         if Node_Prelude.Is_Null then
            return False;
         end if;

         return Compare (Node_Prelude.Sloc_Range, Sloc) = Inside;
      end;
   end Is_Sort_Dependencies_Available;

   --------------------------------
   -- Create_Dependencies_Sorter --
   --------------------------------

   function Create_Dependencies_Sorter
     (Compilation_Unit : Libadalang.Analysis.Compilation_Unit)
      return Dependencies_Sorter
   is (Dependencies_Sorter'(Compilation_Unit => Compilation_Unit));

   function Less (Left, Right : Unbounded_Text_Type) return Boolean is
     (Ada.Strings.Less_Case_Insensitive
        (To_UTF8 (To_Text (Left)), To_UTF8 (To_Text (Right))));

   type Clause_Pragma_Type is record
      Pragma_Node       : Libadalang.Analysis.Pragma_Node;
      Leading_Comments  : Unbounded_Text_Type;
      Trailing_Comments : Unbounded_Text_Type;
   end record;

   package Clause_Pragma_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Clause_Pragma_Type);

   subtype Clause_Pragma_Vector is Clause_Pragma_Vectors.Vector;

   type Clause_Type is record
      Name                     : Unbounded_Text_Type;
      First_Name               : Unbounded_Text_Type;
      Has_With                 : Boolean;
      With_Clause_Node         : With_Clause;
      With_Leading_Comments    : Unbounded_Text_Type;
      With_Trailing_Comments   : Unbounded_Text_Type;
      Has_Use                  : Boolean;
      Use_Package_Clause_Node  : Use_Package_Clause;
      Use_Leading_Comments     : Unbounded_Text_Type;
      Use_Trailing_Comments    : Unbounded_Text_Type;
      Associated_Pragmas       : Clause_Pragma_Vector;
   end record;

   No_Clause_Type : constant Clause_Type :=
     Clause_Type'
       (Name                     =>
          Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String,
        First_Name               =>
          Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String,
        Has_With                 => False,
        With_Clause_Node         => No_With_Clause,
        With_Leading_Comments    =>
          Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String,
        With_Trailing_Comments   =>
          Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String,
        Has_Use                  => False,
        Use_Package_Clause_Node  => No_Use_Package_Clause,
        Use_Leading_Comments     =>
          Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String,
        Use_Trailing_Comments    =>
          Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String,
        Associated_Pragmas       => Clause_Pragma_Vectors.Empty_Vector);

   function Equal_By_Name (Left, Right : Clause_Type) return Boolean is
     (Ada.Strings.Equal_Case_Insensitive
        (To_UTF8 (To_Text (Left.Name)), To_UTF8 (To_Text (Right.Name))));

   package Clause_Ordered_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_Text_Type,
      Element_Type => Clause_Type,
      "<"          => Less,
      "="          => Equal_By_Name);

   subtype Clause_Ordered_Map is Clause_Ordered_Maps.Map;

   function Is_Multi_Line_Break (Token : Token_Reference) return Boolean;
   --  Checks if Token is a Whitespace token with multiple line breaks

   function Is_Single_Line_Break (Token : Token_Reference) return Boolean;
   --  Checks if Token is a Whitespace token with multiple line breaks

   function Exceptional_Leading_Comments
     (Node : Ada_Node'Class)
      return Unbounded_Text_Type;
   --  Gets the block of comments before Node up until a double line break is
   --  found.

   procedure Exceptional_Leading_Comments
     (Node        : Ada_Node'Class;
      Token_Start : out Token_Reference;
      Token_End   : out Token_Reference);
   --  Gets the block of comments before Node up until a double line break is
   --  found.

   function Trailing_Comments
     (Node : Ada_Node'Class)
      return Unbounded_Text_Type;
   --  Gets the block of comments after Node up until the next Node is found
   --  or it's exceptional leading comments.

   procedure Trailing_Comments
     (Node        : Ada_Node'Class;
      Token_Start : out Token_Reference;
      Token_End   : out Token_Reference);
   --  Gets the block of comments after Node up until the next Node is found
   --  or it's exceptional leading comments.

   function Text
     (Token_Start, Token_End : Token_Reference)
      return Unbounded_Text_Type;
   --  Returns the text between Token_Start and Token_End inclusive.

   -------------------------
   -- Is_Multi_Line_Break --
   -------------------------

   function Is_Multi_Line_Break (Token : Token_Reference) return Boolean is
     (Kind (Data (Token)) in Ada_Whitespace
      and then Sloc_Range (Data (Token)).End_Line
               - Sloc_Range (Data (Token)).Start_Line
               > 1);

   --------------------------
   -- Is_Single_Line_Break --
   --------------------------

   function Is_Single_Line_Break (Token : Token_Reference) return Boolean is
     (Kind (Data (Token)) in Ada_Whitespace
      and then Sloc_Range (Data (Token)).End_Line
               - Sloc_Range (Data (Token)).Start_Line
               = 1);

   ----------------------------------
   -- Exceptional_Leading_Comments --
   ----------------------------------

   function Exceptional_Leading_Comments
     (Node : Ada_Node'Class)
      return Unbounded_Text_Type
   is
      Token_Start : Token_Reference;
      Token_End   : Token_Reference;

   begin
      Exceptional_Leading_Comments (Node, Token_Start, Token_End);

      return Text (Token_Start, Token_End);
   end Exceptional_Leading_Comments;

   ----------------------------------
   -- Exceptional_Leading_Comments --
   ----------------------------------

   procedure Exceptional_Leading_Comments
     (Node        : Ada_Node'Class;
      Token_Start : out Token_Reference;
      Token_End   : out Token_Reference)
   is
      Node_Token_Start : constant Token_Reference := Node.Token_Start;
      Current_Token    : Token_Reference := Previous (Node_Token_Start);

      Comment_Seen          : Boolean := False;
      Multi_Line_Break_Seen : Boolean := False;

   begin
      Token_Start := No_Token;
      Token_End := No_Token;

      if Current_Token = No_Token then
         return;
      else
         Token_End := Current_Token;
      end if;

      loop
         exit when Current_Token = No_Token
           or else Kind (Data (Current_Token))
         not in Ada_Whitespace | Ada_Comment;
         Comment_Seen := @ or Kind (Data (Current_Token)) in Ada_Comment;
         if Is_Multi_Line_Break (Current_Token) then
            Multi_Line_Break_Seen := True;
            exit;
         elsif Is_Single_Line_Break (Current_Token)
           and then Kind (Data (Previous (Current_Token))) in Ada_Comment
         then
            Token_Start := Current_Token;
         else
            Token_Start := Current_Token;
         end if;
         Current_Token := Previous (Current_Token);
      end loop;

      if not Comment_Seen or else not Multi_Line_Break_Seen then
         Token_Start := No_Token;
         Token_End := No_Token;
      end if;
   end Exceptional_Leading_Comments;

   -----------------------
   -- Trailing_Comments --
   -----------------------

   function Trailing_Comments
     (Node : Ada_Node'Class)
      return Unbounded_Text_Type
   is
      Token_Start : Token_Reference;
      Token_End   : Token_Reference;

   begin
      Trailing_Comments (Node, Token_Start, Token_End);

      return Text (Token_Start, Token_End);
   end Trailing_Comments;

   -----------------------
   -- Trailing_Comments --
   -----------------------

   procedure Trailing_Comments
     (Node        : Ada_Node'Class;
      Token_Start : out Token_Reference;
      Token_End   : out Token_Reference)
   is
      Node_Token_End : constant Token_Reference := Node.Token_End;
      Current_Token  : Token_Reference := Next (Node_Token_End);

      Comment_Block         : Token_Reference := No_Token;
      Comment_Seen          : Boolean := False;
      Multi_Line_Break_Seen : Boolean := False;

   begin
      Token_Start := No_Token;
      Token_End := No_Token;

      if Current_Token = No_Token then
         return;
      end if;

      Token_Start := Current_Token;

      loop
         exit when Current_Token = No_Token
                   or else Kind (Data (Current_Token))
                           not in Ada_Whitespace | Ada_Comment;

         Comment_Seen := @ or Kind (Data (Current_Token)) in Ada_Comment;

         if Multi_Line_Break_Seen then
            if Is_Multi_Line_Break (Current_Token) then
               Token_End := Comment_Block;
            elsif Kind (Data (Current_Token)) in Ada_Comment then
               Comment_Block := Current_Token;
            end if;

         else
            if Is_Multi_Line_Break (Current_Token) then
               Multi_Line_Break_Seen := True;
            elsif Kind (Data (Current_Token)) in Ada_Comment then
               Token_End := Current_Token;
            end if;
         end if;

         Current_Token := Next (Current_Token);
      end loop;

      if not Comment_Seen then
         Token_Start := No_Token;
         Token_End := No_Token;
         return;
      end if;
   end Trailing_Comments;

   ----------
   -- Text --
   ----------

   function Text
     (Token_Start, Token_End : Token_Reference)
      return Unbounded_Text_Type
   is
      use Ada.Strings.Wide_Wide_Unbounded;

      Result : Unbounded_Text_Type :=
        Null_Unbounded_Wide_Wide_String;
      Current_Token : Token_Reference := Token_Start;

   begin
      if Token_Start = No_Token or else Token_End = No_Token then
         return Null_Unbounded_Wide_Wide_String;
      end if;

      loop
         Append (Result, Text (Current_Token));
         exit when Current_Token = Token_End;
         Current_Token := Next (Current_Token);
      end loop;

      return Result;
   end Text;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Dependencies_Sorter;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      Edits : Refactoring_Edits;

      Initial_Node             : Ada_Node;
      Final_Node_Non_Inclusive : Ada_Node;

      Public_Clauses     : Clause_Ordered_Map;
      Private_Clauses    : Clause_Ordered_Map;
      Limited_Clauses    : Clause_Ordered_Map;

      Previous_Package_Name : Unbounded_Text_Type :=
        Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;

      procedure Process_Compilation_Unit
        (Compilation_Unit : Libadalang.Analysis.Compilation_Unit);

      procedure Process_Prelude_Node
        (Prelude_Node : Libadalang.Analysis.Ada_Node'Class)
        with Pre => Prelude_Node.Kind in
                      Ada_With_Clause_Range
                      | Ada_Use_Package_Clause_Range
                      | Ada_Pragma_Node_Range;

      ------------------------------
      -- Process_Compilation_Unit --
      ------------------------------

      procedure Process_Compilation_Unit
        (Compilation_Unit : Libadalang.Analysis.Compilation_Unit)
      is
         procedure Compute_Edits;

         -------------------
         -- Compute_Edits --
         -------------------

         procedure Compute_Edits is
            Clauses_Text : Unbounded_Text_Type;

            procedure Compute_Edits_Helper (Clauses : Clause_Ordered_Map);

            --------------------------
            -- Compute_Edits_Helper --
            --------------------------

            procedure Compute_Edits_Helper (Clauses : Clause_Ordered_Map) is
               use Ada.Containers;
               use Ada.Strings.Wide_Wide_Unbounded;

               Previous_Public_Clause : Clause_Type := No_Clause_Type;

            begin
               for Clause of Clauses loop
                  if Clause.Has_With then
                     if Previous_Public_Clause /= No_Clause_Type
                       and then not Ada.Strings.Wide_Wide_Unbounded."="
                         (Previous_Public_Clause.First_Name,
                          Clause.First_Name)
                     then
                        Append
                          (Clauses_Text,
                           Ada.Characters.Wide_Wide_Latin_1.LF);
                     end if;
                     Previous_Public_Clause := Clause;
                     Append (Clauses_Text, Clause.With_Leading_Comments);
                     Append (Clauses_Text, Clause.With_Clause_Node.Text);
                     Append (Clauses_Text, Clause.With_Trailing_Comments);

                     if Clause.Has_Use then
                        if Ada.Strings.Wide_Wide_Unbounded."="
                             (Clause.With_Trailing_Comments,
                              Null_Unbounded_Wide_Wide_String)
                        then
                           if Ada.Strings.Wide_Wide_Unbounded."="
                                (Clause.Use_Leading_Comments,
                                 Null_Unbounded_Wide_Wide_String)
                           then
                              Append
                                (Clauses_Text,
                                 Ada.Characters.Wide_Wide_Latin_1.Space);
                           else
                              Append
                                (Clauses_Text,
                                 Ada.Characters.Wide_Wide_Latin_1.LF);
                           end if;
                        else
                           Append
                             (Clauses_Text,
                              Ada.Characters.Wide_Wide_Latin_1.LF);
                        end if;
                        Append
                          (Clauses_Text,
                           Clause.Use_Leading_Comments);
                        Append
                          (Clauses_Text,
                           Clause.Use_Package_Clause_Node.Text);
                        Append
                          (Clauses_Text,
                           Clause.Use_Trailing_Comments);
                        Append
                          (Clauses_Text,
                           Ada.Characters.Wide_Wide_Latin_1.LF);

                     else
                        Append
                          (Clauses_Text,
                           Ada.Characters.Wide_Wide_Latin_1.LF);
                     end if;
                     for Associated_Pragma of
                       Clause.Associated_Pragmas
                     loop
                        Append
                          (Clauses_Text,
                           Associated_Pragma.Leading_Comments);
                        Append
                          (Clauses_Text,
                           Associated_Pragma.Pragma_Node.Text);
                        Append
                          (Clauses_Text,
                           Associated_Pragma.Trailing_Comments);
                        Append
                          (Clauses_Text,
                           Ada.Characters.Wide_Wide_Latin_1.LF);
                     end loop;
                  end if;
               end loop;
               if Public_Clauses.Length > 0 then
                  Append
                    (Clauses_Text,
                     Ada.Characters.Wide_Wide_Latin_1.LF);
               end if;
            end Compute_Edits_Helper;

            Token_Start : Token_Reference := No_Token;
            Token_End_Dummy : Token_Reference := No_Token;

         begin
            Compute_Edits_Helper (Public_Clauses);
            Compute_Edits_Helper (Private_Clauses);
            Compute_Edits_Helper (Limited_Clauses);
            Exceptional_Leading_Comments
              (Initial_Node, Token_Start, Token_End_Dummy);
            declare
               Initial_Location : constant Source_Location :=
                 (if Token_Start /= No_Token then
                    Start_Sloc (Sloc_Range (Data (Token_Start)))
                  else
                     Start_Sloc (Initial_Node.Sloc_Range));
               Final_Location   : constant Source_Location :=
                 Start_Sloc (Final_Node_Non_Inclusive.Sloc_Range);
               Edit_SLOC_Range  : constant Source_Location_Range :=
                 Make_Range (Initial_Location, Final_Location);

               Edit : constant Text_Edit :=
                 Text_Edit'
                   (Location => Edit_SLOC_Range,
                    Text     =>
                      To_Unbounded_String
                        (To_UTF8 (To_Text (Clauses_Text))));

            begin
               Safe_Insert
                 (Edits.Text_Edits, Compilation_Unit.Unit.Get_Filename, Edit);
            end;
         end Compute_Edits;

         In_Initial_Pragma_List   : Boolean := True;

      begin
         Final_Node_Non_Inclusive := Compilation_Unit.F_Body;
         for Prelude_Node of Compilation_Unit.F_Prelude loop
            case Prelude_Node.Kind is
               when Ada_Pragma_Node_Range =>
                  --  If Prelude_Node belongs to the initial list of pragma
                  --  nodes, ignore it.

                  if In_Initial_Pragma_List then
                     null;

                  else
                     Process_Prelude_Node (Prelude_Node);
                  end if;

               when Ada_With_Clause_Range | Ada_Use_Package_Clause_Range =>
                  if In_Initial_Pragma_List then
                     In_Initial_Pragma_List := False;
                     Initial_Node := Prelude_Node.As_Ada_Node;
                  end if;

                  Process_Prelude_Node (Prelude_Node);

               when others =>
                  --  Unexpected Node.Kind. No need to fail, however, this
                  --  should be logged in the future.
                  null;
            end case;
         end loop;

         Compute_Edits;
      end Process_Compilation_Unit;

      --------------------------
      -- Process_Prelude_Node --
      --------------------------

      procedure Process_Prelude_Node
        (Prelude_Node : Libadalang.Analysis.Ada_Node'Class) is
      begin
         if Prelude_Node.Kind in Ada_With_Clause_Range then
            declare
               With_Clause       :
                 constant Libadalang.Analysis.With_Clause :=
                   Prelude_Node.As_With_Clause;
               Leading_Comments  : constant Unbounded_Text_Type :=
                 Laltools.Refactor.Sort_Dependencies.
                   Exceptional_Leading_Comments (With_Clause);
               Trailing_Comments : constant Unbounded_Text_Type :=
                 Laltools.Refactor.Sort_Dependencies.
                   Trailing_Comments (With_Clause);

            begin
               for Package_Name of With_Clause.F_Packages loop
                  Previous_Package_Name :=
                    To_Unbounded_Text (Package_Name.Text);
                  if With_Clause.F_Has_Private then
                     if Private_Clauses.Contains
                          (To_Unbounded_Text (Package_Name.Text))
                     then
                        --  Repeated with clause or with after use
                        Private_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).Has_With :=
                             True;
                        Private_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).
                            With_Clause_Node := With_Clause;
                        Private_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).
                            With_Leading_Comments :=
                              Leading_Comments;
                        Private_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).
                             With_Trailing_Comments :=
                               Trailing_Comments;

                     else
                        Private_Clauses.Insert
                          (To_Unbounded_Text (Package_Name.Text),
                           Clause_Type'
                             (Name                     =>
                                  To_Unbounded_Text (Package_Name.Text),
                              First_Name               =>
                                (if Package_Name.Kind in Ada_Dotted_Name then
                                   Package_Name.
                                     As_Dotted_Name.P_As_Symbol_Array (1)
                                 else
                                   To_Unbounded_Text (Package_Name.Text)),
                              Has_With                 => True,
                              With_Clause_Node         => With_Clause,
                              With_Leading_Comments    => Leading_Comments,
                              With_Trailing_Comments   => Trailing_Comments,
                              Has_Use                  => False,
                              Use_Package_Clause_Node  =>
                                No_Use_Package_Clause,
                              Use_Leading_Comments     =>
                                Ada.Strings.Wide_Wide_Unbounded.
                                  Null_Unbounded_Wide_Wide_String,
                              Use_Trailing_Comments    =>
                                Ada.Strings.Wide_Wide_Unbounded.
                                  Null_Unbounded_Wide_Wide_String,
                              Associated_Pragmas       =>
                                Clause_Pragma_Vectors.Empty_Vector));
                     end if;

                  elsif With_Clause.F_Has_Limited then
                     if Limited_Clauses.Contains
                          (To_Unbounded_Text (Package_Name.Text))
                     then
                        --  Repeated with clause or with after use
                        Limited_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).Has_With :=
                             True;
                        Limited_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).
                            With_Clause_Node := With_Clause;
                        Limited_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).
                            With_Leading_Comments :=
                              Leading_Comments;
                        Limited_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).
                            With_Trailing_Comments :=
                              Trailing_Comments;

                     else
                        Limited_Clauses.Insert
                          (To_Unbounded_Text (Package_Name.Text),
                           Clause_Type'
                             (Name                     =>
                                  To_Unbounded_Text (Package_Name.Text),
                              First_Name               =>
                                (if Package_Name.Kind in Ada_Dotted_Name then
                                   Package_Name.
                                     As_Dotted_Name.P_As_Symbol_Array (1)
                                 else
                                   To_Unbounded_Text (Package_Name.Text)),
                              Has_With                 => True,
                              With_Clause_Node         => With_Clause,
                              With_Leading_Comments    => Leading_Comments,
                              With_Trailing_Comments   => Trailing_Comments,
                              Has_Use                  => False,
                              Use_Package_Clause_Node  =>
                                No_Use_Package_Clause,
                              Use_Leading_Comments     =>
                                Ada.Strings.Wide_Wide_Unbounded.
                                  Null_Unbounded_Wide_Wide_String,
                              Use_Trailing_Comments    =>
                                Ada.Strings.Wide_Wide_Unbounded.
                                  Null_Unbounded_Wide_Wide_String,
                              Associated_Pragmas       =>
                                Clause_Pragma_Vectors.Empty_Vector));
                     end if;

                  else
                     if Public_Clauses.Contains
                          (To_Unbounded_Text (Package_Name.Text))
                     then
                        --  Repeated with clause or with after use
                        Public_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).Has_With :=
                             True;
                        Public_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).
                            With_Clause_Node := With_Clause;
                        Public_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).
                            With_Leading_Comments :=
                              Leading_Comments;
                        Public_Clauses.Reference
                          (To_Unbounded_Text (Package_Name.Text)).
                            With_Trailing_Comments :=
                              Trailing_Comments;

                     else
                        Public_Clauses.Insert
                          (To_Unbounded_Text (Package_Name.Text),
                           Clause_Type'
                             (Name                     =>
                                  To_Unbounded_Text (Package_Name.Text),
                              First_Name               =>
                                (if Package_Name.Kind in Ada_Dotted_Name then
                                   Package_Name.
                                     As_Dotted_Name.P_As_Symbol_Array (1)
                                 else
                                   To_Unbounded_Text (Package_Name.Text)),
                              Has_With                 => True,
                              With_Clause_Node         => With_Clause,
                              With_Leading_Comments    => Leading_Comments,
                              With_Trailing_Comments   => Trailing_Comments,
                              Has_Use                  => False,
                              Use_Package_Clause_Node  =>
                                No_Use_Package_Clause,
                              Use_Leading_Comments     =>
                                Ada.Strings.Wide_Wide_Unbounded.
                                  Null_Unbounded_Wide_Wide_String,
                              Use_Trailing_Comments    =>
                                Ada.Strings.Wide_Wide_Unbounded.
                                  Null_Unbounded_Wide_Wide_String,
                              Associated_Pragmas       =>
                                Clause_Pragma_Vectors.Empty_Vector));
                     end if;
                  end if;
               end loop;
            end;

         elsif Prelude_Node.Kind in Ada_Use_Package_Clause_Range then
            declare
               Use_Package_Clause :
                 constant Libadalang.Analysis.Use_Package_Clause :=
                   Prelude_Node.As_Use_Package_Clause;
               Leading_Comments  : constant Unbounded_Text_Type :=
                 Laltools.Refactor.Sort_Dependencies.
                   Exceptional_Leading_Comments (Use_Package_Clause);
               Trailing_Comments : constant Unbounded_Text_Type :=
                 Laltools.Refactor.Sort_Dependencies.
                   Trailing_Comments (Use_Package_Clause);
            begin
               for Package_Name of Use_Package_Clause.F_Packages loop
                  Previous_Package_Name :=
                    To_Unbounded_Text (Package_Name.Text);
                  if Public_Clauses.Contains
                    (To_Unbounded_Text (Package_Name.Text))
                  then
                     --  Repeated with clause or with after use
                     Public_Clauses.Reference
                       (To_Unbounded_Text (Package_Name.Text)).Has_Use :=
                         True;
                     Public_Clauses.Reference
                       (To_Unbounded_Text (Package_Name.Text)).
                         Use_Package_Clause_Node := Use_Package_Clause;
                     Public_Clauses.Reference
                       (To_Unbounded_Text (Package_Name.Text)).
                          Use_Leading_Comments :=
                            Leading_Comments;
                     Public_Clauses.Reference
                       (To_Unbounded_Text (Package_Name.Text)).
                          Use_Trailing_Comments :=
                            Trailing_Comments;

                  else
                     Public_Clauses.Insert
                       (To_Unbounded_Text (Package_Name.Text),
                        Clause_Type'
                          (Name                     =>
                             To_Unbounded_Text (Package_Name.Text),
                              First_Name               =>
                                (if Package_Name.Kind in Ada_Dotted_Name then
                                   Package_Name.
                                     As_Dotted_Name.P_As_Symbol_Array (1)
                                 else
                                   To_Unbounded_Text (Package_Name.Text)),
                           Has_With                 => False,
                           With_Clause_Node         => No_With_Clause,
                           With_Leading_Comments    =>
                             Ada.Strings.Wide_Wide_Unbounded.
                               Null_Unbounded_Wide_Wide_String,
                           With_Trailing_Comments   =>
                             Ada.Strings.Wide_Wide_Unbounded.
                               Null_Unbounded_Wide_Wide_String,
                           Has_Use                  => False,
                           Use_Package_Clause_Node  => Use_Package_Clause,
                           Use_Leading_Comments     => Leading_Comments,
                           Use_Trailing_Comments    => Trailing_Comments,
                           Associated_Pragmas       =>
                             Clause_Pragma_Vectors.Empty_Vector));
                  end if;
               end loop;
            end;

         else
            declare
               Clause_Pragma_Node : constant Pragma_Node :=
                 Prelude_Node.As_Pragma_Node;
               Leading_Comments   : constant Unbounded_Text_Type :=
                 Laltools.Refactor.Sort_Dependencies.
                   Exceptional_Leading_Comments (Clause_Pragma_Node);
               Trailing_Comments  : constant Unbounded_Text_Type :=
                 Laltools.Refactor.Sort_Dependencies.
                   Trailing_Comments (Clause_Pragma_Node);

               Clause_Pragma : constant Clause_Pragma_Type :=
                 Clause_Pragma_Type'
                   (Clause_Pragma_Node,
                    Leading_Comments,
                    Trailing_Comments);

            begin
               if Public_Clauses.Contains (Previous_Package_Name) then
                  Public_Clauses.Reference (Previous_Package_Name).
                    Associated_Pragmas.Append (Clause_Pragma);

               elsif Private_Clauses.Contains (Previous_Package_Name) then
                  Private_Clauses.Reference (Previous_Package_Name).
                    Associated_Pragmas.Append (Clause_Pragma);

               elsif Limited_Clauses.Contains (Previous_Package_Name) then
                  Limited_Clauses.Reference (Previous_Package_Name).
                    Associated_Pragmas.Append (Clause_Pragma);
               else
                  --  Ignore associated entities that have not been
                  --  been seen in a with use clause.
                  null;
               end if;
            end;
         end if;
      end Process_Prelude_Node;

   begin
      Process_Compilation_Unit (Self.Compilation_Unit);

      return Edits;
   end Refactor;

end Laltools.Refactor.Sort_Dependencies;
