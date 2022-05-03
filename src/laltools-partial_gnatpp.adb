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

with Libadalang.Common; use Libadalang.Common;
with Laltools.Common;   use Laltools.Common;

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

   function Get_Common_Enclosing_Parent_Node
     (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node;
   --  Starting from 2 given nodes, get the first enclosing common parent node.

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

      return
        Unit.Root.Lookup (Start_Sloc (Sloc_Range (Data (Crt_Token))))
          .As_Ada_Node;
   end Lookup;

   ------------------------------------------
   --  Get_Selected_Region_Enclosing_Node  --
   ------------------------------------------

   procedure Get_Selected_Region_Enclosing_Node
     (Unit           :     Analysis_Unit; SL_Range : Source_Location_Range;
      Start_Node     : out Ada_Node; End_Node : out Ada_Node;
      Enclosing_Node : out Ada_Node)
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
        (Kind in Ada_Decl_Block | Ada_Entry_Body |
         Ada_Package_Body | Ada_Subp_Body | Ada_Task_Body |
         Ada_Begin_Block | Ada_Extended_Return_Stmt |
         Ada_Accept_Stmt_With_Stmts |
         Ada_Type_Decl | Ada_Compilation_Unit);

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

      if not Is_Relevant_Parent_Kind (Kind (Crt_Start_Node)) then
         Find_Matching_Parents
           (Crt_Start_Node, Is_Relevant_Parent_Node'Access,
            Is_Relevant_Parent_Node_Callback'Access);
      end if;
      Start_Node := Parent_Node.As_Ada_Node;

      --  ??? We might want to get the relevant syntactic parent of Start_Node
      --  in some situations

      --  Find the first relevant parent of Crt_End_Node
      Parent_Node := Crt_End_Node;

      if not Is_Relevant_Parent_Kind (Kind (Crt_End_Node)) then
         Find_Matching_Parents
           (Crt_End_Node, Is_Relevant_Parent_Node'Access,
            Is_Relevant_Parent_Node_Callback'Access);
      end if;
      End_Node := Parent_Node.As_Ada_Node;

      --  ??? We might want to get the relevant syntactic parent of End_Node
      --  in some situations

      --  This case could be when the selection contains different parts of
      --  different nodes. In this situation we need to find the first
      --  encolsing parent node, otherwise the Enclosing_Node will be equal to
      --  Start_Node or End_Node in some situations.

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
   end Get_Selected_Region_Enclosing_Node;

   ----------------------------
   --  Get_Previous_Sibling  --
   ----------------------------

   function Get_Previous_Sibling (Node : Ada_Node) return Ada_Node
   is
   begin
      pragma Assert (Node /= No_Ada_Node);
      return Node.Previous_Sibling.As_Ada_Node;
   end Get_Previous_Sibling;

   ------------------------
   --  Get_Next_Sibling  --
   ------------------------

   function Get_Next_Sibling (Node : Ada_Node) return Ada_Node
   is
   begin
      pragma Assert (Node /= No_Ada_Node);
      return Node.Next_Sibling.As_Ada_Node;
   end Get_Next_Sibling;

end Laltools.Partial_GNATPP;
