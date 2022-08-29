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

with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Latin_1;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Hashed_Sets;

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Wide_Wide_Fixed;

with Libadalang.Analysis; use Libadalang.Analysis;
with Langkit_Support.Text;

package body Laltools.Refactor.Extract_Subprogram is

   Tool_Name : constant String := "Extract Subprogram";

   function Get_Parameter_Mode_From_Reference
     (Reference : Base_Id'Class)
      return Ada_Mode;
   --  Assuming that Reference will be passed as a subprogram argument,
   --  computes its mode by checking if Reference is being read or written.

   function Get_Statements_To_Extract
     (Start_Stmt : Stmt;
      End_Stmt   : Stmt)
      return Stmt_Vector;
   --  Returns a vector with all Stmt nodes between Start_Stmt and End_Stmt,
   --  inclusive. The vector is ordered from Start_Stmt to End_Stmt. If
   --  Start_Stmt = End_Stmt, then the returned vector only contains
   --  Start_Stmt.

   function Is_Declaration_Owner
     (Node : Ada_Node'Class)
      return Boolean
   is (Is_Declarative_Part_Owner (Node)
       or else Node.Kind in
         Ada_Accept_Stmt_With_Stmts_Range
         | Ada_For_Loop_Stmt_Range
         | Ada_Exception_Handler_Range
         | Ada_Extended_Return_Stmt_Range)
     with Pre => not Node.Is_Null;

   type Direction  is (Forward, Backward);

   function Lookup
     (Unit  : Analysis_Unit;
      Token : Token_Reference;
      Going : Direction)
      return Ada_Node;
   --  Finds the next Ada_Node relative to Token. Going controls the lookup
   --  direction. If Token already belongs to an Ada_Node, that node is
   --  returned. Returns No_Ada_Node if no node is found or if
   --  Token = No_Token.

   function Next_Non_Whitespace
     (Token : Token_Reference;
      Going : Direction)
      return Token_Reference;
   --  Finds the next non white Token_Reference relative to Token. Going
   --  controls the lookup direction. Returns No_Token if no whitespace
   --  if found or if Token = No_Token.

   function To_String
     (Mode          : Ada_Mode;
      Default_As_In : Boolean := False)
      return String;

   function To_String_Vector
     (Set : Defining_Name_Ordered_Set)
      return String_Vector;
   --  Converts a Defining_Name_Ordered_Set into a String_Vector

   procedure Update_Parameter_Mode
     (Parameter_Mode : in out Ada_Mode;
      New_Mode       : Ada_Mode);
   --  This is a specific OR operation between modes.
   --  If New_Mode has an In component, then it is added to Old_Mode.
   --  Same for the Out component.

   procedure Update_Parameter_Mode
     (Parameters_Mode : in out Parameters_Mode_Map;
      Parameter       : Defining_Name'Class;
      Mode            : Ada_Mode);
   --  Updates Parameter key in Parameters_Mode with Mode. If Parameter is not
   --  in Parameters_Mode, it is added with value Mode.

   -------------------------------
   -- Get_Statements_To_Extract --
   -------------------------------

   function Get_Statements_To_Extract
     (Start_Stmt : Stmt;
      End_Stmt   : Stmt)
      return Stmt_Vector
   is
      Stmts    : Stmt_Vector;
      Aux_Node : Ada_Node;

   begin
      Stmts.Append (Start_Stmt);

      Aux_Node := Start_Stmt.As_Ada_Node;

      loop
         exit when Aux_Node.As_Stmt = End_Stmt;
         Aux_Node := Aux_Node.Next_Sibling;
         exit when Aux_Node.Is_Null;
         Stmts.Append (Aux_Node.As_Stmt);
      end loop;

      return Stmts;
   end Get_Statements_To_Extract;

   ---------------------------------------
   -- Get_Parameter_Mode_From_Reference --
   ---------------------------------------

   function Get_Parameter_Mode_From_Reference
     (Reference : Base_Id'Class)
      return Ada_Mode
   is
      Mode : Ada_Mode := Ada_Mode_Default;

   begin
      if Reference.Parent.Kind in Ada_Assign_Stmt_Range then
         if Reference = Reference.Parent.As_Assign_Stmt.F_Expr then
            Mode := Ada_Mode_Default;
         else
            Assert (Reference = Reference.Parent.As_Assign_Stmt.F_Dest);
            Mode := Ada_Mode_Out;
         end if;

      elsif Reference.Parent.Parent.Kind in Ada_Assign_Stmt_Range then
         if Reference.Parent = Reference.Parent.Parent.As_Assign_Stmt.F_Dest
           and then Reference.Parent =
                      Reference.Parent.Parent.As_Assign_Stmt.F_Expr
         then
            Mode := Ada_Mode_In_Out;
         elsif Reference.Parent =
           Reference.Parent.Parent.As_Assign_Stmt.F_Dest
         then
            Mode := Ada_Mode_Out;
         else
            Assert (Reference.Parent =
                      Reference.Parent.Parent.As_Assign_Stmt.F_Expr);
            Mode := Ada_Mode_Default;
         end if;

      elsif Reference.Parent.Kind in Ada_Param_Assoc then
         for Parameter of
           Reference.Parent.As_Param_Assoc.P_Get_Params
         loop
            Mode :=
              Ada_Mode (Parameter.P_Basic_Decl.As_Param_Spec.F_Mode.Kind);

            --  This is a list of parameters in a Param_Spec. They all have the
            --  same mode, so only one needs to be checked.
            exit;
         end loop;

      elsif Reference.Parent.Parent.Kind in Ada_Param_Assoc then
         for Parameter of
           Reference.Parent.Parent.As_Param_Assoc.P_Get_Params
         loop
            Mode :=
              Ada_Mode (Parameter.P_Basic_Decl.As_Param_Spec.F_Mode.Kind);

            --  This is a list of parameters in a Param_Spec. They all have the
            --  same mode, so only one needs to be checked.
            exit;
         end loop;
      end if;

      if Mode = Ada_Mode_In then
         Mode := Ada_Mode_Default;
      end if;

      return Mode;
   end Get_Parameter_Mode_From_Reference;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Unit  : Analysis_Unit;
      Token : Token_Reference;
      Going : Direction)
      return Ada_Node
   is
      Aux_Token      : Token_Reference := Token;
      Aux_Token_Kind : Libadalang.Common.Token_Kind :=
        Kind (Data (Aux_Token));

   begin
      --  Do nothing if Aux_Token <=> Token is a No_Token or already belongs to
      --  an Ada_Node.

      while not (Aux_Token = No_Token)
        and then Aux_Token_Kind in Ada_Comment | Ada_Whitespace
      loop
         case Going is
            when Forward => Aux_Token := Next (Aux_Token);
            when Backward => Aux_Token := Previous (Aux_Token);
         end case;

         Aux_Token_Kind := Kind (Data (Aux_Token));
      end loop;

      --  No Ada_Node was found relative to Token

      if Aux_Token = No_Token then
         return No_Ada_Node;
      end if;

      return Unit.Root.Lookup (Start_Sloc (Sloc_Range (Data (Aux_Token))));
   end Lookup;

   -------------------------
   -- Next_Non_Whitespace --
   -------------------------

   function Next_Non_Whitespace
     (Token : Token_Reference;
      Going : Direction)
      return Token_Reference
   is
      Result : Token_Reference := Token;

   begin
      --  Do nothing if Result <=> Token is a No_Token

      while Result /= No_Token loop
         case Going is
            when Forward => Result := Next (Result);
            when Backward => Result := Previous (Result);
         end case;

         exit when Kind (Data (Result)) /= Ada_Whitespace;
      end loop;

      if Result /= No_Token
         and then Kind (Data (Result)) /= Ada_Whitespace
      then
         --  No whitespace relative to Token
         return Result;

      else
         return No_Token;
      end if;
   end Next_Non_Whitespace;

   ----------------------
   -- To_String_Vector --
   ----------------------

   function To_String_Vector
     (Set : Defining_Name_Ordered_Set)
      return String_Vector
   is
      Vector : String_Vector;

      use Langkit_Support.Text;

   begin
      for Element of Set loop
         Vector.Append (To_UTF8 (Element.Text));
      end loop;

      return Vector;
   end To_String_Vector;

   ---------------------------
   -- Update_Parameter_Mode --
   ---------------------------

   procedure Update_Parameter_Mode
     (Parameters_Mode : in out Parameters_Mode_Map;
      Parameter       : Defining_Name'Class;
      Mode            : Ada_Mode) is
   begin
      if Parameters_Mode.Contains (Parameter) then
         Parameters_Mode.Replace (Parameter, Mode);
      else
         Parameters_Mode.Insert (Parameter, Mode);
      end if;
   end Update_Parameter_Mode;

   ---------------------------
   -- Update_Parameter_Mode --
   ---------------------------

   procedure Update_Parameter_Mode
     (Parameter_Mode : in out Ada_Mode;
      New_Mode       : Ada_Mode) is
   begin
      if New_Mode = Ada_Mode_In_Out then
         Parameter_Mode := New_Mode;
      elsif Parameter_Mode in Ada_Mode_Default | Ada_Mode_In
        and then New_Mode = Ada_Mode_Out
      then
         Parameter_Mode := Ada_Mode_In_Out;
      elsif Parameter_Mode = Ada_Mode_Out
        and then New_Mode in Ada_Mode_Default | Ada_Mode_In
      then
         Parameter_Mode := Ada_Mode_In_Out;
      end if;
   end Update_Parameter_Mode;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Mode          : Ada_Mode;
      Default_As_In : Boolean := False)
      return String is
     (case Mode is
         when Ada_Mode_Default_Range => (if Default_As_In then "in " else ""),
         when Ada_Mode_In_Range => "in ",
         when Ada_Mode_Out_Range => "out ",
         when Ada_Mode_In_Out_Range => "in out ");

   -------------
   -- Extract --
   -------------

   function Extract
     (Self : Extract_Subprogram_Strategy'Class)
      return Refactoring_Edits
   is
      Target_Statements : constant Stmt_Vector :=
        Get_Statements_To_Extract (Self.Start_Stmt, Self.End_Stmt);

      Parameter_Candidates : constant Defining_Name_Ordered_Set :=
        Self.Get_Local_Declarations;

      Parameters_To_Extract : constant Defining_Name_Ordered_Set :=
        Self.Get_Parameters_To_Extract
          (Parameter_Candidates, Target_Statements);

      Parameters_Mode : constant Parameters_Mode_Map :=
        Self.Get_Parameters_Mode (Parameters_To_Extract, Target_Statements);

      Parameters_Type : constant Parameters_Type_Map :=
        Self.Get_Parameters_Type (Parameters_To_Extract);

      Extracted_Subprogram_Name : constant String := To_String (Self.Name);

      Extracted_Subprogram_Parameters : constant String_Vector :=
        Self.Build_Extracted_Subprogram_Parameters
          (Parameters_To_Extract,
           Parameters_Mode,
           Parameters_Type);

      Extracted_Subprogram_Statements : constant String_Vector :=
        Self.Build_Extracted_Subprogram_Statements;

      Enclosing_Declarative_Part : constant Declarative_Part :=
        Get_Enclosing_Declarative_Part (Self.Start_Stmt);

      Extracted_Subprogram : constant String :=
        Self.Build_Extracted_Subprogram
          (Extracted_Subprogram_Parameters,
           Extracted_Subprogram_Statements,
           Natural (Enclosing_Declarative_Part.Sloc_Range.End_Column) - 1);

      Extracted_Subprogram_Arguments : constant String_Vector :=
        To_String_Vector (Parameters_To_Extract);

      Extracted_Subprogram_Call : constant String :=
        Self.Build_Extracted_Subprogram_Call
          (Extracted_Subprogram_Name,
           Extracted_Subprogram_Arguments);

      Insertion_Point : constant Source_Location_Range :=
        (Enclosing_Declarative_Part.Sloc_Range.End_Line,
         Enclosing_Declarative_Part.Sloc_Range.End_Line,
         1,
         1);

      Start_Line : constant Line_Number :=
        Line_Number'Min
          (Sloc_Range (Data (Self.Start_Token)).Start_Line,
           Self.Start_Stmt.Sloc_Range.Start_Line);
      End_Line   : constant Line_Number :=
        Line_Number'Max
          (Sloc_Range (Data (Self.End_Token)).End_Line,
           Self.End_Stmt.Sloc_Range.End_Line);

      Start_Column : constant Column_Number :=
        Column_Number'Min
          (Sloc_Range (Data (Self.Start_Token)).Start_Column,
           Self.Start_Stmt.Sloc_Range.Start_Column);
      End_Column   : constant Column_Number :=
        Column_Number'Max
          (Sloc_Range (Data (Self.End_Token)).End_Column,
           Self.End_Stmt.Sloc_Range.End_Column);

      Text_Edits : Text_Edit_Ordered_Set;

      Edits : Refactoring_Edits;

   begin
      Text_Edits.Insert
        ((Insertion_Point,
          To_Unbounded_String (Extracted_Subprogram)));
      Text_Edits.Insert
        (((Start_Line, End_Line, Start_Column, End_Column),
          To_Unbounded_String (Extracted_Subprogram_Call)));
      Edits.Text_Edits.Insert (Self.Unit.Get_Filename, Text_Edits);

      return Edits;
   end Extract;

   ----------------------------
   -- Get_Local_Declarations --
   ----------------------------

   function Get_Local_Declarations
     (Self : Extract_Subprogram_Strategy)
      return Defining_Name_Ordered_Set
   is
      Candidates : Defining_Name_Ordered_Set;

      procedure Declaration_Owner_Callback
        (Parent : Ada_Node;
         Stop   : in out Boolean);
      --  If Parent.Kind is in:
      --     Ada_Accept_Stmt_With_Stmts_Range
      --     Ada_For_Loop_Stmt_Range
      --     Ada_Exception_Handler_Range
      --     Ada_Extended_Return_Stmt_Range
      --  Then checks if there's a local object declaration. If so, adds it to
      --  candidates.
      --  Iteration stops when Is_Declarative_Part_Owner (Parent).

      --------------------------------
      -- Declaration_Owner_Callback --
      --------------------------------

      procedure Declaration_Owner_Callback
        (Parent : Ada_Node;
         Stop   : in out Boolean) is
      begin
         if Is_Declarative_Part_Owner (Parent) then
            Stop := True;

         else
            case Parent.Kind is
               when Ada_Accept_Stmt_With_Stmts_Range =>
                  for Param_Spec
                    of Parent.As_Accept_Stmt_With_Stmts.F_Params.
                      F_Params.F_Params
                  loop
                     for Parameter_Defining_Name of Param_Spec.F_Ids loop
                        Candidates.Insert
                          (Parameter_Defining_Name.As_Defining_Name);
                     end loop;
                  end loop;

               when Ada_For_Loop_Stmt_Range =>
                  Candidates.Insert
                    (Parent.As_For_Loop_Stmt.F_Spec.As_For_Loop_Spec.
                       F_Var_Decl.F_Id);

               when Ada_Exception_Handler_Range =>
                  Candidates.Insert
                    (Parent.As_Exception_Handler.F_Exception_Name);

               when Ada_Extended_Return_Stmt_Range =>
                  Candidates.Insert
                    (Defining_Name_List_Element
                       (Parent.As_Extended_Return_Stmt.F_Decl.F_Ids,
                        Defining_Name_List_First
                          (Parent.As_Extended_Return_Stmt.F_Decl.F_Ids)).
                         As_Defining_Name);

               when others =>
                  raise Assertion_Error;
            end case;
         end if;
      end Declaration_Owner_Callback;

   begin
      --  Find all objects declarated in:
      --  Ada_Accept_Stmt_With_Stmts_Range
      --  Ada_For_Loop_Stmt_Range
      --  Ada_Exception_Handler_Range
      --  Ada_Extended_Return_Stmt_Range

      Find_Matching_Parents
        (Self.Start_Stmt,
         Is_Declaration_Owner'Access,
         Declaration_Owner_Callback'Access);

      return Candidates;
   end Get_Local_Declarations;

   -------------------------
   -- Get_Parameters_Type --
   -------------------------

   function Get_Parameters_Type
     (Self       : Extract_Subprogram_Strategy;
      Parameters : Defining_Name_Ordered_Set)
      return Parameters_Type_Map
   is
      pragma Unreferenced (Self);

      Parameters_Base_Type_Decl : Parameters_Type_Map;
      Parameter_Base_Type_Decl  : Basic_Decl;

   begin
      for Parameter of Parameters loop
         case Parameter.P_Basic_Decl.Kind is
            when Ada_For_Loop_Var_Decl_Range =>
               Parameter_Base_Type_Decl :=
                 Parameter.P_Basic_Decl.As_For_Loop_Var_Decl.P_Defining_Name.
                   P_Expression_Type.As_Basic_Decl;

            when Ada_Exception_Handler =>
               Parameter_Base_Type_Decl := No_Exception_Handler.As_Basic_Decl;

            when Ada_Param_Spec =>
               Parameter_Base_Type_Decl :=
                 Parameter.P_Basic_Decl.Parent.As_Param_Spec.
                   F_Type_Expr.P_Designated_Type_Decl.As_Basic_Decl;

            when Ada_Extended_Return_Stmt_Object_Decl =>
               Parameter_Base_Type_Decl :=
                 Parameter.P_Basic_Decl.
                   As_Extended_Return_Stmt_Object_Decl.
                     P_Type_Expression.P_Designated_Type_Decl.As_Basic_Decl;

            when others =>
               raise Assertion_Error with "Unexpected parameter type";
         end case;

         if not Parameter_Base_Type_Decl.Is_Null
           and then Parameter_Base_Type_Decl =
                      Parameter_Base_Type_Decl.P_Universal_Int_Type
         then
            Parameters_Base_Type_Decl.Insert
              (Parameter, Parameter_Base_Type_Decl.P_Int_Type.As_Basic_Decl);

         else
            Parameters_Base_Type_Decl.Insert
              (Parameter, Parameter_Base_Type_Decl);
         end if;
      end loop;

      return Parameters_Base_Type_Decl;
   end Get_Parameters_Type;

   -------------------------------------------
   -- Build_Extracted_Subprogram_Parameters --
   -------------------------------------------

   function Build_Extracted_Subprogram_Parameters
     (Self            : Extract_Subprogram_Strategy;
      Parameters      : Defining_Name_Ordered_Set;
      Parameters_Mode : Parameters_Mode_Map;
      Parameters_Type : Parameters_Type_Map)
      return String_Vector
   is
      pragma Unreferenced (Self);

      use Langkit_Support.Text;

      function Build_Param_Spec
        (Name               : String;
         Mode               : Ada_Node_Kind_Type;
         Subtype_Indication : Basic_Decl'Class)
         return String;
      --  Build the Ada code of a Param_Spec:
      --  "Name : Mode Subtype_Indication"

      ----------------------
      -- Build_Param_Spec --
      ----------------------

      function Build_Param_Spec
        (Name               : String;
         Mode               : Ada_Node_Kind_Type;
         Subtype_Indication : Basic_Decl'Class)
         return String
      is

         Parameter_Mode : constant String := To_String (Mode);
         Parameter_Type : constant String :=
           (if Subtype_Indication.Is_Null then
               "Ada.Exceptions.Exception_Occurrence"
            else
               To_UTF8 (Subtype_Indication.P_Defining_Name.Text));

      begin
         return Name & " : " & Parameter_Mode & Parameter_Type;
      end Build_Param_Spec;

   begin
      return Extracted_Subprogram_Parameters : String_Vector do
         for Parameter of Parameters loop
            Extracted_Subprogram_Parameters.Append
              (Build_Param_Spec
                 (To_UTF8 (Parameter.Text),
                  Parameters_Mode.Element (Parameter),
                  Parameters_Type.Element (Parameter)));
         end loop;
      end return;
   end Build_Extracted_Subprogram_Parameters;

   -------------------------------
   -- Get_Parameters_To_Extract --
   -------------------------------

   function Get_Parameters_To_Extract
     (Self       : Procedure_Extractor;
      Candidates : Defining_Name_Ordered_Set;
      Statements : Stmt_Vector)
      return Defining_Name_Ordered_Set is
   begin
      return Parameters : Defining_Name_Ordered_Set do
         --  Check if they have references in Statemets. If they do, then it
         --  is needed as a parameter

         for Candidate of Candidates loop
            Find_Candidate_In_Statements :
            for Stmt of Statements loop
               if Candidate.P_Find_Refs (Stmt)'Length > 0 then
                  Parameters.Insert (Candidate);
                  exit Find_Candidate_In_Statements;
               end if;
            end loop Find_Candidate_In_Statements;
         end loop;
      end return;
   end Get_Parameters_To_Extract;

   -------------------------
   -- Get_Parameters_Mode --
   -------------------------

   overriding
   function Get_Parameters_Mode
     (Self       : Procedure_Extractor;
      Parameters : Defining_Name_Ordered_Set;
      Statements : Stmt_Vector)
      return Parameters_Mode_Map
   is
      Parameters_Mode : Parameters_Mode_Map;

   begin
      for Parameter of Parameters loop
         declare
            Mode : Ada_Mode;
            First_Reference : Boolean := True;

         begin
            Compute_Mode :
            for Stmt of Statements loop
               for Reference of Parameter.P_Find_Refs (Root => Stmt) loop
                  if First_Reference then
                     Mode :=
                       Get_Parameter_Mode_From_Reference (Ref (Reference));
                     First_Reference := False;
                  else
                     Update_Parameter_Mode
                       (Mode,
                        Get_Parameter_Mode_From_Reference (Ref (Reference)));
                  end if;
               end loop;

               --  Early exit when we the parameter mode must include out
               exit Compute_Mode
                 when not First_Reference
                      and then Mode in Ada_Mode_Out | Ada_Mode_In_Out;
            end loop Compute_Mode;

            Assert (not First_Reference);

            Update_Parameter_Mode (Parameters_Mode, Parameter, Mode);
         end;
      end loop;

      return Parameters_Mode;
   end Get_Parameters_Mode;

   -------------------------------------------
   -- Build_Extracted_Subprogram_Statements --
   -------------------------------------------

   function Build_Extracted_Subprogram_Statements
     (Self : Procedure_Extractor)
      return String_Vector
   is
      Start_Line : constant Line_Number :=
        Line_Number'Min
          (Sloc_Range (Data (Self.Start_Token)).Start_Line,
           Self.Start_Stmt.Sloc_Range.Start_Line);
      End_Line   : constant Line_Number :=
        Line_Number'Max
          (Sloc_Range (Data (Self.End_Token)).End_Line,
           Self.End_Stmt.Sloc_Range.End_Line);

      Start_Column : constant Column_Number :=
        Column_Number'Min
          (Sloc_Range (Data (Self.Start_Token)).Start_Column,
           Self.Start_Stmt.Sloc_Range.Start_Column);
      End_Column   : constant Column_Number :=
        Column_Number'Max
          (Sloc_Range (Data (Self.End_Token)).End_Column,
           Self.End_Stmt.Sloc_Range.End_Column)
        - 1;

      Real_Start_Token : constant Token_Reference :=
        Self.Unit.Lookup_Token ((Start_Line, Start_Column));
      Real_End_Token   : constant Token_Reference :=
        Self.Unit.Lookup_Token ((End_Line, End_Column));

      Lines_Number : constant Positive :=
        1 + Positive (End_Line) - Positive (Start_Line);

      Start_Index_To_Extract : array (Start_Line .. End_Line) of Natural :=
        [others => 0];
      End_Index_To_Extract : array (Start_Line .. End_Line) of Natural :=
        [others => 0];

      Minimum_Indentation : Natural := Natural'Last;

      Extracted_Subprogram_Statements : String_Vector;

      use Langkit_Support.Text;

   begin
      if Lines_Number = 1 then
         Minimum_Indentation := 0;
         Start_Index_To_Extract (Start_Line) := Text (Real_Start_Token)'First;
         End_Index_To_Extract (Start_Line) := Text (Real_End_Token)'Last;
      else
         --  Find the Minimum_Indentation, i.e., the indentation
         --  of the least indented statement or comment.

         declare
            use Ada.Strings.Wide_Wide_Fixed;
            Start_Line_First_Non_Blank : constant Positive :=
              Index_Non_Blank (Self.Unit.Get_Line (Positive (Start_Line)));
            Include_Start_Line : constant Boolean :=
              Start_Line_First_Non_Blank = Text (Real_Start_Token)'First;

            Indentation : Natural;

         begin
            for Line_Number in
              (if Include_Start_Line then Start_Line else Start_Line + 1)
              .. Self.End_Stmt.Sloc_Range.Start_Line
            loop
               declare
                  Line : constant Text_Type :=
                    Self.Unit.Get_Line (Positive (Line_Number));

               begin
                  if Line /= "" then
                     Indentation := Index_Non_Blank (Line) - Line'First;

                     if Indentation < Minimum_Indentation then
                        Minimum_Indentation := Indentation;
                     end if;
                  end if;
               end;
            end loop;

            if Include_Start_Line then
               Start_Index_To_Extract (Start_Line) :=
                 Self.Unit.Get_Line (Integer (Start_Line))'First
                   + Minimum_Indentation;
               End_Index_To_Extract (Start_Line) :=
                 Self.Unit.Get_Line (Integer (Start_Line))'Last;
            else
               Start_Index_To_Extract (Start_Line) :=
                 Text (Real_Start_Token)'First;
               End_Index_To_Extract (Start_Line) :=
                 Self.Unit.Get_Line (Integer (Start_Line))'Last;
            end if;
         end;

         for Line_Number in Start_Line + 1 .. End_Line - 1 loop
            Start_Index_To_Extract (Line_Number) :=
              Self.Unit.Get_Line (Integer (Line_Number))'First
              + Minimum_Indentation;
            End_Index_To_Extract (Line_Number) :=
              Self.Unit.Get_Line (Integer (Line_Number))'Last;
         end loop;
         Start_Index_To_Extract (End_Line) :=
           Self.Unit.Get_Line (Integer (End_Line))'First
           + Minimum_Indentation;
         End_Index_To_Extract (End_Line) := Text (Real_End_Token)'Last;
      end if;

      for Line_Number in Start_Line .. End_Line loop
         Extracted_Subprogram_Statements.Append
           (To_UTF8
              (Self.Unit.Get_Line (Integer (Line_Number))
                 (Start_Index_To_Extract (Line_Number)
                  .. End_Index_To_Extract (Line_Number))));
      end loop;

      return Extracted_Subprogram_Statements;
   end Build_Extracted_Subprogram_Statements;

   --------------------------------
   -- Build_Extracted_Subprogram --
   --------------------------------

   overriding
   function Build_Extracted_Subprogram
     (Self        : Procedure_Extractor;
      Params      : String_Vector;
      Statements  : String_Vector;
      Offset      : Natural;
      Indentation : Natural := 3)
      return String
   is
      use Ada.Characters.Latin_1;
      use Ada.Containers;

      function Padding (N : Integer) return Unbounded_String is
        ((Offset + N * Indentation) * " ");

      function Build_Procedure_Spec return String;
      --  Builds the ada code of the extracted procedure spec

      function Build_Procedure_Body return String;
--  Builds the ada code of the extracted procedure spec

      --------------------------
      -- Build_Procedure_Spec --
      --------------------------

      function Build_Procedure_Spec return String
      is
         Subp_Spec : Unbounded_String;

      begin
         if Params.Is_Empty then
            Append
              (Subp_Spec, Padding (1) & "procedure " & Self.Name & ";" & LF);

         else
            if Params.Length = 1 then
               Append
                 (Subp_Spec,
                  Padding (1) & "procedure " & Self.Name & LF
                  & Padding (1) & "  (" & Params.First_Element & ");" & LF);
            else
               Append
                 (Subp_Spec,
                  Padding (1) & "procedure " & Self.Name & LF
                  & Padding (1) & "  (" & Params.First_Element & ";" & LF);
               for J in Params.First_Index + 1 .. Params.Last_Index - 1 loop
                  Append
                    (Subp_Spec, Padding (2) & Params.Element (J) & ";" & LF);
               end loop;
               Append
                 (Subp_Spec, Padding (2) & Params.Last_Element & ");" & LF);
            end if;
         end if;

         return To_String (Subp_Spec);
      end Build_Procedure_Spec;

      --------------------------
      -- Build_Procedure_Body --
      --------------------------

      function Build_Procedure_Body return String
      is
         use Ada.Strings;
         use Ada.Strings.Fixed;

         Subp_Body : Unbounded_String;

      begin
         if Params.Is_Empty then
            Append
              (Subp_Body,
               Padding (1) & "procedure " & Self.Name & " is" & LF
               & Padding (1) & "begin" & LF);
            for Stmt of Statements loop
               if Stmt /= "" then
                  Append (Subp_Body, Padding (2) & Trim (Stmt, Right) & LF);
               else
                  Append (Subp_Body, Stmt & LF);
               end if;
            end loop;
            Append (Subp_Body, Padding (1) & "end " & Self.Name & ";" & LF);

         else
            if Params.Length = 1 then
               Append
                 (Subp_Body,
                  Padding (1) & "procedure " & Self.Name & LF
                  & Padding (1) & "  (" & Params.First_Element & ")" & " is"
                  & LF);
            else
               Append
                 (Subp_Body,
                  Padding (1) & "procedure " & Self.Name & LF
                  & Padding (1) & "  (" & Params.First_Element & ";" & LF);
               for J in Params.First_Index + 1 .. Params.Last_Index - 1 loop
                  Append
                    (Subp_Body, Padding (2) & Params.Element (J) & ";" & LF);
               end loop;
               Append
                 (Subp_Body,
                  Padding (2) & Params.Last_Element & ")" & " is" & LF);
            end if;
            Append (Subp_Body, Padding (1) & "begin" & LF);
            for Stmt of Statements loop
               if Stmt /= "" then
                  Append (Subp_Body, Padding (2) & Trim (Stmt, Right) & LF);
               else
                  Append (Subp_Body, Stmt & LF);
               end if;
            end loop;
            Append (Subp_Body, Padding (1) & "end " & Self.Name & ";" & LF);
         end if;

         return To_String (Subp_Body);
      end Build_Procedure_Body;

   begin
      return
        LF & Build_Procedure_Spec & LF & Build_Procedure_Body & LF;
   end Build_Extracted_Subprogram;

   -------------------------------------
   -- Build_Extracted_Subprogram_Call --
   -------------------------------------

   overriding
   function Build_Extracted_Subprogram_Call
     (Self       : Procedure_Extractor;
      Name       : String;
      Parameters : String_Vector)
      return String
   is
      use Ada.Containers;
      use String_Vectors;

   begin
      if Parameters.Is_Empty then
         return Name & ";";
      else
         declare
            Call_Parameters : Unbounded_String;
            C : Cursor := Parameters.First;
            L : constant Cursor := Parameters.Last;

         begin
            String_Vectors.Next (C);
            if Parameters.Length = 1 then
               Append (Call_Parameters, "(" & Parameters.First_Element & ");");
               return Name & " " & To_String (Call_Parameters);
            else
               Append (Call_Parameters, "(" & Parameters.First_Element & ", ");
               while C /= L loop
                  Append (Call_Parameters, Element (C) & ", ");
                  String_Vectors.Next (C);
               end loop;
               Append (Call_Parameters, Parameters.Last_Element & ")");
               return Name & " " & To_String (Call_Parameters) & ";";
            end if;
         end;
      end if;
   end Build_Extracted_Subprogram_Call;

   -------------------------------
   -- Get_Parameters_To_Extract --
   -------------------------------

   function Get_Parameters_To_Extract
     (Self       : Function_Extractor;
      Candidates : Defining_Name_Ordered_Set;
      Statements : Stmt_Vector)
      return Defining_Name_Ordered_Set
   is
      function All_Parameters return Defining_Name_Ordered_Set;
      --  Returns the Defining_Names of Candidates that have references in
      --  Statements.

      --------------------
      -- All_Parameters --
      --------------------

      function All_Parameters return Defining_Name_Ordered_Set is
      begin
         return Parameters : Defining_Name_Ordered_Set do
            --  Check if they have references in Statemets. If they do, then it
            --  is needed as a parameter

            for Candidate of Candidates loop
               Find_Candidate_In_Statements :
               for Stmt of Statements loop
                  if Candidate.P_Find_Refs (Stmt)'Length > 0 then
                     Parameters.Insert (Candidate);
                     exit Find_Candidate_In_Statements;
                  end if;
               end loop Find_Candidate_In_Statements;
            end loop;
         end return;
      end All_Parameters;

      Parameters_To_Extract : Defining_Name_Ordered_Set := All_Parameters;

      Assigned_Definition           : constant Defining_Name :=
        (if Statements.Last_Element.Kind in Ada_Assign_Stmt_Range then
            Statements.Last_Element.As_Assign_Stmt.F_Dest.
              P_Referenced_Decl.P_Defining_Name
         else No_Defining_Name);
      Assigned_Definition_Is_Needed : Boolean := False;

   begin
      if Assigned_Definition.Is_Null then
         return Parameters_To_Extract;
      end if;

      Find_Assigned_Definition_In_Statements :
      for Statement of Statements loop
         if Statement = Statements.Last_Element then
            if Assigned_Definition.P_Find_Refs
              (Statement.As_Assign_Stmt.F_Expr)'Length > 0
            then
               Assigned_Definition_Is_Needed := True;
            end if;

         else
            if Assigned_Definition.P_Find_Refs (Statement)'Length > 0 then
               Assigned_Definition_Is_Needed := True;
            end if;
         end if;

         exit Find_Assigned_Definition_In_Statements
           when Assigned_Definition_Is_Needed;
      end loop Find_Assigned_Definition_In_Statements;

      if not Assigned_Definition_Is_Needed then
         Parameters_To_Extract.Exclude (Assigned_Definition);
      end if;

      return Parameters_To_Extract;
   end Get_Parameters_To_Extract;

   -------------------------
   -- Get_Parameters_Mode --
   -------------------------

   overriding
   function Get_Parameters_Mode
     (Self       : Function_Extractor;
      Parameters : Defining_Name_Ordered_Set;
      Statements : Stmt_Vector)
      return Parameters_Mode_Map
   is
      Parameters_Mode : Parameters_Mode_Map;

   begin
      for Parameter of Parameters loop
         declare
            Mode            : Ada_Mode;
            First_Reference : Boolean := True;
            Subtree         : Ada_Node;

         begin
            Compute_Mode :
            for Stmt of Statements loop
               Subtree :=
                 (if Stmt = Statements.Last_Element
                    and then Stmt.Kind in Ada_Assign_Stmt_Range
                  then
                     Stmt.As_Assign_Stmt.F_Expr.As_Ada_Node
                  else
                     Stmt.As_Ada_Node);

               for Reference of Parameter.P_Find_Refs (Root => Subtree) loop
                  if First_Reference then
                     Mode :=
                       Get_Parameter_Mode_From_Reference (Ref (Reference));
                     First_Reference := False;
                  else
                     Update_Parameter_Mode
                       (Mode,
                        Get_Parameter_Mode_From_Reference (Ref (Reference)));
                  end if;
               end loop;

               --  Early exit when we the parameter mode must include out
               exit Compute_Mode
                 when not First_Reference
                      and then Mode in Ada_Mode_Out | Ada_Mode_In_Out;
            end loop Compute_Mode;

            Assert (not First_Reference);

            Update_Parameter_Mode (Parameters_Mode, Parameter, Mode);
         end;
      end loop;

      return Parameters_Mode;
   end Get_Parameters_Mode;

   -------------------------------------------
   -- Build_Extracted_Subprogram_Statements --
   -------------------------------------------

   overriding
   function Build_Extracted_Subprogram_Statements
     (Self : Function_Extractor)
      return String_Vector
   is
      use Langkit_Support.Text;

      --  Section To Extract SLOC.
      --  <Start,End>_Token can be in the middle of a statement. Therefore,
      --  find the <Start,End>_<Line,Column> by comparing the <Start/End>_Token
      --  and <Start,End>_Stmt SLOC.

      Start_Line   : constant Line_Number :=
        Line_Number'Min
          (Sloc_Range (Data (Self.Start_Token)).Start_Line,
           Self.Start_Stmt.Sloc_Range.Start_Line);
      End_Line     : constant Line_Number :=
        Line_Number'Max
          (Sloc_Range (Data (Self.End_Token)).End_Line,
           Self.End_Stmt.Sloc_Range.End_Line);
      Start_Column : constant Column_Number :=
        Column_Number'Min
          (Sloc_Range (Data (Self.Start_Token)).Start_Column,
           Self.Start_Stmt.Sloc_Range.Start_Column);
      End_Column   : constant Column_Number :=
        Column_Number'Max
          (Sloc_Range (Data (Self.End_Token)).End_Column,
           Self.End_Stmt.Sloc_Range.End_Column);

      --  Find the True_<Start,End>_Token given the <Start,End>_<Line,Column>
      --  computed above.

      True_Start_Token : constant Token_Reference :=
        Self.Unit.Lookup_Token ((Start_Line, Start_Column));
      True_End_Token   : constant Token_Reference :=
        Self.Unit.Lookup_Token ((End_Line, End_Column - 1));

      --  The section to extract can be a single line or multiple lines.
      --  If it's a single line, it can be the fully or partially extracted.
      --  If it's multiple lines, the same is applicable for the start and
      --  end line.
      --  Total_Lines is how many lines we'll extract.

      Total_Lines : constant Positive :=
        Natural (End_Line) - Natural (Start_Line) + 1;

      --  Also, the lines can have a wrong indentation, so the best
      --  we can do is find the minimum indentation and remove the leading
      --  whitespaces relative to it.

      Minimum_Indentation : Natural := Natural'Last;

      --  Exception messages

      Unexpected_End_Stmt_Kind_Exception_Message : constant String :=
        "Trying to extract a function, however, the last statement is neither "
        & "an Ada_Return_Stmt_Range nor an Ada_Assign_Stmt_Range";

      --  Result of this function, with all the extracted statements, without
      --  padding.

      Extracted_Stmts : String_Vector;

   begin
      if Total_Lines = 1 then
         --  Only one line to extract.
         --  The following algorithm assumes that there can't be any comments
         --  before Start_Stmt on this line, since comments can only appear in
         --  the end of the line, or in the whole line.

         Assert
           (Start_Line = End_Line,
            "Only one line to extract, however, Start_Line and End_Line are "
            & "different");

         Assert
           (Start_Sloc (Self.Start_Stmt.Sloc_Range)
            = (Start_Line, Start_Column),
            "Only one line to extract, but the start Source_Location of "
            & "Start_Stmt is /= than (Start_Line, Start_Column)");

         if Self.Start_Stmt = Self.End_Stmt then
            --  Only one line and one statement to extract

            case Self.Start_Stmt.Kind is
               when Ada_Return_Stmt_Range  =>
                  declare
                     Target_Line : constant Text_Type :=
                       Self.Unit.Get_Line (Positive (Start_Line));

                     Return_Stmt : constant Text_Type :=
                       Target_Line
                         (Text (True_Start_Token)'First
                          .. Text (True_End_Token)'Last);

                  begin
                     Extracted_Stmts.Append (To_UTF8 (Return_Stmt));
                  end;

               when Ada_Assign_Stmt_Range =>
                  --  There might be comments after the Assign_Stmt node.
                  --  Transform the Assign_Stmt into a Return_Stmt, and append
                  --  these comments.

                  declare
                     Target_Line : constant Text_Type :=
                       Self.Unit.Get_Line (Positive (Start_Line));

                     --  Might be empty
                     Comments    : constant Text_Type :=
                       Target_Line
                         (Target_Line'First
                            + Positive (Self.Start_Stmt.Sloc_Range.End_Column)
                            - 1
                          .. Target_Line'Last);
                     Return_Stmt : constant Text_Type :=
                       "return " & Self.Start_Stmt.As_Assign_Stmt.F_Expr.Text
                       & ";";

                  begin
                     Extracted_Stmts.Append (To_UTF8 (Return_Stmt & Comments));
                  end;

               when others =>
                  raise Assertion_Error with
                    Unexpected_End_Stmt_Kind_Exception_Message;
            end case;

         else
            --  Only one line with multiple statements to extract

            case Self.End_Stmt.Kind is
               when Ada_Return_Stmt_Range  =>
                  declare
                     Target_Line : constant Text_Type :=
                       Self.Unit.Get_Line (Positive (Start_Line));

                     Return_Stmt : constant Text_Type :=
                       Target_Line
                         (Text (True_Start_Token)'First
                          .. Text (True_End_Token)'Last);

                  begin
                     Extracted_Stmts.Append (To_UTF8 (Return_Stmt));
                  end;

               when Ada_Assign_Stmt_Range =>
                  --  There might be comments after the Assign_Stmt node.
                  --  Transform End_Stmt into a Return_Stmt, prepend the
                  --  previous statments and lastly, append any comments.

                  declare
                     Target_Line : constant Text_Type :=
                       Self.Unit.Get_Line (Positive (Start_Line));

                     Initial_Stmts : constant Text_Type :=
                       Target_Line
                         (Target_Line'First
                          + Positive
                            (Self.Start_Stmt.Sloc_Range.Start_Column)
                            - 1
                          .. Target_Line'First
                            + Positive (Self.End_Stmt.Sloc_Range.Start_Column)
                            - 1
                            - 1);

                     --  Might be empty
                     Comments    : constant Text_Type :=
                       Target_Line
                         (Target_Line'First
                            + Positive (Self.End_Stmt.Sloc_Range.End_Column)
                            - 1
                          .. Target_Line'Last);

                     Return_Stmt : constant Text_Type :=
                       "return " & Self.End_Stmt.As_Assign_Stmt.F_Expr.Text
                       & ";";

                  begin
                     Extracted_Stmts.Append
                       (To_UTF8 (Initial_Stmts & Return_Stmt & Comments));
                  end;

               when others =>
                  raise Assertion_Error with
                    Unexpected_End_Stmt_Kind_Exception_Message;
            end case;
         end if;

      else
         if Self.Start_Stmt = Self.End_Stmt then
            --  Single Stmt across multiple lines. There might be some comments
            --  before, in the middle, or after it. Impossible predict where
            --  the comments are and to know how the user wants the code to be
            --  formated, so just gather all the comments and add them before
            --  the return statements.
            --
            --  Example:
            --
            --  A :=
            --    Foo     -- Some comment about Foo
            --      (B,   -- Some comment about B
            --       C,   -- Some comment about C
            --       D);  -- Some comment about D
            --
            --  Will result in
            --
            --  -- Some comment about Foo
            --  -- Some comment about B
            --  -- Some comment about C
            --  -- Some comment about D
            --  return Foo (B, C, D);

            case Self.End_Stmt.Kind is
               when Ada_Return_Stmt_Range =>
                  declare
                     Return_Stmt : constant Text_Type :=
                       Self.End_Stmt.As_Return_Stmt.Text;

                     Aux_Token : Token_Reference := Self.Start_Token;

                  begin
                     loop
                        if Kind (Data (Aux_Token)) in Ada_Comment then
                           Extracted_Stmts.Append (To_UTF8 (Text (Aux_Token)));
                        end if;

                        exit when Aux_Token = Self.End_Token;
                        Aux_Token := Next (Aux_Token);
                     end loop;

                     Extracted_Stmts.Append (To_UTF8 (Return_Stmt));
                  end;

               when Ada_Assign_Stmt_Range =>
                  declare
                     Return_Stmt : constant Text_Type :=
                       "return " & Self.End_Stmt.As_Assign_Stmt.F_Expr.Text
                       & ";";

                     Aux_Token : Token_Reference := Self.Start_Token;

                  begin
                     loop
                        if Kind (Data (Aux_Token)) in Ada_Comment then
                           Extracted_Stmts.Append (To_UTF8 (Text (Aux_Token)));
                        end if;

                        exit when Aux_Token = Self.End_Token;
                        Aux_Token := Next (Aux_Token);
                     end loop;

                     Extracted_Stmts.Append (To_UTF8 (Return_Stmt));
                  end;

               when others =>
                  raise Assertion_Error with
                    Unexpected_End_Stmt_Kind_Exception_Message;

            end case;

         else
            --  Multiple statements accross multiple lines. There might be some
            --  comments before, in the middle, or after it.

            --  Find the Minimum_Indentation, i.e., the indentation
            --  of the least indented statement or comment.

            declare
               use Ada.Strings.Wide_Wide_Fixed;
               Start_Line_First_Non_Blank : constant Positive :=
                 Index_Non_Blank (Self.Unit.Get_Line (Positive (Start_Line)));
               Include_Start_Line : constant Boolean :=
                 Start_Line_First_Non_Blank = Text (True_Start_Token)'First;

               Indentation : Natural;

            begin
               for Line_Number in
                 (if Include_Start_Line then Start_Line else Start_Line + 1)
                   .. Self.End_Stmt.Sloc_Range.Start_Line
               loop
                  declare
                     Line : constant Text_Type :=
                       Self.Unit.Get_Line (Positive (Line_Number));

                  begin
                     if Line /= "" then
                        Indentation := Index_Non_Blank (Line) - Line'First;

                        if Indentation < Minimum_Indentation then
                           Minimum_Indentation := Indentation;
                        end if;
                     end if;
                  end;
               end loop;
            end;

            --  Add all statements from the first line up to the line with
            --  the last statement

            Extracted_Stmts.Append
              (To_UTF8
                 (Self.Unit.Get_Line (Positive (Start_Line))
                    (Text (True_Start_Token)'First
                     .. Self.Unit.Get_Line (Positive (Start_Line))'Last)));

            for Line_Number in
              Start_Line + 1 .. Self.End_Stmt.Sloc_Range.Start_Line - 1
            loop
               --  Note that if this is an empty line,
               --  Start_Index_To_Extract (Line_Number) >
               --  End_Index_To_Extract (Line_Number).
               --  Since Start_Index_To_Extract and End_Index_To_Extract
               --  are use to create a string slice, this is not a problem.
               --  It will result in an empty string, which is the same as
               --  an empty line.

               Extracted_Stmts.Append
                 (To_UTF8
                    (Self.Unit.Get_Line (Positive (Line_Number))
                       (Self.Unit.Get_Line (Positive (Line_Number))'First
                        + Minimum_Indentation
                        .. Self.Unit.Get_Line
                             (Positive (Line_Number))'Last)));
            end loop;

            --  Add the lines with the last statement

            case Self.End_Stmt.Kind is
               when Ada_Return_Stmt_Range =>
                  for Line_Number in
                    Self.End_Stmt.Sloc_Range.Start_Line
                      .. Self.End_Stmt.Sloc_Range.End_Line
                  loop
                     if Line_Number = Self.End_Stmt.Sloc_Range.End_Line
                       and then Line_Number = End_Line
                     then
                        Extracted_Stmts.Append
                          (To_UTF8
                             (Self.Unit.Get_Line (Positive (Line_Number))
                                (Self.Unit.Get_Line
                                   (Positive (Line_Number))'First
                                 + Minimum_Indentation
                                 .. Text (Self.End_Token)'Last)));
                     else
                        Extracted_Stmts.Append
                          (To_UTF8
                             (Self.Unit.Get_Line (Positive (Line_Number))
                               (Self.Unit.Get_Line
                                  (Positive (Line_Number))'First
                                   + Minimum_Indentation
                                   .. Self.Unit.Get_Line
                                        (Positive (Start_Line))'Last)));
                     end if;
                  end loop;

               when Ada_Assign_Stmt_Range =>
                  declare
                     Dest             : constant Ada_Node'Class :=
                       Self.End_Stmt.As_Assign_Stmt.F_Dest;
                     Dest_Line        : constant Text_Type :=
                       Self.Unit.Get_Line
                         (Positive (Dest.Sloc_Range.Start_Line));
                     Dest_Start_Index : constant Positive := Dest.Text'First;

                     New_Dest_Text  : constant String :=
                       To_UTF8
                         (Dest_Line (Dest_Line'First + Minimum_Indentation
                                     .. Dest_Start_Index - 1)
                          & "return ");

                     Expr                  : constant Ada_Node'Class :=
                       Self.End_Stmt.As_Assign_Stmt.F_Expr;
                     Expr_Line             : constant Text_Type :=
                       Self.Unit.Get_Line
                         (Positive (Expr.Sloc_Range.Start_Line));
                     Expr_Line_Start_Index : constant Positive :=
                       Expr.Text'First;
                     Expr_Line_End_Index   : constant Positive :=
                       (if Expr.Sloc_Range.Start_Line = End_Line then
                           Text (Self.End_Token)'Last
                        else
                           Expr_Line'Last);
                     New_Expr_Text         : constant String :=
                       To_UTF8 (Expr_Line (Expr_Line_Start_Index
                                           .. Expr_Line_End_Index));

                  begin
                     --  Add any comments found between Dest and Expr

                     declare
                        Aux_Token : Token_Reference := Dest.Token_Start;

                     begin
                        loop
                           if Kind (Data (Aux_Token)) in Ada_Comment then
                              Extracted_Stmts.Append
                                (To_UTF8 (Text (Aux_Token)));
                           end if;

                           exit when Aux_Token = Expr.Token_Start;
                           Aux_Token := Next (Aux_Token);
                        end loop;
                     end;

                     Extracted_Stmts.Append (New_Dest_Text & New_Expr_Text);

                     for Line_Number in
                       Expr.Sloc_Range.Start_Line + 1
                         .. Expr.Sloc_Range.End_Line
                     loop
                        Extracted_Stmts.Append
                          (To_UTF8
                             (Self.Unit.Get_Line (Positive (Line_Number))
                               (Self.Unit.Get_Line
                                  (Positive (Line_Number))'First
                                + Minimum_Indentation
                                .. Self.Unit.Get_Line
                                     (Positive (Line_Number))'Last)));
                     end loop;
                  end;

               when others =>
                  raise Assertion_Error with
                    Unexpected_End_Stmt_Kind_Exception_Message;

            end case;

            --  Add any lines after the last statement line.
            --  Consider these as whole line comments.

            for Line_Number in
              Self.End_Stmt.Sloc_Range.End_Line + 1 .. End_Line
            loop
               Extracted_Stmts.Append
                 (To_UTF8
                    (Self.Unit.Get_Line (Positive (Line_Number))
                      (Self.Unit.Get_Line (Positive (Line_Number))'First
                       + Minimum_Indentation
                       .. Self.Unit.Get_Line (Positive (Line_Number))'Last)));
            end loop;
         end if;
      end if;

      return Extracted_Stmts;
   end Build_Extracted_Subprogram_Statements;

   --------------------------------
   -- Build_Extracted_Subprogram --
   --------------------------------

   overriding
   function Build_Extracted_Subprogram
     (Self        : Function_Extractor;
      Params      : String_Vector;
      Statements  : String_Vector;
      Offset      : Natural;
      Indentation : Natural := 3)
      return String
   is
      use Ada.Characters.Latin_1;
      use String_Vectors;
      use Ada.Containers;

      function Get_Return_Type return String
        with Pre => not Self.End_Stmt.Is_Null
                    and then Self.End_Stmt.Kind in Ada_Assign_Stmt_Range
                                                   | Ada_Return_Stmt_Range;
      ---------------------
      -- Get_Return_Type --
      ---------------------

      function Get_Return_Type return String
      is
         subtype Stmt_Kind_Type is Ada_Node_Kind_Type
           with Static_Predicate => Stmt_Kind_Type in Ada_Assign_Stmt_Range
                                                      | Ada_Return_Stmt_Range;

         Stmt_Kind : constant Stmt_Kind_Type := Self.End_Stmt.Kind;

         use Langkit_Support.Text;

      begin
         case Stmt_Kind is
            when Ada_Assign_Stmt_Range =>
               return To_UTF8 (Self.End_Stmt.As_Assign_Stmt.F_Dest.
                                 P_Referenced_Defining_Name.
                                   P_Basic_Decl.P_Type_Expression.Text);

            when Ada_Return_Stmt_Range =>
               return To_UTF8 (Self.End_Stmt.P_Parent_Basic_Decl.
                                 As_Base_Subp_Body.F_Subp_Spec.F_Subp_Returns.
                                   Text);
         end case;
      end Get_Return_Type;

      Return_Type : constant String := Get_Return_Type;

      function Padding (N : Integer) return Unbounded_String is
        ((Offset + N * Indentation) * " ");

      --  Analyse the

      function Build_Function_Spec return String;
      --  Builds the ada code of the extracted function spec

      function Build_Function_Body return String;
      --   Builds the ada code of the extracted function body

      -------------------------
      -- Build_Function_Spec --
      -------------------------

      function Build_Function_Spec return String
      is
         Subp_Spec : Unbounded_String;

      begin
         if Params.Is_Empty then
            Append
              (Subp_Spec,
               Padding (1) & "function " & Self.Name & " return "
               & Return_Type & ";" & LF);

         else
            if Params.Length = 1 then
               Append
                 (Subp_Spec,
                  Padding (1) & "function " & Self.Name & LF
                  & Padding (1) & "  (" & Params.First_Element & ")" & LF
                  & Padding (2) & "return " & Return_Type & ";" & LF);

            else
               Append
                 (Subp_Spec,
                  Padding (1) & "function " & Self.Name & LF
                  & Padding (1) & "  (" & Params.First_Element & ";" & LF);
               for J in Params.First_Index + 1 .. Params.Last_Index - 1 loop
                  Append
                    (Subp_Spec,
                     Padding (2) & Params.Element (J) & ";" & LF);
               end loop;
               Append
                 (Subp_Spec,
                  Padding (2) & Params.Last_Element & ")" & LF
                  & Padding (2) & "return " & Return_Type & ";" & LF);
            end if;
         end if;

         return To_String (Subp_Spec);
      end Build_Function_Spec;

      -------------------------
      -- Build_Function_Body --
      -------------------------

      function Build_Function_Body return String
      is
         use Ada.Strings;
         use Ada.Strings.Fixed;

         Function_Body : Unbounded_String;

      begin
         if Params.Is_Empty then
            Append
              (Function_Body,
               Padding (1) & "function " & Self.Name & " return "
               & Return_Type & " is" & LF);
            Append
              (Function_Body,
               Padding (1) & "begin" & LF);
            for Statement of Statements loop
               if Statement /= "" then
                  Append
                    (Function_Body,
                     Padding (2) & Trim (Statement, Right) & LF);
               else
                  Append (Function_Body, Statement & LF);
               end if;
            end loop;
            Append
              (Function_Body,
               Padding (1) & "end " & Self.Name & ";" & LF);

         else
            if Params.Length = 1 then
               Append
                 (Function_Body,
                  Padding (1) & "function " & Self.Name & LF
                  & Padding (1) & "  (" & Params.First_Element & ")" & LF
                  & Padding (2) & "return " & Return_Type & " is" & LF);

            else
               Append
                 (Function_Body,
                  Padding (1) & "function " & Self.Name & LF
                  & Padding (1) & "  (" & Params.First_Element & ";" & LF);
               for J in Params.First_Index + 1 .. Params.Last_Index - 1 loop
                  Append
                    (Function_Body,
                     Padding (2) & Params.Element (J) & ";" & LF);
               end loop;
               Append
                 (Function_Body,
                  Padding (2) & Params.Last_Element & ")" & LF
                  & Padding (2) & "return " & Return_Type & " is" & LF);
            end if;

            Append
              (Function_Body,
               Padding (1) & "begin" & LF);
            for Statement of Statements loop
               if Statement /= "" then
                  Append
                    (Function_Body,
                     Padding (2) & Trim (Statement, Right) & LF);
               else
                  Append (Function_Body, Statement & LF);
               end if;
            end loop;
            Append
              (Function_Body,
               Padding (1) & "end " & Self.Name & ";" & LF);
         end if;

         return To_String (Function_Body);
      end Build_Function_Body;

   begin
      return LF & Build_Function_Spec & LF & Build_Function_Body & LF;
   end Build_Extracted_Subprogram;

   -------------------------------------
   -- Build_Extracted_Subprogram_Call --
   -------------------------------------

   overriding
   function Build_Extracted_Subprogram_Call
     (Self       : Function_Extractor;
      Name       : String;
      Parameters : String_Vector)
      return String
   is
      use Langkit_Support.Text;

      Reduced_Function_Arguments : Unbounded_String;

      Destination : constant String :=
        (if Get_Statements_To_Extract
              (Self.Start_Stmt, Self.End_Stmt).Last_Element.Kind
           in Ada_Return_Stmt_Range
         then
            ""
         else
            To_UTF8
              (Get_Statements_To_Extract (Self.Start_Stmt, Self.End_Stmt).
                 Last_Element.As_Assign_Stmt.F_Dest.Text));

   begin
      if Destination /= "" then
         if Parameters.Is_Empty then
            return Destination & " := " & Name & ";";
         else
            Append
               (Reduced_Function_Arguments,
               "(" & Parameters.First_Element);
            for J in
               Parameters.First_Index + 1
                  .. Parameters.Last_Index
            loop
               Append
                  (Reduced_Function_Arguments,
                  ", " & Parameters.Element (J));
            end loop;
            Append
               (Reduced_Function_Arguments, ")");

            return Destination & " := " & Name & " "
               & To_String (Reduced_Function_Arguments) & ";";
         end if;

      else
         if Parameters.Is_Empty then
            return "return " & Name & ";";

         else
            Append
               (Reduced_Function_Arguments, "(" & Parameters.First_Element);
            for J in Parameters.First_Index + 1 .. Parameters.Last_Index loop
               Append
                 (Reduced_Function_Arguments, ", " & Parameters.Element (J));
            end loop;
            Append (Reduced_Function_Arguments, ")");

            return
              "return " & Name & " "
              & To_String (Reduced_Function_Arguments) & ";";
         end if;
      end if;
   end Build_Extracted_Subprogram_Call;

   -------------------------------------
   -- Is_Extract_Subprogram_Available --
   -------------------------------------

   function Is_Extract_Subprogram_Available
     (Unit                       : Analysis_Unit;
      Section_To_Extract         : Source_Location_Range;
      Available_Subprogram_Kinds : out Available_Subprogram_Kinds_Type)
      return Boolean
   is
      Start_Token : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'
             (Line   => Section_To_Extract.Start_Line,
              Column => Section_To_Extract.Start_Column));

      End_Token   : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'
             (Line   => Section_To_Extract.End_Line,
              Column => Section_To_Extract.End_Column));

      Start_Node : constant Ada_Node := Lookup (Unit, Start_Token, Forward);
      End_Node   : constant Ada_Node := Lookup (Unit, End_Token, Backward);

      Start_Stmt : Stmt := No_Stmt;
      End_Stmt   : Stmt := No_Stmt;
      It_Stmt    : Stmt := No_Stmt;
      --  Used to iterate from Start_Stmt to End_Stmt

      Aux : Ada_Node := No_Ada_Node;

      function Is_Stmt (Node : Ada_Node'Class) return Boolean is
        (not Node.Is_Null and then Node.Kind in Ada_Stmt);

      procedure Is_Stmt_Callback (Parent : Ada_Node; Stop : in out Boolean);
      --  When Parent is a Stmt, stops the search and sets Aux to Parent

      ----------------------
      -- Is_Stmt_Callback --
      ----------------------

      procedure Is_Stmt_Callback (Parent : Ada_Node; Stop : in out Boolean) is
      begin
         Stop := True;
         Aux := Parent;
      end Is_Stmt_Callback;

   begin
      if Start_Node.Is_Null or else End_Node.Is_Null then
         Available_Subprogram_Kinds := [others => False];
         return False;
      end if;

      if Start_Node.Kind not in Ada_Stmt then
         Find_Matching_Parents
           (Start_Node, Is_Stmt'Access, Is_Stmt_Callback'Access);

         if Aux.Is_Null then
            Available_Subprogram_Kinds := [others => False];
            return False;
         end if;

         Start_Stmt := Aux.As_Stmt;

      else
         Start_Stmt := Start_Node.As_Stmt;
      end if;

      if Start_Stmt.Kind in Ada_Base_Loop_Stmt
        and then Start_Stmt.Parent.Kind in Ada_Named_Stmt_Range
      then
         Start_Stmt := Start_Stmt.Parent.As_Stmt;
      end if;

      Aux := No_Ada_Node;

      if End_Node.Kind not in Ada_Stmt then
         Find_Matching_Parents
           (End_Node, Is_Stmt'Access, Is_Stmt_Callback'Access);

         if Aux.Is_Null then
            Available_Subprogram_Kinds := [others => False];
            return False;
         end if;

         End_Stmt := Aux.As_Stmt;

      else
         End_Stmt := End_Node.As_Stmt;
      end if;

      if End_Stmt.Kind in Ada_Base_Loop_Stmt
        and then End_Stmt.Parent.Kind in Ada_Named_Stmt_Range
      then
         End_Stmt := End_Stmt.Parent.As_Stmt;
      end if;

      if Start_Stmt.Parent /= End_Stmt.Parent then
         Available_Subprogram_Kinds := [others => False];
         return False;
      end if;

      --  Check for return statements that cannot be extracted.
      --  A return statement can only be extracted iff it is in a nested
      --  function body.
      --  End_Stmt is allowed to be a return statement since we can extract
      --  a function.

      if End_Stmt.Kind not in Ada_Return_Stmt_Range then
         It_Stmt := Start_Stmt;
         loop
            if It_Stmt.Kind in Ada_Return_Stmt_Range then
               --  From the if statement before this one, we know that if
               --  It_Stmt = End_Stmt => It_Stmt.Kind not in
               --                          Ada_Return_Stmt_Range.
               --
               --  If any sibling node in [Start_Stmt, End_Stmt[ is a return
               --  statement node, we can immediately conclude that we cannot
               --  extract.

               Available_Subprogram_Kinds := [others => False];
               return False;

            else
               declare
                  Found_Forbidden_Return_Stmt : Boolean := False;
                  --  Flag to be filled by the Look_Up_Forbidden_Return_Stmts
                  --  traverse function.

                  function Look_Up_Forbidden_Return_Stmts
                    (Node : Ada_Node'Class)
                     return Visit_Status;
                  --  Traverse function that looks for return statements that
                  --  cannot be extracted. A return statement can only be
                  --  extracted iff it is in a nested function body.

                  ------------------------------------
                  -- Look_Up_Forbidden_Return_Stmts --
                  ------------------------------------

                  function Look_Up_Forbidden_Return_Stmts
                    (Node : Ada_Node'Class)
                     return Visit_Status is
                  begin
                     if Node.Is_Null then
                        return Over;

                     elsif Node.Kind in Ada_Return_Stmt_Range then
                        Found_Forbidden_Return_Stmt := True;
                        return Stop;

                     elsif Node.Kind in Ada_Subp_Body_Range
                       and then Node.As_Subp_Body.F_Subp_Spec.F_Subp_Kind in
                         Ada_Subp_Kind_Function_Range
                     then
                        --  Ignore any child return statements of this
                        --  function body since they are safe to be
                        --  extracted.
                        return Over;

                     else
                        return Into;
                     end if;
                  end Look_Up_Forbidden_Return_Stmts;

               begin
                  It_Stmt.Traverse (Look_Up_Forbidden_Return_Stmts'Access);

                  if Found_Forbidden_Return_Stmt then
                     Available_Subprogram_Kinds := [others => False];
                     return False;
                  end if;
               end;
            end if;

            exit when It_Stmt = End_Stmt;

            --  It_Stmt.Next_Sibling should never be null, because it should be
            --  End_Stmt before going beyond the last statement. However, with
            --  invalid code, it might be null. In that case, the refactoring
            --  is not available.
            if It_Stmt.Next_Sibling.Is_Null then
               return False;

            else
               It_Stmt := It_Stmt.Next_Sibling.As_Stmt;
            end if;
         end loop;
      end if;

      --  Check for exit statements that cannot be extracted
      It_Stmt := Start_Stmt;
      loop
         if It_Stmt.Kind in Ada_Exit_Stmt then
            --  If any sibling node from Start_Stmt to End_Stmt is an Exit_Stmt
            --  node we can immediately conclude that its correspondent
            --  loop statement will not be extracted, therefore, neither does
            --  this exit statement.
            Available_Subprogram_Kinds := [others => False];
            return False;

         else
            declare
               Found_Forbidden_Exit_Stmt : Boolean := False;
               --  Flag to be filled by the Look_Up_Forbidden_Exit_Stmts
               --  traverse function.

               package Defining_Name_Indefinite_Hashed_Sets is new
                 Ada.Containers.Indefinite_Hashed_Sets
                   (Element_Type        => Defining_Name'Class,
                    Hash                => Defining_Name_Hash,
                    Equivalent_Elements => "=",
                    "="                 => "=");

               subtype Named_Loop_Indefinite_Hashed_Set is
                 Defining_Name_Indefinite_Hashed_Sets.Set;

               Found_Loop : Boolean := It_Stmt.Kind in Ada_Base_Loop_Stmt;
               --  This variable will state if a loop node was found. If True
               --  before finding an unnamed exit statement, then the exit
               --  statement is allowed to be removed.
               --  This is not applicable for named exit statements since
               --  the correspondent named loop needs to be analysed.

               Named_Loop_Stmts : Named_Loop_Indefinite_Hashed_Set;
               --  A named exit statement can only be extracted iff its
               --  corresponding named loop statement was found first. This set
               --  will hold all the named loop statement that are to be
               --  extracted.

               function Look_Up_Forbidden_Exit_Stmts
                 (Node : Ada_Node'Class)
                  return Visit_Status;
               --  Traverse function that looks for exit statments that
               --  cannot be extracted.
               --  An exit statement can only be extract iff it's correspondent
               --  loop statement is also extracted.

               ----------------------------------
               -- Look_Up_Forbidden_Exit_Stmts --
               ----------------------------------

               function Look_Up_Forbidden_Exit_Stmts
                 (Node : Ada_Node'Class)
                  return Visit_Status
               is
                  Loop_Name                     : Name := No_Name;
                  Loop_Referenced_Defining_Name : Defining_Name :=
                    No_Defining_Name;

               begin
                  if Node.Is_Null then
                        return Over;

                  elsif Node.Kind in Ada_Base_Loop_Stmt then
                     Found_Loop := True;

                     if Node.Parent.Kind in Ada_Named_Stmt_Range then
                        Named_Loop_Stmts.Include
                          (Node.Parent.As_Named_Stmt.F_Decl.F_Name);
                     end if;

                  elsif Node.Kind in Ada_Exit_Stmt then
                     --  Determine if it's a named exit statement or not
                     Loop_Name := Node.As_Exit_Stmt.F_Loop_Name;
                     Loop_Referenced_Defining_Name :=
                       (if not Loop_Name.Is_Null then
                          Loop_Name.P_Referenced_Defining_Name
                        else
                          No_Defining_Name);

                     --  If no loop was found yet, then it does not matter
                     --  if it's a named exit statement or not - it cannot
                     --  be extracted.
                     --  Othewise, check if it's named and if its
                     --  corresponding named loop is going to be extracted too.
                     if not Found_Loop
                       or else (not Loop_Referenced_Defining_Name.Is_Null
                                and then not Named_Loop_Stmts.Contains
                                               (Loop_Referenced_Defining_Name))
                     then
                        Found_Forbidden_Exit_Stmt := True;
                        return Stop;
                     end if;
                  end if;
                  return Into;
               end Look_Up_Forbidden_Exit_Stmts;

            begin
               if It_Stmt.Kind in Ada_Base_Loop_Stmt
                 and then It_Stmt.Parent.Kind in Ada_Named_Stmt_Range
               then
                  Named_Loop_Stmts.Include
                    (It_Stmt.Parent.As_Named_Stmt.F_Decl.F_Name);
               end if;

               It_Stmt.Traverse (Look_Up_Forbidden_Exit_Stmts'Access);

               if Found_Forbidden_Exit_Stmt then
                  Available_Subprogram_Kinds := [others => False];
                  return False;
               end if;
            end;
         end if;

         exit when It_Stmt = End_Stmt;

         --  It_Stmt.Next_Sibling should never be null, because it should be
         --  End_Stmt before going beyond the last statement. However, with
         --  invalid code, it might be null. In that case, the refactoring is
         --  not available.
         if It_Stmt.Next_Sibling.Is_Null then
            return False;

         else
            It_Stmt := It_Stmt.Next_Sibling.As_Stmt;
         end if;
      end loop;

      --  Check what kind of subprograms can be extracted
      if End_Stmt.Kind in Ada_Return_Stmt_Range then
         Available_Subprogram_Kinds :=
           [Ada_Subp_Kind_Function  => True,
            Ada_Subp_Kind_Procedure => False];

      elsif End_Stmt.Kind in Ada_Assign_Stmt_Range then
         Available_Subprogram_Kinds :=
           [Ada_Subp_Kind_Function  => True,
            Ada_Subp_Kind_Procedure => True];

      else
         Available_Subprogram_Kinds :=
           [Ada_Subp_Kind_Function  => False,
            Ada_Subp_Kind_Procedure => True];
      end if;

      return True;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Is_Refactoring_Tool_Available_Default_Error_Message (Tool_Name));
         return False;
   end Is_Extract_Subprogram_Available;

   ---------------------------------------
   -- Default_Extracted_Subprogram_Name --
   ---------------------------------------

   function Default_Extracted_Subprogram_Name
     (Unit            : Analysis_Unit;
      Location        : Source_Location)
      return Unbounded_String
   is
      use Ada.Containers;
      use Ada.Strings;
      use Langkit_Support.Text;

      Node                     : constant Ada_Node :=
        Unit.Root.Lookup (Location);
      Nearest_Declarative_Part : constant Declarative_Part :=
        Get_Enclosing_Declarative_Part (Node);

      function Hash (US : Unbounded_String) return Hash_Type is
        (Ada.Strings.Hash_Case_Insensitive (To_String (US)));

      package Unbounded_String_Hash_Sets is new
        Ada.Containers.Hashed_Sets
          (Element_Type => Unbounded_String,
           Hash => Hash,
           Equivalent_Elements => "=",
           "=" => "=");

      subtype Unbounded_String_Ordered_Set is Unbounded_String_Hash_Sets.Set;

      --  Subprogram names in Nearest_Declarative_Part
      Subprogram_Names : Unbounded_String_Ordered_Set;

      --  Default name when there are no collisions
      Target_Name     : constant Unbounded_String :=
        To_Unbounded_String ("Extracted");
      Aux_Target_Name : Unbounded_String := Target_Name;
      Counter         : Positive := 1;

   begin
      --  Find all subprogram names in Nearest_Declarative_Part
      for Decl of Nearest_Declarative_Part.F_Decls loop
         if Decl.Kind in Ada_Basic_Decl
           and then Is_Subprogram (Decl.As_Basic_Decl)
         then
            Subprogram_Names.Include
              (To_Unbounded_String
                 (To_UTF8 (Decl.As_Basic_Decl.P_Defining_Name.Text)));
         end if;
      end loop;

      --  The extracted subprogram name will have an index if the default
      --  name (Target_Name) already exists. The full signature is not
      --  compared, only the name.

      while Subprogram_Names.Contains (Aux_Target_Name) loop
         Aux_Target_Name :=
           Target_Name
           & ("_" & Trim (To_Unbounded_String (Counter'Image), Both));
         Counter := Counter + 1;
      end loop;

      return Aux_Target_Name;
   end Default_Extracted_Subprogram_Name;

   ---------------------------------
   -- Create_Subprogram_Extractor --
   ---------------------------------

   function Create_Subprogram_Extractor
     (Unit               : Analysis_Unit;
      Section_To_Extract : Source_Location_Range;
      Subprogram_Kind    : Ada_Subp_Kind;
      Subprogram_Name    : Unbounded_String)
      return Subprogram_Extractor
   is
      Start_Token : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'
             (Section_To_Extract.Start_Line,
              Section_To_Extract.Start_Column));
      End_Token   : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'
             (Section_To_Extract.End_Line,
              Section_To_Extract.End_Column));

      Start_Node : constant Ada_Node := Lookup (Unit, Start_Token, Forward);
      End_Node   : constant Ada_Node := Lookup (Unit, End_Token, Backward);

      --  Section_To_Extract might no be complete, for instance, the
      --  Start_Line/Column might be in a whitespace.
      --  Therefore True_Start_Token and True_End_Token must be computed using
      --  Start_Token and End_Token.
      True_Start_Token : Token_Reference;
      True_End_Token   : Token_Reference;

      --  Statements to be extracted
      Start_Stmt : Stmt := No_Stmt;
      End_Stmt   : Stmt := No_Stmt;

      --  Used to find the first parent Stmt node of Start_Node and End_Node
      Aux : Ada_Node := No_Ada_Node;

      function Is_Stmt (Node : Ada_Node'Class) return Boolean is
        (not Node.Is_Null and then Node.Kind in Ada_Stmt);

      procedure Is_Stmt_Callback (Parent : Ada_Node; Stop : in out Boolean);
      --  When Parent is a Stmt, stops the search and sets Aux to Parent

      ----------------------
      -- Is_Stmt_Callback --
      ----------------------

      procedure Is_Stmt_Callback (Parent : Ada_Node; Stop : in out Boolean) is
      begin
         Stop := True;
         Aux := Parent;
      end Is_Stmt_Callback;

   begin
      --  Find the first Stmt node that is a parent of Start_Node

      Aux := Start_Node;

      if Start_Node.Kind not in Ada_Stmt then
         Find_Matching_Parents
           (Start_Node, Is_Stmt'Access, Is_Stmt_Callback'Access);
      end if;

      Start_Stmt := Aux.As_Stmt;

      if Start_Stmt.Kind in Ada_Base_Loop_Stmt
        and then Start_Stmt.Parent.Kind in Ada_Named_Stmt_Range
      then
         Start_Stmt := Start_Stmt.Parent.As_Stmt;
      end if;

      --  Find the first Stmt node that is a parent of End_Node

      Aux := End_Node;

      if End_Node.Kind not in Ada_Stmt then
         Find_Matching_Parents
           (End_Node, Is_Stmt'Access, Is_Stmt_Callback'Access);
      end if;

      End_Stmt := Aux.As_Stmt;

      if End_Stmt.Kind in Ada_Base_Loop_Stmt
        and then End_Stmt.Parent.Kind in Ada_Named_Stmt_Range
      then
         End_Stmt := End_Stmt.Parent.As_Stmt;
      end if;

      --  If Start_Token is an Ada_Whitespace, then find the next token that is
      --  not an Ada_Whitespace. That will be the first guess for the
      --  True_Start_Token.

      True_Start_Token :=
        (if Kind (Data (Start_Token)) = Ada_Whitespace then
            Next_Non_Whitespace (Start_Token, Forward)
         else
            Start_Token);

      --  If True_Start_Token comes after the first token of Start_Stmt
      --  then update it.

      True_Start_Token :=
        (if Start_Stmt.Compare
              (Start_Sloc (Sloc_Range (Data (True_Start_Token))))
            = Before
         then
            True_Start_Token
         else
            Unit.Lookup_Token
              (Source_Location'
                 (Start_Stmt.Sloc_Range.Start_Line,
                  Start_Stmt.Sloc_Range.Start_Column)));

      --  Do the exact same algorithm to find the True_End_Token, but in the
      --  opposite search direction.

      True_End_Token :=
        (if Kind (Data (End_Token)) = Ada_Whitespace then
           Next_Non_Whitespace (End_Token, Backward)
         else
           End_Token);

      True_End_Token :=
        (if End_Stmt.Compare (End_Sloc (Sloc_Range (Data (True_End_Token))))
            = After
         then
            True_End_Token
         else
            Unit.Lookup_Token
              (Source_Location'
                 (End_Stmt.Sloc_Range.End_Line,
                  End_Stmt.Sloc_Range.End_Column - 1)));

      return Subprogram_Extractor'
        (Unit,
         True_Start_Token,
         True_End_Token,
         Start_Stmt,
         End_Stmt,
         Subprogram_Name,
         Subprogram_Kind);
   end Create_Subprogram_Extractor;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Subprogram_Extractor;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits is
   begin
      case Self.Kind is
         when Ada_Subp_Kind_Function_Range =>
            return
              Function_Extractor'
                (Self.Unit,
                 Self.Start_Token,
                 Self.End_Token,
                 Self.Start_Stmt,
                 Self.End_Stmt,
                 Self.Name).Extract;
         when Ada_Subp_Kind_Procedure_Range =>
            return
              Procedure_Extractor'
                (Self.Unit,
                 Self.Start_Token,
                 Self.End_Token,
                 Self.Start_Stmt,
                 Self.End_Stmt,
                 Self.Name).Extract;
      end case;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));
         return No_Refactoring_Edits;
   end Refactor;

end Laltools.Refactor.Extract_Subprogram;
