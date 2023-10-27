------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
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

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Common; use Libadalang.Common;

with TGen.LAL_Utils; use TGen.LAL_Utils;
with TGen.Strings;   use TGen.Strings;

package body TGen.Wrappers is

   --  When dealing with preconditions, we end up with a DNF formula. We
   --  represent it as a list of conjunctive clauses, i.e. conjunction of
   --  literals where each literal is an atom or its negation.

   type Literal is record
      Neg   : Boolean;
      Value : Unbounded_String;
   end record;

   function "<" (L, R : Literal) return Boolean is
     (if L.Neg = R.Neg
      then L.Value < R.Value
      else L.Neg < R.Neg);

   function "=" (L, R : Literal) return Boolean is
     (L.Neg = R.Neg and then L.Value = R.Value);

   package Literal_Sets is new Ada.Containers.Ordered_Sets (Literal);
   package Literal_Set_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Literal_Sets.Set,
      "="          => Literal_Sets."=");
   subtype Formula_Type is Literal_Set_Vectors.Vector;

   type Polarity_Kind is new Boolean;
   Dnf_Kind     : constant Polarity_Kind := True;
   Dnf_Neg_Kind : constant Polarity_Kind := False;

   function Dnf
     (E        : Expr;
      Polarity : Polarity_Kind := Dnf_Kind) return Formula_Type;
   --  Return the DNF for a precondition expression. The Polarity parameter
   --  is there to propagate the negation to literals (and puts (not E) in
   --  DNF).

   function Subprogram_Wrapper_Specification
     (Subprogram : Basic_Decl) return String;
   --  Return the code for the wrapper specification

   procedure Write_Wrapper
     (F_Body            : File_Type;
      Call_To_Orig_Subp : String;
      Formula           : Formula_Type);

   ---------
   -- Dnf --
   ---------

   function Dnf
     (E        : Expr;
      Polarity : Polarity_Kind := Dnf_Kind) return Formula_Type
   is
      Result : Formula_Type;
   begin
      case Kind (E) is
         when Ada_Un_Op =>

            case Kind (E.As_Un_Op.F_Op) is
               when Ada_Op_Not =>
                  return Dnf (E.As_Un_Op.F_Expr, not Polarity);

               when others =>
                  null;
            end case;

         when Ada_Bin_Op =>
            declare
               As_Bin_Op : constant Bin_Op := E.As_Bin_Op;
               Ada_Op    : constant Ada_Node_Kind_Type :=
                 Kind (As_Bin_Op.F_Op);
            begin
               case Ada_Op is
                  when Ada_Op_And_Then
                     | Ada_Op_And
                     | Ada_Op_Or_Else
                     | Ada_Op_Or
                     =>
                     declare
                        Dnf_Left : constant Formula_Type :=
                          Dnf (As_Bin_Op.F_Left, Polarity);
                        Dnf_Right : constant Formula_Type :=
                          Dnf (As_Bin_Op.F_Right, Polarity);
                     begin
                        --  Depending on the polarity: we are either building
                        --  a DNF, or a CNF, distribute respectively the "and"
                        --  operator, or the "or" operator.

                        if (Polarity = Dnf_Kind
                            and then Ada_Op in Ada_Op_And_Then | Ada_Op_And)
                          or else
                            (Polarity = Dnf_Neg_Kind
                             and then Ada_Op in Ada_Op_Or_Else | Ada_Op_Or)
                        then
                           for Clause_Left of Dnf_Left loop
                              for Clause_Right of Dnf_Right loop
                                 Result.Append
                                   (Clause_Left.Union (Clause_Right));
                              end loop;
                           end loop;

                        else
                           Result.Append (Dnf_Left);
                           Result.Append (Dnf_Right);
                        end if;
                        return Result;
                     end;
                  when others =>
                     null;
               end case;
            end;

         when Ada_Paren_Expr =>
            return Dnf (E.As_Paren_Expr.F_Expr, Polarity);

         when others =>
            null;
      end case;

      Result.Append
        (Literal_Sets.To_Set
           (Literal'
                (Neg   => not Boolean (Polarity),
                 Value => To_Unbounded_String (+E.Text))));
      return Result;
   end Dnf;

   -------------------
   -- Write_Wrapper --
   -------------------

   procedure Write_Wrapper
     (F_Body            : File_Type;
      Call_To_Orig_Subp : String;
      Formula           : Formula_Type)
   is
      use type Literal_Sets.Set;
      Indent : constant String := "      ";
   begin
      for Literal_Set of Formula loop
         if Literal_Set = Formula.First_Element then
            Put (F_Body, Indent & "if ");
         else
            Put (F_Body, Indent & "elsif ");
         end if;

         for Literal of Literal_Set loop
            if Literal /= Literal_Set.First_Element then
               Put (F_Body, "and then ");
            end if;
            if Literal.Neg then
               Put (F_Body, "not ");
            end if;
            Put (F_Body, +Literal.Value & " ");
         end loop;
         Put_Line (F_Body, " then");

         Put_Line (F_Body, Indent & "   " & Call_To_Orig_Subp);
      end loop;
   end Write_Wrapper;

   --------------------------------------
   -- Subprogram_Wrapper_Specification --
   --------------------------------------

   function Subprogram_Wrapper_Specification
     (Subprogram : Basic_Decl) return String
   is
      Result          : Unbounded_String;
      Designated_Decl : Basic_Decl := Subprogram;
   begin
      --  If this is a generic instantiation, grab the synthetic designated
      --  declaration.

      if Kind (Subprogram) = Ada_Generic_Subp_Instantiation then
         Designated_Decl :=
           Subprogram.As_Generic_Subp_Instantiation.P_Designated_Generic_Decl;
      end if;

      declare
         Subp_Spec : constant Base_Subp_Spec :=
           Designated_Decl.P_Subp_Spec_Or_Null;
         Subp_Kind : constant Ada_Subp_Kind :=
           Subp_Spec.As_Subp_Spec.F_Subp_Kind;
         Params    : constant Param_Spec_Array := Subp_Spec.P_Params;
      begin
         case Subp_Kind is
            when Ada_Subp_Kind_Function =>
               Append (Result, "function ");
            when Ada_Subp_Kind_Procedure =>
               Append (Result, "procedure ");
         end case;

         Append (Result, +Subprogram.P_Defining_Name.F_Name.Text);

         --  Then, deal with the parameters

         if Params'Length /= 0 then
            Append (Result, "(");
            for I in Params'Range loop
               declare
                  Param : constant Param_Spec := Params (I);
                  Ids   : constant Defining_Name_List := Param.F_Ids;
                  Cur   : Positive := Ids.Defining_Name_List_First;
               begin
                  if I /= Params'First then
                     Append (Result, "; ");
                  end if;
                  while Ids.Defining_Name_List_Has_Element (Cur) loop
                     if Cur /= Ids.Defining_Name_List_First then
                        Append (Result, ", ");
                     end if;
                     Append
                       (Result,
                        +Ids.Defining_Name_List_Element (Cur).F_Name.Text);
                     Cur := Ids.Defining_Name_List_Next (Cur);
                  end loop;
                  Append (Result, " : ");
                  Append (Result, +Param.F_Mode.Text & " ");
                  Append
                    (Result,
                     +Param.F_Type_Expr
                     .P_Designated_Type_Decl
                     .P_Fully_Qualified_Name);
               end;
            end loop;
            Append (Result, ")");
         end if;

         --  Then, deal with the return type expression if this is a function

         if Subp_Kind = Ada_Subp_Kind_Function then
            Append (Result, " return ");
            Append
              (Result,
               +Subp_Spec.P_Returns.P_Designated_Type_Decl.F_Name.Text);
         end if;

         return To_String (Result);
      end;
   end Subprogram_Wrapper_Specification;

   -------------------------------------
   -- Generate_Wrapper_For_Subprogram --
   -------------------------------------

   procedure Generate_Wrapper_For_Subprogram
     (F_Spec, F_Body     : File_Type;
      Subprogram         : Basic_Decl;
      Templates_Root_Dir : String)
   is
      FQN_Name : constant String :=
        +Subprogram.P_Defining_Name.P_Fully_Qualified_Name;
      F_Name   : constant Text_Type :=
        Subprogram.P_Defining_Name.F_Name.Text;

      Subp_Spec : constant Base_Subp_Spec := Subprogram.P_Subp_Spec_Or_Null;
      Subp_Kind : constant Ada_Subp_Kind := Subp_Spec.As_Subp_Spec.F_Subp_Kind;
      Params    : constant Param_Spec_Array := Subp_Spec.P_Params;

      Subp_Spec_String : constant String :=
        Subprogram_Wrapper_Specification (Subprogram);
      --  String representation of the wrapper specification, e.g.
      --  "procedure Foo (I : Integer)".

      Call_To_User_Subp : Unbounded_String;
      --  Call to the original user subprogram

   begin
      --  Compute the call to the original function

      case Subp_Kind is
         when Ada_Subp_Kind_Function =>
            Append (Call_To_User_Subp, "return ");
         when Ada_Subp_Kind_Procedure =>
            null;
      end case;

      Append (Call_To_User_Subp, FQN_Name);

      if Params'Size > 0 then
         Append (Call_To_User_Subp, " (");
      end if;

      for I in Params'Range loop
         declare
            Ids : constant Defining_Name_List := Params (I).F_Ids;
            Cur : Positive := Defining_Name_List_First (Ids);
         begin
            while Defining_Name_List_Has_Element (Ids, Cur) loop
               Append
                 (Call_To_User_Subp,
                  +Defining_Name_List_Element (Ids, Cur).Text);
               Cur := Defining_Name_List_Next (Ids, Cur);
               if Defining_Name_List_Has_Element (Ids, Cur) then
                  Append (Call_To_User_Subp, ", ");
               end if;
            end loop;
         end;
         if I /= Params'Last then
            Append (Call_To_User_Subp, ", ");
         end if;
      end loop;
      if Params'Size > 0 then
         Append (Call_To_User_Subp, ");");
      end if;

      --  Declare the wrapper specification in the spec package

      Put_Line (F_Spec, Subp_Spec_String & ";");

      --  ... and in the body

      Put_Line (F_Body, "   " & Subp_Spec_String & " is");
      Put_Line (F_Body, "   begin");

      --  Check whether there is a precondition attached to this subprogram

      if Subprogram.P_Has_Aspect (+String'("Pre")) then
         declare
            --  Get the formula for the precondition

            F : constant Formula_Type :=
              Dnf (Subprogram.P_Get_Aspect_Spec_Expr (+String'("Pre")));
         begin
            --  Then, write the wrapper

            Write_Wrapper
              (F_Body            => F_Body,
               Call_To_Orig_Subp => +Call_To_User_Subp,
               Formula           => F);

            --  Don't forget to write the last else case, which is the
            --  Precondition_Error case.

            Put_Line (F_Body, "      else");
            Put_Line (F_Body, "         raise TGen.Precondition_Error;");
            Put_Line (F_Body, "      end if;");
         end;
      else
         Put_Line (F_Body, "      " & (+Call_To_User_Subp));
      end if;
      Put_Line (F_Body, "   end " & (+F_Name) & ";");
      New_Line (F_Body);

   end Generate_Wrapper_For_Subprogram;

end TGen.Wrappers;
