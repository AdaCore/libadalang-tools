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

   function "<" (L, R : Literal) return Boolean
   is (if L.Neg = R.Neg then L.Value < R.Value else L.Neg < R.Neg);

   function "=" (L, R : Literal) return Boolean
   is (L.Neg = R.Neg and then L.Value = R.Value);

   package Literal_Sets is new Ada.Containers.Ordered_Sets (Literal);
   package Literal_Set_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Literal_Sets.Set,
        "="          => Literal_Sets."=");
   subtype Formula_Type is Literal_Set_Vectors.Vector;

   type Polarity_Kind is new Boolean;
   Dnf_Kind     : constant Polarity_Kind := True;
   Dnf_Neg_Kind : constant Polarity_Kind := False;

   function Dnf
     (E : Expr; Polarity : Polarity_Kind := Dnf_Kind) return Formula_Type;
   --  Return the DNF for a precondition expression. The Polarity parameter
   --  is there to propagate the negation to literals (and puts (not E) in
   --  DNF).

   function Subprogram_Wrapper_Specification (F : Function_Typ) return String;
   --  Return the code for the wrapper specification

   procedure Write_Wrapper
     (F_Body : File_Type; Call_To_Orig_Subp : String; Formula : Formula_Type);

   function Subp_Kind (F : Function_Typ) return Ada_Subp_Kind
   is (if F.Ret_Typ = null
       then Ada_Subp_Kind_Procedure
       else Ada_Subp_Kind_Function);

   ---------
   -- Dnf --
   ---------

   function Dnf
     (E : Expr; Polarity : Polarity_Kind := Dnf_Kind) return Formula_Type
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
                        Dnf_Left  : constant Formula_Type :=
                          Dnf (As_Bin_Op.F_Left, Polarity);
                        Dnf_Right : constant Formula_Type :=
                          Dnf (As_Bin_Op.F_Right, Polarity);
                     begin
                        --  Depending on the polarity: we are either building
                        --  a DNF, or a CNF, distribute respectively the "and"
                        --  operator, or the "or" operator.

                        if (Polarity = Dnf_Kind
                            and then Ada_Op in Ada_Op_And_Then | Ada_Op_And)
                          or else (Polarity = Dnf_Neg_Kind
                                   and then Ada_Op
                                            in Ada_Op_Or_Else | Ada_Op_Or)
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

            --  Do not unwrap the Paren_Expr if is is legally required by the
            --  parenthesized expression, such as if-expressions or quantified
            --  expressions.

            if E.As_Paren_Expr.F_Expr.Kind
               not in Ada_Cond_Expr | Ada_Quantified_Expr | Ada_Decl_Expr
            then
               return Dnf (E.As_Paren_Expr.F_Expr, Polarity);
            end if;

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
     (F_Body : File_Type; Call_To_Orig_Subp : String; Formula : Formula_Type)
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

   function Subprogram_Wrapper_Specification (F : Function_Typ) return String
   is
      Kind   : constant Ada_Subp_Kind := Subp_Kind (F);
      Result : Unbounded_String;
   begin
      declare
      begin
         case Kind is
            when Ada_Subp_Kind_Procedure =>
               Append (Result, "procedure ");

            when Ada_Subp_Kind_Function =>
               Append (Result, "function ");
         end case;

         Append (Result, F.Simple_Name);

         --  Then, deal with the parameters

         if not F.Param_Order.Is_Empty then
            Append (Result, "(");
            for Param_Name of F.Param_Order loop
               declare
                  Param_Type : constant Typ_Access :=
                    F.Component_Types.Element (Param_Name);
               begin
                  Append (Result, Param_Name);
                  Append (Result, " : ");
                  Append
                    (Result, Image (F.Param_Modes.Element (Param_Name)) & " ");
                  Append
                    (Result,
                     "TGen.TGen_Std."
                     & Param_Type.all.FQN (No_Std => True)
                     & " ");
                  if Param_Name /= F.Param_Order.Last_Element then
                     Append (Result, " ; ");
                  end if;
               end;
            end loop;
            Append (Result, ")");
         end if;

         --  Then, deal with the return type expression if this is a function

         if Kind = Ada_Subp_Kind_Function then
            Append (Result, " return ");
            Append
              (Result, "TGen.TGen_Std." & F.Ret_Typ.all.FQN (No_Std => True));
         end if;

         return To_String (Result);
      end;
   end Subprogram_Wrapper_Specification;

   -------------------------------------
   -- Generate_Wrapper_For_Subprogram --
   -------------------------------------

   procedure Generate_Wrapper_For_Subprogram
     (F_Spec, F_Body     : File_Type;
      Subprogram         : Function_Typ;
      Precond            : Ada_Node;
      Templates_Root_Dir : String)
   is
      use type Ada.Containers.Count_Type;

      Subp_Spec_String : constant String :=
        Subprogram_Wrapper_Specification (Subprogram);
      --  String representation of the wrapper specification, e.g.
      --  "procedure Foo (I : Integer)".

      Call_To_User_Subp : Unbounded_String;
      --  Call to the original user subprogram

      Local_Package_Name : Ada_Qualified_Name;
      --  Name of the subprogram in its compilation unit. This is its fully
      --  qualified name, from which the compilation unit's fully qualified
      --  name has been removed.

   begin
      --  Compute local name. Do not take into account the last element as this
      --  is its hash.

      for I in
        Subprogram.Last_Comp_Unit_Idx + 1 .. Subprogram.Name.Last_Index - 1
      loop
         Local_Package_Name.Append (Subprogram.Name.Element (I));
      end loop;

      --  Compute the call to the original function

      case Subp_Kind (Subprogram) is
         when Ada_Subp_Kind_Function =>
            Append (Call_To_User_Subp, "return ");

         when Ada_Subp_Kind_Procedure =>
            null;
      end case;

      Append
        (Call_To_User_Subp,
         Source_Package_Renaming & "." & To_Ada (Local_Package_Name));

      if Subprogram.Param_Order.Length > 0 then
         Append (Call_To_User_Subp, " (");
      end if;

      for Param_Name of Subprogram.Param_Order loop
         Append (Call_To_User_Subp, Param_Name);
         if Param_Name /= Subprogram.Param_Order.Last_Element then
            Append (Call_To_User_Subp, ", ");
         end if;
      end loop;
      if Subprogram.Param_Order.Length > 0 then
         Append (Call_To_User_Subp, ");");
      end if;

      --  Declare the wrapper specification in the spec package

      Put_Line (F_Spec, Subp_Spec_String & ";");

      --  ... and in the body

      Put_Line (F_Body, "   " & Subp_Spec_String & " is");
      Put_Line (F_Body, "   begin");

      --  Check whether there is a precondition attached to this subprogram

      if not Precond.Is_Null then
         declare
            --  Get the formula for the precondition

            F : constant Formula_Type := Dnf (Precond.As_Expr);
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
      Put_Line (F_Body, "   end " & Subprogram.Simple_Name & ";");
      New_Line (F_Body);

   end Generate_Wrapper_For_Subprogram;

end TGen.Wrappers;
