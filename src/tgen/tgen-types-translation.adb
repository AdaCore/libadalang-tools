------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Exceptions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text;

with Libadalang.Common;    use Libadalang.Common;
--  with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;

package body TGen.Types.Translation is
   use LAL;

   package Text renames Langkit_Support.Text;

   function "+" (Str : String) return Unbounded_String is
     (To_Unbounded_String (Str));

   function Translate_Int_Decl
     (Decl : Base_Type_Decl) return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Int_Type;

   function Translate_Enum_Decl
     (Decl : Base_Type_Decl; Root_Enum_Decl : Base_Type_Decl)
      return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Enum_Type;

      ------------------------
      -- Translate_Int_Decl --
      ------------------------

   function Translate_Int_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   is
      Rang : constant Discrete_Range := Decl.P_Discrete_Range;

      Max : constant Integer :=
        Integer'Value (High_Bound (Rang).P_Eval_As_Int.Image);
      --  Since Decl is a static subtype, its bounds are also static
      --  expressions according to RM 4.9(26/2).
   begin
      if Is_Null (Low_Bound (Rang)) then
         return
           (Success => True,
            Res     =>
              new Mod_Int_Typ'
                (Name => Decl.P_Defining_Name, Mod_Value => Max));
      else
         declare
            Min : constant Integer :=
              Integer'Value (Low_Bound (Rang).P_Eval_As_Int.Image);
         begin
            return
              (Success => True,
               Res     =>
                 new Signed_Int_Typ'
                   (Name => Decl.P_Defining_Name,
                    Rang => (Min => Min, Max => Max)));
         end;
      end if;
   end Translate_Int_Decl;

   -------------------------
   -- Translate_Enum_Decl --
   -------------------------

   function Translate_Enum_Decl
     (Decl : Base_Type_Decl; Root_Enum_Decl : Base_Type_Decl)
      return Translation_Result
   is
      Enum_Lits : Enum_Literal_Maps.Map;

      Index : Natural := 0;

      Max : constant Natural :=
        Natural'Value (High_Bound (Decl.P_Discrete_Range).P_Eval_As_Int.Image);
      Min : constant Natural :=
        Natural'Value (Low_Bound (Decl.P_Discrete_Range).P_Eval_As_Int.Image);

   begin
      for Literal of Root_Enum_Decl.As_Type_Decl.F_Type_Def.As_Enum_Type_Def
        .F_Enum_Literals
      loop
         Enum_Lits.Insert (Index, Literal.F_Name);
         Index := Index + 1;
      end loop;

      for Pos in Enum_Lits.First_Key .. Min - 1 loop
         Enum_Lits.Delete (Pos);
      end loop;

      for Pos in Max + 1 .. Enum_Lits.Last_Key loop
         Enum_Lits.Delete (Pos);
      end loop;

      return
        (Success => True,
         Res     =>
           new Other_Enum_Typ'
             (Name => Decl.P_Defining_Name, Literals => Enum_Lits.Copy));
   end Translate_Enum_Decl;

   function Translate (N : LAL.Type_Expr) return Translation_Result is
      Type_Decl_Node : Base_Type_Decl;
      Root_Type      : Base_Type_Decl;
   begin
      if Kind (N) in Ada_Anonymous_Type then
         return
           (Success     => False,
            Diagnostics => +"Anonymous types not supported yet");
      end if;

      Type_Decl_Node :=
        N.As_Subtype_Indication.P_Designated_Type_Decl.P_Full_View;
      Root_Type := Type_Decl_Node.P_Root_Type;

      if not Type_Decl_Node.P_Is_Static_Decl then
         return
           (Success     => False,
            Diagnostics => +"Non static subtypes not supported yet");
      end if;

      if Type_Decl_Node.P_Is_Int_Type then
         return Translate_Int_Decl (Type_Decl_Node);
      elsif P_Is_Derived_Type
          (Node       => Type_Decl_Node,
           Other_Type => Type_Decl_Node.P_Bool_Type.As_Base_Type_Decl)
      then
         return
           (Success => True,
            Res     => new Bool_Typ'(Name => Type_Decl_Node.P_Defining_Name));
      elsif Type_Decl_Node.P_Is_Enum_Type then
         declare
            Root_Type_Name : constant String :=
              Text.Image (Root_Type.P_Unique_Identifying_Name);
         begin
            if Root_Type_Name = "standard.character"
              or else Root_Type_Name = "standard.wide_character"
              or else Root_Type_Name = "standard.wide_wide_character"
            then
               return
                 (Success => True,
                  Res     =>
                    new Char_Typ'(Name => Type_Decl_Node.P_Defining_Name));
            else
               return Translate_Enum_Decl (Type_Decl_Node, Root_Type);
            end if;
         end;
      end if;

      return (Success => False, Diagnostics => +"Unknown type kind");

   exception
      when Exc : Property_Error =>
         return
           (Success     => False,
            Diagnostics =>
              +"Error translating " & N.Image & " : " &
              Ada.Exceptions.Exception_Message (Exc));

   end Translate;

end TGen.Types.Translation;
