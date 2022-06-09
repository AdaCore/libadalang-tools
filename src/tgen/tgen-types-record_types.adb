------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Unchecked_Deallocation;

with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with TGen.Gen_Strategies_Utils; use TGen.Gen_Strategies_Utils;
with TGen.Types.Array_Types;    use TGen.Types.Array_Types;

package body TGen.Types.Record_Types is

   LF : constant String := [1 => ASCII.LF];

   Pad : constant Unbounded_String := 3 * ' ';

   function PP_Variant
     (Var : Variant_Part_Acc; Padding : Natural := 0) return Unbounded_String;

   procedure Fill_Components
     (Self :        Variant_Part; Constraints : Disc_Value_Map;
      Res  : in out Component_Maps.Map);
   --  Fill Res with the list of components in Self that are present given
   --  a map of discriminant constraints.

   -----------
   -- Image --
   -----------

   function Image (Self : Record_Typ) return String is
     (Image_Internal (Self, 0));

   --------------------
   -- Image_Internal --
   --------------------

   function Image_Internal
     (Self : Record_Typ; Padding : Natural := 0) return String
   is
      use Component_Maps;
      Str : Unbounded_String := To_Unbounded_String (Typ (Self).Image);
      Current_Component : Cursor;
   begin
      if Self.Component_Types.Is_Empty then
         Str := Str & ": null record";
      else
         Str               := Str & ": record" & LF;
         Current_Component := Self.Component_Types.First;
         while Has_Element (Current_Component) loop
            Str :=
              Str & (Padding + 1) * Pad &
              (+Key (Current_Component)) & " : ";
            if Element (Current_Component).Get.Kind in Record_Typ_Range then
               Str :=
                 Str &
                 String'
                   (As_Record_Typ (Element (Current_Component)).Image_Internal
                      (Padding + 1)) &
                 LF;
            else
               Str := Str & Element (Current_Component).Get.Image & LF;
            end if;
            Next (Current_Component);
         end loop;
         Str := Str & "end record";
      end if;
      return To_String (Str);
   end Image_Internal;

   ------------------
   -- Free_Variant --
   ------------------

   procedure Free_Variant (Var : in out Variant_Part_Acc) is
      use Variant_Choice_Lists;
      procedure Free is new Ada.Unchecked_Deallocation
        (Variant_Part, Variant_Part_Acc);

      procedure Destroy_Var_Choice (Var_Choice : in out Variant_Choice);

      ------------------------
      -- Destroy_Var_Choice --
      ------------------------

      procedure Destroy_Var_Choice (Var_Choice : in out Variant_Choice) is
      begin
         if Var_Choice.Variant /= null then
            Free_Variant (Var_Choice.Variant);
         end if;
      end Destroy_Var_Choice;
   begin
      if Var /= null then
         declare
            Cur : Cursor := Var.Variant_Choices.First;
         begin
            while Has_Element (Cur) loop
               Var.Variant_Choices.Update_Element
               (Cur, Destroy_Var_Choice'Access);
               Next (Cur);
            end loop;
            Free (Var);
         end;
      end if;
   end Free_Variant;

   -------------------
   -- Clone_Variant --
   -------------------

   function Clone (Var : Variant_Part_Acc) return Variant_Part_Acc is
      Res : Variant_Part_Acc;
   begin
      if Var = null then
         return null;
      end if;
      Res            := new Variant_Part;
      Res.Discr_Name := Var.Discr_Name;
      for Choice of Var.Variant_Choices loop
         Res.Variant_Choices.Append
           (Variant_Choice'
              (Alt_Set      => Choice.Alt_Set.Copy,
               Components   => Choice.Components.Copy,
               Variant      => Clone (Choice.Variant)));
      end loop;
      return Res;
   end Clone;

   ---------------------------
   -- Constraints_Respected --
   ---------------------------

   function Constraints_Respected
     (Self : Discriminated_Record_Typ; Discriminant_Values : Disc_Value_Map)
      return Boolean
   is
      use Discriminant_Constraint_Maps;
      use Disc_Value_Maps;
      use Ada.Numerics.Big_Numbers.Big_Integers;

      Constraint_Cur : Discriminant_Constraint_Maps.Cursor;
      Value_Cur      : Disc_Value_Maps.Cursor;
   begin
      if not Self.Constrained then
         return True;
      end if;

      Constraint_Cur := Self.Discriminant_Constraint.First;
      while Has_Element (Constraint_Cur) loop
         if Element (Constraint_Cur).Kind = Static then
            Value_Cur := Discriminant_Values.Find (Key (Constraint_Cur));
            if Has_Element (Value_Cur)
              and then Element (Value_Cur) /= Element (Constraint_Cur).Int_Val
            then
               return False;
            end if;
         end if;
         Next (Constraint_Cur);
      end loop;
      return True;
   end Constraints_Respected;

   ---------------------
   -- Fill_Components --
   ---------------------

   function Components
     (Self : Discriminated_Record_Typ; Discriminant_Values : Disc_Value_Map)
      return Component_Maps.Map
   is
      Res : Component_Maps.Map := Self.Component_Types.Copy;
   begin
      if Self.Variant /= null then
         Fill_Components (Self.Variant.all, Discriminant_Values, Res);
      end if;
      return Res;
   end Components;

   ---------------------
   -- Fill_Components --
   ---------------------

   procedure Fill_Components
     (Self        : Variant_Part;
      Constraints : Disc_Value_Map;
      Res         : in out Component_Maps.Map)
   is
      Disc_Val_Cur : constant Disc_Value_Maps.Cursor :=
        Constraints.Find (Self.Discr_Name);
      Discr_Val : Big_Int.Big_Integer;

      use Big_Int;
   begin
      if Disc_Value_Maps.Has_Element (Disc_Val_Cur) then
         Discr_Val := Disc_Value_Maps.Element (Disc_Val_Cur);
      end if;
      for Choice of Self.Variant_Choices loop
         declare
            Choice_Matches : Boolean := False;
            Comp_Cur : Component_Maps.Cursor := Choice.Components.First;
         begin
            for Interval_Set of Choice.Alt_Set loop
               if Discr_Val >= Interval_Set.Min
                 and then Discr_Val <= Interval_Set.Max
               then
                  Choice_Matches := True;
               end if;
            end loop;
            if Choice_Matches then
               while Component_Maps.Has_Element (Comp_Cur) loop
                  Res.Insert
                    (Component_Maps.Key (Comp_Cur),
                     Component_Maps.Element (Comp_Cur));
                  Component_Maps.Next (Comp_Cur);
               end loop;
               if Choice.Variant /= null then
                  Fill_Components (Choice.Variant.all, Constraints, Res);
               end if;
            end if;
            exit when Choice_Matches;
         end;
      end loop;
   end Fill_Components;

   ----------------
   -- PP_Variant --
   ----------------

   function PP_Variant
     (Var : Variant_Part_Acc; Padding : Natural := 0) return Unbounded_String
   is
      Res      : Unbounded_String := (Padding * Pad) & "case ";
      Comp_Cur : Component_Maps.Cursor;
   begin
      Res := Res & (+Var.Discr_Name) & " is" & LF;
      for Var_Choice of Var.Variant_Choices loop
         Res := Res & (Padding + 2) * Pad & "when ";
         for Alt of Var_Choice.Alt_Set loop
            Res :=
               Res & Big_Int.To_String (Alt.Min) & " .. "
               & Big_Int.To_String (Alt.Max);
            if Alt /= Var_Choice.Alt_Set.Last_Element then
               Res := Res & " | ";
            end if;
         end loop;
         Res := Res & " => " & LF;
         Comp_Cur := Var_Choice.Components.First;
         while Component_Maps.Has_Element (Comp_Cur) loop
            Res :=
              Res & (Padding + 3) * Pad &
              (+Component_Maps.Key (Comp_Cur)) & " : ";
            if Component_Maps.Element (Comp_Cur).Get.Kind in Record_Typ_Range
            then
               Res :=
                 Res &
                 String'
                   (As_Record_Typ (Component_Maps.Element (Comp_Cur))
                      .Image_Internal
                      (Padding + 3));
            else
               Res := Res & Component_Maps.Element (Comp_Cur).Get.Image;
            end if;
            Component_Maps.Next (Comp_Cur);
            Res := Res & LF;
         end loop;
         if Var_Choice.Variant /= null then
            Res := Res & PP_Variant (Var_Choice.Variant, Padding + 3);
         end if;
      end loop;
      Res := Res & (Padding + 1) * Pad & "end case" & LF;
      return Res;
   end PP_Variant;

   -----------
   -- Image --
   -----------

   function Image (Self : Discriminated_Record_Typ) return String is
     (Image_Internal (Self, 0));

   --------------------
   -- Image_Internal --
   --------------------

   function Image_Internal
     (Self : Discriminated_Record_Typ; Padding : Natural := 0) return String
   is
      use Component_Maps;
      Str : Unbounded_String := To_Unbounded_String (Typ (Self).Image);
      Current_Component : Component_Maps.Cursor;
   begin
      --  First display the discriminants in line
      Str :=
        Str & ": " & (if Self.Mutable then "" else "non ") &
        "mutable record (";
      Current_Component := Self.Discriminant_Types.First;
      loop
         exit when not Has_Element (Current_Component);
         Str :=
           Str & (+Key (Current_Component)) & ": " &
           Element (Current_Component).Get.Image;
         Next (Current_Component);
         exit when not Has_Element (Current_Component);
         Str := Str & "; ";
      end loop;
      Str := Str & ")" & LF;
      if Self.Component_Types.Is_Empty and Self.Variant = null then
         Str := Str & Padding * Pad & " no components" & LF;
      else
         if not Self.Component_Types.Is_Empty then
            Current_Component := Self.Component_Types.First;
            while Has_Element (Current_Component) loop
               Str :=
                 Str & (Padding + 1) * Pad &
                 (+Key (Current_Component)) & " : ";
               if Element (Current_Component).Get.Kind in Record_Typ_Range then
                  Str :=
                    Str &
                    String'
                      (As_Record_Typ (Element (Current_Component))
                         .Image_Internal
                         (Padding + 1));
               else
                  Str := Str & Element (Current_Component).Get.Image & LF;
               end if;
               Next (Current_Component);
            end loop;
         end if;

         if Self.Variant /= null then
            Str :=
              Str & (Padding + 1) * Pad &
              PP_Variant (Self.Variant, Padding + 1);
         end if;
      end if;
      Str := Str & "end record";
      return To_String (Str);
   end Image_Internal;

   ------------------
   -- Free_Content --
   ------------------

   procedure Free_Content (Self : in out Discriminated_Record_Typ) is
   begin
      if Self.Variant /= null then
         Free_Variant (Self.Variant);
      end if;
   end Free_Content;

   function "="
     (L : Static_Strategy_Type'Class;
      R : Static_Strategy_Type'Class)
      return Boolean;

   function "="
     (L : Static_Strategy_Type'Class;
      R : Static_Strategy_Type'Class)
      return Boolean
   is
      pragma Unreferenced (L);
      pragma Unreferenced (R);
   begin
      return False;
   end "=";
   --  TODO: Implement this properly

   --  Static generation

   package Strategy_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Static_Strategy_Type'Class,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   subtype Strategy_Map is Strategy_Maps.Map;

   type Record_Static_Strategy_Type is new Static_Strategy_Type with
      record
         T : SP.Ref;
         Component_Strats : Strategy_Map;
         Generate : access function
           (T                : Record_Typ'Class;
            Component_Strats : in out Strategy_Map;
            Disc_Values      : Disc_Value_Map) return Static_Value'Class;
      end record;

   overriding function Generate_Static_Value
     (S           : in out Record_Static_Strategy_Type;
      Disc_Values : Disc_Value_Map) return Static_Value'Class;

   ---------------------------
   -- Generate_Static_Value --
   ---------------------------

   function Generate_Static_Value
     (S           : in out Record_Static_Strategy_Type;
      Disc_Values : Disc_Value_Map) return Static_Value'Class
   is
      T : constant Typ'Class := S.T.Get;
   begin
      return
        S.Generate (Record_Typ (T), S.Component_Strats, Disc_Values);
   end Generate_Static_Value;

   ------------------------
   -- Get_All_Components --
   ------------------------

   function Get_All_Components
     (Self : Discriminated_Record_Typ) return Component_Map
   is
      Res : Component_Map := Self.Component_Types;

      procedure Get_All_Components_Rec
        (Variant_Part : Variant_Part_Acc);

      procedure Get_All_Components_Rec
        (Variant_Part : Variant_Part_Acc)
      is
      begin
         if Variant_Part /= null then
            for Choice of Variant_Part.Variant_Choices loop
               for Comp_Cursor in Choice.Components.Iterate loop
                  declare
                     use Component_Maps;
                     Comp_Name : constant Unbounded_Text_Type :=
                       Key (Comp_Cursor);
                     Comp_Type : constant SP.Ref := Element (Comp_Cursor);
                  begin
                     Res.Insert (Comp_Name, Comp_Type);
                  end;
               end loop;
               Get_All_Components_Rec (Choice.Variant);
            end loop;
         end if;
      end Get_All_Components_Rec;
   begin
      Get_All_Components_Rec (Self.Variant);
      return Res;
   end Get_All_Components;

   function Generate_Record_Typ
     (Self        : Record_Typ'Class;
      Comp_Strats : in out Strategy_Map;
      Disc_Values : Disc_Value_Map) return Static_Value'Class;

   -------------------------
   -- Generate_Record_Typ --
   -------------------------

   function Generate_Record_Typ
     (Self        : Record_Typ'Class;
      Comp_Strats : in out Strategy_Map;
      Disc_Values : Disc_Value_Map) return Static_Value'Class
   is
      Res : Unbounded_String;
      use Component_Maps;
   begin
      for Comp in Self.Component_Types.Iterate loop
         declare
            Comp_Name : constant Unbounded_Text_Type := Key (Comp);

            procedure Generate_Val
              (Comp_Name  : Unbounded_Text_Type;
               Comp_Strat : in out Static_Strategy_Type'Class);

            procedure Generate_Val
              (Comp_Name  : Unbounded_Text_Type;
               Comp_Strat : in out Static_Strategy_Type'Class)
            is
               pragma Unreferenced (Comp_Name);
            begin
               Append
                 (Res,
                  Comp_Strat.Generate_Static_Value (Disc_Values).To_String);
            end Generate_Val;
         begin
            Append (Res, +Comp_Name);
            Append (Res, " => ");

            --  Generate a value

            Comp_Strats.Update_Element
              (Comp_Strats.Find (Comp_Name), Generate_Val'Access);

            Append (Res, ", ");
         end;
      end loop;
      Res := Remove_Trailing_Comma_And_Spaces (Res);
      return Base_Static_Value'(Value => Res);
   end Generate_Record_Typ;

   function Pick_Samples_For_Disc
     (Variant : Variant_Part_Acc; Disc_Name : Unbounded_Text_Type)
      return Alternatives_Set_Vector;
   --  Returns a list of samples for the discriminant name, recurring through
   --  all the variant parts and checking whether the discriminant is present
   --  as a control value.
   --
   --  type A (I : Integer) is record with
   --     case I is
   --       when 0      => J : Integer;
   --       when others => null;
   --     end case;
   --  end record;
   --
   --  Would return there {[0:0]}, {[Integer'First:-1], [1:Integer'Last]} if
   --  we call it with Disc_Name = I.
   --
   --  If this record contains discriminated records whose discriminants are
   --  this record discriminants, we won't try to pick sample in this
   --  sub-record. TODO: we maybe should.

   ---------------------------
   -- Pick_Samples_For_Disc --
   ---------------------------

   function Pick_Samples_For_Disc
     (Variant : Variant_Part_Acc; Disc_Name : Unbounded_Text_Type)
      return Alternatives_Set_Vector
   is
      use Alternatives_Set_Vectors;

      Res : Alternatives_Set_Vector;
   begin
      if Variant = null then
         return Alternatives_Set_Vectors.Empty_Vector;
      end if;

      if Langkit_Support.Text."=" (Variant.all.Discr_Name, Disc_Name) then
         for Choice of Variant.Variant_Choices loop
            Res.Append (Choice.Alt_Set);
         end loop;
      else
         for Choice of Variant.Variant_Choices loop
            Res.Append_Vector
              (Pick_Samples_For_Disc (Choice.Variant, Disc_Name));
         end loop;
      end if;

      return Res;
   end Pick_Samples_For_Disc;

   function Pick_Strat_For_Disc
     (Self      : Discriminated_Record_Typ;
      Disc_Name : Unbounded_Text_Type;
      Disc_Type : Discrete_Typ'Class;
      Context   : in out Generation_Context)
      return Static_Strategy_Type'Class;
   --  Return a generation strategy for the given discriminant

   -------------------------
   -- Pick_Strat_For_Disc --
   -------------------------

   function Pick_Strat_For_Disc
     (Self      : Discriminated_Record_Typ;
      Disc_Name : Unbounded_Text_Type;
      Disc_Type : Discrete_Typ'Class;
      Context   : in out Generation_Context)
      return Static_Strategy_Type'Class
   is
      Default_Strategy : constant Static_Strategy_Type'Class :=
        Self.Discriminant_Types.Element (Disc_Name).Get.Generate_Static
        (Context);
      Samples          : Alternatives_Set_Vector;
   begin

      Context.Strategies.Include (Default_Strategy);

      --  TODO: special strategies when discriminant also is an array index
      --  constraint, as we don't want to be purely random there, otherwise
      --  we would end up generating arrays that are too big.

      declare
         Found      : Boolean;
         Constraint : TGen.Types.Constraints.Index_Constraint;
      begin
         Self.Disc_Constrains_Array (Disc_Name, Found, Constraint);
         if Found then
            return Disc_Type.Generate_Array_Index_Constraint_Strategy
              (Disc_Name, Constraint, Context);
         end if;
      end;

      Samples := Pick_Samples_For_Disc (Self.Variant, Disc_Name);

      --  If non empty Sample, make a sampling strategy

      if not Samples.Is_Empty then
         declare
            Sample_Strat      : Static_Strategy_Acc;
            Dispatching_Strat : Dispatching_Static_Strategy_Type;
         begin
            if Disc_Type in Discrete_Typ'Class then
               Sample_Strat :=
                 new Static_Strategy_Type'Class'
                   (Disc_Type.Generate_Sampling_Strategy (Samples));
            else
               raise Program_Error
                 with "Unsupported discriminant type";
            end if;
            Dispatching_Strat.Bias := 0.5;
            Dispatching_Strat.S1   := Sample_Strat;
            Dispatching_Strat.S2   :=
              new Static_Strategy_Type'Class'(Default_Strategy);
            return Dispatching_Strat;
         end;
      end if;

      return Default_Strategy;
   end Pick_Strat_For_Disc;

   --  Static strategy for record types

   type Nondisc_Record_Static_Strategy_Type is
     new Record_Static_Strategy_Type with null record;
   overriding function Generate_Static_Value
     (S            : in out Nondisc_Record_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class;

   ---------------------------
   -- Generate_Static_Value --
   ---------------------------

   function Generate_Static_Value
     (S            : in out Nondisc_Record_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class
   is
      Res : Unbounded_String;
   begin
      Append (Res, "(");
      Append
        (Res,
         S.Generate
           (As_Record_Typ (S.T), S.Component_Strats, Disc_Context)
         .To_String);
      Append (Res, ")");
      return Base_Static_Value'(Value => Res);
   end Generate_Static_Value;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self    : Nondiscriminated_Record_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
      use Component_Maps;

      Strat : Nondisc_Record_Static_Strategy_Type;
   begin
      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      Strat.Generate := Generate_Record_Typ'Access;
      for Component in Self.Component_Types.Iterate loop
         declare
            Comp_Name : constant Unbounded_Text_Type := Key (Component);
         begin
            Strat.Component_Strats.Insert
              (Comp_Name, Element (Component).Get.Generate_Static (Context));
         end;
      end loop;
      return Strat;
   end Generate_Static;

   --  Static strategy for discriminated record types

   type Disc_Record_Static_Strategy_Type is
     new Record_Static_Strategy_Type with
      record
         Disc_Strats : Strategy_Map;
      end record;
   overriding function Generate_Static_Value
     (S            : in out Disc_Record_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class;

   ---------------------------
   -- Generate_Static_Value --
   ---------------------------

   function Generate_Static_Value
     (S            : in out Disc_Record_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class
   is
      T           : constant Typ'Class := S.T.Get;
      Disc_Record : constant Discriminated_Record_Typ :=
        Discriminated_Record_Typ (T);

      use Disc_Value_Maps;

      Current_Context : Disc_Value_Map;
      --  This context holds the values for the discriminant of the record
      --  being generated.

      Res : Unbounded_String;
   begin
      --  Start of by filling the discriminant context

      if Disc_Record.Constrained then

         --  If there are constraints, then we have to get their actual value
         --  from the Disc_Values.

         for Constraint_Cursor in Disc_Record.Discriminant_Constraint.Iterate
         loop
            declare
               use Discriminant_Constraint_Maps;

               Discriminant_Name : constant Unbounded_Text_Type :=
                 Key (Constraint_Cursor);
               Constraint        : constant Discrete_Constraint_Value :=
                 Element (Constraint_Cursor);
            begin
               pragma Assert (Constraint.Kind /= Non_Static);
               case Constraint.Kind is
                  when Static =>
                     Current_Context.Insert
                       (Discriminant_Name, Constraint.Int_Val);

                  when Discriminant =>

                     --  Make the correspondence here

                     Current_Context.Insert
                       (Discriminant_Name,
                        Disc_Context.Element (Constraint.Disc_Name));

                  when others =>
                     raise Program_Error
                       with "unsupported non static generation";
               end case;
            end;
         end loop;
      else
         for D_Strat_Cursor in S.Disc_Strats.Iterate loop
            declare
               use Strategy_Maps;

               procedure Generate_Val
                 (Disc_Name  : Unbounded_Text_Type;
                  Disc_Strat : in out Static_Strategy_Type'Class);

               procedure Generate_Val
                 (Disc_Name  : Unbounded_Text_Type;
                  Disc_Strat : in out Static_Strategy_Type'Class)
               is
                  Val : constant Discrete_Static_Value'Class :=
                    Discrete_Static_Value'Class
                      (Disc_Strat.Generate_Static_Value (Current_Context));
               begin
                  Current_Context.Insert (Disc_Name, Val.Value);
               end Generate_Val;

            begin
               S.Disc_Strats.Update_Element
                 (D_Strat_Cursor, Generate_Val'Access);
            end;
         end loop;
      end if;

      --  Write the generated discriminant values; they are part of the
      --  generated record value.

      Append (Res, "(");

      for Disc_Cursor in Current_Context.Iterate loop
         declare
            Disc_Name           : constant Unbounded_Text_Type :=
              Key (Disc_Cursor);
            Disc_Type_Classwide : constant Typ'Class :=
              Disc_Record.Discriminant_Types.Element (Disc_Name).Get;
            Disc_Type           : constant Discrete_Typ'Class :=
              Discrete_Typ'Class (Disc_Type_Classwide);
            Disc_Value          : constant Big_Integer :=
              Element (Disc_Cursor);
         begin
            Append (Res, +Disc_Name);
            Append (Res, " => ");
            Append (Res, Disc_Type.Lit_Image (Disc_Value));
            Append (Res, ", ");
         end;
      end loop;

      --  Now, generate values for the components

      declare
         Components : constant Component_Map :=
           Disc_Record.Components (Current_Context);
         R          : constant Record_Typ :=
           (Name            => Disc_Record.Name,
            Component_Types => Components,
            Static_Gen      => Disc_Record.Static_Gen);
      begin
         Append
           (Res,
            S.Generate (R, S.Component_Strats, Current_Context).To_String);
      end;

      --  If the record has no components, we'll get a spurious comma; simply
      --  remove it.

      Res := Remove_Trailing_Comma_And_Spaces (Res);
      Append (Res, ")");
      return Base_Static_Value'(Value => Res);

   end Generate_Static_Value;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self    : Discriminated_Record_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
      Strat : Disc_Record_Static_Strategy_Type;
      use Component_Maps;
   begin

      --  Pick the strategy for each discriminant

      for Disc in Self.Discriminant_Types.Iterate loop
         declare
            Disc_Name : constant Unbounded_Text_Type := Key (Disc);
         begin
            Strat.Disc_Strats.Insert
              (Disc_Name,
               Self.Pick_Strat_For_Disc
                 (Disc_Name, Discrete_Typ'Class
                      (Element (Disc).Unchecked_Get.all),
                  Context));
         end;
      end loop;

      --  Generate the strategies for the record components

      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      Strat.Generate := Generate_Record_Typ'Access;
      for Component in Self.Get_All_Components.Iterate loop
         declare
            Comp_Name : constant Unbounded_Text_Type := Key (Component);
         begin
            Strat.Component_Strats.Insert
              (Comp_Name, Element (Component).Get.Generate_Static (Context));
         end;
      end loop;

      return Strat;
   end Generate_Static;

   --  Dynamic generation

   ------------------------------------------
   -- Generate_Constrained_Random_Strategy --
   ------------------------------------------

   function Generate_Constrained_Random_Strategy
     (Self : Discriminated_Record_Typ; Context : Generation_Context)
      return Strategy_Type'Class
   is
      use Component_Maps;

      Res : Dynamic_Strategy_Type (Kind => Random_Kind, Constrained => False);
      F_Body      : Unbounded_String;
      Indentation : Unbounded_String := +"";
      Rec_Name    : constant String  := "Rec";

      procedure A (Str : String);
      procedure NL;
      procedure I;
      procedure UI;
      procedure Pp_Components (Components : Component_Maps.Map);
      procedure Pp_Variant
        (Components : Component_Vector; Variant : Variant_Part);

      procedure A (Str : String) is
      begin
         Append (F_Body, Str);
      end A;

      procedure NL is
      begin
         Append (F_Body, Ada.Characters.Latin_1.LF & (+Indentation));
      end NL;

      procedure I is
      begin
         Append (Indentation, "   ");
      end I;

      procedure UI is
      begin
         Indentation := Head (Indentation, Length (Indentation) - 3);
      end UI;

      procedure Pp_Components (Components : Component_Maps.Map) is
      begin
         for Component in Components.Iterate loop
            declare
               Component_Name : constant Unbounded_Text_Type :=
                 Key (Component);
               Component_Type : constant Typ'Class := Element (Component).Get;
            begin
               A ((+Component_Name) & " : ");
               A (To_Ada (Component_Type.Name) & " :=");
               I;
               NL;
               A (Component_Type.Gen_Random_Function_Name & ";");
               UI;
            end;
         end loop;
      end Pp_Components;

      procedure Pp_Variant
        (Components : Component_Vector; Variant : Variant_Part)
      is
         use Variant_Choice_Lists;

      begin
         A ("case " & (+Self.Variant.Discr_Name) & " is ");
         I;
         NL;
         for Choice of Variant.Variant_Choices loop
            declare
               use Component_Vectors;

               New_Components : Component_Vector := Components.Copy;
            begin
               for C in Choice.Components.Iterate loop
                  Append (New_Components, Key (C));
               end loop;
               --  A ("when " & (+Choice.Alternatives.Text) & " => ");
               I;
               NL;
               if Choice.Components.Length /= 0 then
                  A ("declare");
                  I;
                  NL;
                  Pp_Components (Choice.Components);
                  UI;
                  NL;
                  A ("begin");
                  I;
               end if;

               if Choice.Variant /= null then
                  Pp_Variant (New_Components, Choice.Variant.all);

               else
                  if New_Components.Length = 0 then
                     A ("null;");
                  end if;
                  for C of New_Components loop
                     NL;
                     A (Rec_Name & "." & (+C) & " := " & (+C) & ";");
                  end loop;
               end if;

               if Choice.Components.Length /= 0 then
                  UI;
                  NL;
                  A ("end;");
               end if;
               UI;
               NL;
            end;
         end loop;
         UI;
         NL;
         A ("end case;");
         NL;
      end Pp_Variant;

      Constrained_Function : constant Subprogram_Data :=
        Gen_Constrained_Function (Self);
   begin

      A ("declare");
      I;
      NL;
      A (Rec_Name & " : " & Self.Fully_Qualified_Name);

      --  Then add the discriminants to the record type declaration

      if Self.Discriminant_Types.Length > 0 then
         A (" (");
         for Disc in Self.Discriminant_Types.Iterate loop
            A (+Key (Disc));
            if Next (Disc) /= No_Element then
               A (", ");
            end if;
         end loop;
         A (");");
      end if;

      --  Now generate a function that is similar to the record declaration:
      --  generate the different variant parts, and when there are no more
      --  variant, assign the list of generated components to the resulting
      --  record.

      UI;
      NL;
      A ("begin");
      I;
      NL;

      Pp_Components (Self.Component_Types);
      if Self.Variant /= null then
         Pp_Variant (Component_Vectors.Empty, Self.Variant.all);
      end if;

      A ("return " & Rec_Name & ";");
      UI;
      NL;
      A ("end;");

      Res.Strategy_Function := Constrained_Function;
      Res.Strategy_Body     := +(+F_Body);

      return Res;
   end Generate_Constrained_Random_Strategy;

   ------------------------------
   -- Generate_Random_Strategy --
   ------------------------------

   function Generate_Random_Strategy
     (Self : Discriminated_Record_Typ; Context : in out Generation_Context)
      return Strategy_Type'Class
   is
      Result : Dynamic_Strategy_Type
        (Kind => Random_Kind, Constrained => True);
      F_Body : Unbounded_String;

      Indentation : Natural := 0;

      Constrained_Function : constant Subprogram_Data :=
        Gen_Constrained_Function (Self);
   begin

      Write_Line (F_Body, "declare", Indentation);
      Write_Line (F_Body, "begin", Indentation);

      Write_Line
        (F_Body, "return " & (+Constrained_Function.Name), Indentation);

      Indentation := Indentation + 3;

      S_Write (F_Body, "( ", Indentation);

      --  Expected parameters are discriminant values; pass them

      for Disc_Cursor in Self.Discriminant_Types.Iterate loop
         declare
            use Component_Maps;

            Disc_Name : constant String := +Key (Disc_Cursor);
            Disc_Type : constant Typ'Class := Element (Disc_Cursor).Get;

            Suffix : constant String :=
              (if Next (Disc_Cursor) = No_Element then ");" else " ,");
         begin
            Write_Line
              (F_Body,
               Disc_Name & " => " & Disc_Type.Gen_Random_Function_Name &
               Suffix,
               Indentation);
         end;
      end loop;

      Indentation := Indentation - 3;
      Write_Line (F_Body, "end;", Indentation);

      Result.Strategy_Body     := +(+F_Body);
      Result.Strategy_Function := Self.Random_Strategy_Function;

      --  This uses the constrained strategy

      declare
         Constrained_Strategy : aliased Dynamic_Strategy_Type :=
           Dynamic_Strategy_Type
             (Self.Generate_Constrained_Random_Strategy (Context));
      begin
         Context.Strategies.Insert (Constrained_Strategy);
         Result.Constrained_Strategy_Function :=
           Constrained_Strategy'Unchecked_Access;
      end;

      return Result;
   end Generate_Random_Strategy;

   procedure Disc_Constrains_Array
     (Component_Types     : Component_Map;
      Variant             : Variant_Part_Acc;
      Disc_Name           : Unbounded_Text_Type;
      Disc_Correspondence : UTT_Map;
      Found               : out Boolean;
      Constraint          : out TGen.Types.Constraints.Index_Constraint);
   --  Internal for the spec-declared Disc_Constrains_Array procedure.
   --  Component_Types are the currently analyzed component types, Variant is
   --  the currently analyzed variant, and Disc_Correspondence gives the
   --  current correspondence from sub-record discriminant names to top-level
   --  record discriminant names.

   ---------------------------
   -- Disc_Constrains_Array --
   ---------------------------

   procedure Disc_Constrains_Array
     (Component_Types     : Component_Map;
      Variant             : Variant_Part_Acc;
      Disc_Name           : Unbounded_Text_Type;
      Disc_Correspondence : UTT_Map;
      Found               : out Boolean;
      Constraint          : out TGen.Types.Constraints.Index_Constraint)
   is
      T_Ref           : SP.Ref;
      Constraints     : Constraint_Acc;
      Has_Constraints : Boolean;
   begin

      Found := False;
      Constraint := (Present => False);

      --  Check the components of the record

      for Component_Type of Component_Types loop

         Has_Constraints := False;
         T_Ref := Component_Type;

         if Component_Type.Get.Kind in Anonymous_Kind then
            Has_Constraints := True;
            Constraints :=
              As_Anonymous_Typ
                (Component_Type).Subtype_Constraints;
            T_Ref := As_Named_Typ (As_Anonymous_Typ (Component_Type));
         end if;

         if T_Ref.Get.Kind in Constrained_Array_Kind then
            declare
               T : constant Constrained_Array_Typ'Class :=
                 As_Constrained_Array_Typ (T_Ref);
            begin
               T.Is_Constrained_By_Variable (Disc_Name, Found, Constraint);

               if Found then

                  --  We got the constraint, but we still must translate it
                  --  according to the discriminant correspondence. The
                  --  constraint must be expressed in the terms of the
                  --  top-level record.

                  if Constraint.Present then
                     declare
                        CLB : constant Discrete_Constraint_Value :=
                          Constraint.Discrete_Range.Low_Bound;
                        CHB : constant Discrete_Constraint_Value :=
                          Constraint.Discrete_Range.High_Bound;
                     begin
                        if CLB.Kind = Discriminant then
                           Constraint.Discrete_Range.Low_Bound.Disc_Name :=
                             Disc_Correspondence.Element (CLB.Disc_Name);
                        end if;
                        if CHB.Kind = Discriminant then
                           Constraint.Discrete_Range.High_Bound.Disc_Name :=
                             Disc_Correspondence.Element (CHB.Disc_Name);
                        end if;
                     end;
                  end if;

                  --  The discriminant could constrain several arrays, but
                  --  there is no point taking them all into account. So stop
                  --  as soon as we found a match.

                  return;
               end if;
            end;
         end if;

         if T_Ref.Get.Kind in Disc_Record_Kind then

            --  If there are constraints, they are necessarily discriminant
            --  constraints. Records the discriminant correspondence to have
            --  an index constraint that can be applied to the top-level record
            --  at the end.

            if Has_Constraints then
               declare
                  Disc_Constraints : constant Discriminant_Constraint_Map :=
                    Discriminant_Constraints (Constraints.all).Constraint_Map;

                  New_Disc_Correspondence : UTT_Map;

                  Sub_Record : constant Discriminated_Record_Typ'Class :=
                    As_Discriminated_Record_Typ (T_Ref);

                  Inspect_Subrecord       : Boolean := False;
                  Correspondent_Disc_Name : Unbounded_Text_Type;
                  --  Whether the discriminant Disc_Name can affect the sub-
                  --  record, i.e. if it is propagated through a discriminant
                  --  constraint. If it is the case, Correspondent_Disc_Name
                  --  will hold the name of the discriminant that corresponds
                  --  to the original Disc_Name, in the nested sub-record.

               begin
                  for Constraint_Cursor in Disc_Constraints.Iterate loop
                     declare
                        Disc_Constrained : constant Unbounded_Text_Type :=
                          Discriminant_Constraint_Maps.Key (Constraint_Cursor);

                        Constraint : constant Discrete_Constraint_Value :=
                          Discriminant_Constraint_Maps.Element
                            (Constraint_Cursor);
                     begin

                        --  If the constraint is a discriminant constraint,
                        --  then we have a new correspondence.

                        if Constraint.Kind = Discriminant then
                           declare
                              Orig_Disc : constant Unbounded_Text_Type :=
                                Disc_Correspondence.Element
                                  (Constraint.Disc_Name);
                           begin
                              Inspect_Subrecord := True;
                              Correspondent_Disc_Name := Disc_Constrained;
                              New_Disc_Correspondence.Insert
                                (Disc_Constrained,
                                 Orig_Disc);
                           end;
                        end if;
                     end;
                  end loop;

                  if Inspect_Subrecord then
                     Disc_Constrains_Array
                       (Sub_Record.Component_Types,
                        Sub_Record.Variant,
                        Correspondent_Disc_Name,
                        New_Disc_Correspondence,
                        Found,
                        Constraint);
                  end if;
               end;

            else
               --  If we don't have any constraint, then we just drop it all,
               --  as none of the discriminant in the top level record will
               --  affect the types of a sub-record. Note that this is the case
               --  where the sub-record is mutable (i.e. has default values for
               --  discriminants), as we can't have an unconstrained value as
               --  a component of a record.

               null;
            end if;

         end if;
      end loop;

      --  Check the variant part

      if Variant /= null then
         for Variant_Choice of Variant.Variant_Choices loop
            Disc_Constrains_Array
              (Variant_Choice.Components,
               Variant_Choice.Variant,
               Disc_Name,
               Disc_Correspondence,
               Found,
               Constraint);
            if Found then
               return;
            end if;
         end loop;
      end if;
   end Disc_Constrains_Array;

   ---------------------------
   -- Disc_Constrains_Array --
   ---------------------------

   procedure Disc_Constrains_Array
     (Self       : Discriminated_Record_Typ;
      Disc_Name  : Unbounded_Text_Type;
      Found      : out Boolean;
      Constraint : out TGen.Types.Constraints.Index_Constraint)
   is
      Disc_Correspondence : UTT_Map;
   begin
      for Disc_Cursor in Self.Discriminant_Types.Iterate loop
         declare
            Disc_Name : constant Unbounded_Text_Type :=
              Component_Maps.Key (Disc_Cursor);
         begin
            Disc_Correspondence.Insert (Disc_Name, Disc_Name);
         end;
      end loop;

      Disc_Constrains_Array
        (Self.Component_Types,
         Self.Variant,
         Disc_Name,
         Disc_Correspondence,
         Found,
         Constraint);
   end Disc_Constrains_Array;

end TGen.Types.Record_Types;
