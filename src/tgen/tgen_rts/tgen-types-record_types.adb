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

with Ada.Containers;        use Ada.Containers;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with TGen.Types.Array_Types; use TGen.Types.Array_Types;

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

   function Generate_Record_Typ
     (T           : SP.Ref;
      Comp_Strats : in out Strategy_Map;
      Disc_Values : Disc_Value_Map) return JSON_Value;
   --  Generate a value for a record type by generating values for its
   --  components using the strategies defined by the Comp_Strats mapping.

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
      Current_Component : Component_Maps.Cursor;
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

   ---------------------
   -- Get_Diagnostics --
   ---------------------

   function Get_Diagnostics (Self : Record_Typ) return String is
      use Component_Maps;
   begin
      for Comp_Cur in Self.Component_Types.Iterate loop
         declare
            Diags : constant String := Element (Comp_Cur).Get.Get_Diagnostics;
         begin
            if Diags'Length > 0 then
               return Diags;
            end if;
         end;
      end loop;
      return "";
   end Get_Diagnostics;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : Record_Typ; Val : JSON_Value) return JSON_Value
   is
      use Component_Maps;
      Components : constant JSON_Value := Val.Get ("components").Clone;
   begin
      return Res : constant JSON_Value := Create_Object do
         for Cur in Self.Component_Types.Iterate loop
            declare
               Comp_Name : constant String := To_String (Key (Cur));
               Comp_Val  : constant JSON_Value := Components.Get (Comp_Name);
            begin
               Components.Set_Field
                 (Comp_Name, Element (Cur).Get.Encode (Comp_Val));
            end;
         end loop;
         Res.Set_Field ("components", Components);
      end return;
   end Encode;

   --  Random generation for record types

   --------------
   -- Generate --
   --------------

   function Generate
     (S           : in out Record_Strategy_Type;
      Disc_Values : Disc_Value_Map) return JSON_Value
   is
      Result : constant JSON_Value := Create_Object;
   begin
      --  Set the component values

      Set_Field
        (Result,
        "components",
         Generate_Record_Typ (S.T, S.Component_Strats, Disc_Values));
      return Result;
   end Generate;

   ----------------------
   -- Default_Strategy --
   ----------------------

   function Default_Strategy
     (Self : Record_Typ) return Strategy_Type'Class
   is
      use Component_Maps;

      Strat : Record_Strategy_Type;
   begin
      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      for Component in Self.Component_Types.Iterate loop
         declare
            Comp_Name : constant Unbounded_String := Key (Component);
         begin
            Strat.Component_Strats.Insert
              (Comp_Name,
               new Strategy_Type'Class'
                 (Strategy_Type'Class
                    (Element (Component).Get.Default_Strategy)));
         end;
      end loop;
      return Strat;
   end Default_Strategy;

   --  Enumerated generation for record types

   ----------
   -- Init --
   ----------

   procedure Init (S : in out Enum_Record_Strategy_Type) is
   begin
      S.Varying_Index := 1;
      for Cur in S.Component_Strats.Iterate loop
         S.Component_Strats.Reference (Cur).Strat.Init;
         S.Component_Strats.Reference (Cur).Values := Empty_Array;
         S.Component_Strats.Reference (Cur).Index := 0;
      end loop;
   end Init;

   --------------
   -- Has_Next --
   --------------

   overriding function Has_Next (S : Enum_Record_Strategy_Type) return Boolean
   is
   begin

      --  For null records, S.Varying_Index is used as a flag to indicate that
      --  we have already generated the null record aggregate. Init sets it to
      --  1 when the strategy is initialized, so anything other than 1 means
      --  the value has already been generated.

      if S.Component_Strats.Is_Empty then
         return S.Varying_Index = 1;
      end if;

      --  We have generated all the values when the last components has no more
      --  values to generate, and all the component indices are maxed out.

      return (for some Comp of S.Component_Strats =>
                Comp.Strat.Has_Next or else Comp.Index < Length (Comp.Values));
   end Has_Next;

   --------------
   -- Generate --
   --------------

   overriding function Generate
     (S            : in out Enum_Record_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      use Component_Info_Vectors;
      Result : constant JSON_Value := Create_Object;
      Comps  : constant JSON_Value := Create_Object;
   begin

      --  Handle the case of null records: We'll only generate a single value,
      --  and use S.Varying_Index as a flag to determine if we have already
      --  generated a value or not. Init sets it to 1, so increment it to 2 to
      --  flag that we have generated a value.

      if S.Component_Strats.Is_Empty
        and then S.Varying_Index = 1
      then
         S.Varying_Index := 2;
         Result.Set_Field ("components", Comps);
         return Result;
      end if;

      --  Do the initial generation if needed, when the index of the first
      --  component is zero, this means we are in the first call, and we need
      --  to generate a value for all the components but the first for the
      --  rest of the algorithm to work properly.

      if S.Component_Strats.First_Element.Index = 0 then
         for Idx in 2 .. Positive (S.Component_Strats.Length) loop
            declare
               Comp : constant Reference_Type :=
                 S.Component_Strats.Reference (Idx);
            begin
               Append (Comp.Values, Comp.Strat.Generate (Disc_Context));
               Comp.Index := 1;
            end;
         end loop;
      end if;

      --  Find the next component that can have its index bumped (either we
      --  haven't reached the end of the array or it can still generate new
      --  values).

      loop
         declare
            Current_Comp : constant Reference_Type :=
              S.Component_Strats.Reference (S.Varying_Index);
         begin
            --  First case: we still have already-generated values to use for
            --  this component.

            if Current_Comp.Index < Length (Current_Comp.Values) then
               Current_Comp.Index := @ + 1;
               exit;

            --  Second case: We have already used all pre-existing values for
            --  this component, but we can generate more. Lets do that and
            --  increment the index.

            elsif Current_Comp.Strat.Has_Next then
               Append
                 (Current_Comp.Values,
                  Current_Comp.Strat.Generate (Disc_Context));
               Current_Comp.Index := @ + 1;
               exit;

            --  Last case: There are no new values for this component, we must
            --  thus make another component vary its index.
            else
               pragma Assert
                 (S.Varying_Index < Positive (S.Component_Strats.Length));
               S.Varying_Index := S.Varying_Index + 1;
            end if;
         end;
      end loop;

      --  Now that we incremented an index, reset all the indices of the
      --  previous components.

      for Idx in 1 .. S.Varying_Index - 1 loop
         S.Component_Strats.Reference (Idx).Index := 1;
      end loop;

      --  And reset the varying index

      S.Varying_Index := 1;

      --  Generate a value from the current components.

      for Comp of S.Component_Strats loop
         Comps.Set_Field (+Comp.Comp_Name, Get (Comp.Values, Comp.Index));
      end loop;

      Result.Set_Field ("components", Comps);
      return Result;
   end Generate;

   ---------------------------
   -- Default_Enum_Strategy --
   ---------------------------

   function Default_Enum_Strategy
     (Self : Record_Typ) return Enum_Strategy_Type'Class
   is
      Strat : Enum_Record_Strategy_Type;
   begin
      for Cur in Self.Component_Types.Iterate loop
         declare
            use Component_Maps;
            Comp_Name : constant Unbounded_String := Key (Cur);
            Comp_Type : constant SP.Ref := Element (Cur);
            Comp_Info : constant Enum_Strat_Component_Info :=
              (Comp_Name => Comp_Name,
               Strat     => new Enum_Strategy_Type'Class'
                 (Enum_Strategy_Type'Class
                      (Comp_Type.Get.Default_Enum_Strategy)),
               Values    => Empty_Array,
               Index     => 0);
         begin
            Strat.Component_Strats.Append (Comp_Info);
         end;
      end loop;

      return Strat;
   end Default_Enum_Strategy;

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
            Cur : Variant_Choice_Lists.Cursor := Var.Variant_Choices.First;
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
              and then Element (Value_Cur).Get
                /= Element (Constraint_Cur).Int_Val
            then
               return False;
            end if;
         end if;
         Next (Constraint_Cur);
      end loop;
      return True;
   end Constraints_Respected;

   ----------------
   -- Components --
   ----------------

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
   begin
      if Disc_Value_Maps.Has_Element (Disc_Val_Cur) then
         Discr_Val := Disc_Value_Maps.Element (Disc_Val_Cur).Get;
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

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : Discriminated_Record_Typ; Val : JSON_Value) return JSON_Value
   is
      use Component_Maps;
      Disc_Values   : Disc_Value_Map;
      Comp_Map      : Component_Map;
      Discriminants : constant JSON_Value := Val.Get ("discriminants").Clone;
      Components    : constant JSON_Value := Val.Get ("components").Clone;
      Res           : constant JSON_Value := Create_Object;
   begin
      --  Encode the discriminants

      for Cur in Self.Discriminant_Types.Iterate loop
         declare
            Disc_Val : constant JSON_Value :=
              Discriminants.Get (To_String (Key (Cur)));
         begin
            Disc_Values.Insert (Key (Cur), Disc_Val);
            Discriminants.Set_Field
              (To_String (Key (Cur)), Element (Cur).Get.Encode (Disc_Val));
         end;
      end loop;
      Res.Set_Field ("discriminants", Discriminants);

      --  Encode the components

      Comp_Map := Self.Components (Disc_Values);
      for Cur in Comp_Map.Iterate loop
         declare
            Comp_Name : constant String := To_String (Key (Cur));
         begin
            Components.Set_Field
              (Comp_Name,
               Element (Cur).Get.Encode
               (Components.Get (Comp_Name)));
         end;
      end loop;
      Res.Set_Field ("components", Components);
      return Res;
   end Encode;

   ---------------------
   -- Get_Diagnostics --
   ---------------------

   function Get_Diagnostics (Self : Discriminated_Record_Typ) return String is
      Comp_Res : constant String := Record_Typ (Self).Get_Diagnostics;

      function Inspect_Variant (Var : Variant_Part_Acc) return String;
      --  Inspect the variant part for unsupported types, and return the first
      --  diagnostics found, if any.

      ---------------------
      -- Inspect_Variant --
      ---------------------

      function Inspect_Variant (Var : Variant_Part_Acc) return String is
      begin
         if Var = null then
            return "";
         end if;
         for Choice of Var.Variant_Choices loop
            for Comp of Choice.Components loop
               declare
                  Diags : constant String := Comp.Get.Get_Diagnostics;
               begin
                  if Diags'Length > 0 then
                     return Diags;
                  end if;
               end;
            end loop;
            declare
               Subvar_Res : constant String :=
                 Inspect_Variant (Choice.Variant);
            begin
               if Subvar_Res'Length > 0 then
                  return Subvar_Res;
               end if;
            end;
         end loop;
         return "";
      end Inspect_Variant;
   begin
      if Comp_Res'Length > 0 then
         return Comp_Res;
      end if;
      return Inspect_Variant (Self.Variant);
   end Get_Diagnostics;

   ------------------
   -- Supports_Gen --
   ------------------

   function Supports_Gen (Self : Discriminated_Record_Typ) return Boolean is

      function Inspect_Variant (Var : Variant_Part_Acc) return Boolean;
      --  Traverse the variant part tree to determine whether some type is not
      --  supported for generation.

      ---------------------
      -- Inspect_Variant --
      ---------------------

      function Inspect_Variant (Var : Variant_Part_Acc) return Boolean is
         Res : Boolean := True;
      begin
         if Var = null then
            return True;
         end if;
         for Choice of Var.Variant_Choices loop
            Res := (for all Cmp of Choice.Components => Cmp.Get.Supports_Gen);
            exit when not Res;
            Res := Inspect_Variant (Choice.Variant);
            exit when not Res;
         end loop;
         return Res;
      end Inspect_Variant;
   begin
      if not Record_Typ (Self).Supports_Gen then
         return False;
      end if;
      return Inspect_Variant (Self.Variant);
   end Supports_Gen;

   ------------------
   -- Free_Content --
   ------------------

   procedure Free_Content (Self : in out Discriminated_Record_Typ) is
   begin
      if Self.Variant /= null then
         Free_Variant (Self.Variant);
      end if;
   end Free_Content;

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
                     Comp_Name : constant Unbounded_String :=
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

   -------------------------
   -- Generate_Record_Typ --
   -------------------------

   function Generate_Record_Typ
     (T           : SP.Ref;
      Comp_Strats : in out Strategy_Map;
      Disc_Values : Disc_Value_Map) return JSON_Value
   is
      Rec : Record_Typ'Class renames Record_Typ'Class (T.Unchecked_Get.all);
      Res : constant JSON_Value := Create_Object;
      use Component_Maps;
   begin
      for Comp in Rec.Component_Types.Iterate loop
         declare
            Comp_Name : constant Unbounded_String := Key (Comp);

            procedure Generate_Val
              (Comp_Name  : Unbounded_String;
               Comp_Strat : Strategy_Acc);

            ------------------
            -- Generate_Val --
            ------------------

            procedure Generate_Val
              (Comp_Name  : Unbounded_String;
               Comp_Strat : Strategy_Acc) is
            begin
               Set_Field
                 (Val        => Res,
                  Field_Name => +Comp_Name,
                  Field      => Comp_Strat.Generate (Disc_Values));
            end Generate_Val;
         begin
            --  Generate a value

            Strategy_Maps.Query_Element
              (Comp_Strats.Find (Comp_Name), Generate_Val'Access);
         end;
      end loop;
      return Res;
   end Generate_Record_Typ;

   procedure Resolve_Disc_Value_From_Ctx
     (Global_Ctx : Disc_Value_Map;
      T          : SP.Ref;
      Local_Ctx  : out Disc_Value_Map) with
     Pre => (T.Get.Kind in Disc_Record_Kind)
             and then As_Discriminated_Record_Typ (T).Constrained;
   --  Given a global context, and a constrained discriminated record type,
   --  create a discriminant value map suitable for use as a local discriminant
   --  map.

   procedure Resolve_Disc_Value_From_Ctx
     (Global_Ctx : Disc_Value_Map;
      T          : SP.Ref;
      Local_Ctx  : out Disc_Value_Map)
   is
      Disc_Record : Discriminated_Record_Typ renames
        Discriminated_Record_Typ (T.Unchecked_Get.all);
   begin
      Local_Ctx.Clear;
      for Constraint_Cursor in Disc_Record.Discriminant_Constraint.Iterate
      loop
         declare
            use Discriminant_Constraint_Maps;

            Discriminant_Name : constant Unbounded_String :=
               Key (Constraint_Cursor);
            Constraint        : constant Discrete_Constraint_Value :=
               Element (Constraint_Cursor);
         begin
            pragma Assert (Constraint.Kind /= Non_Static);
            case Constraint.Kind is
               when Static =>
                  Local_Ctx.Insert
                    (Discriminant_Name,
                     TGen.JSON.Create (Constraint.Int_Val));

               when Discriminant =>

                  --  Make the correspondence here

                  Local_Ctx.Insert
                    (Discriminant_Name,
                     Global_Ctx.Element (Constraint.Disc_Name));

               when others =>
                  raise Program_Error with "unsupported non static generation";
            end case;
         end;
      end loop;
   end Resolve_Disc_Value_From_Ctx;

   function Pick_Samples_For_Disc
     (Variant : Variant_Part_Acc; Disc_Name : Unbounded_String)
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
     (Variant : Variant_Part_Acc; Disc_Name : Unbounded_String)
      return Alternatives_Set_Vector
   is
      use Alternatives_Set_Vectors;

      Res : Alternatives_Set_Vector;
   begin
      if Variant = null then
         return Alternatives_Set_Vectors.Empty_Vector;
      end if;

      if Variant.all.Discr_Name = Disc_Name then
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
      Disc_Name : Unbounded_String;
      Disc_Type : Discrete_Typ'Class)
      return Strategy_Type'Class;
   --  Return a generation strategy for the given discriminant

   -------------------------
   -- Pick_Strat_For_Disc --
   -------------------------

   function Pick_Strat_For_Disc
     (Self      : Discriminated_Record_Typ;
      Disc_Name : Unbounded_String;
      Disc_Type : Discrete_Typ'Class)
      return Strategy_Type'Class
   is
      Default_Strategy : constant Strategy_Type'Class :=
        Self.Discriminant_Types.Element (Disc_Name).Get.Default_Strategy;
      Samples          : Alternatives_Set_Vector;
   begin
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
              (Disc_Name, Constraint);
         end if;
      end;

      Samples := Pick_Samples_For_Disc (Self.Variant, Disc_Name);

      --  If non empty Sample, make a sampling strategy

      if not Samples.Is_Empty then
         declare
            Sample_Strat      : Strategy_Acc;
            Dispatching_Strat : Dispatching_Strategy_Type;
         begin
            if Disc_Type in Discrete_Typ'Class then
               Sample_Strat :=
                 new Strategy_Type'Class'
                   (Disc_Type.Generate_Sampling_Strategy (Samples));
            else
               raise Program_Error
                 with "Unsupported discriminant type";
            end if;
            Dispatching_Strat.Bias := 0.5;
            Dispatching_Strat.S1   := Sample_Strat;
            Dispatching_Strat.S2   :=
              new Strategy_Type'Class'(Default_Strategy);
            return Dispatching_Strat;
         end;
      end if;

      return Default_Strategy;
   end Pick_Strat_For_Disc;

   --  Static strategy for discriminated record types

   --------------
   -- Generate --
   --------------

   function Generate
     (S            : in out Disc_Record_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      T           : constant Typ'Class := S.T.Get;
      Disc_Record : constant Discriminated_Record_Typ :=
        Discriminated_Record_Typ (T);

      use Disc_Value_Maps;

      Current_Context : Disc_Value_Map;
      --  This context holds the values for the discriminant of the record
      --  being generated.

      Discriminants : constant JSON_Value := Create_Object;
      Result        : constant JSON_Value := Create_Object;
   begin
      --  Start of by filling the discriminant context

      if Disc_Record.Constrained then

         --  If there are constraints, then we have to get their actual value
         --  from the Disc_Values.

         Resolve_Disc_Value_From_Ctx
           (Global_Ctx => Disc_Context,
            T          => S.T,
            Local_Ctx  => Current_Context);
      else
         for D_Strat_Cursor in S.Disc_Strats.Iterate loop
            declare
               procedure Generate_Val
                 (Disc_Name  : Unbounded_String;
                  Disc_Strat : Strategy_Acc);

               procedure Generate_Val
                 (Disc_Name  : Unbounded_String;
                  Disc_Strat : Strategy_Acc)
               is
               begin
                  Current_Context.Insert
                    (Disc_Name,
                     Disc_Strat.Generate (Current_Context));
               end Generate_Val;

            begin
               Strategy_Maps.Query_Element
                 (D_Strat_Cursor, Generate_Val'Access);
            end;
         end loop;
      end if;

      --  Write the generated discriminant values; they are part of the
      --  generated record value.

      for Disc_Cursor in Current_Context.Iterate loop
         Set_Field
           (Val        => Discriminants,
            Field_Name => +Key (Disc_Cursor),
            Field      => Element (Disc_Cursor));
      end loop;
      Set_Field (Result, "discriminants", Discriminants);

      --  Now, generate values for the components

      declare
         Components : constant Component_Map :=
           Disc_Record.Components (Current_Context);
         R          : constant Nondiscriminated_Record_Typ :=
           (Name                => Disc_Record.Name,
             Last_Comp_Unit_Idx => Disc_Record.Last_Comp_Unit_Idx,
            Component_Types     => Components,
            Static_Gen          => Disc_Record.Static_Gen,
            Fully_Private       => Disc_Record.Fully_Private,
            Private_Extension   => Disc_Record.Private_Extension);
         R_Ref      : SP.Ref;
      begin
         R_Ref.Set (R);
         Set_Field
           (Result, "components",
            Generate_Record_Typ (R_Ref, S.Component_Strats, Current_Context));
      end;
      return Result;
   end Generate;

   ----------------------
   -- Default_Strategy --
   ----------------------

   function Default_Strategy
     (Self : Discriminated_Record_Typ) return Strategy_Type'Class
   is
      Strat : Disc_Record_Strategy_Type;
      use Component_Maps;
   begin

      --  Pick the strategy for each discriminant

      for Disc in Self.Discriminant_Types.Iterate loop
         declare
            Disc_Name : constant Unbounded_String := Key (Disc);
         begin
            Strat.Disc_Strats.Insert
              (Disc_Name,
               new Strategy_Type'Class'
                 (Self.Pick_Strat_For_Disc
                    (Disc_Name, Discrete_Typ'Class
                       (Element (Disc).Unchecked_Get.all))));
         end;
      end loop;

      --  Generate the strategies for the record components

      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      for Component in Self.Get_All_Components.Iterate loop
         declare
            Comp_Name : constant Unbounded_String := Key (Component);
         begin
            Strat.Component_Strats.Insert
              (Comp_Name,
               new Strategy_Type'Class'
                 (Element (Component).Get.Default_Strategy));
         end;
      end loop;

      return Strat;
   end Default_Strategy;

   procedure Disc_Constrains_Array
     (Component_Types     : Component_Map;
      Variant             : Variant_Part_Acc;
      Disc_Name           : Unbounded_String;
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
      Disc_Name           : Unbounded_String;
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
                  Correspondent_Disc_Name : Unbounded_String;
                  --  Whether the discriminant Disc_Name can affect the sub-
                  --  record, i.e. if it is propagated through a discriminant
                  --  constraint. If it is the case, Correspondent_Disc_Name
                  --  will hold the name of the discriminant that corresponds
                  --  to the original Disc_Name, in the nested sub-record.

               begin
                  for Constraint_Cursor in Disc_Constraints.Iterate loop
                     declare
                        Disc_Constrained : constant Unbounded_String :=
                          Discriminant_Constraint_Maps.Key (Constraint_Cursor);

                        Constraint : constant Discrete_Constraint_Value :=
                          Discriminant_Constraint_Maps.Element
                            (Constraint_Cursor);
                     begin

                        --  If the constraint is a discriminant constraint,
                        --  then we have a new correspondence.

                        if Constraint.Kind = Discriminant then
                           declare
                              Orig_Disc : constant Unbounded_String :=
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
      Disc_Name  : Unbounded_String;
      Found      : out Boolean;
      Constraint : out TGen.Types.Constraints.Index_Constraint)
   is
      Disc_Correspondence : UTT_Map;
   begin
      for Disc_Cursor in Self.Discriminant_Types.Iterate loop
         declare
            Disc_Name : constant Unbounded_String :=
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

   ----------
   -- Init --
   ----------

   overriding procedure Init (S : in out Disc_Record_Enum_Strat_Type) is
   begin
      S.Disc_Strat.Init;

      --  Dummy initialize the Current_Component_Strat so that it returns False
      --  on Has_Next.

      S.Current_Comp_Strat.Varying_Index := 2;
      S.Current_Comp_Strat.Component_Strats.Clear;

      --  Clear the current discriminant values

      S.Current_Disc_Values.Clear;

      --  Reset all the component strats, if we have some

      for Comp of S.All_Comp_Strats loop
         Comp.Init;
      end loop;
   end Init;

   --------------
   -- Has_Next --
   --------------

   overriding function Has_Next
     (S : Disc_Record_Enum_Strat_Type) return Boolean is
     (S.Current_Comp_Strat.Has_Next
      or else S.Disc_Strat.Has_Next);

   --------------
   -- Generate --
   --------------

   overriding function Generate
     (S            : in out Disc_Record_Enum_Strat_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      Rec : Discriminated_Record_Typ renames
        Discriminated_Record_Typ (S.T.Unchecked_Get.all);
   begin

      --  There are two cases in which we can't generate a new value fro the
      --  component, but Has_Next still returned True for this type:
      --  - We have a constrained type, and this is the first call to Generate.
      --    In that case, grab the discriminant values from the context.
      --  - We have a unconstrained type, and we can still generate new values
      --    for the discriminants.

      if not S.Current_Comp_Strat.Has_Next then

         --  First, get new discriminant values

         if Rec.Constrained then
            Resolve_Disc_Value_From_Ctx
              (Global_Ctx => Disc_Context,
               T          => S.T,
               Local_Ctx  => S.Current_Disc_Values);

            --  Ensure the strat returns False to Has_Next calls once we have
            --  generated all the values for this discriminant set.

            S.Disc_Strat.Varying_Index := 2;
         else
            declare
               use Component_Maps;
               New_Discs : constant JSON_Value :=
                 S.Disc_Strat.Generate (Disc_Context).Get ("components");
            begin
               S.Current_Disc_Values.Clear;
               for Disc_Cur in Rec.Discriminant_Types.Iterate loop
                  declare
                     Disc_Name : constant Unbounded_String := Key (Disc_Cur);
                     Disc_Val  : constant JSON_Value :=
                       New_Discs.Get (+Disc_Name);
                  begin
                     S.Current_Disc_Values.Insert (Disc_Name, Disc_Val);
                  end;
               end loop;
            end;
         end if;

         --  Then generate a new strat for the relevant components based on the
         --  discriminant values.

         S.Current_Comp_Strat.Component_Strats.Clear;
         declare
            use Component_Maps;
            Actual_Comps : constant Component_Map :=
              Rec.Components (S.Current_Disc_Values);
         begin
            --  For each of the components that are present in the record,
            --  based on the current discriminant value, generate or re-use one
            --  of the strategies.

            for Comp in Actual_Comps.Iterate loop
               declare
                  use Strategy_Maps;
                  Comp_Name  : constant Unbounded_String := Key (Comp);
                  Comp_Strat : Strategy_Maps.Cursor :=
                    S.All_Comp_Strats.Find (Comp_Name);
                  Dummy_Inst : Boolean;
               begin
                  --  Generate a new strategy if it is the first time we'll be
                  --  generating values for this component.

                  if not Has_Element (Comp_Strat) then
                     S.All_Comp_Strats.Insert
                       (Key      => Comp_Name,
                        New_Item => new Strategy_Type'Class'
                          (Strategy_Type'Class
                             (Element (Comp).Get.Default_Enum_Strategy)),
                        Position => Comp_Strat,
                        Inserted => Dummy_Inst);
                  end if;

                  --  Clear the component strategy, in case it was already used
                  --  before.

                  Element (Comp_Strat).Init;
                  S.Current_Comp_Strat.Component_Strats.Append
                    (Enum_Strat_Component_Info'
                       (Comp_Name => Key (Comp),
                        Strat     =>
                          Enum_Strategy_Type_Acc (Element (Comp_Strat)),
                        Values    => Empty_Array,
                        Index     => 1));
               end;
            end loop;
         end;
         S.Current_Comp_Strat.Varying_Index := 1;
         S.Current_Comp_Strat.Init;
      end if;

      --  Now generate values for this set of components, using the
      --  non-discriminated record strat.

      declare
         use Component_Maps;
         Res : constant JSON_Value :=
            S.Current_Comp_Strat.Generate (S.Current_Disc_Values);
         Disc_JSON : constant JSON_Value := Create_Object;
      begin
         for Disc_Cur in Rec.Discriminant_Types.Iterate loop
            Disc_JSON.Set_Field
              (+Key (Disc_Cur),
               S.Current_Disc_Values.Element (Key (Disc_Cur)));
         end loop;
         Res.Set_Field ("discriminants", Disc_JSON);
         return Res;
      end;
   end Generate;

   function Default_Enum_Strategy
     (Self : Discriminated_Record_Typ) return Enum_Strategy_Type'Class
   is
      use Component_Maps;
      Res : Disc_Record_Enum_Strat_Type;
   begin
      Res.T.From_Element (Self'Unrestricted_Access);
      Res.Current_Comp_Strat.Component_Strats.Clear;
      Res.Current_Comp_Strat.Varying_Index := 2;
      Res.Current_Disc_Values.Clear;

      --  Null record strategy, ensures the first call to Has_Next will
      --  return True;

      Res.Disc_Strat.Varying_Index := 1;
      Res.Disc_Strat.Component_Strats.Clear;

      --  The generate appropriate strat if the record is not constrained. Use
      --  appropriate strategy if a discriminant constrains a record (i.e. cap
      --  its value so that the length of the array doesn't exceed 1000
      --  elements).

      if not Self.Constrained then
         for Disc in Self.Discriminant_Types.Iterate loop
            declare
               Active     : Boolean;
               Constraint : Index_Constraint;
               Strat      : Enum_Strategy_Type_Acc;
            begin
               Self.Disc_Constrains_Array (Key (Disc), Active, Constraint);
               if Active then
                  pragma Assert (Constraint.Present);
                  if Constraint.Discrete_Range.Low_Bound.Kind = Discriminant
                    and then Constraint.Discrete_Range.High_Bound.Kind =
                               Discriminant
                  then
                     Strat := new Enum_Strategy_Type'Class'
                       (Make_Dual_Array_Constraint_Strat
                          (Element (Disc), Constraint, Key (Disc)));
                  else
                     Strat := new Enum_Strategy_Type'Class'
                       (Make_Single_Array_Constraint_Strat
                          (Element (Disc), Constraint));
                  end if;
               else
                  Strat := new Enum_Strategy_Type'Class'
                             (Element (Disc).Get.Default_Enum_Strategy);
               end if;
               Res.Disc_Strat.Component_Strats.Append
                 (Enum_Strat_Component_Info'
                    (Comp_Name => Key (Disc),
                     Strat     => Strat,
                     Values    => Empty_Array,
                     Index     => 0));
            end;
         end loop;
         Res.Disc_Strat.Varying_Index := 1;
      end if;
      return Res;
   end Default_Enum_Strategy;

   ---------
   -- FQN --
   ---------

   function FQN (Self : Function_Typ) return String is
      Result : Ada_Qualified_Name := Self.Name.Copy;
   begin
      Result.Delete_Last;
      return To_Ada (Result);
   end FQN;

   ------------------------
   -- JSON_Test_Filename --
   ------------------------

   function JSON_Test_Filename (Self : Function_Typ) return String is
     (To_Filename (Self.Compilation_Unit_Name) & ".json");

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : Function_Typ; Val : JSON_Value) return JSON_Value
   is
      use Component_Maps;
      Params  : constant JSON_Value := Val.Get ("param_values").Clone;
      Globals : constant JSON_Value := Val.Get ("global_values").Clone;
   begin
      return Res : constant JSON_Value := Create_Object do
         for Cur in Self.Component_Types.Iterate loop
            declare
               Param_Name : constant String := To_String (Key (Cur));
               Param_Val  : constant JSON_Value := Params.Get (Param_Name);
            begin
               Params.Set_Field
                 (Param_Name, Element (Cur).Get.Encode (Param_Val));
            end;
         end loop;
         for Cur in Self.Globals.Iterate loop
            declare
               Global_Name : constant String := To_String (Key (Cur));
               Global_Val  : constant JSON_Value := Globals.Get (Global_Name);
            begin
               Globals.Set_Field
                 (Global_Name, Element (Cur).Get.Encode (Global_Val));
            end;
         end loop;
         Res.Set_Field ("param_values", Params);
         Res.Set_Field ("global_values", Globals);
      end return;
   end Encode;

   type Function_Strategy_Type is new Random_Strategy_Type with
      record
         T                : SP.Ref;
         Component_Strats : Strategy_Map;
      end record;
   --  Strategy to generate test case vectors for subprograms

   function Generate
     (S           : in out Function_Strategy_Type;
      Disc_Values : Disc_Value_Map) return JSON_Value;

   --------------
   -- Generate --
   --------------

   function Generate
     (S           : in out Function_Strategy_Type;
      Disc_Values : Disc_Value_Map) return JSON_Value
   is
      use Component_Maps;
      FN_Typ : constant Function_Typ'Class := As_Function_Typ (S.T);
      Result : constant JSON_Value := Create_Object;

      Generated_Value : JSON_Value;

      Param_Values  : constant JSON_Value := Create_Object;
      Global_Values : constant JSON_Value := Create_Object;
   begin
      --  Generate the parameter values + global values

      declare
         FN_As_Rec_Typ     : Record_Typ;
         FN_As_Rec_Typ_Ref : SP.Ref;
      begin
         FN_As_Rec_Typ.Component_Types := FN_Typ.Component_Types;
         for Cur in FN_Typ.Globals.Iterate loop
            FN_As_Rec_Typ.Component_Types.Insert (Key (Cur), Element (Cur));
         end loop;
         SP.Set (FN_As_Rec_Typ_Ref, FN_As_Rec_Typ);
         Generated_Value :=
           Generate_Record_Typ
             (FN_As_Rec_Typ_Ref, S.Component_Strats, Disc_Values);
      end;

      --  Set the component values

      for Cur in FN_Typ.Component_Types.Iterate loop
         declare
            Param_Name : constant String := +Key (Cur);
         begin
            if Generated_Value.Has_Field (Param_Name) then
               Param_Values.Set_Field
                 (Param_Name,
                  JSON_Value'(Generated_Value.Get (Param_Name)));
            end if;
         end;
      end loop;
      for Cur in FN_Typ.Globals.Iterate loop
         declare
            Global_Name : constant String := +Key (Cur);
         begin
            if Generated_Value.Has_Field (Global_Name) then
               Global_Values.Set_Field
                 (Global_Name,
                  JSON_Value'(Generated_Value.Get (Global_Name)));
            end if;
         end;
      end loop;
      Set_Field (Result, "param_values", Param_Values);
      Set_Field (Result, "global_values", Global_Values);
      return Result;
   end Generate;

   ----------------------
   -- Default_Strategy --
   ----------------------

   overriding function Default_Strategy
     (Self : Function_Typ) return Strategy_Type'Class
   is
      use Component_Maps;

      Strat : Function_Strategy_Type;

      procedure Process_Components (Comps : Component_Map);

      ------------------------
      -- Process_Components --
      ------------------------

      procedure Process_Components (Comps : Component_Map) is
      begin
         for Component in Comps.Iterate loop
            declare
               Comp_Name : constant Unbounded_String := Key (Component);
            begin
               Strat.Component_Strats.Insert
                 (Comp_Name,
                  new Strategy_Type'Class'
                    (Element (Component).Get.Default_Strategy));
            end;
         end loop;
      end Process_Components;
   begin
      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      Process_Components (Self.Component_Types);
      Process_Components (Self.Globals);
      return Strat;
   end Default_Strategy;

   type Enum_Function_Strategy_Type is new Enum_Record_Strategy_Type with
      record
         T : SP.Ref;
         --  Reference to the function type, to know parameter and global
         --  names.

      end record;

   overriding function Generate
     (S            : in out Enum_Function_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value;

   --------------
   -- Generate --
   --------------

   overriding function Generate
     (S            : in out Enum_Function_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      use Component_Maps;
      Intermediate_Result : constant JSON_Value :=
        Generate (Enum_Record_Strategy_Type (S), Disc_Context)
          .Get ("components");
      T                   : constant Function_Typ'Class :=
        As_Function_Typ (S.T);

      Result        : constant JSON_Value := Create_Object;
      Param_Values  : constant JSON_Value := Create_Object;
      Global_Values : constant JSON_Value := Create_Object;
   begin
      for Param_Cur in T.Component_Types.Iterate loop
         declare
            Name : constant String := +Key (Param_Cur);
         begin
            Param_Values.Set_Field
              (Name, JSON_Value'(Intermediate_Result.Get (Name)));
         end;
      end loop;
      for Global_Cur in T.Globals.Iterate loop
         declare
            Name : constant String := +Key (Global_Cur);
         begin
            Global_Values.Set_Field
              (Name, JSON_Value'(Intermediate_Result.Get (Name)));
         end;
      end loop;
      Result.Set_Field ("param_values", Param_Values);
      Result.Set_Field ("global_values", Global_Values);
      return Result;
   end Generate;

   ---------------------------
   -- Default_Enum_Strategy --
   ---------------------------

   function Default_Enum_Strategy
     (Self : Function_Typ) return Enum_Strategy_Type'Class
   is
      Strat : Enum_Function_Strategy_Type;

      procedure Process_Components (Components : Component_Map);

      ------------------------
      -- Process_Components --
      ------------------------

      procedure Process_Components (Components : Component_Map) is
      begin
         for Cur in Components.Iterate loop
            declare
               use Component_Maps;
               Comp_Name : constant Unbounded_String := Key (Cur);
               Comp_Type : constant SP.Ref := Element (Cur);
               Comp_Info : constant Enum_Strat_Component_Info :=
                 (Comp_Name => Comp_Name,
                  Strat     => new Enum_Strategy_Type'Class'
                    (Comp_Type.Get.Default_Enum_Strategy),
                  Values    => Empty_Array,
                  Index     => 0);
            begin
               Strat.Component_Strats.Append (Comp_Info);
            end;
         end loop;
      end Process_Components;

   begin
      SP.Set (Strat.T, Self);
      Process_Components (Self.Component_Types);
      Process_Components (Self.Globals);
      return Strat;
   end Default_Enum_Strategy;

end TGen.Types.Record_Types;
