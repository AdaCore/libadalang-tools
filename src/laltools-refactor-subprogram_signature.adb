------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with Ada.Assertions; use Ada.Assertions;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Laltools.Subprogram_Hierarchy; use Laltools.Subprogram_Hierarchy;

package body Laltools.Refactor.Subprogram_Signature is

   procedure Add_To_Empty_Params
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Edit_Map)
     with Pre => Subp.P_Is_Subprogram
     or else Subp.Kind in Ada_Generic_Subp_Decl_Range;
   --  Adds a parameter defined by 'Data' as the first parameter of a 'Subp'
   --  that has no parameters.

   procedure Add_As_First_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Edit_Map)
     with Pre => Subp.P_Is_Subprogram
     or else Subp.Kind in Ada_Generic_Subp_Decl_Range;
   --  Adds a parameter defined by 'Data' as the first parameter.

   procedure Add_As_Last_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Edit_Map)
     with Pre => Subp.P_Is_Subprogram
     or else Subp.Kind in Ada_Generic_Subp_Decl_Range;
   --  Adds a parameter defined by 'Data' as the last parameter.

   procedure Add_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Index : Positive;
      Edits : in out Edit_Map)
     with Pre => Subp.P_Is_Subprogram
     or else Subp.Kind in Ada_Generic_Subp_Decl_Range;
   --  Adds a parameter defined by 'Data' to position defined by 'Index'.

   procedure Change_Mode
     (Subp               : Basic_Decl'Class;
      Parameters_Indices : Parameter_Indices_Range_Type;
      New_Mode           : Ada_Mode;
      Edits              : in out Edit_Map);
   --  Changes the parameter mode of the parameters defined by
   --  'Parameter_Indices_Range' to 'New_Mode'.

   procedure Move_Left
     (Subp            : Basic_Decl'Class;
      Parameter_Index : Positive;
      Edits           : in out Edit_Map);
   --  Moves the parameter defined by 'Parameter_Index' to the left.

   function Parameters_And_Arguments_Slocs
     (Subp  : Basic_Decl'Class;
      Units : Analysis_Unit_Array)
      return Edit_Map
     with Pre => Subp.P_Is_Subprogram
     or else Subp.Kind in Ada_Generic_Subp_Decl_Range;
   --  Returns a map with the location of the parameters of 'Subp' and the
   --  arguments of its calls.

   function Parameters_And_Arguments_Slocs
     (Target : Params;
      Units  : Analysis_Unit_Array)
      return Edit_Map
   is (Parameters_And_Arguments_Slocs
       (Target.Parent.Parent.As_Basic_Decl, Units));
   --  Returns a map with the location of the parameters of 'Subp' and the
   --  arguments of its calls.

   function Parameters_And_Arguments_Slocs
     (Subp                     : Basic_Decl'Class;
      Parameter_Indices_Ranges : Parameter_Indices_Ranges_Type;
      Units                    : Analysis_Unit_Array)
      return Edit_Map
     with Pre => Subp.P_Is_Subprogram
     or else Subp.Kind in Ada_Generic_Subp_Decl_Range;
   --  Returns a map with the location of the parameters of 'Subp' defined by
   --  'Parameter_Indices_Ranges' and their associated arguments of Subp calls.

   function Parameters_And_Arguments_Slocs
     (Subp            : Basic_Decl'Class;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Edit_Map
     with Pre => Subp.P_Is_Subprogram
     or else Subp.Kind in Ada_Generic_Subp_Decl_Range;
   --  Returns a map with the location of the parameter of 'Subp' defined by
   --  'Parameter_Index' and its associated argument of Subp calls.

   function Parameters_And_Arguments_Slocs
     (Subp              : Basic_Decl'Class;
      Parameter_Indices : Parameter_Indices_Type;
      Units             : Analysis_Unit_Array)
      return Edit_Map
     with Pre => Subp.P_Is_Subprogram
     or else Subp.Kind in Ada_Generic_Subp_Decl_Range;
   --  Returns a map with the location of the parameters of 'Subp' defined by
   --  'Parameter_Indices' and their associated arguments of Subp calls.

   -----------
   -- Image --
   -----------

   function Image (Data : Parameter_Data_Type) return Unbounded_String is
      S : Unbounded_String;
   begin
      Append (S, Data.Name);
      Append (S, " : ");

      if Data.Mode /= Null_Unbounded_String then
         Append (S, Data.Mode & " ");
      end if;

      Append (S, Data.Type_Indication);

      if Data.Default_Expr /= Null_Unbounded_String then
         Append (S, " := " & Data.Default_Expr);
      end if;

      return S;
   end Image;

   --------------------------------
   -- Is_Add_Parameter_Available --
   --------------------------------

   function Is_Add_Parameter_Available
     (Node            : Ada_Node'Class;
      Subp            : out Basic_Decl;
      Parameter_Index : out Positive;
      Requires_Type   : out Boolean)
      return Boolean
   is
      Parent_Defining_Name       : Defining_Name := No_Defining_Name;
      Parent_Defining_Name_List  : Defining_Name_List := No_Defining_Name_List;
      Parent_Params              : Params := No_Params;
      Parent_Subp_Decl           : Basic_Decl := No_Basic_Decl;

      Parameter_Absolute_Index : Positive;

      Total_Parameters : Natural := 0;

      Aux_Node : Ada_Node := Node.As_Ada_Node;

   begin
      Subp := No_Basic_Decl;
      Parameter_Index := 1;
      Requires_Type := False;

      Find_Subp :
      while not Aux_Node.Is_Null loop
         exit Find_Subp when Aux_Node.Kind in Ada_Subp_Spec_Range;

         case Aux_Node.Kind is
            when Ada_Defining_Name =>
               Parent_Defining_Name := Aux_Node.As_Defining_Name;

            when Ada_Defining_Name_List =>
               Parent_Defining_Name_List := Aux_Node.As_Defining_Name_List;

            when Ada_Params =>
               Parent_Params := Aux_Node.As_Params;

            when others => null;
         end case;

         Aux_Node := Aux_Node.Parent;
      end loop Find_Subp;

      if Aux_Node.Is_Null then
         return False;
      end if;

      Assert (Aux_Node.Kind in Ada_Subp_Spec_Range);

      Parent_Subp_Decl := Aux_Node.As_Subp_Spec.P_Parent_Basic_Decl;

      if not Parent_Subp_Decl.P_Is_Subprogram then
         return False;
      end if;

      if Parent_Params /= No_Params
        and then Parent_Defining_Name_List /= No_Defining_Name_List
        and then Parent_Defining_Name /= No_Defining_Name
      then
         Parameter_Absolute_Index :=
           Get_Parameter_Absolute_Index (Parent_Defining_Name);

         Subp := Parent_Subp_Decl;
         Parameter_Index := Parameter_Absolute_Index + 1;
         Requires_Type := False;

         return True;

      elsif Parent_Params = No_Params then
         Subp := Parent_Subp_Decl;
         Parameter_Index := 1;

         Parent_Params := Get_Subp_Params (Parent_Subp_Decl);

         if not Parent_Params.Is_Null then
            Total_Parameters :=
              Count_Subp_Parameters (Parent_Params);
         end if;

         if Total_Parameters = 0 then
            Requires_Type := True;
         else
            Requires_Type := False;
         end if;

         return True;
      end if;

      return False;
   end Is_Add_Parameter_Available;

   ------------------------------
   -- Is_Change_Mode_Available --
   ------------------------------

   function Is_Change_Mode_Available
     (Node                    : Ada_Node'Class;
      Subp                    : out Basic_Decl;
      Parameter_Indices_Range : out Parameter_Indices_Range_Type;
      Mode_Alternatives       : out Mode_Alternatives_Type)
      return Boolean
   is
      Parent_Defining_Name       : Defining_Name := No_Defining_Name;
      Parent_Subptype_Indication : Subtype_Indication := No_Subtype_Indication;
      Parent_Defining_Name_List  : Defining_Name_List := No_Defining_Name_List;
      Parent_Param_Spec          : Param_Spec := No_Param_Spec;
      Parent_Param_Spec_List     : Param_Spec_List := No_Param_Spec_List;
      Parent_Subp_Decl           : Basic_Decl := No_Basic_Decl;

      Parameter_Absolute_Index : Positive;

      Aux_Node : Ada_Node := Node.As_Ada_Node;

   begin
      Subp := No_Basic_Decl;
      Parameter_Indices_Range := (1, 1);
      Mode_Alternatives :=
        (Ada_Mode_Default, Ada_Mode_Default, Ada_Mode_Default);

      Find_Subp :
      while not Aux_Node.Is_Null loop
         exit Find_Subp when Aux_Node.Kind in Ada_Subp_Spec_Range;

         case Aux_Node.Kind is
            when Ada_Defining_Name =>
               Parent_Defining_Name := Aux_Node.As_Defining_Name;

            when Ada_Subtype_Indication =>
               Parent_Subptype_Indication := Aux_Node.As_Subtype_Indication;

            when Ada_Defining_Name_List =>
               Parent_Defining_Name_List := Aux_Node.As_Defining_Name_List;

            when Ada_Param_Spec =>
               Parent_Param_Spec := Aux_Node.As_Param_Spec;

            when Ada_Param_Spec_List =>
               Parent_Param_Spec_List := Aux_Node.As_Param_Spec_List;

            when others => null;
         end case;

         Aux_Node := Aux_Node.Parent;
      end loop Find_Subp;

      if Aux_Node.Is_Null then
         return False;
      end if;

      Parent_Subp_Decl := Aux_Node.As_Subp_Spec.P_Parent_Basic_Decl;

      if not Parent_Subp_Decl.P_Is_Subprogram then
         return False;
      end if;

      if Parent_Defining_Name_List /= No_Defining_Name_List
        and then Parent_Defining_Name /= No_Defining_Name
      then
         Parameter_Absolute_Index :=
           Get_Parameter_Absolute_Index (Parent_Defining_Name);

         Subp := Parent_Subp_Decl;
         Parameter_Indices_Range.First := Parameter_Absolute_Index;
         Parameter_Indices_Range.Last := Parameter_Absolute_Index;
         Mode_Alternatives :=
           Mode_Alternatives_Map (Parent_Param_Spec.F_Mode.Kind);

         return True;

      elsif Parent_Subptype_Indication /= No_Subtype_Indication then
         Subp := Parent_Subp_Decl;

         Parameter_Absolute_Index := 1;

         for Param_Spec of Parent_Param_Spec_List loop
            if Param_Spec = Parent_Param_Spec then
               Parameter_Indices_Range.First := Parameter_Absolute_Index;
               Parameter_Indices_Range.Last :=
                 Parameter_Absolute_Index + Length (Param_Spec.F_Ids) - 1;

            else
               Parameter_Absolute_Index :=
                 Parameter_Absolute_Index + Length (Param_Spec.F_Ids);
            end if;
         end loop;

         Mode_Alternatives :=
           Mode_Alternatives_Map (Parent_Param_Spec.F_Mode.Kind);

         return True;

      else
         return False;
      end if;
   end Is_Change_Mode_Available;

   ---------------------------------
   -- Is_Move_Parameter_Available --
   ---------------------------------

   function Is_Move_Parameter_Available
     (Node            : Ada_Node'Class;
      Subp            : out Basic_Decl;
      Parameter_Index : out Positive;
      Move_Directions : out Move_Direction_Availability_Type)
      return Boolean
   is
      --  Aux_Node must have the following parent nodes in order to guarantee
      --  that Aux_Node is related to a parameter.
      Parent_Defining_Name       : Defining_Name := No_Defining_Name;
      Parent_Defining_Name_List  : Defining_Name_List := No_Defining_Name_List;
      Parent_Param_Spec          : Param_Spec := No_Param_Spec;

      --  Number of parameters the target subprogram has
      Total_Parameters         : Positive;

      --  Auxiliary node to climb the tree
      Aux_Node : Ada_Node := Node.As_Ada_Node;

   begin
      Subp := No_Basic_Decl;
      Parameter_Index := 1;
      Move_Directions := (False, False);

      Find_Subp :
      while not Aux_Node.Is_Null loop
         exit Find_Subp when Aux_Node.Kind in Ada_Subp_Spec_Range;

         case Aux_Node.Kind is
            when Ada_Defining_Name =>
               Parent_Defining_Name := Aux_Node.As_Defining_Name;

            when Ada_Defining_Name_List =>
               Parent_Defining_Name_List := Aux_Node.As_Defining_Name_List;

            when Ada_Param_Spec =>
               Parent_Param_Spec := Aux_Node.As_Param_Spec;

            when others => null;
         end case;

         Aux_Node := Aux_Node.Parent;
      end loop Find_Subp;

      if Aux_Node.Is_Null
        or else Parent_Defining_Name = No_Defining_Name
        or else Parent_Defining_Name_List = No_Defining_Name_List
        or else Parent_Param_Spec = No_Param_Spec
      then
         return False;
      end if;

      Subp := Aux_Node.As_Subp_Spec.P_Parent_Basic_Decl;

      Parameter_Index :=
        Get_Parameter_Absolute_Index (Parent_Defining_Name);

      Total_Parameters :=
        Count_Subp_Parameters (Get_Subp_Params (Subp));

      --  If the target subprogram only has one parameter, then it can't be
      --  moved.

      if Total_Parameters = 1 then
         return False;
      end if;

      Assert (Total_Parameters >= Parameter_Index);

      --  If the target parameter is the first one, then it can only be moved
      --  to the right. If it is the last one, then it can only be moved to
      --  the left. Otherwise, both directions are valid.

      if Parameter_Index = 1 then
         Move_Directions := Only_Right;

      elsif Parameter_Index = Total_Parameters then
         Move_Directions := Only_Left;

      else
         Move_Directions := Both_Directions;
      end if;

      return True;
   end Is_Move_Parameter_Available;

   -----------------------------------
   -- Is_Remove_Parameter_Available --
   -----------------------------------

   function Is_Remove_Parameter_Available
     (Node                    : Ada_Node'Class;
      Subp                    : out Basic_Decl;
      Parameter_Indices_Range : out Parameter_Indices_Range_Type)
      return Boolean
   is
      Parent_Identifier          : Identifier := No_Identifier;
      Parent_Defining_Name       : Defining_Name := No_Defining_Name;
      Parent_Subptype_Indication : Subtype_Indication := No_Subtype_Indication;
      Parent_Defining_Name_List  : Defining_Name_List := No_Defining_Name_List;
      Parent_Param_Spec          : Param_Spec := No_Param_Spec;
      Parent_Param_Spec_List     : Param_Spec_List := No_Param_Spec_List;
      Parent_Subp_Decl           : Basic_Decl := No_Basic_Decl;

      Parameter_Absolute_Index : Positive;

      Total_Parameters : Natural;

      Aux_Node : Ada_Node := Node.As_Ada_Node;

   begin
      Subp := No_Basic_Decl;
      Parameter_Indices_Range := (1, 1);

      Find_Subp :
      while not Aux_Node.Is_Null loop
         exit Find_Subp when Aux_Node.Kind in Ada_Subp_Spec_Range;

         case Aux_Node.Kind is
            when Ada_Identifier =>
               Parent_Identifier := Aux_Node.As_Identifier;

            when Ada_Defining_Name =>
               Parent_Defining_Name := Aux_Node.As_Defining_Name;

            when Ada_Subtype_Indication =>
               Parent_Subptype_Indication := Aux_Node.As_Subtype_Indication;

            when Ada_Defining_Name_List =>
               Parent_Defining_Name_List := Aux_Node.As_Defining_Name_List;

            when Ada_Param_Spec =>
               Parent_Param_Spec := Aux_Node.As_Param_Spec;

            when Ada_Param_Spec_List =>
               Parent_Param_Spec_List := Aux_Node.As_Param_Spec_List;

            when others => null;
         end case;

         Aux_Node := Aux_Node.Parent;
      end loop Find_Subp;

      if Aux_Node.Is_Null then
         return False;
      end if;

      Assert (Aux_Node.Kind in Ada_Subp_Spec_Range);

      Parent_Subp_Decl := Aux_Node.As_Subp_Spec.P_Parent_Basic_Decl;

      if not (Parent_Subp_Decl.P_Is_Subprogram
              or else Parent_Subp_Decl.Kind in Ada_Generic_Subp_Decl_Range)
      then
         return False;
      end if;

      if Parent_Defining_Name_List /= No_Defining_Name_List
        and then Parent_Defining_Name /= No_Defining_Name
      then
         Parameter_Absolute_Index :=
           Get_Parameter_Absolute_Index (Parent_Defining_Name);

         Subp := Parent_Subp_Decl;
         Parameter_Indices_Range.First := Parameter_Absolute_Index;
         Parameter_Indices_Range.Last  := Parameter_Absolute_Index;

         return True;

      elsif Parent_Subptype_Indication /= No_Subtype_Indication then
         Assert (Parent_Param_Spec_List /= No_Param_Spec_List
                 and then Parent_Param_Spec /= No_Param_Spec);

         Subp := Parent_Subp_Decl;

         Parameter_Absolute_Index := 1;

         for Param_Spec of Parent_Param_Spec_List loop
            if Param_Spec = Parent_Param_Spec then
               Parameter_Indices_Range.First := Parameter_Absolute_Index;
               Parameter_Indices_Range.Last :=
                 Parameter_Absolute_Index + Length (Param_Spec.F_Ids) - 1;

            else
               Parameter_Absolute_Index :=
                 Parameter_Absolute_Index + Length (Param_Spec.F_Ids);
            end if;
         end loop;

         return True;

      else
         if Parent_Identifier = No_Identifier then
            return False;
         end if;

         Total_Parameters :=
           Count_Subp_Parameters (Get_Subp_Params (Parent_Subp_Decl));

         if Total_Parameters = 0 then
            return False;

         else
            Subp := Parent_Subp_Decl;
            Parameter_Indices_Range.First := 1;
            Parameter_Indices_Range.Last := Total_Parameters;

            return True;
         end if;
      end if;
   end Is_Remove_Parameter_Available;

   -------------------
   -- Add_Parameter --
   -------------------

   function Add_Parameter
     (Subp            : Basic_Decl;
      New_Parameter   : Parameter_Data_Type;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Edit_Map
   is
      Target_Params : constant Params := Get_Subp_Params (Subp);
      Params_Length : constant Natural :=
        (if Target_Params.Is_Null or else Target_Params = No_Params
         then 0
         else Length (Target_Params.F_Params));

      Edits : Edit_Map;

      procedure Add_Parameter_Callback (Relative_Subp : Basic_Decl'Class);
      --  Determines the necessary changes to 'Relative_Subp' specification,
      --  and adds them to 'Edits'.

      ----------------------------
      -- Add_Parameter_Callback --
      ----------------------------

      procedure Add_Parameter_Callback (Relative_Subp : Basic_Decl'Class) is
         Relative_Subp_Body : constant Base_Subp_Body :=
           Find_Subp_Body (Relative_Subp);

      begin
         if Params_Length = 0 then
            if Relative_Subp.P_Is_Subprogram
              or else Relative_Subp.Kind in Ada_Generic_Subp_Decl_Range
            then
               Add_To_Empty_Params
                 (Relative_Subp, New_Parameter, Edits);

               if not Relative_Subp_Body.Is_Null then
                  Add_To_Empty_Params
                    (Relative_Subp_Body, New_Parameter, Edits);
               end if;
            end if;

         elsif Parameter_Index = 1 then
            if Relative_Subp.P_Is_Subprogram
              or else Relative_Subp.Kind in Ada_Generic_Subp_Decl_Range
            then
               Add_As_First_Parameter
                 (Relative_Subp, New_Parameter, Edits);

               if not Relative_Subp_Body.Is_Null then
                  Add_As_First_Parameter
                    (Relative_Subp_Body, New_Parameter, Edits);
               end if;
            end if;

         elsif Parameter_Index = Params_Length + 1 then
            if Relative_Subp.P_Is_Subprogram
              or else Relative_Subp.Kind in Ada_Generic_Subp_Decl_Range
            then
               Add_As_Last_Parameter
                 (Relative_Subp, New_Parameter, Edits);

               if not Relative_Subp_Body.Is_Null then
                  Add_As_Last_Parameter
                    (Relative_Subp_Body, New_Parameter, Edits);
               end if;
            end if;

         else
            Assert (Parameter_Index in 2 .. Params_Length);

            if Relative_Subp.P_Is_Subprogram
              or else Relative_Subp.Kind in Ada_Generic_Subp_Decl_Range
            then
               Add_Parameter
                 (Relative_Subp,
                  New_Parameter,
                  Parameter_Index,
                  Edits);

               if not Relative_Subp_Body.Is_Null then
                  Add_Parameter
                    (Relative_Subp_Body,
                     New_Parameter,
                     Parameter_Index,
                     Edits);
               end if;
            end if;
         end if;
      end Add_Parameter_Callback;

   begin
      Find_Subp_Relatives
        (Subp           => Subp,
         Units          => Units,
         Decl_Callback  => Add_Parameter_Callback'Access);

      return Edits;
   end Add_Parameter;

   -----------------
   -- Change_Mode --
   -----------------

   function Change_Mode
     (Subp                    : Basic_Decl;
      Parameter_Indices_Range : Parameter_Indices_Range_Type;
      New_Mode                : Ada_Mode;
      Units                   : Analysis_Unit_Array)
      return Edit_Map
   is
      Edits : Edit_Map;

      procedure Change_Mode_Callback (Relative_Subp : Basic_Decl'Class);
      --  Determines the necessary changes to 'Relative_Subp' specification,
      --  and adds them to 'Edits'.

      --------------------------
      -- Change_Mode_Callback --
      --------------------------

      procedure Change_Mode_Callback (Relative_Subp : Basic_Decl'Class) is
         Relative_Subp_Body : constant Base_Subp_Body :=
           Find_Subp_Body (Relative_Subp);

      begin
         if Relative_Subp.P_Is_Subprogram
           or else Relative_Subp.Kind in Ada_Generic_Subp_Decl_Range
         then
            Change_Mode
              (Relative_Subp,
               Parameter_Indices_Range,
               New_Mode,
               Edits);

            if not Relative_Subp_Body.Is_Null then
               Change_Mode
                 (Relative_Subp_Body,
                  Parameter_Indices_Range,
                  New_Mode,
                  Edits);
            end if;
         end if;
      end Change_Mode_Callback;

   begin
      Find_Subp_Relatives
        (Subp           => Subp,
         Units          => Units,
         Decl_Callback  => Change_Mode_Callback'Access);

      return Edits;
   end Change_Mode;

   ---------------
   -- Move_Left --
   ---------------

   function Move_Left
     (Subp            : Basic_Decl;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Edit_Map
   is
      Edits : Edit_Map;

      procedure Move_Parameter_Callback (Relative_Subp : Basic_Decl'Class);
      --  Callback that adds to Edits the necessary edits to 'Relative_Subp'
      --  specification.

      procedure Move_Arguments_Callback  (Call : Call_Stmt);
      --  Callback that adds to Edits the necessary edits to 'Call'

      --------------------------
      -- Change_Mode_Callback --
      --------------------------

      procedure Move_Parameter_Callback (Relative_Subp : Basic_Decl'Class) is
         Relative_Subp_Body : constant Base_Subp_Body :=
           Find_Subp_Body (Relative_Subp);

      begin
         if Relative_Subp.P_Is_Subprogram
           or else Relative_Subp.Kind in Ada_Generic_Subp_Decl_Range
         then
            Move_Left
              (Relative_Subp, Parameter_Index, Edits);

            if not Relative_Subp_Body.Is_Null then
               Move_Left
                 (Relative_Subp_Body, Parameter_Index, Edits);
            end if;
         end if;
      end Move_Parameter_Callback;

      -----------------------------
      -- Move_Arguments_Callback --
      -----------------------------

      procedure Move_Arguments_Callback (Call : Call_Stmt)
      is
         Call_Expression : Call_Expr;
         Call_Name       : Name;

         function Has_Parameter_Association return Boolean;

         procedure Process_Dot_Call;

         procedure Process_Non_Dot_Call (Target_Index : Positive);

         function Has_Parameter_Association return Boolean
         is
            Has_Designator : Boolean;

         begin
            for Param_Assoc of Call_Expression.F_Suffix.As_Assoc_List loop
               Has_Designator :=
                 not Param_Assoc.As_Param_Assoc.F_Designator.Is_Null;

               --  We just need to check the first argument. If it has a
               --  designator, then all the other ones must also have.

               exit;
            end loop;

            return Has_Designator;
         end Has_Parameter_Association;

         procedure Process_Dot_Call is
            Edits_Set : Edit_Ordered_Set;

            Object_Name : Base_Id := No_Base_Id;
            Call_Name   : Base_Id := No_Base_Id;

            Object_Name_Sloc : Source_Location_Range :=
              No_Source_Location_Range;
            Call_Name_Sloc : Source_Location_Range :=
              No_Source_Location_Range;

            Object_Name_Text : Unbounded_String := Null_Unbounded_String;

            Delition_Sloc : Source_Location_Range :=
              No_Source_Location_Range;

            Insertion_Sloc : Source_Location_Range :=
              No_Source_Location_Range;

            First_Parameter_Name : Unbounded_String := Null_Unbounded_String;

            New_Text : Unbounded_String := Null_Unbounded_String;

         begin
            if Parameter_Index = 2 then
               Call_Name :=
                 Call_Expression.F_Name.As_Dotted_Name.F_Suffix;

               if Call_Expression.F_Name.As_Dotted_Name.F_Prefix.Kind =
                 Ada_Dotted_Name
               then
                  Object_Name :=
                    Call_Expression.F_Name.
                      As_Dotted_Name.F_Prefix.As_Dotted_Name.F_Suffix;

               else
                  Object_Name :=
                    Call_Expression.F_Name.
                      As_Dotted_Name.F_Prefix.As_Base_Id;
               end if;

               Object_Name_Sloc := Object_Name.Sloc_Range;
               Call_Name_Sloc := Call_Name.Sloc_Range;

               Object_Name_Text := To_Unbounded_String
                 (Ada.Characters.Conversions.To_String
                    (Object_Name.Text));

               for Param_Assoc of
                 Call_Expression.F_Suffix.As_Assoc_List
               loop
                  Insertion_Sloc := Source_Location_Range'
                    (Param_Assoc.Sloc_Range.End_Line,
                     Param_Assoc.Sloc_Range.End_Line,
                     Param_Assoc.Sloc_Range.End_Column,
                     Param_Assoc.Sloc_Range.End_Column);

                  exit;
               end loop;

               if Has_Parameter_Association then
                  First_Parameter_Name := To_Unbounded_String
                    (Ada.Characters.Conversions.To_String
                       (Get_Parameter_Name (Subp, 1)));

                  New_Text := ", "
                    & First_Parameter_Name
                    & " => "
                    & Object_Name_Text;
               else
                  New_Text := ", " & Object_Name_Text;
               end if;

               Delition_Sloc := Source_Location_Range'
                 (Object_Name_Sloc.Start_Line,
                  Call_Name_Sloc.Start_Line,
                  Object_Name_Sloc.Start_Column,
                  Call_Name_Sloc.Start_Column);

               Edits_Set.Insert
                 ((Delition_Sloc, Null_Unbounded_String));
               Edits_Set.Insert
                 ((Insertion_Sloc, New_Text));

            elsif not Has_Parameter_Association then
               Process_Non_Dot_Call (Parameter_Index - 1);
            end if;
         end Process_Dot_Call;

         procedure Process_Non_Dot_Call (Target_Index : Positive) is
            Arg_A : Param_Assoc := No_Param_Assoc;
            Arg_B : Param_Assoc := No_Param_Assoc;

            Arg_A_Sloc : Source_Location_Range := No_Source_Location_Range;
            Arg_B_Sloc : Source_Location_Range := No_Source_Location_Range;

            Arg_A_Text : Unbounded_String := Null_Unbounded_String;
            Arg_B_Text : Unbounded_String := Null_Unbounded_String;

            Param_Assoc_Index : Positive := 1;

            Edits_Set : Edit_Ordered_Set;

         begin
            for Param_Assoc of Call_Expression.F_Suffix.As_Assoc_List loop
               if Param_Assoc_Index = Target_Index - 1 then
                  Arg_A := Param_Assoc.As_Param_Assoc;

               elsif Param_Assoc_Index = Target_Index then
                  Arg_B := Param_Assoc.As_Param_Assoc;

                  exit;
               end if;

               Param_Assoc_Index := Param_Assoc_Index + 1;
            end loop;

            Assert (not Arg_A.Is_Null and not Arg_B.Is_Null);

            Arg_A_Sloc := Arg_A.Sloc_Range;
            Arg_B_Sloc := Arg_B.Sloc_Range;

            Arg_A_Text := To_Unbounded_String
              (Ada.Characters.Conversions.To_String (Arg_A.Text));
            Arg_B_Text := To_Unbounded_String
              (Ada.Characters.Conversions.To_String (Arg_B.Text));

            Edits_Set.Insert ((Arg_B_Sloc, Arg_A_Text));
            Edits_Set.Insert ((Arg_A_Sloc, Arg_B_Text));

            for Edit of Edits_Set loop
               Insert (Edits,
                       Call_Expression.Unit.Get_Filename,
                       Edit.Location,
                       Edit.Text);
            end loop;
         end Process_Non_Dot_Call;

      begin
         if Call.F_Call.Is_Null then
            return;
         end if;

         Call_Expression := Call.F_Call.As_Call_Expr;

         if Call_Expression.Is_Null then
            return;
         end if;

         Call_Name := Call_Expression.F_Name;

         case Call_Name.P_Is_Dot_Call is
            when True =>
               Process_Dot_Call;

            when False =>
               if not Has_Parameter_Association then
                  Process_Non_Dot_Call (Parameter_Index);
               end if;

         end case;
      end Move_Arguments_Callback;

   begin
      Find_Subp_Relatives
        (Subp           => Subp,
         Units          => Units,
         Decl_Callback  => Move_Parameter_Callback'Access,
         Find_Calls     => True,
         Calls_Callback => Move_Arguments_Callback'Access);

      return Edits;
   end Move_Left;

   ----------------------
   -- Remove_Parameter --
   ----------------------

   function Remove_Parameter
     (Subp            : Basic_Decl;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Edit_Map
   is (Parameters_And_Arguments_Slocs (Subp, Parameter_Index, Units));

   ----------------------
   -- Remove_Parameter --
   ----------------------

   function Remove_Parameter
     (Subp              : Basic_Decl;
      Parameter_Indices : Parameter_Indices_Type;
      Units             : Analysis_Unit_Array)
      return Edit_Map
   is (Parameters_And_Arguments_Slocs (Subp, Parameter_Indices, Units));

   ----------------------
   -- Remove_Parameter --
   ----------------------

   function Remove_Parameter
     (Subp                    : Basic_Decl;
      Parameter_Indices_Range : Parameter_Indices_Range_Type;
      Units                   : Analysis_Unit_Array)
      return Edit_Map
   is
      Edits : Edit_Map;
      Parameter_Indices_Ranges : constant
        Parameter_Indices_Ranges_Type (1 .. 1) :=
          (1 => Parameter_Indices_Range);

   begin
      if Parameter_Indices_Range.First = 1
        and then Parameter_Indices_Range.Last =
          Count_Subp_Parameters (Get_Subp_Params (Subp))
      then
         --  Remove all parameters

         Edits := Parameters_And_Arguments_Slocs
           (Get_Subp_Params (Subp.P_Canonical_Part), Units);

      else
         Edits := Parameters_And_Arguments_Slocs
           (Subp.P_Canonical_Part, Parameter_Indices_Ranges, Units);
      end if;

      return Edits;
   end Remove_Parameter;

   ----------------------
   -- Remove_Parameter --
   ----------------------

   function Remove_Parameter
     (Subp                     : Basic_Decl;
      Parameter_Indices_Ranges : Parameter_Indices_Ranges_Type;
      Units                    : Analysis_Unit_Array)
      return Edit_Map
   is
      Edits : Edit_Map;

   begin
      for Indices_Range of Parameter_Indices_Ranges loop
         Merge (Edits, Remove_Parameter (Subp, Indices_Range, Units));
      end loop;

      return Edits;
   end Remove_Parameter;

   ------------------------------------
   -- Parameters_And_Arguments_Slocs --
   ------------------------------------

   function Parameters_And_Arguments_Slocs
     (Subp  : Basic_Decl'Class;
      Units : Analysis_Unit_Array)
      return Edit_Map
   is
      use Source_Location_Range_Maps;

      Slocs : constant Source_Location_Range_Map :=
        Params_Slocs (Subp, Units);
      Slocs_Cursor : Cursor :=
        Slocs.First;

   begin
      return Edits : Edit_Map do
         while Has_Element (Slocs_Cursor) loop
            declare
               Edits_Set : Edit_Ordered_Set;
            begin
               for Sloc of Slocs.Element (Key (Slocs_Cursor)) loop
                  Edits_Set.Insert ((Sloc, To_Unbounded_String ("")));
               end loop;

               Edits.Insert (Key (Slocs_Cursor), Edits_Set);
            end;

            Next (Slocs_Cursor);
         end loop;
      end return;
   end Parameters_And_Arguments_Slocs;

   ------------------------------------
   -- Parameters_And_Arguments_Slocs --
   ------------------------------------

   function Parameters_And_Arguments_Slocs
     (Subp            : Basic_Decl'Class;
      Parameter_Index : Positive;
      Units           : Analysis_Unit_Array)
      return Edit_Map
   is
      use Source_Location_Range_Maps;

      Slocs : constant Source_Location_Range_Map :=
        Parameters_Slocs (Subp, Parameter_Index, Units);
      Slocs_Cursor : Cursor :=
        Slocs.First;

   begin
      return Edits : Edit_Map do
         while Has_Element (Slocs_Cursor) loop
            declare
               Edits_Set : Edit_Ordered_Set;
            begin
               for Sloc of Slocs.Element (Key (Slocs_Cursor)) loop
                  Edits_Set.Insert ((Sloc, To_Unbounded_String ("")));
               end loop;

               Edits.Insert (Key (Slocs_Cursor), Edits_Set);
            end;

            Next (Slocs_Cursor);
         end loop;
      end return;
   end Parameters_And_Arguments_Slocs;

   ------------------------------------
   -- Parameters_And_Arguments_Slocs --
   ------------------------------------

   function Parameters_And_Arguments_Slocs
     (Subp              : Basic_Decl'Class;
      Parameter_Indices : Parameter_Indices_Type;
      Units             : Analysis_Unit_Array)
      return Edit_Map
   is
      use Source_Location_Range_Maps;

      Slocs : constant Source_Location_Range_Map :=
        Parameters_Slocs (Subp, Parameter_Indices, Units);
      Slocs_Cursor : Cursor :=
        Slocs.First;

   begin
      return Edits : Edit_Map do
         while Has_Element (Slocs_Cursor) loop
            declare
               Edits_Set : Edit_Ordered_Set;
            begin
               for Sloc of Slocs.Element (Key (Slocs_Cursor)) loop
                  Edits_Set.Insert ((Sloc, To_Unbounded_String ("")));
               end loop;

               Edits.Insert (Key (Slocs_Cursor), Edits_Set);
            end;

            Next (Slocs_Cursor);
         end loop;
      end return;
   end Parameters_And_Arguments_Slocs;

   ------------------------------------
   -- Parameters_And_Arguments_Slocs --
   ------------------------------------

   function Parameters_And_Arguments_Slocs
     (Subp                     : Basic_Decl'Class;
      Parameter_Indices_Ranges : Parameter_Indices_Ranges_Type;
      Units                    : Analysis_Unit_Array)
      return Edit_Map
   is
      use Source_Location_Range_Maps;

      Slocs : constant Source_Location_Range_Map :=
        Parameters_And_Arguments_Slocs (Subp, Parameter_Indices_Ranges, Units);
      Slocs_Cursor : Cursor :=
        Slocs.First;

   begin
      return Edits : Edit_Map do
         while Has_Element (Slocs_Cursor) loop
            declare
               Edits_Set : Edit_Ordered_Set;
            begin
               for Sloc of Slocs.Element (Key (Slocs_Cursor)) loop
                  Edits_Set.Insert ((Sloc, To_Unbounded_String ("")));
               end loop;

               Edits.Insert (Key (Slocs_Cursor), Edits_Set);
            end;

            Next (Slocs_Cursor);
         end loop;
      end return;
   end Parameters_And_Arguments_Slocs;

   ------------
   -- Create --
   ------------

   function Create
     (Target                  : Basic_Decl;
      Parameter_Indices_Range : Parameter_Indices_Range_Type;
      Configuration           : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Parameter_Remover
   is (Parameter_Remover'(Target, Parameter_Indices_Range, Configuration));

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self : Parameter_Remover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Edit_Map is
   begin
      return Remove_Parameter
        (Subp                    => Self.Subp,
         Parameter_Indices_Range => Self.Parameter_Indices_Range,
         Units                   => Analysis_Units.all);
   end Refactor;

   ------------
   -- Create --
   ------------

   function Create
     (Target         : Basic_Decl;
      New_Parameter  : Parameter_Data_Type;
      Index          : Positive;
      Configuration  : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Parameter_Adder is
   begin
      return Parameter_Adder'
        (Subp            => Target,
         New_Parameter   => New_Parameter,
         Parameter_Index => Index,
         Configuration   => Configuration);
   end Create;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Parameter_Adder;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Edit_Map is
   begin
      return Add_Parameter
        (Self.Subp,
         Self.New_Parameter,
         Self.Parameter_Index,
         Analysis_Units.all);
   end Refactor;

   -------------------------
   -- Add_To_Empty_Params --
   -------------------------

   procedure Add_To_Empty_Params
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Edit_Map)
   is
      Definition_Sloc : constant Source_Location_Range :=
        Subp.P_Defining_Name.Sloc_Range;

   begin
      Insert
        (Edits    => Edits,
         Filename => Subp.Unit.Get_Filename,
         Location => Source_Location_Range'
           (Start_Line   => Definition_Sloc.End_Line,
            End_Line     => Definition_Sloc.End_Line,
            Start_Column => Definition_Sloc.End_Column,
            End_Column   => Definition_Sloc.End_Column),
         Text     => " (" & Image (Data) & ")");
   end Add_To_Empty_Params;

   ----------------------------
   -- Add_As_First_Parameter --
   ----------------------------

   procedure Add_As_First_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Edit_Map)
   is
      Target_Params_List_Sloc : constant Source_Location_Range :=
        Get_Subp_Params (Subp).F_Params.Sloc_Range;
   begin
      if Data.Type_Indication /= Null_Unbounded_String then
         Insert
           (Edits    => Edits,
            Filename => Subp.Unit.Get_Filename,
            Location => Source_Location_Range'
              (Start_Line   => Target_Params_List_Sloc.Start_Line,
               End_Line     => Target_Params_List_Sloc.Start_Line,
               Start_Column => Target_Params_List_Sloc.Start_Column,
               End_Column   => Target_Params_List_Sloc.Start_Column),
            Text     => Image (Data) & "; ");
      end if;
   end Add_As_First_Parameter;

   ---------------------------
   -- Add_As_Last_Parameter --
   ---------------------------

   procedure Add_As_Last_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Edits : in out Edit_Map)
   is
      Target_Params_List_Sloc : constant Source_Location_Range :=
        Get_Subp_Params (Subp).F_Params.Sloc_Range;

   begin
      Insert
        (Edits    => Edits,
         Filename => Subp.Unit.Get_Filename,
         Location => Source_Location_Range'
           (Start_Line   => Target_Params_List_Sloc.End_Line,
            End_Line     => Target_Params_List_Sloc.End_Line,
            Start_Column => Target_Params_List_Sloc.End_Column,
            End_Column   => Target_Params_List_Sloc.End_Column),
         Text     => "; " & Image (Data));
   end Add_As_Last_Parameter;

   -------------------
   -- Add_Parameter --
   -------------------

   procedure Add_Parameter
     (Subp  : Basic_Decl'Class;
      Data  : Parameter_Data_Type;
      Index : Positive;
      Edits : in out Edit_Map)
   is
      Current_Parameter_Index : Positive := 1;
      Param_Spec_Ids_Length : Positive;

   begin
      for Param_Spec of Get_Subp_Params (Subp).F_Params loop
         Param_Spec_Ids_Length := Length (Param_Spec.F_Ids);

         if Index = Current_Parameter_Index then
            Insert
              (Edits    => Edits,
               Filename => Subp.Unit.Get_Filename,
               Location => Source_Location_Range'
                 (Start_Line   => Param_Spec.Sloc_Range.Start_Line,
                  End_Line     => Param_Spec.Sloc_Range.Start_Line,
                  Start_Column => Param_Spec.Sloc_Range.Start_Column,
                  End_Column   => Param_Spec.Sloc_Range.Start_Column),
               Text     => Image (Data) & "; ");
            exit;

         elsif Index in
           Current_Parameter_Index + 1 ..
             Current_Parameter_Index + Param_Spec_Ids_Length - 1
         then
            declare
               Last_Parameter        : Defining_Name :=
                 No_Defining_Name;
               Before_Last_Parameter : Defining_Name :=
                 No_Defining_Name;

               Mode_Text    : constant String :=
                 To_String (Param_Spec.F_Mode.Text);
               Subtype_Text : constant String :=
                 To_String (Param_Spec.F_Type_Expr.Text);
            begin
               for Param of Param_Spec.F_Ids loop
                  if Current_Parameter_Index = Index - 1 then
                     Before_Last_Parameter := Param.As_Defining_Name;

                  elsif Current_Parameter_Index = Index then
                     Last_Parameter := Param.As_Defining_Name;
                     exit;
                  end if;

                  Current_Parameter_Index := Current_Parameter_Index + 1;
               end loop;

               Insert
                 (Edits    => Edits,
                  Filename => Subp.Unit.Get_Filename,
                  Location =>
                    (Before_Last_Parameter.Sloc_Range.End_Line,
                     Last_Parameter.Sloc_Range.Start_Line,
                     Before_Last_Parameter.Sloc_Range.End_Column,
                     Last_Parameter.Sloc_Range.Start_Column),
                  Text     =>
                    To_Unbounded_String
                      (" : " & Mode_Text & " " & Subtype_Text & "; ")
                  & Image (Data) & "; ");
            end;
            exit;

         end if;

         Current_Parameter_Index :=
           Current_Parameter_Index + Param_Spec_Ids_Length;
      end loop;
   end Add_Parameter;

   ------------
   -- Create --
   ------------

   function Create
     (Target          : Basic_Decl;
      Parameter_Index : Natural;
      New_Mode        : Ada_Mode;
      Configuration   : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Mode_Changer
   is
      Parameter_Indices_Range : constant Parameter_Indices_Range_Type :=
        (Parameter_Index, Parameter_Index);
   begin
      return Mode_Changer'
        (Subp                    => Target,
         Parameter_Indices_Range => Parameter_Indices_Range,
         New_Mode                => New_Mode,
         Configuration           => Configuration);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Target                  : Basic_Decl;
      Parameter_Indices_Range : Parameter_Indices_Range_Type;
      New_Mode                : Ada_Mode;
      Configuration           : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Mode_Changer is
   begin
      return Mode_Changer'
        (Subp                    => Target,
         Parameter_Indices_Range => Parameter_Indices_Range,
         New_Mode                => New_Mode,
         Configuration           => Configuration);
   end Create;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self : Mode_Changer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Edit_Map is
   begin
      return Change_Mode
        (Self.Subp,
         Self.Parameter_Indices_Range,
         Self.New_Mode,
         Analysis_Units.all);
   end Refactor;

   -----------------
   -- Change_Mode --
   -----------------

   procedure Change_Mode
     (Subp               : Basic_Decl'Class;
      Parameters_Indices : Parameter_Indices_Range_Type;
      New_Mode           : Ada_Mode;
      Edits              : in out Edit_Map)
   is
      Current_Parameter_Index  : Positive := 1;
      First_Parameter_Index    : Positive :=
        Parameters_Indices.First;
      Last_Parameter_Index     : constant Positive :=
        Parameters_Indices.Last;
      N_Of_Parameters_Left     : Natural :=
        Last_Parameter_Index - First_Parameter_Index + 1;

      Param_Spec_Length               : Positive;
      Param_Spec_Last_Parameter_Index : Positive;

      New_Mode_Text : Unbounded_String;

   begin
      case New_Mode is
         when Ada_Mode_Default =>
            New_Mode_Text := To_Unbounded_String ("");

         when Ada_Mode_In =>
            New_Mode_Text := To_Unbounded_String ("in");

         when Ada_Mode_Out =>
            New_Mode_Text := To_Unbounded_String ("out");

         when Ada_Mode_In_Out =>
            New_Mode_Text := To_Unbounded_String ("in out");
      end case;

      for Param_Spec of Get_Subp_Params (Subp).F_Params loop

         exit when N_Of_Parameters_Left = 0;

         Param_Spec_Length := Length (Param_Spec.F_Ids);
         Param_Spec_Last_Parameter_Index :=
           Current_Parameter_Index + Param_Spec_Length - 1;

         if First_Parameter_Index = Current_Parameter_Index then
            if Param_Spec_Last_Parameter_Index < Last_Parameter_Index then
               --  Case 1: Checked

               Insert
                 (Edits    => Edits,
                  Filename => Subp.Unit.Get_Filename,
                  Location => Source_Location_Range'
                    (Start_Line   => Param_Spec.F_Mode.Sloc_Range.Start_Line,
                     End_Line     => Param_Spec.F_Mode.Sloc_Range.End_Line,
                     Start_Column => Param_Spec.F_Mode.Sloc_Range.Start_Column,
                     End_Column   => Param_Spec.F_Mode.Sloc_Range.End_Column),
                  Text     => New_Mode_Text);

               First_Parameter_Index := Param_Spec_Last_Parameter_Index + 1;
               N_Of_Parameters_Left :=
                 N_Of_Parameters_Left - Param_Spec_Length;

            elsif Param_Spec_Last_Parameter_Index = Last_Parameter_Index then
               --  Case 2: Checked

               if To_String (Param_Spec.F_Mode.Text) /= New_Mode_Text then
                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Start_Line   =>
                            Param_Spec.F_Mode.Sloc_Range.Start_Line,
                        End_Line     =>
                          Param_Spec.F_Mode.Sloc_Range.End_Line,
                        Start_Column =>
                          Param_Spec.F_Mode.Sloc_Range.Start_Column,
                        End_Column   =>
                          Param_Spec.F_Mode.Sloc_Range.End_Column),
                     Text     => New_Mode_Text);
               end if;

               N_Of_Parameters_Left := 0;

               exit;

            else
               --  Case 3: Checked

               declare
                  Parameter_A  : Defining_Name := No_Defining_Name;
                  Parameter_B  : Defining_Name := No_Defining_Name;
                  Subtype_Text : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index = Last_Parameter_Index then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index =
                       Last_Parameter_Index + 1
                     then
                        Parameter_B := Parameter.As_Defining_Name;
                        exit;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Start_Line   => Parameter_A.Sloc_Range.End_Line,
                        End_Line     => Parameter_B.Sloc_Range.Start_Line,
                        Start_Column => Parameter_A.Sloc_Range.End_Column,
                        End_Column   => Parameter_B.Sloc_Range.Start_Column),
                     Text     =>
                       " : " & New_Mode_Text & " " & Subtype_Text & ";");
               end;

               N_Of_Parameters_Left := 0;

               exit;

            end if;

         elsif First_Parameter_Index in
           Current_Parameter_Index + 1 .. Param_Spec_Last_Parameter_Index
         then
            if First_Parameter_Index = Param_Spec_Last_Parameter_Index then
               --  Case 4: Checked

               declare
                  Parameter_A  : Defining_Name := No_Defining_Name;
                  Parameter_B  : Defining_Name := No_Defining_Name;

                  Parameter_B_Text : Unbounded_String;
                  Subtype_Text     : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index =
                       First_Parameter_Index - 1
                     then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index = First_Parameter_Index then
                        Parameter_B := Parameter.As_Defining_Name;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Start_Line   => Parameter_A.Sloc_Range.End_Line,
                        End_Line     => Parameter_B.Sloc_Range.End_Line,
                        Start_Column => Parameter_A.Sloc_Range.End_Column,
                        End_Column   => Parameter_B.Sloc_Range.End_Column),
                     Text     => To_Unbounded_String (""));

                  Parameter_B_Text := To_Unbounded_String
                    (To_String (Parameter_B.Text));
                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Start_Line   => Param_Spec.Sloc_Range.End_Line,
                        End_Line     => Param_Spec.Sloc_Range.End_Line,
                        Start_Column => Param_Spec.Sloc_Range.End_Column,
                        End_Column   => Param_Spec.Sloc_Range.End_Column),
                     Text     =>
                       "; " & Parameter_B_Text & " : " & New_Mode_Text
                     & " " & Subtype_Text);
               end;

               First_Parameter_Index := Current_Parameter_Index;
               N_Of_Parameters_Left := N_Of_Parameters_Left - 1;

            elsif Last_Parameter_Index > Param_Spec_Last_Parameter_Index then
               --  Case 5: Checked

               declare
                  Parameter_A  : Defining_Name := No_Defining_Name;
                  Parameter_B  : Defining_Name := No_Defining_Name;

                  Mode_Text       : constant String :=
                    To_String (Param_Spec.F_Mode.Text);
                  Subtype_Text     : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index =
                       First_Parameter_Index - 1
                     then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index = First_Parameter_Index then
                        Parameter_B := Parameter.As_Defining_Name;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  if To_String (Param_Spec.F_Mode.Text) /= New_Mode_Text then
                     Insert
                       (Edits    => Edits,
                        Filename => Subp.Unit.Get_Filename,
                        Location => Param_Spec.F_Mode.Sloc_Range,
                        Text     => New_Mode_Text);
                  end if;

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Start_Line   => Parameter_A.Sloc_Range.End_Line,
                        End_Line     => Parameter_B.Sloc_Range.Start_Line,
                        Start_Column => Parameter_A.Sloc_Range.End_Column,
                        End_Column   => Parameter_B.Sloc_Range.Start_Column),
                     Text     =>
                       To_Unbounded_String
                         (" : " & Mode_Text & " " & Subtype_Text & "; "));
               end;

               N_Of_Parameters_Left :=
                 N_Of_Parameters_Left -
                   ((Current_Parameter_Index - 1) - First_Parameter_Index);
               First_Parameter_Index := Current_Parameter_Index;

            elsif Last_Parameter_Index = Param_Spec_Last_Parameter_Index then
               --  Case 6: Checked

               declare
                  Parameter_A  : Defining_Name := No_Defining_Name;
                  Parameter_B  : Defining_Name := No_Defining_Name;

                  Mode_Text    : constant String :=
                    To_String (Param_Spec.F_Mode.Text);
                  Subtype_Text : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index =
                       First_Parameter_Index - 1
                     then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index = First_Parameter_Index then
                        Parameter_B := Parameter.As_Defining_Name;
                        exit;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  if To_String (Param_Spec.F_Mode.Text) /= New_Mode_Text then
                     Insert
                       (Edits    => Edits,
                        Filename => Subp.Unit.Get_Filename,
                        Location => Param_Spec.F_Mode.Sloc_Range,
                        Text     => New_Mode_Text);
                  end if;

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Start_Line   => Parameter_A.Sloc_Range.End_Line,
                        End_Line     => Parameter_B.Sloc_Range.Start_Line,
                        Start_Column => Parameter_A.Sloc_Range.End_Column,
                        End_Column   => Parameter_B.Sloc_Range.Start_Column),
                     Text     =>
                       To_Unbounded_String
                         (" : " & Mode_Text & " " & Subtype_Text & "; "));
               end;

               N_Of_Parameters_Left := 0;

               exit;

            else
               --  Case 7: Checked

               Assert (Last_Parameter_Index < Param_Spec_Last_Parameter_Index);

               declare
                  Parameter_A  : Defining_Name := No_Defining_Name;
                  Parameter_B  : Defining_Name := No_Defining_Name;
                  Parameter_C  : Defining_Name := No_Defining_Name;
                  Parameter_D  : Defining_Name := No_Defining_Name;

                  Mode_Text    : constant String :=
                    To_String  (Param_Spec.F_Mode.Text);
                  Subtype_Text : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index =
                       First_Parameter_Index - 1
                     then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index = First_Parameter_Index then
                        Parameter_B := Parameter.As_Defining_Name;
                     end if;

                     if Current_Parameter_Index = Last_Parameter_Index
                     then
                        Parameter_C := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index =
                       Last_Parameter_Index + 1
                     then
                        Parameter_D := Parameter.As_Defining_Name;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Start_Line   => Parameter_A.Sloc_Range.End_Line,
                        End_Line     => Parameter_B.Sloc_Range.Start_Line,
                        Start_Column => Parameter_A.Sloc_Range.End_Column,
                        End_Column   => Parameter_B.Sloc_Range.Start_Column),
                     Text     =>
                       To_Unbounded_String
                         (" : " & Mode_Text & " " & Subtype_Text & "; "));

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Start_Line   => Parameter_C.Sloc_Range.End_Line,
                        End_Line     => Parameter_D.Sloc_Range.Start_Line,
                        Start_Column => Parameter_C.Sloc_Range.End_Column,
                        End_Column   => Parameter_D.Sloc_Range.Start_Column),
                     Text     =>
                       " : " & New_Mode_Text & " " & Subtype_Text & "; ");
               end;

               N_Of_Parameters_Left := 0;

               exit;

            end if;
         end if;

         Current_Parameter_Index := Param_Spec_Last_Parameter_Index + 1;
      end loop;
   end Change_Mode;

   ------------
   -- Create --
   ------------

   function Create
     (Target          : Basic_Decl;
      Parameter_Index : Natural;
      Configuration   : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Left_Mover
   is
   begin
      return Left_Mover'
        (Subp            => Target,
         Parameter_Index => Parameter_Index,
         Configuration   => Configuration);
   end Create;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self : Left_Mover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Edit_Map is
   begin
      return Move_Left (Self.Subp, Self.Parameter_Index, Analysis_Units.all);
   end Refactor;

   ----------
   -- Move --
   ----------

   procedure Move_Left
     (Subp            : Basic_Decl'Class;
      Parameter_Index : Positive;
      Edits           : in out Edit_Map)
   is
      Param_Spec_Length          : Positive;
      Current_Parameter_Index    : Positive := 1;
      Previous_Param_Spec        : Param_Spec := No_Param_Spec;
      Previous_Param_Spec_Length : Positive := 1;

   begin
      for Param_Spec of Get_Subp_Params (Subp).F_Params loop
         Param_Spec_Length := Length (Param_Spec.F_Ids);

         if Parameter_Index = Current_Parameter_Index
           and then Param_Spec_Length = 1
         then
            if Previous_Param_Spec_Length = 1 then
               --  Case 1:
               --
               --  Parameter B is the only one of the Param_Spec, and the
               --  parameter on his left is alsoi the only one of the
               --  Previous_Param_Spec.
               --
               --  A : in Integer; B : out Float
               --  --------------  -------------
               --
               --  B : out Float; A : in Integer
               --  +++++++++++++  ++++++++++++++

               Insert
                 (Edits    => Edits,
                  Filename => Subp.Unit.Get_Filename,
                  Location => Param_Spec.Sloc_Range,
                  Text     => To_Unbounded_String
                    (To_String (Previous_Param_Spec.Text)));

               Insert
                 (Edits    => Edits,
                  Filename => Subp.Unit.Get_Filename,
                  Location => Previous_Param_Spec.Sloc_Range,
                  Text     => To_Unbounded_String
                    (To_String (Param_Spec.Text)));

            else
               --  Case 2:
               --
               --  Parameter C is the only one of the Param_Spec, and the
               --  parameter on his left is not the only on the the
               --  Previous_Param_Spec
               --
               --  A, B : in Integer; C : out Float
               --   ---
               --
               --  A : in Integer; C : out Float; B : in Integer
               --                               ++++++++++++++++

               declare
                  Parameter_A : Defining_Name := No_Defining_Name;
                  Parameter_B : Defining_Name := No_Defining_Name;

                  Mode_Text : constant String :=
                    To_String (Previous_Param_Spec.F_Mode.Text);
                  Type_Text : constant String :=
                    To_String (Previous_Param_Spec.F_Type_Expr.Text);

                  New_Text : Unbounded_String;

                  Relative_Index : Positive := 1;

               begin
                  for Parameter of Previous_Param_Spec.F_Ids loop
                     if Relative_Index = Previous_Param_Spec_Length - 1 then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Relative_Index = Previous_Param_Spec_Length then
                        Parameter_B := Parameter.As_Defining_Name;
                     end if;

                     Relative_Index := Relative_Index + 1;
                  end loop;

                  New_Text := To_Unbounded_String
                    ("; "
                     & To_String (Parameter_B.Text)
                     & " : "
                     & Mode_Text
                     & " "
                     & Type_Text);

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Parameter_A.Sloc_Range.End_Line,
                        Parameter_B.Sloc_Range.End_Line,
                        Parameter_A.Sloc_Range.End_Column,
                        Parameter_B.Sloc_Range.End_Column),
                     Text     => To_Unbounded_String (""));

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Param_Spec.Sloc_Range.End_Line,
                        Param_Spec.Sloc_Range.End_Line,
                        Param_Spec.Sloc_Range.End_Column,
                        Param_Spec.Sloc_Range.End_Column),
                     Text     => New_Text);
               end;
            end if;

            exit;

         elsif Parameter_Index = Current_Parameter_Index
           and then Param_Spec_Length > 1
         then
            if Previous_Param_Spec_Length = 1 then
               --  Case 3 :
               --
               --  Parameter B is the first one of Param_Spec, and the
               --  parameter on his left is the only one of the previous
               --  Param_Spec.
               --
               --  A : in Integer; B, C : out Float
               --                  ---
               --
               --  B : out Float; A : in Integer; C : out Float
               --  +++++++++++++++
               --

               declare
                  Parameter_B : Defining_Name := No_Defining_Name;
                  Parameter_C : Defining_Name := No_Defining_Name;

                  Mode_Text : constant String :=
                    To_String (Param_Spec.F_Mode.Text);
                  Type_Text : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

                  New_Text : Unbounded_String;

               begin
                  for Parameter of Param_Spec.F_Ids loop
                     if Current_Parameter_Index = Parameter_Index then
                        Parameter_B := Parameter.As_Defining_Name;

                     elsif Current_Parameter_Index =
                       Parameter_Index + 1
                     then
                        Parameter_C := Parameter.As_Defining_Name;
                     end if;

                     Current_Parameter_Index := Current_Parameter_Index + 1;
                  end loop;

                  Assert (Parameter_B /= No_Defining_Name
                          and then Parameter_C /= No_Defining_Name);

                  New_Text := To_Unbounded_String
                    (To_String (Parameter_B.Text)
                     & " : "
                     & Mode_Text
                     & " "
                     & Type_Text
                     & "; ");

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Parameter_B.Sloc_Range.Start_Line,
                        Parameter_C.Sloc_Range.Start_Line,
                        Parameter_B.Sloc_Range.Start_Column,
                        Parameter_C.Sloc_Range.Start_Column),
                     Text     => To_Unbounded_String (""));

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Previous_Param_Spec.Sloc_Range.Start_Line,
                        Previous_Param_Spec.Sloc_Range.Start_Line,
                        Previous_Param_Spec.Sloc_Range.Start_Column,
                        Previous_Param_Spec.Sloc_Range.Start_Column),
                     Text     => New_Text);
               end;

            else
               --  Case 4
               --
               --  Parameter C is the first one of Param_Spec, and the
               --  parameter on his left is not the only one of the previous
               --  Param_Spec.
               --
               --  A, B : in Integer; C, D : out Float
               --   --                ---
               --
               --  A : in Integer; C : out Float; B : in Integer; D : out Float
               --   ++++++++++++++++++++++++++++++
               --

               declare
                  Parameter_A : Defining_Name := No_Defining_Name;
                  Parameter_B : Defining_Name := No_Defining_Name;
                  Parameter_C : Defining_Name := No_Defining_Name;
                  Parameter_D : Defining_Name := No_Defining_Name;

                  First_Mode_Text : constant String :=
                    To_String (Previous_Param_Spec.F_Mode.Text);
                  First_Type_Text : constant String :=
                    To_String (Previous_Param_Spec.F_Type_Expr.Text);
                  Second_Mode_Text : constant String :=
                    To_String (Param_Spec.F_Mode.Text);
                  Second_Type_Text : constant String :=
                    To_String (Param_Spec.F_Type_Expr.Text);

                  New_Text : Unbounded_String;

                  Relative_Index : Positive := 1;

               begin
                  for Parameter of Previous_Param_Spec.F_Ids loop
                     if Relative_Index = Previous_Param_Spec_Length - 1 then
                        Parameter_A := Parameter.As_Defining_Name;

                     elsif Relative_Index = Previous_Param_Spec_Length then
                        Parameter_B := Parameter.As_Defining_Name;
                     end if;

                     Relative_Index := Relative_Index + 1;
                  end loop;

                  Relative_Index := 1;

                  for Parameter of Param_Spec.F_Ids loop
                     if Relative_Index = 1 then
                        Parameter_C := Parameter.As_Defining_Name;

                     elsif Relative_Index = 2 then
                        Parameter_D := Parameter.As_Defining_Name;
                     end if;

                     Relative_Index := Relative_Index + 1;
                  end loop;

                  New_Text := To_Unbounded_String
                    (" : "
                     & First_Mode_Text
                     & " "
                     & First_Type_Text
                     & "; "
                     & To_String (Parameter_C.Text)
                     & " : "
                     & Second_Mode_Text
                     & " "
                     & Second_Type_Text
                     & "; ");

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Parameter_C.Sloc_Range.Start_Line,
                        Parameter_D.Sloc_Range.Start_Line,
                        Parameter_C.Sloc_Range.Start_Column,
                        Parameter_D.Sloc_Range.Start_Column),
                     Text     => To_Unbounded_String (""));

                  Insert
                    (Edits    => Edits,
                     Filename => Subp.Unit.Get_Filename,
                     Location => Source_Location_Range'
                       (Parameter_A.Sloc_Range.End_Line,
                        Parameter_B.Sloc_Range.Start_Line,
                        Parameter_A.Sloc_Range.End_Column,
                        Parameter_B.Sloc_Range.Start_Column),
                     Text     => New_Text);
               end;
            end if;

            exit;

         elsif Param_Spec_Length > 1
           and then Parameter_Index in
             Current_Parameter_Index + 1 ..
               Current_Parameter_Index + Param_Spec_Length - 1
         then
            --  Case 5
            --
            --  Parameter B is not the first one of Param_Spec.
            --
            --  A, B : in Integer
            --  -  -
            --
            --  B, A : in Integer
            --  +  +

            declare
               Parameter_A : Defining_Name := No_Defining_Name;
               Parameter_B : Defining_Name := No_Defining_Name;

            begin
               for Parameter of Param_Spec.F_Ids loop
                  if Current_Parameter_Index = Parameter_Index - 1 then
                     Parameter_A := Parameter.As_Defining_Name;

                  elsif Current_Parameter_Index = Parameter_Index then
                     Parameter_B := Parameter.As_Defining_Name;
                     exit;
                  end if;

                  Current_Parameter_Index := Current_Parameter_Index + 1;
               end loop;

               Assert (Parameter_A /= No_Defining_Name
                       and then Parameter_B /= No_Defining_Name);

               Insert
                 (Edits    => Edits,
                  Filename => Subp.Unit.Get_Filename,
                  Location => Parameter_B.Sloc_Range,
                  Text     => To_Unbounded_String
                    (To_String (Parameter_A.Text)));

               Insert
                 (Edits    => Edits,
                  Filename => Subp.Unit.Get_Filename,
                  Location => Parameter_A.Sloc_Range,
                  Text     => To_Unbounded_String
                    (To_String (Parameter_B.Text)));
            end;

            exit;

         else
            Current_Parameter_Index :=
              Current_Parameter_Index + Param_Spec_Length;
         end if;

         Previous_Param_Spec := Param_Spec.As_Param_Spec;
         Previous_Param_Spec_Length := Param_Spec_Length;
      end loop;
   end Move_Left;

   ------------
   -- Create --
   ------------

   function Create
     (Target          : Basic_Decl;
      Parameter_Index : Natural;
      Configuration   : Signature_Changer_Configuration_Type :=
        Default_Configuration)
      return Right_Mover
   is (Right_Mover'
         (Mover => Create (Target, Parameter_Index + 1, Configuration)));

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self : Right_Mover;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Edit_Map is
     (Self.Mover.Refactor (Analysis_Units));

end Laltools.Refactor.Subprogram_Signature;
