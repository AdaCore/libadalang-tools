------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;

with Libadalang.Common; use Libadalang.Common;

with TGen.LAL_Utils;         use TGen.LAL_Utils;
with TGen.Strings;           use TGen.Strings;
with TGen.Types;             use TGen.Types;
with TGen.Types.Constraints; use TGen.Types.Constraints;
with TGen.Types.Translation; use TGen.Types.Translation;

package body TGen.Parse_Strategy is

   --  TODO: also parse the strategies for the base type decl

   function Is_Standard_Strategy (Str : String) return Boolean is
     (Str'Length > 5
      and then To_Lower (Str (Str'First .. Str'First + 4)) = "tgen_");

   function Check_Strategy
     (Prefix               : Ada_Qualified_Name;
      Last_Comp_Unit_Index : Positive;
      T                    : Typ'Class;
      Strategy             : Expr'Class;
      Strategies           : out FQN_To_Parsed_Strat_Map) return Typ'Class;
   --  Check the strategy that applies to the type T, and return a new type
   --  where the type of each parameter / parameter component this strategy
   --  applies to has been replaced by an instance type. The prefix keeps
   --  track of the traversed function / record components to reach the type
   --  T, and is used to specify the instance type (specific to a parameter
   --  or a parameter component).

   function Check_Strategy_For_Scalar
     (Prefix               : Ada_Qualified_Name;
      Last_Comp_Unit_Index : Positive;
      T                    : Scalar_Typ'Class;
      Strategy             : Expr'Class;
      Strategies           : out FQN_To_Parsed_Strat_Map) return Typ'Class;
   --  Check the strategy that applies to a parameter / parameter component
   --  that is a scalar type. If this strategy is valid, add an entry in the
   --  Strategies map and return a new instance type that should replace this
   --  parameter / parameter component type in the englobbing function /
   --  record type respectively.

   function Check_Strategy_For_Record
     (Prefix               : Ada_Qualified_Name;
      Last_Comp_Unit_Index : Positive;
      Rec_Typ              : Record_Typ'Class;
      Strategy             : Expr'Class;
      Strategies           : out FQN_To_Parsed_Strat_Map) return Typ'Class;
   --  Same as Check_Strategy, but for a record type in tgen terminology
   --  (that is a function, a record or a discriminated record type).

   function Clone (T : Typ'Class) return Typ'Class;
   --  Clone a given type recursively

   function Clone (T : Typ'Class) return Typ'Class
   is
      use Component_Maps;
   begin
      if T in Scalar_Typ'Class then
         return T;
      elsif T in Record_Typ'Class then
         declare
            Rec_Type : constant Record_Typ'Class := Record_Typ'Class (T);
            Result : Record_Typ'Class := Rec_Type;
         begin
            Result.Component_Types.Clear;
            for Comp in Rec_Type.Component_Types.Iterate loop
               declare
                  Comp_Typ : constant Typ'Class := Clone (Element (Comp).Get);
                  Comp_Ref : SP.Ref;
               begin
                  Comp_Ref.Set (Comp_Typ);
                  Result.Component_Types.Include (Key (Comp), Comp_Ref);
               end;
            end loop;
         end;
         --  TODO: rest of the implementation
      end if;
      return T;
   end Clone;

   -------------------------------
   -- Check_Strategy_For_Scalar --
   -------------------------------

   function Check_Strategy_For_Scalar
     (Prefix               : Ada_Qualified_Name;
      Last_Comp_Unit_Index : Positive;
      T                    : Scalar_Typ'Class;
      Strategy             : Expr'Class;
      Strategies           : out FQN_To_Parsed_Strat_Map) return Typ'Class
   is
      Result : Instance_Typ;
   begin
      --  For a scalar strategy, we just have to check the type of the
      --  strategy. We can either have a predefined strategy, which are
      --  prefixed with TGen and can either be:
      --    * Random
      --  for scalar types.
      --
      --  We can also have the name of a function over which we should have
      --  visibility in which case we need to check that this function returns
      --  type is the expected one.

      if Is_Standard_Strategy ("") then
         --  TODO
         raise Strategy_Parsing_Error;
      else
         --  Check the function return type

         if Kind (Strategy) not in Libadalang.Common.Ada_Identifier then
            Put_Line
              ("Expecting a function name as scalar strategy specifier");
            raise Strategy_Parsing_Error;
         else
            declare
               All_Overloads   : constant Ada_Node_Array :=
                 Strategy.As_Base_Id.P_All_Env_Elements;
               Expected_Type   : constant Base_Type_Decl :=
                 Type_Decl_Cache.Element (T.Name);
               Fct_Return_Type : Base_Type_Decl;
               Strat           : Parsed_Strategy (Kind => Custom);
            begin
               --  Now check that we have a function of the right returning
               --  type.

               for Entity of All_Overloads loop
                  if Kind (Entity) in Ada_Expr_Function
                    | Ada_Subp_Kind_Function
                  then
                     Fct_Return_Type :=
                       Entity.As_Basic_Decl.P_Subp_Spec_Or_Null.P_Returns
                         .P_Designated_Type_Decl;
                  end if;
                  if Fct_Return_Type.P_Matching_Type (Expected_Type) then
                     --  Add to the strategy map an element and specialize
                     --  the type.

                     Result.Orig_Typ.Set (T);
                     Result.Name := Prefix;
                     Result.Last_Comp_Unit_Idx := Last_Comp_Unit_Index;
                     Strat.Generate_Name := +(+Strategy.As_Base_Id.Text);
                     Strategies.Insert (+Result.Fully_Qualified_Name, Strat);
                     return Result;
                  end if;
               end loop;
            end;
         end if;
      end if;
      raise Strategy_Parsing_Error;
   end Check_Strategy_For_Scalar;

   -------------------------------
   -- Check_Strategy_For_Record --
   -------------------------------

   function Check_Strategy_For_Record
     (Prefix               : Ada_Qualified_Name;
      Last_Comp_Unit_Index : Positive;
      Rec_Typ              : Record_Typ'Class;
      Strategy             : Expr'Class;
      Strategies           : out FQN_To_Parsed_Strat_Maps.Map) return Typ'Class
   is
      use type Ada_Qualified_Name;
      Result : Record_Typ'Class := Record_Typ'Class (Clone (Rec_Typ));
   begin
      --  TODO: the strategy expression can be a function name

      --  The strategy expression must be an aggregate

      if Kind (Strategy) not in Ada_Aggregate then
         Put_Line
           ("Expecting an aggregate as a record strategy specification");
         raise Strategy_Parsing_Error;
      end if;

      for Assoc of Strategy.As_Base_Aggregate.F_Assocs loop
         declare
            Assoc_Identifier : constant Unbounded_String :=
              +(+Assoc.As_Aggregate_Assoc.F_Designators.Text);
         begin
            if not Result.Component_Types.Contains (Assoc_Identifier) then
               declare
                  Error_Msg : constant String :=
                    (if Rec_Typ in Function_Typ'Class
                     then +Assoc_Identifier
                     & " is not a parameter of the function."
                     else +Assoc_Identifier
                     & " is not a member of the record.");
               begin
                  Put_Line (Error_Msg & ". Ignoring it.");
               end;

            else
               declare
                  New_Typ : constant Typ'Class :=
                    Check_Strategy
                      (Prefix & TGen.Strings.Ada_Identifier (Assoc_Identifier),
                       Last_Comp_Unit_Index,
                       Rec_Typ.Component_Types.Element (Assoc_Identifier).Get,
                       Assoc.As_Aggregate_Assoc.F_R_Expr,
                       Strategies);
                  New_Typ_Ref : SP.Ref;
               begin
                  New_Typ_Ref.Set (New_Typ);
                  Result.Component_Types.Include
                    (Assoc_Identifier, New_Typ_Ref);
               end;
            end if;
         end;
      end loop;
      return Result;
   end Check_Strategy_For_Record;

   --------------------
   -- Check_Strategy --
   --------------------

   function Check_Strategy
     (Prefix               : Ada_Qualified_Name;
      Last_Comp_Unit_Index : Positive;
      T                    : Typ'Class;
      Strategy             : Expr'Class;
      Strategies           : out FQN_To_Parsed_Strat_Map) return Typ'Class is
   begin
      pragma Warnings (Off);
      if T in Record_Typ'Class then
         return Check_Strategy_For_Record
           (Prefix,
            Last_Comp_Unit_Index,
            Record_Typ'Class (T),
            Strategy,
            Strategies);
      elsif T in Scalar_Typ'Class then
         return Check_Strategy_For_Scalar
           (Prefix,
            Last_Comp_Unit_Index,
            Scalar_Typ'Class (T),
            Strategy,
            Strategies);
      end if;
      pragma Warnings (On);
   end Check_Strategy;

   --------------------
   -- Parse_Strategy --
   --------------------

   procedure Parse_Strategy
     (Fct_Typ    : in out Function_Typ'Class;
      Aspect     : Libadalang.Analysis.Aspect_Assoc;
      Strategies : out FQN_To_Parsed_Strat_Maps.Map)
   is
   begin
      pragma Assert (To_Lower (+Aspect.F_Id.Text) = "generation");

      --  Then, we expect an aggregate with the following fields:
      --  Strategies
      --  Driving
      --  Nb_Tests

      if Kind (Aspect.F_Expr) not in Ada_Aggregate then
         Put_Line ("Expecting an aggregate as the generation aspect expr");
         raise Strategy_Parsing_Error;
      end if;

      for Assoc of Aspect.F_Expr.As_Base_Aggregate.F_Assocs loop
         declare
            Assoc_Identifier : constant String :=
              To_Lower (+Assoc.As_Aggregate_Assoc.F_Designators.Text);
         begin
            if Assoc_Identifier = "strategies" then
               --  Now parse the strategies

               --  Start by checking them

               Fct_Typ := Function_Typ'Class
                 (Check_Strategy
                    (Fct_Typ.Name,
                     Fct_Typ.Last_Comp_Unit_Idx,
                     Fct_Typ,
                     Assoc.As_Aggregate_Assoc.F_R_Expr,
                     Strategies));

            else
               Put_Line ("Wrong parameter for the generation aspect");
               raise Strategy_Parsing_Error;
            end if;
         end;
      end loop;

   end Parse_Strategy;

end TGen.Parse_Strategy;
