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

with Ada.Containers; use Ada.Containers;
with Ada.Tags;    use Ada.Tags;

with Libadalang.Common; use Libadalang.Common;

with TGen.Random;            use TGen.Random;

package body TGen.Strategies is

   function Function_Image_Spec (F : Subprogram_Data) return String;
   function Function_Image_Body
     (F      : Subprogram_Data;
      F_Body : Unbounded_Text_Type) return String;

   -------------------------
   -- Function_Image_Spec --
   -------------------------

   function Function_Image_Spec (F : Subprogram_Data) return String
   is
      Result : Unbounded_String;
      Indent : Natural := 0;
      Index  : Positive := 1;
   begin
      if F.Kind = Ada_Subp_Kind_Procedure then
         S_Write (Result, "procedure ", Indent);
      else
         S_Write (Result, "function ", Indent);
      end if;
      Write (Result, +F.Name);
      New_Line (Result);

      Indent := Indent + 3;
      for Param of F.Parameters_Data loop
         if Index = 1 then
            S_Write (Result, "(", Indent - 1);
         else
            S_Write (Result, "", Indent);
         end if;
         Write
           (Result,
            (+Param.Name) & " : " & (+Param.Type_Fully_Qualified_Name));
         if Count_Type (Index) = F.Parameters_Data.Length then
            Write (Result, ")");
         else
            Write (Result, ";");
         end if;
         New_Line (Result);
         Index := Index + 1;
      end loop;

      if F.Kind = Ada_Subp_Kind_Function then
         if F.Parameters_Data.Is_Empty then
            S_Write (Result, "", Indent - 1);
         end if;
         Write (Result,
                " return "
                & (+F.Return_Type_Fully_Qualified_Name));
      end if;
      Indent := Indent - 3;
      return +Result;
   end Function_Image_Spec;

   -------------------------
   -- Function_Image_Body --
   -------------------------

   function Function_Image_Body
     (F      : Subprogram_Data;
      F_Body : Unbounded_Text_Type) return String
   is
      Result  : Unbounded_String;
      Body_US : Unbounded_String := +(+F_Body);
      Indent  : Natural := 0;
   begin
      Write_Line (Result, Function_Image_Spec (F), Indent);
      Write_Line (Result, "is", Indent);
      Write_Line (Result, "begin", Indent);
      Indent := @ + 3;
      Indent_String (Body_US, Indent);
      Write_Line (Result, +Body_US, 0);
      Indent := @ - 3;
      Write_Line (Result, "end " & (+F.Name) & ";", Indent);
      return +Result;
   end Function_Image_Body;

   ----------------
   -- Image_Spec --
   ----------------

   function Image_Spec (Strat : Dynamic_Strategy_Type) return String
   is
      Result : Unbounded_String;
      Indent : constant Natural := 0;
   begin

      if not Strat.Generated then
         raise Program_Error
           with "User-defined strat should not be regenerated";
      end if;

      case Strat.Kind is
         when State_Kind =>
            Write_Line
              (Result,
               Function_Image_Spec (Strat.Initialize_Function) & ";",
               Indent);
         when others =>
            null;
      end case;

      --  For constrained strats, write the function with constraints set (as
      --  parameters).

      if Strat.Constrained then
         Write_Line (Result, "", Indent);
         --  Write_Line
         --    (Result,
         --     Image_Spec (Strat.Constrained_Strategy_Function.all),
         --     Indent);
      end if;

      --  Write the strategy function spec

      Write_Line
        (Result, Function_Image_Spec (Strat.Strategy_Function) & ";", 0);

      return +Result;
   end Image_Spec;

   ----------------
   -- Image_Body --
   ----------------

   function Image_Body (Strat : Dynamic_Strategy_Type) return String is
      Result : Unbounded_String;
      Indent : Natural := 0;
   begin
      if not Strat.Generated then
         raise Program_Error
           with "User-defined strat should not be regenerated";
      end if;

      --  For stateful strats, write the initialize function

      case Strat.Kind is
         when State_Kind =>
            Write_Line
              (Result,
               Function_Image_Body
                 (Strat.Initialize_Function, Strat.Initialize_Body),
               Indent);
         when others =>
            null;
      end case;

      --  For constrained strats, write the function with constraints set (as
      --  parameters).

      if Strat.Constrained then
         Write_Line (Result, "", Indent);
         --  Write_Line
         --    (Result,
         --     Image_Body (Strat.Constrained_Strategy_Function.all),
         --     Indent);
      end if;

      --  Then write the strategy body

      Write_Line (Result, "", Indent);
      declare
         F_Body : Unbounded_String :=
           +Function_Image_Body (Strat.Strategy_Function, Strat.Strategy_Body);
      begin
         Indent := @ + 3;
         Indent_String (F_Body, Indent);
         Write_Line (Result, +F_Body, 0);
         Indent := @ - 3;
      end;
      return +Result;
   end Image_Body;

   ---------------------------
   -- Generate_Static_Value --
   ---------------------------

   function Generate_Static_Value
     (S            : in out Dispatching_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class
   is
      Rnd : constant Float := Rand_Float;
   begin
      if Rnd <= S.Bias then
         return S.S1.Generate_Static_Value (Disc_Context);
      else
         return S.S2.Generate_Static_Value (Disc_Context);
      end if;
   end Generate_Static_Value;

   ----------------------------
   -- Make_Dispatching_Strat --
   ----------------------------

   function Make_Dispatching_Strat
     (S1, S2 : Static_Strategy_Type'Class;
      Bias   : Float := 0.5) return Dispatching_Static_Strategy_Type
   is
      Strat : Dispatching_Static_Strategy_Type;
   begin
      Strat.Bias := Bias;
      Strat.S1 := new Static_Strategy_Type'Class'(S1);
      Strat.S2 := new Static_Strategy_Type'Class'(S2);
      return Strat;
   end Make_Dispatching_Strat;

   package body Equivalence_Classes_Strategy_Package is

      ---------------------------
      -- Generate_Static_Value --
      ---------------------------

      function Generate_Static_Value
        (S            : in out Equivalence_Class_Strategy_Type;
         Disc_Context : Disc_Value_Map) return Static_Value'Class
      is
         Idx : constant Integer := Rand_Int (1, Integer (S.Classes.Length));
      begin
         return S.Draw (S.T, S.Classes.Element (Idx));
      end Generate_Static_Value;

   end Equivalence_Classes_Strategy_Package;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Strategy_Type'Class) return Boolean is
   begin

      --  TODO: code properly this function

      if L'Tag = R'Tag then
         if L'Tag = Dispatching_Static_Strategy_Type'Tag then
            if Dispatching_Static_Strategy_Type (L).S1.all <
              Dispatching_Static_Strategy_Type (R).S1.all
            then
               return True;
            else
               return Dispatching_Static_Strategy_Type (L).S1.all <
                 Dispatching_Static_Strategy_Type (R).S1.all;
            end if;
         elsif L'Tag = Basic_Static_Strategy_Type'Tag then
            return Basic_Static_Strategy_Type (L).T <
              Basic_Static_Strategy_Type (R).T;
         else
            return False;
         end if;
      else
         return Expanded_Name (L'Tag) < Expanded_Name (R'Tag);
      end if;
   end "<";

end TGen.Strategies;
