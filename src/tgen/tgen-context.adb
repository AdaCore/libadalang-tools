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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with TGen.Strings; use TGen.Strings;

package body TGen.Context is

   ---------------
   -- To_String --
   ---------------

   function To_String (Subp : Subprogram_Data) return String is
      Res : Unbounded_String;
      I : Natural := 0;
   begin
      Write_Line (Res, "function " & (+Subp.Name), I);
      I := I + 3;
      S_Write (Res, "(", I);

      --  Append the discriminants as parameters

      for Param_Cursor in Subp.Parameters_Data.Iterate loop
         declare
            use Parameters_Data_Vectors;

            Param_Data : Parameter_Data := Element (Param_Cursor);
            Param_Name : Unbounded_Text_Type := Param_Data.Name;
            Param_Type : Unbounded_Text_Type :=
              Param_Data.Type_Fully_Qualified_Name;
         begin
            S_Write (Res, (+Param_Name) & " : ", I);
            Write (Res, +Param_Type);
            if Next (Param_Cursor) = Parameters_Data_Vectors.No_Element then
               Write (Res, ")");
               Write
                 (Res,
                  " return "
                  & (+Subp.Return_Type_Fully_Qualified_Name));
               I := I - 3;
               New_Line (Res);
            else
               Write (Res, ";");
            end if;
         end;
      end loop;

      return +Res;
   end To_String;

   function Function_Image_Spec (F : Subprogram_Data) return String;
   function Function_Image_Body
     (F      : Subprogram_Data;
      F_Body : Unbounded_Text_Type) return String;

   -------------------------
   -- Function_Image_Spec --
   -------------------------

   function Function_Image_Spec (F : Subprogram_Data) return String
   is
      use type Ada.Containers.Count_Type;

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

   function Image_Spec (Strat : Strategy_Type) return String
   is
      Result : Unbounded_String;
      Indent : Natural := 0;
      Index : Positive := 1;
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

   function Image_Body (Strat : Strategy_Type) return String is
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

end TGen.Context;
