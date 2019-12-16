------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                      G N A T T E S T . C O M M O N                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
------------------------------------------------------------------------------

with Libadalang.Common; use Libadalang.Common;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNATCOLL.Traces; use GNATCOLL.Traces;

with GNAT.SHA1;

with Utils.Command_Lines; use Utils.Command_Lines;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Test.Common is

   Me_Hash    : constant Trace_Handle := Create ("Hash", Default => On);

   function Operator_Image (Node : Ada_Node'Class) return String;
   --  According to operator symbols returns their literal names to make the
   --  names of the testing routines correct.

   -----------------
   -- Create_Dirs --
   -----------------

   procedure Create_Dirs (Target_Dirs : File_Array_Access) is
      First : Integer;
      use GNAT.OS_Lib;
   begin
      for J in Target_Dirs'Range loop
         declare
            Target_Dir : constant String :=
                           Target_Dirs.all (J).Display_Full_Name;
         begin
            First := Target_Dir'First;

            if Is_Regular_File (Target_Dir) then
               Cmd_Error_No_Help ("gnattest: cannot create dir " & Target_Dir);
            end if;

            for Idx in Target_Dir'Range loop
               if Target_Dir (Idx) = Directory_Separator
                 or else Idx = Target_Dir'Last
               then
                  if not Is_Directory (Target_Dir (First .. Idx)) then
                     begin
                        Make_Dir (Target_Dir (First .. Idx));
                     exception
                        when Directory_Error =>
                           Cmd_Error_No_Help
                             ("gnattest: cannot create dir "
                              & Target_Dir (First .. Idx));
                     end;
                  end if;
               end if;
            end loop;
         end;
      end loop;
   end Create_Dirs;

   -----------------
   -- Mangle_Hash --
   -----------------

   function Mangle_Hash (Subp : Ada_Node'Class) return String
   is
      Full_Hash : Unbounded_String;
   begin

      Full_Hash := To_Unbounded_String (Mangle_Hash_Full (Subp));

      return
        Test_Routine_Prefix
        & Get_Subp_Name (Subp)
        & "_"
        & To_String (Head (Full_Hash, 6));
   end Mangle_Hash;

   ----------------------
   -- Mangle_Hash_Full --
   ----------------------

   function Mangle_Hash_Full
     (Subp           : Ada_Node'Class;
      Case_Sensitive : Boolean := False;
      N_Controlling  : Boolean := False) return String
   is
      pragma Unreferenced (N_Controlling);
      Subp_Name : constant String :=
        Common.Node_Image (P_Defining_Name (As_Basic_Subp_Decl (Subp)));

      L_Subp      : Base_Subp_Spec;
      L_Subp_Span : Source_Location_Range;

      Sign_Image, Hash_Result : Unbounded_String;

      Tagged_Rec  : Base_Type_Decl := No_Base_Type_Decl;
      Root_Ignore : Base_Type_Decl;

      Subp_Depth : Natural := 0;
      --  Indicates how deep in subprogrmam specs the analysis is.
      --  Only first level parameters are subjects to substitution for root
      --  tagged type.
      --  Also, for compatibility reasons the last parameter on depth 1
      --  should have ";" after itself, which is not the case for depths >= 2.

      function Handle_Parameters
        (Params         : Param_Spec_Array;
         Result_Profile : Type_Expr) return String;
      --  Returns an image of the types from parameters list and the result
      --  type in case of a function for a given list of parameter
      --  specifications.

      function Parameter_Image (Param : Param_Spec) return String;
      --  Returns the image of given subprogram parameter including the ammount
      --  of defining identifiers in parameter specification.

      function Parameter_Type_Image (Param : Type_Expr) return String;
      --  Returns the image of given type specification.

      function Handle_Parameters
        (Params         : Param_Spec_Array;
         Result_Profile : Type_Expr) return String
      is
         Result : Unbounded_String := To_Unbounded_String ("");
         Param  : Param_Spec;
      begin
         Subp_Depth := Subp_Depth + 1;
         for I in Params'Range loop
            Param := Params (I);

            Result :=
              Result
              & (if I = Params'First then "(" else ";")
              & Parameter_Image (Param);
         end loop;

         Append (Result, (if Subp_Depth = 1 then ";)" else ")"));

         if Result_Profile /= No_Type_Expr then
            Append
              (Result,
               Parameter_Type_Image (Result_Profile)
               & (if Subp_Depth = 1 then ";" else ""));
         end if;

         Subp_Depth := Subp_Depth - 1;
         return To_String (Result);
      end Handle_Parameters;

      function Parameter_Image (Param : Param_Spec) return String is
         Param_Names : constant Defining_Name_List := F_Ids (Param);
         Param_Names_Size : Natural := 0;
      begin
         for N of Param_Names loop
            Param_Names_Size := Param_Names_Size + 1;
         end loop;

         return
           Trim (Natural'Image (Param_Names_Size), Both)
           & Parameter_Type_Image (F_Type_Expr (Param));
      end Parameter_Image;

      function Parameter_Type_Image (Param : Type_Expr) return String is
         Param_Type_Def  : Type_Def;
         Param_Type_Name : Libadalang.Analysis.Name;
         Anon_Subp_Def   : Access_To_Subp_Def;

         Attr_Flag : Boolean := False;

         Type_Decl : Base_Type_Decl;
      begin
         case Kind (Param) is
            when Ada_Subtype_Indication =>
               Param_Type_Name :=
                 F_Name (As_Subtype_Indication (Param));

               if Kind (Param_Type_Name) = Ada_Attribute_Ref then
                  Attr_Flag := True;
               end if;
               Param_Type_Name := As_Name (P_Relative_Name (Param_Type_Name));
               Type_Decl :=
                 P_Canonical_Type
                   (As_Base_Type_Decl (P_Referenced_Decl (Param_Type_Name)));

               if
                 Subp_Depth < 2 and then not Attr_Flag
                 and then Type_Decl = Tagged_Rec
               then
                  Type_Decl := Root_Ignore;
               end if;

               Param_Type_Name := F_Name (P_Defining_Name (Type_Decl));

               return
                 Get_Nesting (Type_Decl)
                 & "." & Node_Image (Param_Type_Name)
                 & (if Attr_Flag then "'Attr" else "");

            when Ada_Anonymous_Type =>
               Param_Type_Def :=
                 F_Type_Def
                   (As_Type_Decl
                      (F_Type_Decl (As_Anonymous_Type (Param))));

               case Kind (Param_Type_Def) is
                  when Ada_Type_Access_Def =>
                     Param_Type_Name :=
                       F_Name (F_Subtype_Indication
                               (As_Type_Access_Def (Param_Type_Def)));

                     if Kind (Param_Type_Name) = Ada_Attribute_Ref then
                        Attr_Flag := True;
                     end if;
                     Param_Type_Name :=
                       As_Name (P_Relative_Name (Param_Type_Name));
                     Type_Decl :=
                       P_Canonical_Type
                         (As_Base_Type_Decl
                            (P_Referenced_Decl (Param_Type_Name)));

                     if
                       Subp_Depth < 2 and then not Attr_Flag
                       and then Type_Decl = Tagged_Rec
                     then
                        Type_Decl := Root_Ignore;
                     end if;

                     Param_Type_Name := F_Name (P_Defining_Name (Type_Decl));

                     return
                       "@"
                       & Get_Nesting (Type_Decl)
                       & "." & Node_Image (Param_Type_Name)
                       & (if Attr_Flag then "'Attr" else "");

                  when Ada_Access_To_Subp_Def =>
                     Anon_Subp_Def := As_Access_To_Subp_Def (Param_Type_Def);

                     return
                       (if F_Has_Protected (Anon_Subp_Def) then "#" else "")
                       & Handle_Parameters
                       (P_Params (F_Subp_Spec (Anon_Subp_Def)),
                        F_Subp_Returns
                          (As_Subp_Spec (F_Subp_Spec (Anon_Subp_Def))));
                  when others =>
                     Report_Err ("UNKNOWN type def:");
                     Print (Param_Type_Def);
                     return "<..>";
               end case;

            when others =>
               Report_Err ("UNKNOWN type dec:");
               Print (Param);
               return "<,,>";
         end case;
      end Parameter_Type_Image;

   begin
      L_Subp := P_Subp_Decl_Spec (As_Basic_Subp_Decl (Subp));
      L_Subp_Span := Sloc_Range (L_Subp);

      Trace
        (Me_Hash,
         "Mangle_Hash_Full for " & Subp_Name
         & (if L_Subp_Span = No_Source_Location_Range then ""
           else " at line" & L_Subp_Span.Start_Line'Img));
      Increase_Indent (Me_Hash);

      Tagged_Rec := Tagged_Primitive_Owner (L_Subp);

      if Tagged_Rec /= No_Base_Type_Decl then
         Root_Ignore := Root_Type_Declaration (Tagged_Rec);
      end if;

      case Kind (F_Subp_Kind (As_Subp_Spec (L_Subp))) is
         when Ada_Subp_Kind_Function =>
            Sign_Image :=
              To_Unbounded_String
              ("function"
               & Subp_Name & Handle_Parameters
                 (P_Params (L_Subp),
                  F_Subp_Returns (As_Subp_Spec (L_Subp))));
         when Ada_Subp_Kind_Procedure =>
            Sign_Image :=
              To_Unbounded_String
              ("procedure"
               & Subp_Name & Handle_Parameters
                 (P_Params (L_Subp), No_Type_Expr));
         when others =>
            Trace (Me_Hash, "Unexpected element, returning empty hash");
            Trace (Me_Hash, Node_Image (L_Subp));
            Decrease_Indent (Me_Hash);
            return "";
      end case;

      if Case_Sensitive then
         Trace (Me_Hash, "Hash image: " & To_String (Sign_Image));
         Hash_Result :=
           To_Unbounded_String (GNAT.SHA1.Digest (To_String (Sign_Image)));
      else
         Trace (Me_Hash, "Hash image: " & To_Lower (To_String (Sign_Image)));
         Hash_Result :=
           To_Unbounded_String
             (GNAT.SHA1.Digest (To_Lower (To_String (Sign_Image))));
      end if;

      Decrease_Indent (Me_Hash);
      Trace
        (Me_Hash,
         "Hash : " & To_String (Head (Hash_Result, 16)));
      return To_String (Head (Hash_Result, 16));
   end Mangle_Hash_Full;

   -----------------
   -- Get_Nesting --
   -----------------

   function Get_Nesting (Elem : Ada_Node'Class) return String is
      Nesting : constant Ada_Node_Array := Parents (Elem);
      Result  : Unbounded_String := To_Unbounded_String ("");
   begin
      for I in Nesting'First + 1 .. Nesting'Last loop
         if Kind (Nesting (I)) = Ada_Package_Decl then
            if Result = "" then
               Result := To_Unbounded_String
                 (Node_Image (P_Defining_Name (As_Basic_Decl (Nesting (I)))));
            else
               Result :=
                 Debug_Text (Nesting (I).As_Basic_Decl.P_Defining_Name)
                 & "."
                 & Result;
            end if;
         end if;
      end loop;

      return To_String (Result);
   end Get_Nesting;

   ---------------------------
   -- Nesting_Common_Prefix --
   ---------------------------

   function Nesting_Common_Prefix
     (Nesting_1, Nesting_2 : String) return String
   is
      L1, L2   : Integer;
      Last_Dot : Integer;
   begin
      L1 := Nesting_1'First;
      L2 := Nesting_2'First;
      loop

         if Nesting_1 (L1) = Nesting_2 (L2) then

            if L1 = Nesting_1'Last then
               return Nesting_1;
            end if;

            if L2 = Nesting_2'Last then
               return Nesting_2;
            end if;

            if Nesting_1 (L1) = '.' then
               Last_Dot := L1;
            end if;

            L1 := L1 + 1;
            L2 := L2 + 1;
         else
            return Nesting_1 (Nesting_1'First .. Last_Dot - 1);
         end if;

      end loop;

   end Nesting_Common_Prefix;

   ------------------------
   -- Nesting_Difference --
   ------------------------

   function Nesting_Difference
     (Nesting_1, Nesting_2 : String) return String
   is
      L : constant Integer := Integer'Min (Nesting_1'Length, Nesting_2'Length);
   begin

      if Nesting_1'Length > Nesting_2'Length then
         return Nesting_1 (Nesting_1'First + L + 1 .. Nesting_1'Last);
      else
         return Nesting_2 (Nesting_2'First + L + 1 .. Nesting_2'Last);
      end if;

   end Nesting_Difference;

   ----------------------
   --   Get_Subp_Name  --
   ----------------------

   function Get_Subp_Name (Node : Ada_Node'Class) return String is
      Name : constant String :=
        Common.Node_Image (P_Defining_Name (As_Basic_Decl (Node)));
   begin
      --  checking for overloaded operators
      if Name (Name'First) = '"' then
         return Operator_Image (Node);
      else
         return Name;
      end if;

   end Get_Subp_Name;

   ----------------------
   --  Operator_Image  --
   ----------------------

   function Operator_Image (Node : Ada_Node'Class) return String is
      Name : constant String :=
        To_Lower (Common.Node_Image (P_Defining_Name (As_Basic_Decl (Node))));
      Params : constant Param_Spec_Array :=
        P_Params (P_Subp_Spec_Or_Null (As_Basic_Decl (Node)));
      First_Param_Names : constant Defining_Name_List :=
        F_Ids (Params (Params'First));

      First_Param_Names_Size : Natural := 0;
   begin

      for N of First_Param_Names loop
         First_Param_Names_Size := First_Param_Names_Size + 1;
      end loop;

      if Name = """and""" then               -- and
         return """And""";
      elsif Name = """or""" then             -- or
         return """Or""";
      elsif Name = """xor""" then            -- xor
         return """Xor""";
      elsif Name = """=""" then              -- =
         return """Equal""";
      elsif Name = """/=""" then             -- /=
         return """Not_Equal""";
      elsif Name = """<""" then              -- <
         return """Less_Than""";
      elsif Name = """<=""" then             -- <=
         return """Less_Than_Or_Equal""";
      elsif Name = """>""" then              -- >
         return """Greater_Than""";
      elsif Name = """>=""" then             -- >=
         return """Greater_Than_Or_Equal""";
      elsif Name = """+""" then              -- +
         if Params'Length = 2 or else First_Param_Names_Size = 2 then
            return """Plus""";
         else
            return """Unary_Plus""";
         end if;
      elsif Name = """-""" then              -- -
         if Params'Length = 2 or else First_Param_Names_Size = 2 then
            return """Minus""";
         else
            return """Unary_Minus""";
         end if;
      elsif Name = """&""" then              -- &
         return """Concatenate""";
      elsif Name = """*""" then              -- *
         return """Multiply""";
      elsif Name = """/""" then              -- /
         return """Divide""";
      elsif Name = """mod""" then            -- mod
         return """Mod""";
      elsif Name = """rem""" then            -- rem
         return """Rem""";
      elsif Name = """**""" then             -- **
         return """Exponentiate""";
      elsif Name = """abs""" then            -- abs
         return """Abs""";
      elsif Name = """not""" then            -- not
         return """Not""";
      end if;

      return "unknown operator";

   end Operator_Image;

   ----------------------------
   -- Tagged_Primitive_Owner --
   ----------------------------

   function Tagged_Primitive_Owner (Subp : Base_Subp_Spec)
                                    return Base_Type_Decl
   is
      Primitive_Owners : constant Base_Type_Decl_Array :=
        P_Primitive_Subp_Types (As_Subp_Spec (Subp));
   begin
      for Owner of Primitive_Owners loop
         if P_Is_Tagged_Type (As_Type_Decl (Owner)) then
            return Owner;
         end if;
      end loop;
      return No_Base_Type_Decl;
   end Tagged_Primitive_Owner;

   -----------------------------
   -- Parent_Type_Declaration --
   -----------------------------

   function Parent_Type_Declaration
     (Type_Dec : Base_Type_Decl) return Base_Type_Decl
   is
      T_Dec : Base_Type_Decl := Type_Dec;
      T_Def : Type_Def;

      T_Name : Libadalang.Analysis.Name;
   begin
      if Kind (T_Dec) = Ada_Incomplete_Tagged_Type_Decl then
         T_Dec := P_Next_Part (T_Dec);
      end if;

      T_Def := F_Type_Def (As_Type_Decl (T_Dec));

      if Kind (T_Def) /= Ada_Derived_Type_Def then
         return No_Base_Type_Decl;
      end if;

      T_Name :=
        As_Name
          (P_Relative_Name
             (F_Name (F_Subtype_Indication (As_Derived_Type_Def (T_Def)))));

      return P_Canonical_Type (As_Base_Type_Decl (P_Referenced_Decl (T_Name)));
   end Parent_Type_Declaration;

   ---------------------------
   -- Root_Type_Declaration --
   ---------------------------

   function Root_Type_Declaration
     (Type_Dec : Base_Type_Decl) return Base_Type_Decl
   is
      Decl : Base_Type_Decl := Type_Dec;
   begin
      while Decl /= No_Base_Type_Decl loop

         if Parent_Type_Declaration (Decl) = No_Base_Type_Decl then
            return Decl;
         else
            Decl := Parent_Type_Declaration (Decl);
         end if;
      end loop;

      return No_Base_Type_Decl;
   end Root_Type_Declaration;

   ----------------
   -- Report_Err --
   ----------------

   procedure Report_Err (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
   end Report_Err;

   ----------------
   -- Report_Std --
   ----------------

   procedure Report_Std (Message : String; Offset : Integer := 0) is
   begin

      for I in 1 .. Offset loop
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Output, " ");
      end loop;

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output, Message);
   end Report_Std;

   -----------------------
   -- Unit_To_File_Name --
   -----------------------

   function Unit_To_File_Name (Old : String) return String is
      T : String_Access;
   begin
      T := new String'(Old);
      for J in T.all'First .. T.all'Last loop
         if T.all (J) = '.' then
            if J = T.all'First + 1 and then
              T.all (J - 1) in 'a' | 's' | 'i' | 'g' | 'A' | 'S' | 'I' | 'G'
            then
               T.all (J) := '~';
            else
               T.all (J) := '-';
            end if;
         end if;
      end loop;

      return To_Lower (T.all);
   end Unit_To_File_Name;

   -----------
   -- S_Put --
   -----------

   procedure S_Put (Span : Natural; Text : String) is
   begin
      for J in 0 .. Span - 1 loop
         Char_Sequential_IO.Write (Output_File, ' ');
      end loop;
      for J in Text'Range loop
         Char_Sequential_IO.Write (Output_File, Text (J));
      end loop;
   end S_Put;

   ------------
   -- Create --
   ------------

   procedure Create (Name : String) is
      B_Name :          String := Base_Name (Name);
      P_Name : constant String := Dir_Name (Name);
   begin
      if B_Name (B_Name'First + 1) = '-'
        and then B_Name (B_Name'First) in 'a' | 's' | 'i' | 'g'
      then
         B_Name (B_Name'First + 1) := '~';
      end if;
      Char_Sequential_IO.Create
        (Output_File, Char_Sequential_IO.Out_File, P_Name & B_Name);
   end Create;

   -----------
   -- Close --
   -----------

   procedure Close_File is
   begin
      Char_Sequential_IO.Close (Output_File);
   end Close_File;

   ------------------
   -- Put_New_Line --
   ------------------

   procedure Put_New_Line is
   begin
      Char_Sequential_IO.Write (Output_File, ASCII.LF);
   end Put_New_Line;

   ------------------------
   -- Put_Harness_Header --
   ------------------------

   procedure Put_Harness_Header is
   begin
      S_Put
        (0,
         "--  This package has been generated automatically by GNATtest.");
      Put_New_Line;
      S_Put
        (0,
         "--  Do not edit any part of it, "
         & "see GNATtest documentation for more details.");
      Put_New_Line;
      Put_New_Line;
   end Put_Harness_Header;

   --------------------------
   -- Generate_Common_File --
   --------------------------

   procedure Generate_Common_File is
      Harness_Dir : GNAT.OS_Lib.String_Access renames Harness_Dir_Str;

      Common_Package_Name : constant String := "Gnattest_Generated";
      Common_File_Subdir  : constant String :=
        Harness_Dir.all & GNAT.OS_Lib.Directory_Separator & "common";

      use GNAT.OS_Lib;
   begin
      if not Is_Directory (Common_File_Subdir) then
         Make_Dir (Common_File_Subdir);
      end if;

      Create (Common_File_Subdir &
              Directory_Separator &
              Unit_To_File_Name (Common_Package_Name) & ".ads");

      S_Put (0, "package Gnattest_Generated is");
      Put_New_Line;
      S_Put (3, "package GNATtest_Standard renames Standard;");
      Put_New_Line;
      S_Put (3, "Default_Assert_Value : Boolean := ");
      if Skeletons_Fail then
         S_Put (0, "False;");
      else
         S_Put (0, "True;");
      end if;
      Put_New_Line;
      S_Put (0, "end Gnattest_Generated;");

      Close_File;
   end Generate_Common_File;

end Test.Common;
