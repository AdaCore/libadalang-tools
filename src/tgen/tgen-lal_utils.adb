------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                      Copyright (C) 2021-2022, AdaCore                    --
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

with Ada.Characters.Handling;

with Libadalang.Common; use Libadalang.Common;

with Test.Common;

package body TGen.LAL_Utils is

   -----------------------
   -- To_Qualified_Name --
   -----------------------

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Name) return Ada_Qualified_Name is
   begin
      return Result : Ada_Qualified_Name do
         case Ada_Name (Name.Kind) is
            when Ada_Dotted_Name =>
               declare
                  DN     : constant Libadalang.Analysis.Dotted_Name :=
                    Name.As_Dotted_Name;
                  Suffix : constant Ada_Qualified_Name :=
                    To_Qualified_Name (DN.F_Suffix.As_Name);
               begin
                  Result := To_Qualified_Name (DN.F_Prefix);
                  Result.Append (Suffix);
               end;

            when Ada_Single_Tok_Node =>
               declare

                  --  ??? GNATCOLL.Projects does not specify how to encode
                  --  Unicode unit names as strings, so for now, assume that we
                  --  process only codepoints in the ASCII range and thus use
                  --  Langkit_Support.Text.Image.

                  Identifier : constant TGen.Strings.Ada_Identifier :=
                    To_Unbounded_String (Image (Name.Text));
               begin
                  Result.Append (Identifier);
               end;

            when others =>
               raise Constraint_Error
                 with "no qualified name for " & Name.Kind'Image & " nodes";
         end case;
      end return;
   end To_Qualified_Name;

   ----------------------------
   -- Convert_Qualified_Name --
   ----------------------------

   function Convert_Qualified_Name
     (Text_QN : Libadalang.Analysis.Unbounded_Text_Type_Array)
      return Ada_Qualified_Name
   is
      Res : Ada_Qualified_Name;
   begin
      for Ident of Text_QN loop
         Res.Append (To_Unbounded_String (Image (To_Text (Ident))));
      end loop;
      return Res;
   end Convert_Qualified_Name;

   ------------------------
   -- JSON_Test_Filename --
   ------------------------

   function JSON_Test_Filename
     (Subp : Libadalang.Analysis.Basic_Decl) return String
   is
      Comp_Unit : constant Libadalang.Analysis.Compilation_Unit :=
        Subp.P_Enclosing_Compilation_Unit;
   begin
      return
        To_JSON_filename
          (Convert_Qualified_Name
             (Comp_Unit.P_Syntactic_Fully_Qualified_Name));
   end JSON_Test_Filename;

   --------------------------------
   -- Default_Blob_Test_Filename --
   --------------------------------

   function Default_Blob_Test_Filename
     (Subp : Libadalang.Analysis.Basic_Decl) return String
   is
      FQN : Ada_Qualified_Name :=
        To_Qualified_Name (Subp.P_Defining_Name.F_Name);
   begin
      --  Having a filename with double quotes inside is a recipe for disaster,
      --  so map the operator name if Subp is one.

      if Is_Operator (+(Unbounded_String (FQN.Last_Element))) then
         FQN.Replace_Element
           (FQN.Last_Index,
            To_Unbounded_String
              (Map_Operator_Name
                 (To_String (Unbounded_String (FQN.Last_Element)))));
      end if;
      FQN.Append (To_Unbounded_String (Test.Common.Mangle_Hash_Full (Subp)));
      return To_Filename (FQN);
   end Default_Blob_Test_Filename;

   -----------------------------------------
   -- Ultimate_Enclosing_Compilation_Unit --
   -----------------------------------------

   function Ultimate_Enclosing_Compilation_Unit
     (Subp : LAL.Basic_Decl'Class) return LAL.Basic_Decl
   is
      Instantiation_Chain : constant LAL.Generic_Instantiation_Array :=
        Subp.P_Generic_Instantiations;
      Res                 : constant LAL.Basic_Decl :=
        LAL.P_Enclosing_Compilation_Unit
          (if Instantiation_Chain'Length > 0
           then Instantiation_Chain (Instantiation_Chain'Last)
           else Subp)
          .P_Decl;
   begin
      return Res;
   end Ultimate_Enclosing_Compilation_Unit;

   -------------------------------------------
   -- Get_Top_Level_Instantiation_File_Name --
   -------------------------------------------

   function Top_Level_Instantiation_Test_File_Name
     (Unit_Full_Name : String) return String
   is
      Tmp : Ada_Qualified_Name;
   begin
      Tmp.Append
        (TGen.Strings.Ada_Identifier'(To_Unbounded_String (Unit_Full_Name)));
      return
        Ada.Characters.Handling.To_Lower
          ("tgen_" & To_Symbol (Tmp, Sep => '_') & ".json");
   end Top_Level_Instantiation_Test_File_Name;

   --------------------------
   --  Derive_Opaque_Type  --
   --------------------------

   function Derive_Opaque_Type
     (Ty_Decl : LAL.Base_Type_Decl'Class) return Boolean
   is
      Decl : LAL.Base_Type_Decl'Class := Ty_Decl;
   begin
      while (Decl.Kind = Ada_Concrete_Type_Decl
             and then Decl.As_Concrete_Type_Decl.F_Type_Def.Kind
                      = Ada_Derived_Type_Def)
        or Decl.Kind in Ada_Subtype_Decl_Range
      loop
         if Decl.Kind = Ada_Concrete_Type_Decl then
            Decl :=
              LAL.Base_Type_Decl'Class
                (Decl
                   .As_Concrete_Type_Decl
                   .F_Type_Def
                   .As_Derived_Type_Def
                   .F_Subtype_Indication
                   .P_Designated_Type_Decl);
         else
            Decl :=
              LAL.Base_Type_Decl'Class
                (Decl.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl);
         end if;
      end loop;

      return not Decl.P_Private_Completion.Is_Null;
   end Derive_Opaque_Type;

end TGen.LAL_Utils;
