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

with Ada.Calendar;
with Ada.Characters.Latin_9;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Calendar;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Templates_Parser; use Templates_Parser;

with TGen.Files;                use TGen.Files;
with TGen.Gen_Strategies_Utils; use TGen.Gen_Strategies_Utils;
with TGen.Strings;              use TGen.Strings;

package body TGen.Gen_Types_Value is

   function Type_Value_ADS_Text_Template
     (Self : Type_Value_Generator) return Text_Type;
   function Type_Value_ADB_Text_Template
     (Self : Type_Value_Generator) return Text_Type;

   ----------------------------------
   -- Type_Value_ADS_Text_Template --
   ----------------------------------

   function Type_Value_ADS_Text_Template
     (Self : Type_Value_Generator) return Text_Type
   is
      use GNATCOLL.VFS;

      TS : aliased constant Type_Value_ADS_Translator :=
        Create_Type_Value_ADS_Translator (Self.Types);

      Package_Name_Tag : constant String := "PACKAGE_NAME";

      Table : Templates_Parser.Translate_Set;

   begin
      TS.Translate (Table);

      --  All the tables are translated. Now translate the individual
      --  tags.

      Templates_Parser.Insert
        (Table,
         Templates_Parser.Assoc
           (Package_Name_Tag,
            Self.Pkg_Name));

      return
        To_Text
          (Templates_Parser.Parse
             (Filename          => +Get_Template_Type_Strat_ADS.Full_Name,
              Translations      => Table,
              Cached            => True,
              Keep_Unknown_Tags => True));
   end Type_Value_ADS_Text_Template;

   ----------------------------------
   -- Type_Value_ADB_Text_Template --
   ----------------------------------

   function Type_Value_ADB_Text_Template
     (Self : Type_Value_Generator) return Text_Type
   is
      use GNATCOLL.VFS;

      TS : aliased constant Type_Value_ADB_Translator :=
        Create_Type_Value_ADB_Translator (Self.Types);

      WT : aliased constant With_Clauses_Translator :=
        Create_With_Clauses_Translator (Self.Type_Depends, TS'Access);

      Package_Name_Tag : constant String := "PACKAGE_NAME";

      Table : Templates_Parser.Translate_Set;

   begin
      WT.Translate (Table);

      --  All the tables are translated. Now translate the individual
      --  tags.

      Templates_Parser.Insert
        (Table,
         Templates_Parser.Assoc
           (Package_Name_Tag,
            Self.Pkg_Name));

      return
        To_Text
          (Templates_Parser.Parse
             (Filename          => +Get_Template_Type_Strat_ADB.Full_Name,
              Translations      => Table,
              Cached            => True,
              Keep_Unknown_Tags => True));
   end Type_Value_ADB_Text_Template;

     --------------------------
     -- Generate_Source_Code --
     --------------------------

   overriding procedure Generate_Source_Code
     (Self : Type_Value_Generator;
      Ctx  : TGen.Templates.Context'Class)
   is

      Type_Value_ADS_Filename : constant Virtual_File :=
        Gen_File
          (Ctx,
           Unit_To_Filename
             (Ctx.Project, String'(+Self.Pkg_Name), Unit_Spec));
      Type_Value_ADB_Filename : constant Virtual_File :=
        Gen_File
          (Ctx, Unit_To_Filename (Ctx.Project, +Self.Pkg_Name, Unit_Body));

      Type_Value_ADS_File : File_Type;
      Type_Value_ADB_File : File_Type;

   begin

      Create
        (Type_Value_ADS_File, Out_File, +Type_Value_ADS_Filename.Full_Name);
      Create
        (Type_Value_ADB_File, Out_File, +Type_Value_ADB_Filename.Full_Name);

      --  TODO: we should probably write by chunks instead of allocating a
      --  huge string on the secondary stack.

      Put
        (Type_Value_ADS_File,
         To_UTF8 (Self.Type_Value_ADS_Text_Template)
         & Ada.Characters.Latin_9.LF);

      Put
        (Type_Value_ADB_File,
         To_UTF8 (Self.Type_Value_ADB_Text_Template)
         & Ada.Characters.Latin_9.LF);

      Close (Type_Value_ADS_File);
      Close (Type_Value_ADB_File);
   end Generate_Source_Code;

   ------------
   -- Create --
   ------------

   function Create
     (Pkg_Name     : Unbounded_String;
      Types        : Typ_Set;
      Type_Depends : Typ_Set) return Type_Value_Generator
   is
      use type Ada.Calendar.Time;
   begin
      if Get_Template_Type_Strat_ADS.File_Time_Stamp = GNAT.Calendar.No_Time
        or else
          Get_Template_Type_Strat_ADB.File_Time_Stamp = GNAT.Calendar.No_Time
      then
         raise Program_Error with "Template file does not exists.";
      end if;

      return Type_Value_Generator'(Pkg_Name, Types, Type_Depends);
   end Create;

   function Create_Type_Value_ADS_Translator
     (Types : Typ_Set;
      Next  : access constant Translator'Class := null)
      return Type_Value_ADS_Translator is
      ((Next, Types));

   function Create_Type_Value_ADB_Translator
     (Types : Typ_Set;
      Next  : access constant Translator'Class := null)
      return Type_Value_ADB_Translator
   is
     ((Next, Types));

   function Create_With_Clauses_Translator
     (Type_Depends : Typ_Set;
      Next         : access constant Translator'Class := null)
      return With_Clauses_Translator is
     ((Next, Type_Depends));

   ----------------------
   -- Translate_Helper --
   ----------------------

   procedure Translate_Helper
     (Self  : Type_Value_ADS_Translator;
      Table : in out Templates_Parser.Translate_Set)
   is
      use type Templates_Parser.Vector_Tag;

      Type_Strategy_Tag : constant String := "TYPE_STRATEGY";

      Type_Strategy_Vector_Tag : Templates_Parser.Vector_Tag;
   begin
      for T of Self.Types loop
         Templates_Parser.Append
           (Type_Strategy_Vector_Tag,
            "function " & T.Get.Gen_Random_Function_Name & " return "
            & (+T.Get.Fully_Qualified_Name) & ";");
         if T.Get.Is_Constrained then
            Templates_Parser.Append
              (Type_Strategy_Vector_Tag,
               To_String (Gen_Constrained_Function (T.Get)) & ";");
         end if;
      end loop;

      Templates_Parser.Insert
        (Table,
         Templates_Parser.Assoc
           (Type_Strategy_Tag, Type_Strategy_Vector_Tag));
   end Translate_Helper;

   ----------------------
   -- Translate_Helper --
   ----------------------

   procedure Translate_Helper
     (Self  : Type_Value_ADB_Translator;
      Table : in out Templates_Parser.Translate_Set)
   is
      use type Templates_Parser.Vector_Tag;

      Type_Strategy_Tag : constant String := "TYPE_STRATEGY";

      Type_Strategy_Vector_Tag : Templates_Parser.Vector_Tag;
   begin
      for T of Self.Types loop
         Templates_Parser.Append
           (Type_Strategy_Vector_Tag,
            T.Get.Generate_Random_Strategy);

         if T.Get.Is_Constrained then
            Templates_Parser.Append
              (Type_Strategy_Vector_Tag,
               T.Get.Generate_Constrained_Random_Strategy);
         end if;
      end loop;

      Templates_Parser.Insert
        (Table,
         Templates_Parser.Assoc
           (Type_Strategy_Tag, Type_Strategy_Vector_Tag));
   end Translate_Helper;

   ----------------------
   -- Translate_Helper --
   ----------------------

   procedure Translate_Helper
     (Self  : With_Clauses_Translator;
      Table : in out Templates_Parser.Translate_Set)
   is
      use type Templates_Parser.Vector_Tag;

      With_Clauses_Tag : constant String := "WITH_PACKAGE";

      With_Clauses_Vector_Tag : Templates_Parser.Vector_Tag;
   begin
      for T of Self.Types loop
         Templates_Parser.Append
           (With_Clauses_Vector_Tag,
            Type_Strat_Package_Name
              (+T.Get.Parent_Package_Fully_Qualified_Name));
      end loop;

      Templates_Parser.Insert
        (Table,
         Templates_Parser.Assoc
           (With_Clauses_Tag, With_Clauses_Vector_Tag));
   end Translate_Helper;
end TGen.Gen_Types_Value;
