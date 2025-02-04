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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Utils.String_Utilities;

with TGen.Strings; use TGen.Strings;
with TGen.Templates;

package body TGen.Marshalling.JSON_Marshallers is

   --------------------------------------------
   -- Generate_Marshalling_Functions_For_Typ --
   --------------------------------------------

   procedure Generate_Marshalling_Functions_For_Typ
     (Spec_Part          : US_Access;
      Private_Part       : US_Access;
      Body_Part          : US_Access;
      Typ                : TGen.Types.Typ'Class;
      Constrains_Array   : Boolean;
      Templates_Root_Dir : String)
   is
      TRD : constant String :=
        Templates_Root_Dir
        & GNAT.OS_Lib.Directory_Separator
        & "json_templates"
        & GNAT.OS_Lib.Directory_Separator;

      package Templates is new TGen.Templates (TRD);
      use Templates.JSON_Marshalling;

      Ty_Name      : constant String := Typ.FQN (No_Std => True);
      Ty_Prefix    : constant String := Prefix_For_Typ (Typ.Slug);
      Generic_Name : constant String :=
        (if Needs_Header (Typ) then "In_Out_Unconstrained" else "In_Out");
      Assocs       : constant Translate_Table :=
        [1 => Assoc ("TY_NAME", Ty_Name),
         2 => Assoc ("TY_PREFIX", Ty_Prefix),
         3 => Assoc ("MARSHALLING_LIB", Marshalling_Lib),
         4 => Assoc ("GENERIC_NAME", Generic_Name),
         5 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
         6 => Assoc ("NEEDS_HEADER", Needs_Header (Typ)),
         7 => Assoc ("IS_SCALAR", Typ in Scalar_Typ'Class)];

      function Component_Read
        (Assocs : Translate_Table) return Unbounded_String;
      function Component_Write
        (Assocs : Translate_Table) return Unbounded_String;
      function Component_Size
        (Assocs : Translate_Table) return Unbounded_String;
      function Component_Size_Max
        (Assocs : Translate_Table) return Unbounded_String;
      function Variant_Read_Write
        (Assocs : Translate_Table) return Unbounded_String;
      function Variant_Size (Assocs : Translate_Table) return Unbounded_String;
      function Variant_Size_Max
        (Assocs : Translate_Table) return Unbounded_String;
      procedure Print_Header (Assocs : Translate_Table);
      procedure Print_Default_Header (Assocs : Translate_Table) is null;
      procedure Print_Scalar (Assocs : Translate_Table; For_Base : Boolean);
      procedure Print_Array (Assocs : Translate_Table);
      procedure Print_Record (Assocs : Translate_Table);
      procedure Print_Header_Wrappers (Assocs : Translate_Table);

      ---------------------
      -- Component_Write --
      ---------------------

      function Component_Write
        (Assocs : Translate_Table) return Unbounded_String is
      begin
         return Parse (Component_Write_Template, Assocs);
      end Component_Write;

      --------------------
      -- Component_Read --
      --------------------

      function Component_Read
        (Assocs : Translate_Table) return Unbounded_String is
      begin
         return Parse (Component_Read_Template, Assocs);
      end Component_Read;

      --------------------
      -- Component_Size --
      --------------------

      function Component_Size
        (Assocs : Translate_Table with Unreferenced) return Unbounded_String is
      begin
         return +"";
      end Component_Size;

      ------------------------
      -- Component_Size_Max --
      ------------------------

      function Component_Size_Max
        (Assocs : Translate_Table with Unreferenced) return Unbounded_String is
      begin
         return +"";
      end Component_Size_Max;

      ------------------------
      -- Variant_Read_Write --
      ------------------------

      function Variant_Read_Write
        (Assocs : Translate_Table) return Unbounded_String is
      begin
         return Parse (Variant_Read_Write_Template, Assocs);
      end Variant_Read_Write;

      ------------------
      -- Variant_Size --
      ------------------

      function Variant_Size
        (Assocs : Translate_Table with Unreferenced) return Unbounded_String is
      begin
         return +"";
      end Variant_Size;

      ----------------------
      -- Variant_Size_Max --
      ----------------------

      function Variant_Size_Max
        (Assocs : Translate_Table with Unreferenced) return Unbounded_String is
      begin
         return +"";
      end Variant_Size_Max;

      ------------------
      -- Print_Header --
      ------------------

      procedure Print_Header (Assocs : Translate_Table) is
      begin
         Put_Line (Spec_Part, Parse (Header_Spec_Template, Assocs));
         New_Line (Spec_Part);
         Put_Line (Body_Part, Parse (Header_Body_Template, Assocs));
         New_Line (Body_Part);
      end Print_Header;

      ------------------
      -- Print_Scalar --
      ------------------

      procedure Print_Scalar
        (Assocs : Translate_Table; For_Base : Boolean with Unreferenced) is
      begin
         if For_Base and then Typ.Private_Extension then
            Put_Line (Private_Part, Parse (Scalar_Base_Spec_Template, Assocs));
            New_Line (Private_Part);
         else
            Put_Line (Spec_Part, Parse (Scalar_Base_Spec_Template, Assocs));
            New_Line (Spec_Part);
         end if;
         Put_Line (Body_Part, Parse (Scalar_Read_Write_Template, Assocs));
         New_Line (Body_Part);
      end Print_Scalar;

      -----------------
      -- Print_Array --
      -----------------

      procedure Print_Array (Assocs : Translate_Table) is
      begin
         Put_Line (Spec_Part, Parse (Composite_Base_Spec_Template, Assocs));
         New_Line (Spec_Part);
         Put_Line (Body_Part, Parse (Array_Read_Write_Template, Assocs));
         New_Line (Body_Part);
      end Print_Array;

      ------------------
      -- Print_Record --
      ------------------

      procedure Print_Record (Assocs : Translate_Table) is
      begin
         Put_Line (Spec_Part, Parse (Composite_Base_Spec_Template, Assocs));
         New_Line (Spec_Part);
         Put_Line (Body_Part, Parse (Record_Read_Write_Template, Assocs));
         New_Line (Body_Part);
      end Print_Record;

      ---------------------------
      -- Print_Header_Wrappers --
      ---------------------------

      procedure Print_Header_Wrappers (Assocs : Translate_Table) is
      begin
         Put_Line (Spec_Part, Parse (Header_Wrappers_Spec_Template, Assocs));
         New_Line (Spec_Part);
         Put_Line (Body_Part, Parse (Header_Wrappers_Body_Template, Assocs));
         New_Line (Body_Part);
      end Print_Header_Wrappers;

      procedure Generate_Base_Functions_For_Typ_Instance is new
        Generate_Base_Functions_For_Typ
          (Differentiate_Discrete => False,
           Component_Read         => Component_Read,
           Component_Write        => Component_Write,
           Component_Size         => Component_Size,
           Component_Size_Max     => Component_Size_Max,
           Variant_Read_Write     => Variant_Read_Write,
           Variant_Size           => Variant_Size,
           Variant_Size_Max       => Variant_Size_Max,
           Print_Header           => Print_Header,
           Print_Default_Header   => Print_Default_Header,
           Print_Scalar           => Print_Scalar,
           Print_Array            => Print_Array,
           Print_Record           => Print_Record,
           Print_Header_Wrappers  => Print_Header_Wrappers);
   begin
      --  Generate the base functions for Typ

      Generate_Base_Functions_For_Typ_Instance (Typ);

      --  If the type is used as an array index constraint, also generate the
      --  functions for Typ'Base.

      if Constrains_Array then
         Generate_Base_Functions_For_Typ_Instance (Typ, For_Base => True);
      end if;

      --  Generate the Input and Output subprograms

      Put_Line (Spec_Part, Parse (In_Out_Spec_Template, Assocs));
      New_Line (Spec_Part);

      Put_Line (Body_Part, Parse (In_Out_Body_Template, Assocs));
      New_Line (Body_Part);

   end Generate_Marshalling_Functions_For_Typ;

   --------------------------------------
   -- Generate_TC_Serializers_For_Subp --
   --------------------------------------

   procedure Generate_TC_Serializers_For_Subp
     (Spec_Part          : US_Access;
      Private_Part       : US_Access;
      Body_Part          : US_Access;
      FN_Typ             : Function_Typ'Class;
      Templates_Root_Dir : String)
   is
      use Component_Maps;

      TRD : constant String :=
        Templates_Root_Dir
        & GNAT.OS_Lib.Directory_Separator
        & "json_templates"
        & GNAT.OS_Lib.Directory_Separator;

      package Templates is new TGen.Templates (TRD);
      use Templates.JSON_Marshalling;

      Assocs : Translate_Set;

      Param_Names      : Vector_Tag;
      Param_Types      : Vector_Tag;
      Param_Full_Types : Vector_Tag;
      Param_FNs        : Vector_Tag;

      Global_Names      : Vector_Tag;
      Global_Types      : Vector_Tag;
      Global_Full_Types : Vector_Tag;
      Global_Slugs      : Vector_Tag;
      Global_Types_FNs  : Vector_Tag;
      Proc_Name         : constant String := FN_Typ.Slug;
   begin
      if FN_Typ.Component_Types.Is_Empty and then FN_Typ.Globals.Is_Empty then
         return;
      end if;
      Assocs.Insert (Assoc ("GLOBAL_PREFIX", Global_Prefix));
      Assocs.Insert
        (Assoc
           ("PROC_NAME",
            (if Is_Operator (Proc_Name) then Map_Operator_Name (Proc_Name)
             else Proc_Name)));
      Assocs.Insert
        (Assoc
           ("PROC_FQN",
            Utils.String_Utilities.Escape_String_Literal
              (FN_Typ.FQN (No_Std => True))));
      Assocs.Insert (Assoc ("PROC_UID", FN_Typ.Subp_UID));
      if FN_Typ.Ret_Typ /= SP.Null_Ref then
         Assocs.Insert
           (Assoc ("PROC_RETURN_TY", FN_Typ.Ret_Typ.Get.FQN (No_Std => True)));
      else
         Assocs.Insert (Assoc ("PROC_RETURN_TY", ""));
      end if;

      --  Deal with the parameters of the subprogram

      for Param_Cur in FN_Typ.Component_Types.Iterate loop
         Param_Names.Append (Unbounded_String (Key (Param_Cur)));
         Param_Types.Append
           (FN_Typ.Component_Types.Constant_Reference (Param_Cur).Get.FQN
              (No_Std => True));
         Param_Full_Types.Append
           (FN_Typ.Component_Types.Constant_Reference (Param_Cur).Get.FQN
              (No_Std => False));
         Param_FNs.Append
           (Output_Fname_For_Typ
              (FN_Typ.Component_Types.Constant_Reference (Param_Cur)
                 .Get
                 .Name));
      end loop;
      Assocs.Insert (Assoc ("PARAM_NAME", Param_Names));
      Assocs.Insert (Assoc ("PARAM_TY", Param_Types));
      Assocs.Insert (Assoc ("LAL_PARAM_TY", Param_Full_Types));
      Assocs.Insert (Assoc ("PARAM_TY_OUTPUT_FN", Param_FNs));

      --  Deal with the global inputs of the subprogram

      for Global_Cur in FN_Typ.Globals.Iterate loop
         Global_Names.Append (Key (Global_Cur));
         Global_Types.Append
           (FN_Typ.Globals.Constant_Reference (Global_Cur).Get.FQN
              (No_Std => True));
         Global_Full_Types.Append
           (FN_Typ.Globals.Constant_Reference (Global_Cur).Get.FQN
              (No_Std => False));
         Global_Types_FNs.Append
           (Output_Fname_For_Typ
              (FN_Typ.Globals.Constant_Reference (Global_Cur).Get.Name));
         Global_Slugs.Append
           (To_Symbol (To_Qualified_Name (+Key (Global_Cur)), Sep => '_'));
      end loop;
      Assocs.Insert (Assoc ("GLOBAL_NAME", Global_Names));
      Assocs.Insert (Assoc ("GLOBAL_TY", Global_Types));
      Assocs.Insert (Assoc ("LAL_GLOBAL_TY", Global_Full_Types));
      Assocs.Insert (Assoc ("GLOBAL_TY_OUTPUT_FN", Global_Types_FNs));
      Assocs.Insert (Assoc ("GLOBAL_SLUG", Global_Slugs));

      --  First generate the spec, in the correct part of the spec

      if FN_Typ.Fully_Private then
         Assocs.Insert (Assoc ("FOR_SPEC", True));
         Put_Line (Private_Part, Parse (Function_TC_Dump_Template, Assocs));
         New_Line (Private_Part);
      else
         Assocs.Insert (Assoc ("FOR_SPEC", True));
         Put_Line (Spec_Part, Parse (Function_TC_Dump_Template, Assocs));
         New_Line (Spec_Part);
      end if;

      --  Then the body

      Assocs.Insert (Assoc ("FOR_SPEC", False));
      Put_Line (Body_Part, Parse (Function_TC_Dump_Template, Assocs));
      New_Line (Body_Part);

   end Generate_TC_Serializers_For_Subp;

end TGen.Marshalling.JSON_Marshallers;
