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

--  Template files for marshalling generation purposes

generic
   Template_Folder : String;
package TGen.Templates is

   package Binary_Marshalling is
      Array_Read_Write_Template     : constant String :=
        Template_Folder & "array_read_write.tmplt";
      Component_Read_Write_Template : constant String :=
        Template_Folder & "component_read_write.tmplt";
      Component_Size_Max_Template   : constant String :=
        Template_Folder & "component_size_max.tmplt";
      Component_Size_Template       : constant String :=
        Template_Folder & "component_size.tmplt";
      Composite_Base_Spec_Template  : constant String :=
        Template_Folder & "composite_base_spec.tmplt";
      Composite_Size_Max_Template   : constant String :=
        Template_Folder & "composite_size_max.tmplt";
      Header_Body_Template          : constant String :=
        Template_Folder & "header_body.tmplt";
      Header_Private_Template       : constant String :=
        Template_Folder & "header_private.tmplt";
      Default_Header_Spec_Template  : constant String :=
        Template_Folder & "default_header_spec.tmplt";
      Header_Spec_Template          : constant String :=
        Template_Folder & "header_spec.tmplt";
      Header_Wrappers_Body_Template : constant String :=
        Template_Folder & "header_wrappers_body.tmplt";
      Header_Wrappers_Spec_Template : constant String :=
        Template_Folder & "header_wrappers_spec.tmplt";
      In_Out_Body_Template          : constant String :=
        Template_Folder & "in_out_body.tmplt";
      In_Out_Spec_Template          : constant String :=
        Template_Folder & "in_out_spec.tmplt";
      Record_Read_Write_Template    : constant String :=
        Template_Folder & "record_read_write.tmplt";
      Record_Size_Max_Template      : constant String :=
        Template_Folder & "record_size_max.tmplt";
      Record_Size_Template          : constant String :=
        Template_Folder & "record_size.tmplt";
      Scalar_Base_Spec_Template     : constant String :=
        Template_Folder & "scalar_base_spec.tmplt";
      Scalar_Base_Private_Template  : constant String :=
        Template_Folder & "scalar_base_private.tmplt";
      Scalar_Read_Write_Template    : constant String :=
        Template_Folder & "scalar_read_write.tmplt";
      Variant_Read_Write_Template   : constant String :=
        Template_Folder & "variant_read_write.tmplt";
      Variant_Size_Max_Template     : constant String :=
        Template_Folder & "variant_size_max.tmplt";
      Variant_Size_Template         : constant String :=
        Template_Folder & "variant_size.tmplt";
      Array_Size_Max_Template       : constant String :=
        Template_Folder & "array_size_max.tmplt";
      Array_Size_Template           : constant String :=
        Template_Folder & "array_size.tmplt";
   end Binary_Marshalling;

   package JSON_Marshalling is
      Array_Read_Write_Template       : constant String :=
        Template_Folder & "array_read_write.tmplt";
      Component_Write_Template        : constant String :=
        Template_Folder & "component_write.tmplt";
      Component_Read_Template         : constant String :=
        Template_Folder & "component_read.tmplt";
      Component_Read_Indexed_Template : constant String :=
        Template_Folder & "component_read_indexed.tmplt";
      Composite_Base_Spec_Template    : constant String :=
        Template_Folder & "composite_base_spec.tmplt";
      Header_Body_Template            : constant String :=
        Template_Folder & "header_body.tmplt";
      Header_Spec_Template            : constant String :=
        Template_Folder & "header_spec.tmplt";
      Header_Wrappers_Body_Template   : constant String :=
        Template_Folder & "header_wrappers_body.tmplt";
      Header_Wrappers_Spec_Template   : constant String :=
        Template_Folder & "header_wrappers_spec.tmplt";
      In_Out_Body_Template            : constant String :=
        Template_Folder & "in_out_body.tmplt";
      In_Out_Spec_Template            : constant String :=
        Template_Folder & "in_out_spec.tmplt";
      Record_Read_Write_Template      : constant String :=
        Template_Folder & "record_read_write.tmplt";
      Scalar_Base_Spec_Template       : constant String :=
        Template_Folder & "scalar_base_spec.tmplt";
      Scalar_Read_Write_Template      : constant String :=
        Template_Folder & "scalar_read_write.tmplt";
      Variant_Read_Write_Template     : constant String :=
        Template_Folder & "variant_read_write.tmplt";
      Function_TC_Dump_Template       : constant String :=
        Template_Folder & "function_tc_dump.tmplt";
   end JSON_Marshalling;

   package Type_Representation is
      Scalar_Typ_Decl_Template     : constant String :=
        Template_Folder & "scalar_typ_decl.tmplt";
      Scalar_Typ_Init_Template     : constant String :=
        Template_Folder & "scalar_typ_init.tmplt";
      Record_Typ_Decl_Template     : constant String :=
        Template_Folder & "record_typ_decl.tmplt";
      Record_Typ_Init_Template     : constant String :=
        Template_Folder & "record_typ_init.tmplt";
      Anonymous_Typ_Decl_Template  : constant String :=
        Template_Folder & "anonymous_typ_decl.tmplt";
      Anonymous_Typ_Init_Template  : constant String :=
        Template_Folder & "anonymous_typ_init.tmplt";
      Custom_Strat_Spec_Template   : constant String :=
        Template_Folder & "custom_strat_spec.tmplt";
      Custom_Strat_Body_Template   : constant String :=
        Template_Folder & "custom_strat_body.tmplt";
      Variant_Choice_Decl_Template : constant String :=
        Template_Folder & "variant_choice_decl.tmplt";
      Variant_Choice_Init_Template : constant String :=
        Template_Folder & "variant_choice_init.tmplt";
      Variant_Decl_Template        : constant String :=
        Template_Folder & "variant_decl.tmplt";
      Variant_Init_Template        : constant String :=
        Template_Folder & "variant_init.tmplt";
      Constraint_Decl_Template     : constant String :=
        Template_Folder & "constraint_decl.tmplt";
      Constraint_Init_Template     : constant String :=
        Template_Folder & "constraint_init.tmplt";
      Instance_Decl_Template       : constant String :=
        Template_Folder & "instance_typ_decl.tmplt";
      Instance_Init_Template       : constant String :=
        Template_Folder & "instance_typ_init.tmplt";
      Array_Typ_Decl_Template      : constant String :=
        Template_Folder & "array_typ_decl.tmplt";
      Array_Typ_Init_Template      : constant String :=
        Template_Folder & "array_typ_init.tmplt";
      Function_Typ_Init_Template   : constant String :=
        Template_Folder & "function_typ_init.tmplt";
   end Type_Representation;

end TGen.Templates;
