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
   Template_Folder :  String;
package TGen.Marshalling_Templates is
   Array_Read_Write_Template     : constant String :=
     Template_Folder & "array_read_write.tmplt";
   Component_Read_Template       : constant String :=
     Template_Folder & "component_read.tmplt";
   Component_Write_Template      : constant String :=
     Template_Folder & "component_write.tmplt";
   Component_Size_Max_Template   : constant String :=
     Template_Folder & "component_size_max.tmplt";
   Component_Size_Template       : constant String :=
     Template_Folder & "component_size.tmplt";
   Composite_Base_Spec_Template  : constant String :=
     Template_Folder & "composite_base_spec.tmplt";
   Default_Header_Spec_Template  : constant String :=
     Template_Folder & "default_header_spec.tmplt";
   Header_Body_Template          : constant String :=
     Template_Folder & "header_body.tmplt";
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

   package Binary is
      Component_Read_Write_Template : constant String :=
        Template_Folder & "component_read_write.tmplt";
   end Binary;

end TGen.Marshalling_Templates;
