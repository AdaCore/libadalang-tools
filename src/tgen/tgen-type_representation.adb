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

with GNAT.OS_Lib;

pragma Warnings (Off);
with TGen.Strings;                 use TGen.Strings;
with TGen.Templates;
with TGen.Types.Array_Types;       use TGen.Types.Array_Types;
with TGen.Types.Discrete_Types;    use TGen.Types.Discrete_Types;
with TGen.Types.Enum_Types;        use TGen.Types.Enum_Types;
with TGen.Types.Int_Types;         use TGen.Types.Int_Types;
with TGen.Types.Real_Types;        use TGen.Types.Real_Types;
with TGen.Types.Record_Types;      use TGen.Types.Record_Types;
pragma Warnings (On);

package body TGen.Type_Representation is

   ------------------------------------------
   -- Generate_Type_Representation_For_Typ --
   ------------------------------------------

   procedure Generate_Type_Representation_For_Typ
     (F_Spec, F_Body     : File_Type with Unreferenced;
      Typ                : TGen.Types.Typ'Class;
      Templates_Root_Dir : String;
      Init_Package_Code  : in out Tag)
   is
      TRD : constant String :=
        Templates_Root_Dir
        & GNAT.OS_Lib.Directory_Separator
        & "type_representation_templates"
        & GNAT.OS_Lib.Directory_Separator;

      package Templates is new TGen.Templates (TRD);
      use Templates.Type_Representation;

      Ty_Prefix : constant String := Typ.Slug;
      Ty_Name   : constant String := Typ.Fully_Qualified_Name;

      Assocs : constant Translate_Table :=
        [1 => Assoc ("TY_NAME", Ty_Name),
         2 => Assoc ("TY_PREFIX", Ty_Prefix)];
   begin
      if Typ in Record_Typ'Class then
         declare
            Rec_Typ : constant Record_Typ'Class :=
              Record_Typ'Class (Typ);
            Comp_Names : Vector_Tag;
            Comp_Types : Vector_Tag;
         begin
            for Cur in Rec_Typ.Component_Types.Iterate loop
               declare
                  use Component_Maps;
                  Comp_Name : constant Unbounded_String := Key (Cur);
                  Comp_Type : constant Unbounded_String :=
                    +Element (Cur).Get.Slug;
               begin
                  Comp_Names := Comp_Names & Comp_Name;
                  Comp_Types := Comp_Types & Comp_Type;
               end;
            end loop;

            --  put the template

            declare
               Assocs_Rec : constant Translate_Table := Assocs &
               [Assoc ("COMP_NAME", Comp_Names),
                Assoc ("COMP_TYPE", Comp_Types)];
            begin
               Put_Line
                 (F_Spec, Parse (Function_Typ_Spec_Template, Assocs_Rec));
               Init_Package_Code :=
                 Init_Package_Code & String'
                   (Parse (Function_Typ_Init_Template, Assocs_Rec));
            end;
         end;
      elsif Typ in Signed_Int_Typ'Class then
         Put_Line (F_Spec, Parse (Signed_Int_Spec_Template, Assocs));
         Init_Package_Code :=
           Init_Package_Code & String'
             (Parse (Signed_Int_Typ_Init_Template, Assocs));
      end if;

   end Generate_Type_Representation_For_Typ;

end TGen.Type_Representation;
