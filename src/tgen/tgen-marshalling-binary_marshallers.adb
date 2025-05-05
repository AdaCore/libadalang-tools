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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with TGen.Templates;
with TGen.Types.Array_Types;  use TGen.Types.Array_Types;
with TGen.Types.Record_Types; use TGen.Types.Record_Types;

package body TGen.Marshalling.Binary_Marshallers is

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
        & "marshalling_templates"
        & GNAT.OS_Lib.Directory_Separator;

      package Templates is new TGen.Templates (TRD);
      use Templates.Binary_Marshalling;

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
         7 => Assoc ("IS_SCALAR", Typ in Scalar_Typ'Class),
         8 => Assoc ("HAS_STATIC_PREDICATE", Typ.Has_Static_Predicate)];

      function Component_Read
        (Assocs : Translate_Table) return Unbounded_String;
      function Component_Read_Indexed
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
      procedure Print_Default_Header (Assocs : Translate_Table);
      procedure Print_Scalar (Assocs : Translate_Table; For_Base : Boolean);
      procedure Print_Array (Assocs : Translate_Table);
      procedure Print_Record (Assocs : Translate_Table);
      procedure Print_Header_Wrappers (Assocs : Translate_Table);

      --------------------
      -- Component_Read --
      --------------------

      function Component_Read
        (Assocs : Translate_Table) return Unbounded_String is
      begin
         return
           Parse
             (Component_Read_Write_Template,
              Assocs & Assoc ("ACTION", "Read"));
      end Component_Read;

      ----------------------------
      -- Component_Read_Indexed --
      ----------------------------

      function Component_Read_Indexed
        (Assocs : Translate_Table) return Unbounded_String is
      begin
         pragma Unreferenced (Assocs);
         return To_Unbounded_String ("%%_UNSUPPORTED_%%");
      end Component_Read_Indexed;

      ---------------------
      -- Component_Write --
      ---------------------

      function Component_Write
        (Assocs : Translate_Table) return Unbounded_String is
      begin
         return
           Parse
             (Component_Read_Write_Template,
              Assocs & Assoc ("ACTION", "Write"));
      end Component_Write;

      --------------------
      -- Component_Size --
      --------------------

      function Component_Size
        (Assocs : Translate_Table) return Unbounded_String is
      begin
         return Parse (Component_Size_Template, Assocs);
      end Component_Size;

      ------------------------
      -- Component_Size_Max --
      ------------------------

      function Component_Size_Max
        (Assocs : Translate_Table) return Unbounded_String is
      begin
         return Parse (Component_Size_Max_Template, Assocs);
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

      function Variant_Size (Assocs : Translate_Table) return Unbounded_String
      is
      begin
         return Parse (Variant_Size_Template, Assocs);
      end Variant_Size;

      ----------------------
      -- Variant_Size_Max --
      ----------------------

      function Variant_Size_Max
        (Assocs : Translate_Table) return Unbounded_String is
      begin
         return Parse (Variant_Size_Max_Template, Assocs);
      end Variant_Size_Max;

      ------------------
      -- Print_Header --
      ------------------

      procedure Print_Header (Assocs : Translate_Table) is
      begin
         Put_Line (Spec_Part, Parse (Header_Spec_Template, Assocs));
         New_Line (Spec_Part);
         Put_Line (Private_Part, Parse (Header_Private_Template, Assocs));
         New_Line (Private_Part);
         Put_Line (Body_Part, Parse (Header_Body_Template, Assocs));
         New_Line (Body_Part);
      end Print_Header;

      --------------------------
      -- Print_Default_Header --
      --------------------------

      procedure Print_Default_Header (Assocs : Translate_Table) is
      begin
         Put_Line (Spec_Part, Parse (Default_Header_Spec_Template, Assocs));
         New_Line (Spec_Part);
      end Print_Default_Header;

      ------------------
      -- Print_Scalar --
      ------------------

      procedure Print_Scalar (Assocs : Translate_Table; For_Base : Boolean) is
      begin
         if For_Base and then Typ.Private_Extension then
            Put_Line (Private_Part, Parse (Scalar_Base_Spec_Template, Assocs));
            New_Line (Private_Part);
         else
            Put_Line (Spec_Part, Parse (Scalar_Base_Spec_Template, Assocs));
            New_Line (Spec_Part);
         end if;
         Put_Line (Private_Part, Parse (Scalar_Base_Private_Template, Assocs));
         New_Line (Private_Part);
         Put_Line (Body_Part, Parse (Scalar_Read_Write_Template, Assocs));
         New_Line (Body_Part);
      end Print_Scalar;

      -----------------
      -- Print_Array --
      -----------------

      procedure Print_Array (Assocs : Translate_Table) is

         --  Check that the Size_Max function can be declared in the public
         --  part of the support package: this is not the case as soon as
         --  one of the index types of the array is fully private.

         Size_Max_Pub : constant Boolean :=
           not (for some Idx_Typ of Array_Typ'Class (Typ).Index_Types =>
                  Idx_Typ.all.Fully_Private);
      begin
         Put_Line (Spec_Part, Parse (Composite_Base_Spec_Template, Assocs));
         New_Line (Spec_Part);

         if Size_Max_Pub then
            Put_Line (Spec_Part, Parse (Composite_Size_Max_Template, Assocs));
         else
            Put_Line
              (Private_Part, Parse (Composite_Size_Max_Template, Assocs));
         end if;
         Put_Line (Body_Part, Parse (Array_Read_Write_Template, Assocs));
         New_Line (Body_Part);
         Put_Line (Body_Part, Parse (Array_Size_Template, Assocs));
         New_Line (Body_Part);
         Put_Line (Body_Part, Parse (Array_Size_Max_Template, Assocs));
         New_Line (Body_Part);

      end Print_Array;

      procedure Print_Derived_Private_Subtype (Assocs : Translate_Table);

      procedure Print_Derived_Private_Subtype (Assocs : Translate_Table) is
      begin
         New_Line (Body_Part);
         Put_Line
           (Body_Part, Parse (Derived_Private_Subtype_Template, Assocs));
      end Print_Derived_Private_Subtype;

      ------------------
      -- Print_Record --
      ------------------

      procedure Print_Record (Assocs : Translate_Table) is

         --  Check wether the Size_Max function can be placed in the public
         --  part: This is not the case as soon as one of the discriminants
         --  is fully private.

         pragma Style_Checks (Off);
         Size_Max_Pub : constant Boolean :=
           not (Typ in Discriminated_Record_Typ'Class)
           or else not (for some Disc_Typ of
                          Discriminated_Record_Typ'Class (Typ)
                            .Discriminant_Types =>
                          Disc_Typ.all.Fully_Private);
         pragma Style_Checks (On);
      begin
         Put_Line (Spec_Part, Parse (Composite_Base_Spec_Template, Assocs));
         New_Line (Spec_Part);
         if Size_Max_Pub then
            Put_Line (Spec_Part, Parse (Composite_Size_Max_Template, Assocs));
         else
            Put_Line
              (Private_Part, Parse (Composite_Size_Max_Template, Assocs));
         end if;
         Put_Line (Body_Part, Parse (Record_Read_Write_Template, Assocs));
         New_Line (Body_Part);
         Put_Line (Body_Part, Parse (Record_Size_Template, Assocs));
         New_Line (Body_Part);
         Put_Line (Body_Part, Parse (Record_Size_Max_Template, Assocs));
         New_Line (Body_Part);
      end Print_Record;

      ---------------------------
      -- Print_Header_Wrappers --
      ---------------------------

      procedure Print_Header_Wrappers (Assocs : Translate_Table) is
         Header_Wrapper : constant String :=
           Parse (Header_Wrappers_Spec_Template, Assocs);
         pragma Unreferenced (Header_Wrapper);
      begin
         Put_Line (Spec_Part, Parse (Header_Wrappers_Spec_Template, Assocs));
         New_Line (Spec_Part);
         Put_Line (Body_Part, Parse (Header_Wrappers_Body_Template, Assocs));
         New_Line (Body_Part);
      end Print_Header_Wrappers;

      procedure Generate_Base_Functions_For_Typ_Instance is new
        Generate_Base_Functions_For_Typ
          (Differentiate_Discrete        => True,
           Component_Read                => Component_Read,
           Component_Read_Indexed        => Component_Read_Indexed,
           Component_Write               => Component_Write,
           Component_Size                => Component_Size,
           Component_Size_Max            => Component_Size_Max,
           Variant_Read_Write            => Variant_Read_Write,
           Variant_Size                  => Variant_Size,
           Variant_Size_Max              => Variant_Size_Max,
           Print_Header                  => Print_Header,
           Print_Default_Header          => Print_Default_Header,
           Print_Scalar                  => Print_Scalar,
           Print_Array                   => Print_Array,
           Print_Record                  => Print_Record,
           Print_Header_Wrappers         => Print_Header_Wrappers,
           Print_Derived_Private_Subtype => Print_Derived_Private_Subtype);
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

end TGen.Marshalling.Binary_Marshallers;
