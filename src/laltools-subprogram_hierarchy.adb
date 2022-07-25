------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
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
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Laltools.Call_Hierarchy; use Laltools.Call_Hierarchy;

package body Laltools.Subprogram_Hierarchy is

   package Subp_Renaming_Decl_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Subp_Renaming_Decl,
      "="          => "=");

   subtype Subp_Renaming_Decl_Vector is Subp_Renaming_Decl_Vectors.Vector;

   ---------------------------------
   -- Find_Base_Subp_Declarations --
   ---------------------------------

   function Find_Base_Subp_Declarations
     (Subp               : Basic_Decl'Class;
      Exclude_Itself     : Boolean := True;
      Imprecise_Fallback : Boolean := False)
      return Basic_Decl_Array
   is
      Unwinded_Subp : Basic_Decl;

   begin
      if Subp.Is_Null then
         return [];
      end if;

      if Subp.Kind in Ada_Subp_Renaming_Decl_Range then
         Unwinded_Subp :=
           Final_Renamed_Subp (Subp.As_Subp_Renaming_Decl);
      else
         Unwinded_Subp := Subp.As_Basic_Decl;
      end if;

      if Unwinded_Subp.Is_Null then
         return [];
      end if;

      declare
         Lal_Result : constant Basic_Decl_Array :=
           Unwinded_Subp.P_Base_Subp_Declarations (Imprecise_Fallback);
         Our_Result : Basic_Decl_Array (1 .. Lal_Result'Length - 1);
         Index      : Positive := 1;

      begin
         --  Libadalang returns an empty array if this is not a subprogram
         --  that's a primitive of a tagged type.

         if Lal_Result'Length = 0 then
            return [];
         end if;

         --  The result returned by Libadalang includes Subp; we want to remove
         --  this from the list.

         if Exclude_Itself then
            for J of Lal_Result loop
               if J /= Subp then
                  Our_Result (Index) := J;
                  Index := Index + 1;
               end if;
            end loop;

            return Our_Result;

         else
            return Lal_Result;
         end if;
      end;
   end Find_Base_Subp_Declarations;

   --------------------------------------
   -- Find_Generic_Subp_Instantiations --
   --------------------------------------

   function Find_Generic_Subp_Instantiations
     (Refs : Ref_Result_Array)
      return Generic_Subp_Instantiation_Array
   is
      Instantiations : Generic_Subp_Instantiation_Array (1 .. Refs'Length) :=
        [others => No_Generic_Subp_Instantiation];
      True_Length    : Natural := 0;

   begin
      for Reference of Refs loop
         if Ref (Reference).P_Semantic_Parent.Kind in
           Ada_Generic_Subp_Instantiation_Range
         then
            True_Length := True_Length + 1;
            Instantiations (True_Length) :=
              Ref (Reference).P_Semantic_Parent.As_Generic_Subp_Instantiation;
         end if;
      end loop;

      return Instantiations (1 .. True_Length);
   end Find_Generic_Subp_Instantiations;

   -------------------------------------
   -- Find_Subp_Renaming_Declarations --
   -------------------------------------

   function Find_Subp_Renaming_Declarations
     (Refs : Ref_Result_Array)
      return Subp_Renaming_Decl_Array
   is
      Subp_Renames : Subp_Renaming_Decl_Array (1 .. Refs'Length) :=
        [others => No_Subp_Renaming_Decl];
      True_Length  : Natural := 0;

   begin
      for Reference of Refs loop
         if Ref (Reference).P_Semantic_Parent.Kind in
           Ada_Subp_Renaming_Decl_Range
         then
            True_Length := True_Length + 1;
            Subp_Renames (True_Length) :=
              Ref (Reference).P_Semantic_Parent.As_Subp_Renaming_Decl;
         end if;
      end loop;

      return Subp_Renames (1 .. True_Length);
   end Find_Subp_Renaming_Declarations;

   -----------------------------------------------
   -- Find_Subp_Renaming_Declarations_Hierarchy --
   -----------------------------------------------

   function Find_Subp_Renaming_Declarations_Hierarchy
     (Subp  : Basic_Decl'Class;
      Units : Analysis_Unit_Array)
      return Subp_Renaming_Decl_Array
   is
      Unwinded_Subp : Basic_Decl;

      Hierarchy : Subp_Renaming_Decl_Vector;

      procedure Create_Hierarchy (Target : Basic_Decl'Class);
      --  Recursive function that finds all renames of 'Target' and appends
      --  them to Hierarchy. The recursion occurs on the found renames.

      --------------------
      -- Create_Hierarchy --
      --------------------

      procedure Create_Hierarchy (Target : Basic_Decl'Class) is
      begin
         for Rename_Decl of
           Find_Subp_Renaming_Declarations (Target, Units)
         loop
            Hierarchy.Append (Rename_Decl);
            Create_Hierarchy (Rename_Decl);
         end loop;
      end Create_Hierarchy;

   begin
      if Subp.Is_Null then
         return [];
      end if;

      if Subp.Kind in Ada_Subp_Renaming_Decl_Range then
         Unwinded_Subp :=
           Final_Renamed_Subp (Subp.As_Subp_Renaming_Decl);
      else
         Unwinded_Subp := Subp.As_Basic_Decl;
      end if;

      if Unwinded_Subp.Is_Null then
         return [];
      end if;

      Create_Hierarchy (Unwinded_Subp);

      if Hierarchy.Length = 0 then
         return [];

      else
         return Hierarchy_As_Array : Subp_Renaming_Decl_Array
           (1 .. Positive (Hierarchy.Length))
         do
            for J in 1 .. Positive (Hierarchy.Length) loop
               Hierarchy_As_Array (J) := Hierarchy.Element (J);
            end loop;
         end return;
      end if;
   end Find_Subp_Renaming_Declarations_Hierarchy;

   -------------------------
   -- Find_Subp_Relatives --
   -------------------------

   procedure Find_Subp_Relatives
     (Subp               : Basic_Decl'Class;
      Units              : Analysis_Unit_Array;
      Decls_Callback     : access procedure
        (Relative_Subp   : Basic_Decl'Class);
      Find_Calls         : Boolean := False;
      Calls_Callback     : access procedure
        (Call_Identifier : Base_Id'Class;
         Kind            : Ref_Result_Kind;
         Cancel          : in out Boolean) := null;
      Include_Base_Subps : Boolean := True;
      Include_Overrides  : Boolean := True)
   is
      Aux_Decl : Basic_Decl :=
        (if Subp.Is_Null then No_Basic_Decl else Subp.P_Canonical_Part);

      procedure Process_Generic_Subp;
      --  Processes generic subprogram declarations

      procedure Process_Subp;
      --  Processes non-generic subprogram declarations

      --------------------------
      -- Process_Generic_Subp --
      --------------------------

      procedure Process_Generic_Subp is
      begin
         Decls_Callback (Aux_Decl);

         for Instantiation of
           Find_Generic_Subp_Instantiations
             (Aux_Decl.As_Generic_Subp_Decl, Units)
         loop
            if Find_Calls then
               Find_Incoming_Calls
                 (Instantiation.P_Defining_Name, Units, Calls_Callback);
            end if;

            declare
               Rename_Decls : constant Subp_Renaming_Decl_Array :=
                 Find_Subp_Renaming_Declarations_Hierarchy
                   (Instantiation, Units);
            begin
               for Rename_Decl of Rename_Decls loop
                  Decls_Callback (Rename_Decl);

                  if Find_Calls then
                     Find_Incoming_Calls
                       (Rename_Decl.P_Defining_Name, Units, Calls_Callback);
                  end if;
               end loop;
            end;

         end loop;
      end Process_Generic_Subp;

      ------------------
      -- Process_Subp --
      ------------------

      procedure Process_Subp is
      begin
         for Subp_Decl of Get_Subp_Hierarchy
           (Aux_Decl, Units, Include_Base_Subps, Include_Overrides)
         loop
            Decls_Callback (Subp_Decl);

            if Find_Calls then
               Find_Incoming_Calls
                 (Subp_Decl.P_Defining_Name, Units, Calls_Callback, True);
            end if;

            declare
               Rename_Decls : constant Subp_Renaming_Decl_Array :=
                 Find_Subp_Renaming_Declarations_Hierarchy
                   (Subp_Decl, Units);
            begin
               for Rename_Decl of Rename_Decls loop
                  Decls_Callback (Rename_Decl);

                  if Find_Calls then
                     Find_Incoming_Calls
                       (Rename_Decl.P_Defining_Name, Units, Calls_Callback,
                        False);
                  end if;
               end loop;
            end;

         end loop;
      end Process_Subp;

   begin
      if Aux_Decl.Is_Null then
         return;
      end if;

      if Aux_Decl.Kind in Ada_Subp_Renaming_Decl_Range then
         Aux_Decl :=
           Final_Renamed_Subp (Aux_Decl.As_Subp_Renaming_Decl);
      end if;

      if Aux_Decl.Kind in Ada_Generic_Subp_Instantiation_Range then
         Aux_Decl :=
           Aux_Decl.As_Generic_Subp_Instantiation.P_Designated_Generic_Decl;
      end if;

      if Aux_Decl.Kind in Ada_Generic_Subp_Internal_Range
        and then Aux_Decl.Parent.Kind in Ada_Generic_Subp_Decl_Range
      then
         Aux_Decl := Aux_Decl.Parent.As_Basic_Decl;
      end if;

      if Aux_Decl.Kind in Ada_Generic_Subp_Decl_Range then
         Process_Generic_Subp;

      else
         Process_Subp;
      end if;
   end Find_Subp_Relatives;

   ------------------------
   -- Get_Subp_Hierarchy --
   ------------------------

   function Get_Subp_Hierarchy
     (Subp               : Basic_Decl;
      Units              : Analysis_Unit_Array;
      Include_Base_Subps : Boolean := True;
      Include_Overrides  : Boolean := True)
      return Basic_Decl_Array
   is
      function Hash (Decl : Basic_Decl) return Ada.Containers.Hash_Type is
        (Hash (Decl.As_Ada_Node));

      function Equivalent_Elements (L, R : Basic_Decl) return Boolean is
        (L.Image = R.Image);

      package Basic_Decl_Hashed_Sets is new Ada.Containers.Hashed_Sets
        (Element_Type        => Basic_Decl,
         Hash                => Hash,
         Equivalent_Elements => Equivalent_Elements,
         "="                 => "=");

      subtype Basic_Decl_Set is Basic_Decl_Hashed_Sets.Set;

      Hierarchy : Basic_Decl_Set;

      function Hierarchy_As_Array return Basic_Decl_Array;
      --  Convers Hierarchy as a Basic_Decl_Hashed_Set.Set into a
      --  Basic_Decl_Array

      -------------------------
      --  Hierarchy_As_Array --
      -------------------------

      function Hierarchy_As_Array return Basic_Decl_Array is
         Idx : Positive := 1;

      begin
         return A : Basic_Decl_Array (1 .. Positive (Hierarchy.Length))
         do
            for Decl of Hierarchy loop
               A (Idx) := Decl;
               Idx := Idx + 1;
            end loop;
         end return;
      end Hierarchy_As_Array;

   begin
      if Subp.Is_Null then
         return [];
      end if;

      if not Include_Base_Subps and not Include_Overrides then
         Hierarchy.Include (Subp.P_Canonical_Part);
      end if;

      if Include_Base_Subps and Include_Overrides then
         declare
            Root_Subps : constant Basic_Decl_Array :=
              Subp.P_Root_Subp_Declarations;

         begin
            if Root_Subps'Length = 0 then
               Hierarchy.Include (Subp.P_Canonical_Part);

               for Subp_Override of Subp.P_Find_All_Overrides (Units) loop
                  Hierarchy.Include (Subp_Override);
               end loop;

            else
               for Root_Subp of Root_Subps loop
                  Hierarchy.Include (Root_Subp);

                  for Root_Subp_Override
                    of Root_Subp.P_Find_All_Overrides (Units)
                  loop
                     Hierarchy.Include (Root_Subp_Override);
                  end loop;
               end loop;
            end if;
         end;
      end if;

      if Include_Overrides and not Include_Base_Subps then
         Hierarchy.Include (Subp);

         for Override of Subp.P_Find_All_Overrides (Units) loop
            Hierarchy.Include (Override.As_Basic_Decl);
         end loop;

         return Hierarchy_As_Array;
      end if;

      if not Include_Overrides and Include_Base_Subps then
         declare
            Root_Subps : constant Basic_Decl_Array :=
              Subp.P_Root_Subp_Declarations;

         begin
            if Root_Subps'Length = 0 then
               Hierarchy.Include (Subp.P_Canonical_Part);

            else
               for Root_Subp of Root_Subps loop
                  Hierarchy.Include (Root_Subp);

                  for Root_Subp_Override
                    of Root_Subp.P_Find_All_Overrides (Units)
                  loop
                     Hierarchy.Include (Root_Subp_Override);
                  end loop;
               end loop;

               for Subp_Override of Subp.P_Find_All_Overrides (Units) loop
                  Hierarchy.Delete (Subp_Override);
               end loop;
            end if;
         end;
      end if;

      return Hierarchy_As_Array;
   end Get_Subp_Hierarchy;

   -------------------------------
   -- Unwind_Subp_Renaming_Decl --
   -------------------------------

   function Final_Renamed_Subp
     (Subp : Subp_Renaming_Decl'Class)
      return Basic_Decl
   is
      Aux_Subp : Basic_Decl :=
        (if Subp.Is_Null then No_Basic_Decl else Subp.As_Basic_Decl);

   begin
      while not Aux_Subp.Is_Null
        and then Aux_Subp.Kind in Ada_Subp_Renaming_Decl_Range
      loop
         Aux_Subp :=
           Aux_Subp.As_Subp_Renaming_Decl.F_Renames.F_Renamed_Object.
             P_Referenced_Decl;
      end loop;

      return Aux_Subp;
   end Final_Renamed_Subp;

end Laltools.Subprogram_Hierarchy;
