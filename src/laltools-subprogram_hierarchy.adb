------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

   function Find_Subp_Renaming_Decls_Hierarchy
     (Subp  : Basic_Decl'Class;
      Units : Analysis_Unit_Array)
      return Subp_Renaming_Decl_Vector;
   --  TODO

   -------------------------------------
   -- Find_All_Base_Subp_Declarations --
   -------------------------------------

   function Find_All_Base_Subp_Declarations (Subp  : Basic_Decl)
                                             return Basic_Decl_Array
   is
      Lal_Result : constant Basic_Decl_Array := Subp.P_Base_Subp_Declarations;
      Our_Result : Basic_Decl_Array (1 .. Lal_Result'Length - 1);
      Index      : Positive := 1;
   begin
      --  Libadalang returns an empty array if this is not a subprogram
      --  that's a primitive of a tagged type.

      if Lal_Result'Length = 0 then
         return (1 .. 0 => <>);
      end if;

      --  The result returned by Libadalang includes Subp; we want to remove
      --  this from the list.

      for J of Lal_Result loop
         if J /= Subp then
            Our_Result (Index) := J;
            Index := Index + 1;
         end if;
      end loop;

      return Our_Result;
   end Find_All_Base_Subp_Declarations;

   --------------------------------------
   -- Find_Generic_Subp_Instantiations --
   --------------------------------------

   function Find_Generic_Subp_Instantiations
     (Refs : Ref_Result_Array)
      return Generic_Subp_Instantiation_Array
   is
      Instantiations : Generic_Subp_Instantiation_Array (1 .. Refs'Length) :=
        (others => No_Generic_Subp_Instantiation);
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

   ------------------------------
   -- Find_Subp_Renaming_Decls --
   ------------------------------

   function Find_Subp_Renaming_Decls
     (Refs : Ref_Result_Array)
      return Subp_Renaming_Decl_Array
   is
      Subp_Renames : Subp_Renaming_Decl_Array (1 .. Refs'Length) :=
        (others => No_Subp_Renaming_Decl);
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
   end Find_Subp_Renaming_Decls;

   ----------------------------------------
   -- Find_Subp_Renaming_Decls_Hierarchy --
   ----------------------------------------

   function Find_Subp_Renaming_Decls_Hierarchy
     (Subp  : Basic_Decl'Class;
      Units : Analysis_Unit_Array)
      return Subp_Renaming_Decl_Vector
   is
      All_Renames : Subp_Renaming_Decl_Vector;

   begin
      for Rename_Decl of Find_Subp_Renaming_Decls (Subp, Units) loop
         All_Renames.Append (Rename_Decl);
         All_Renames.Append
           (Find_Subp_Renaming_Decls_Hierarchy (Rename_Decl, Units));
      end loop;

      return All_Renames;
   end Find_Subp_Renaming_Decls_Hierarchy;

   ----------------------------------------
   -- Find_Subp_Renaming_Decls_Hierarchy --
   ----------------------------------------

   function Find_Subp_Renaming_Decls_Hierarchy
     (Subp_Decl : Basic_Decl'Class;
      Units    : Analysis_Unit_Array)
      return Subp_Renaming_Decl_Array
   is
      All_Renames : constant Subp_Renaming_Decl_Vector :=
        Find_Subp_Renaming_Decls_Hierarchy (Subp_Decl, Units);

   begin
      if All_Renames.Length = 0 then
         return All_Renames_Array : Subp_Renaming_Decl_Array (1 .. 0);
      else
         return All_Renames_Array : Subp_Renaming_Decl_Array
           (1 .. Positive (All_Renames.Length))
         do
            for J in 1 .. Positive (All_Renames.Length) loop
               All_Renames_Array (J) := All_Renames.Element (J);
            end loop;
         end return;
      end if;
   end Find_Subp_Renaming_Decls_Hierarchy;

   -------------------------
   -- Find_Subp_Relatives --
   -------------------------

   procedure Find_Subp_Relatives
     (Subp           : Basic_Decl'Class;
      Units          : Analysis_Unit_Array;
      Decl_Callback  : access procedure (Relative_Subp : Basic_Decl'Class);
      Find_Calls     : Boolean := False;
      Calls_Callback : access procedure (Call : Call_Stmt) := null)
   is
      Aux_Decl : Basic_Decl := Subp.P_Canonical_Part;

      procedure Process_Generic_Subp;

      procedure Process_Subp;

      --------------------------
      -- Process_Generic_Subp --
      --------------------------

      procedure Process_Generic_Subp is
      begin
         Decl_Callback (Aux_Decl);

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
                 Find_Subp_Renaming_Decls_Hierarchy (Instantiation, Units);
            begin
               for Rename_Decl of Rename_Decls loop
                  Decl_Callback (Rename_Decl);

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
         for Subp_Decl of Get_Subp_Hierarchy (Aux_Decl, Units) loop
            Decl_Callback (Subp_Decl);

            if Find_Calls then
               Find_Incoming_Calls
                 (Subp_Decl.P_Defining_Name, Units, Calls_Callback);
            end if;

            declare
               Rename_Decls : constant Subp_Renaming_Decl_Array :=
                 Find_Subp_Renaming_Decls_Hierarchy (Subp_Decl, Units);
            begin
               for Rename_Decl of Rename_Decls loop
                  Decl_Callback (Rename_Decl);

                  if Find_Calls then
                     Find_Incoming_Calls
                       (Rename_Decl.P_Defining_Name, Units, Calls_Callback);
                  end if;
               end loop;
            end;

         end loop;
      end Process_Subp;

   begin
      if Aux_Decl.Kind in Ada_Subp_Renaming_Decl_Range then
         while Aux_Decl.Kind in Ada_Subp_Renaming_Decl_Range loop
            Aux_Decl :=
              Aux_Decl.As_Subp_Renaming_Decl.F_Renames.F_Renamed_Object.
                P_Referenced_Defining_Name.P_Basic_Decl;
         end loop;

         if Aux_Decl.Kind in Ada_Generic_Subp_Instantiation_Range then
            Aux_Decl :=
              Aux_Decl.As_Generic_Subp_Instantiation.P_Designated_Generic_Decl;
         end if;
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
     (Subp  : Basic_Decl;
      Units : Analysis_Unit_Array)
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

      Hierarchy : Basic_Decl_Hashed_Sets.Set;

      function To_Array return Basic_Decl_Array;
      --  Convers a Basic_Decl_Hashed_Set.Set into an array

      ----------------
      --  To_Vector --
      ----------------

      function To_Array return Basic_Decl_Array is
         Idx : Positive := 1;
      begin
         return A : Basic_Decl_Array (1 .. Positive (Hierarchy.Length))
         do
            for Decl of Hierarchy loop
               A (Idx) := Decl;
               Idx := Idx + 1;
            end loop;
         end return;
      end To_Array;
   begin
      Hierarchy.Insert (Subp.As_Basic_Decl);

      for Override of Subp.P_Find_All_Overrides (Units) loop
         Hierarchy.Insert (Override.As_Basic_Decl);
      end loop;

      for Base of Find_All_Base_Subp_Declarations (Subp) loop
         if not Hierarchy.Contains (Base.As_Basic_Decl) then
            Hierarchy.Insert (Base.As_Basic_Decl);
            for Relative of Base.P_Find_All_Overrides (Units) loop
               if not Hierarchy.Contains (Relative.As_Basic_Decl) then
                  Hierarchy.Insert (Relative.As_Basic_Decl);
               end if;
            end loop;
         end if;
      end loop;

      return To_Array;
   end Get_Subp_Hierarchy;

end Laltools.Subprogram_Hierarchy;
