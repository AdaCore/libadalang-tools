------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Libadalang.Common; use Libadalang.Common;

package body Laltools.Refactor.Replace_Type is

   -------------------------------
   -- Is_Replace_Type_Available --
   -------------------------------

   function Is_Replace_Type_Available
     (Source_Unit     : Analysis_Unit;
      Source_Location : Langkit_Support.Slocs.Source_Location)
      return Boolean
   is
      Source_Node : constant Ada_Node :=
        (if Source_Unit = No_Analysis_Unit
           or else Source_Location = No_Source_Location
         then
            No_Ada_Node
         else
            Source_Unit.Root.Lookup (Source_Location));

   begin
      if not Source_Node.Is_Null
        and then Source_Node.Kind in Ada_Name
      then
         declare
            Source_Node_Parent_Decl : constant Basic_Decl :=
              Source_Node.P_Parent_Basic_Decl;

         begin
            return not Source_Node_Parent_Decl.Is_Null
              and then Source_Node_Parent_Decl.Kind in Ada_Base_Type_Decl;
         end;
      else
         return False;
      end if;
   end Is_Replace_Type_Available;

   --------------------------
   -- Create_Type_Replacer --
   --------------------------

   function Create_Type_Replacer
     (Source_Unit      : Analysis_Unit;
      Source_Type_SLOC : Source_Location;
      New_Type         : Unbounded_String)
      return Type_Replacer
   is
      Node : constant Ada_Node := Source_Unit.Root.Lookup (Source_Type_SLOC);

   begin
      return Type_Replacer'
               (Node.P_Parent_Basic_Decl.P_Canonical_Part.As_Base_Type_Decl,
                New_Type);
   end Create_Type_Replacer;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Type_Replacer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      Units : constant Analysis_Unit_Array := Analysis_Units.all;

      Text_Edits : Text_Edit_Map;

   begin
      for Reference of
        Self.Source_Type.P_Defining_Name.P_Find_All_References (Units)
      loop
         declare
            Reference_Base_Id             : constant Base_Id'Class :=
              Ref (Reference);
            Reference_Base_Id_Parent_Decl : constant Basic_Decl :=
              (declare Parent_Decl : constant Basic_Decl :=
                 Reference_Base_Id.P_Parent_Basic_Decl;
               begin
                 (if not Parent_Decl.Is_Null then Parent_Decl.P_Canonical_Part
                  else No_Basic_Decl));
         begin
            if Self.Source_Type /= Reference_Base_Id_Parent_Decl then
               Safe_Insert
                 (Edits     => Text_Edits,
                  File_Name => Reference_Base_Id.Unit.Get_Filename,
                  Edit      => Text_Edit'
                                 (Location =>
                                    (if Reference_Base_Id.Parent.Kind in
                                       Ada_Dotted_Name
                                     then
                                        Reference_Base_Id.Parent.Sloc_Range
                                     else
                                        Reference_Base_Id.Sloc_Range),
                                  Text     => Self.New_Type));
            end if;
         end;
      end loop;

      return Refactoring_Edits'(Text_Edits => Text_Edits, others => <>);
   end Refactor;

end Laltools.Refactor.Replace_Type;
