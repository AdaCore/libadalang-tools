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
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Common; use Libadalang.Common;

package body Laltools.Refactor.Suppress_Separate is

   Tool_Name : constant String := "Suppress Separate";

   ------------------------------------
   -- Is_Suppress_Separate_Available --
   ------------------------------------

   function Is_Suppress_Separate_Available
     (Node            : Ada_Node;
      Target_Separate : out Basic_Decl)
      return Boolean
   is
      Aux_Node : Ada_Node := Node;
      S_Spec : Subp_Spec := No_Subp_Spec;

      Is_Separate : Boolean;

   begin
      Target_Separate := No_Basic_Decl;

      --  - On the package body, allow this refactoring in any position of the
      --  the separate declaration except the semicolon. For instance (T is
      --  where it returns True and F where it returns False):
      --  procedure Separate_Do_Nothing is separate;
      --  TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTF
      --
      --  - On the separate subunit, only allow this refactoring between the
      --  separate keyword and the subprogram name. For instance:
      --  separate (Test) procedure Separate_Do_Nothing is ...;
      --  TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTFFFFFFFF
      --
      --  - Everywhere else returns False.

      if Aux_Node.Is_Null then
         return False;
      end if;

      --  Node immediately tell us that it's a subunit. No special search is
      --  needed. Fill the out parameter Target_Separate and return True.

      if Aux_Node.Kind in Ada_Subp_Body_Stub_Range then
         Target_Separate := Aux_Node.As_Basic_Decl;
         return True;
      end if;

      if Aux_Node.Kind in Ada_Subunit_Range then
         Target_Separate := Aux_Node.As_Subunit.F_Body.As_Basic_Decl;
         return True;
      end if;

      --  Special search is needed to determine if Node is in a is a suitable
      --  position to suppress a separate.

      while not Aux_Node.Is_Null
        and then not (Aux_Node.Kind in
                       Ada_Subp_Body_Stub_Range | Ada_Subunit_Range)
      loop
         if Aux_Node.Kind in Ada_Subp_Spec_Range then
            S_Spec := Aux_Node.As_Subp_Spec;
         end if;

         Aux_Node := Aux_Node.Parent;
      end loop;

      if Aux_Node.Is_Null then
         return False;
      end if;

      --  Confirm that this is really a separate
      Is_Separate := S_Spec.Parent.Kind in Ada_Subp_Body_Stub
        or else (not S_Spec.Parent.Is_Null
                 and then not S_Spec.Parent.Parent.Is_Null
                 and then S_Spec.Parent.Parent.Kind in Ada_Subunit_Range);

      if not Is_Separate then
         return False;
      end if;

      --  We've confirmed that this is a suitable position to suppress a
      --  separete. Fill the out parameter Target_Separate and return True.

      if Aux_Node.Kind in Ada_Subp_Body_Stub_Range then
         Target_Separate := Aux_Node.As_Basic_Decl;
      end if;

      if Aux_Node.Kind in Ada_Subunit_Range then
         Target_Separate := Aux_Node.As_Subunit.F_Body.As_Basic_Decl;
      end if;

      return True;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Is_Refactoring_Tool_Available_Default_Error_Message (Tool_Name));
         return False;
   end Is_Suppress_Separate_Available;

   -----------------------
   -- Suppress_Separate --
   -----------------------

   function Suppress_Separate
     (Target_Separate : Basic_Decl)
      return Refactoring_Edits
   is
      Edits : Refactoring_Edits;

      function "<" (L, R : Compilation_Unit) return Boolean is
        (L.P_Top_Level_Decl (L.Unit).P_Defining_Name.F_Name.Text <
             R.P_Top_Level_Decl (R.Unit).P_Defining_Name.F_Name.Text);

      function "=" (L, R : Compilation_Unit) return Boolean is
        (L.P_Top_Level_Decl (L.Unit).P_Defining_Name.F_Name.Text =
             R.P_Top_Level_Decl (R.Unit).P_Defining_Name.F_Name.Text);

      package Compilation_Unit_Hashed_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type  => Compilation_Unit,
         "<"           => "<",
         "="           => "=");

      subtype Compilation_Unit_Set is Compilation_Unit_Hashed_Sets.Set;

      function As_Compilation_Unit_Set
        (Units : Compilation_Unit_Array)
         return Compilation_Unit_Set;
      --  Transform a Compilation_Unit_Array into a Compilation_Unit_Set

      -----------------------------
      -- As_Compilation_Unit_Set --
      -----------------------------

      function As_Compilation_Unit_Set
        (Units : Compilation_Unit_Array)
         return Compilation_Unit_Set is
      begin
         return Set : Compilation_Unit_Set do
            for Unit of Units loop
               if not Set.Contains (Unit) then
                  Set.Insert (Unit);
               end if;
            end loop;
         end return;
      end As_Compilation_Unit_Set;

      Separate_Decl      : Basic_Decl := No_Basic_Decl;
      Separate_Stub      : Basic_Decl := No_Basic_Decl;
      Separate_Body      : Basic_Decl := No_Basic_Decl;
      Separate_Stub_Spec : Subp_Spec  := No_Subp_Spec;

      Indentation : Natural := 0;

      Missing_With_Clauses : Compilation_Unit_Set;
      Missing_Used_Clauses : Compilation_Unit_Set;

      New_Text       : Unbounded_Text_Type := Null_Unbounded_Wide_Wide_String;
      New_Text_Slocs : Source_Location_Range := No_Source_Location_Range;

   begin
      --  Implementation note:
      --  The default values of the parameters must be the same in the package
      --  body and in the separate subunit. Therefore, it is assumed that
      --  the package body already imports the necessary packages to resolve
      --  anything that is used on the defualt values definition.
      --  This simplifies the implementation since we only need to do the
      --  following regarding importing packages:
      --  1) For with clauses that are exclusive to the separate subunit,
      --     add them to the parent package body.
      --  2) For use clause that are exclusive to the separate subunit,
      --     add them to the subprogram declarative part, as the first
      --     declarations.

      if Target_Separate.Is_Null or else Target_Separate.Parent.Is_Null then
         return Edits;
      end if;

      --  Find Separate_Decl, Separate_Stub and Separate_Body.
      --  There is always a subprogram stub in the package body, and a
      --  subprogram body in the subunit. However, there might not be
      --  a subprogram declaration on the package spec. Is this case,
      --  Separate_Decl.Is_Null = True.

      if Target_Separate.Kind in Ada_Subp_Body_Stub_Range then
         Separate_Decl := Target_Separate.As_Subp_Body_Stub.
           P_Previous_Part_For_Decl;
         Separate_Stub := Target_Separate;
         Separate_Body := Target_Separate.As_Subp_Body_Stub.
           P_Next_Part_For_Decl;
      elsif Target_Separate.Kind in Ada_Subp_Body
        and then Target_Separate.Parent.Kind in Ada_Subunit_Range
      then
         Separate_Decl := Target_Separate.As_Subp_Body.
           P_Previous_Part_For_Decl.P_Previous_Part_For_Decl;
         Separate_Stub := Target_Separate.As_Subp_Body.
           P_Previous_Part_For_Decl;
         Separate_Body := Target_Separate;
      else
         return Edits;
      end if;

      if Separate_Stub.Is_Null
        or else Separate_Body.Is_Null
      then
         return Edits;
      end if;

      --  In order to resolve the necessary 'with' and 'use' clauses,
      --  we must get all 'with' and 'use' clauses for the subunit, package
      --  body and package spec. If Separate_Decl.Is_Null = True, we reach the
      --  package spec throught the package body instead.

      declare
         --  Package Spec
         Pkg_Spec : constant Package_Decl :=
           (if Separate_Decl.Is_Null then
               Separate_Stub.P_Parent_Basic_Decl.
                 P_Previous_Part_For_Decl.As_Package_Decl
            else Separate_Decl.P_Parent_Basic_Decl.As_Package_Decl);

         Pkg_Spec_Withed_Units : constant Compilation_Unit_Array :=
           Pkg_Spec.Unit.Root.As_Compilation_Unit.P_Withed_Units;
         Pkg_Spec_Used_Units : constant Compilation_Unit_Array :=
           Get_Used_Units (Pkg_Spec.Unit.Root.As_Compilation_Unit);

         --  Package Body
         Pkg_Body_Withed_Units : constant Compilation_Unit_Array :=
           Separate_Stub.Unit.Root.As_Compilation_Unit.P_Withed_Units;
         Pkg_Body_Used_Units   : constant Compilation_Unit_Array :=
           Get_Used_Units (Separate_Stub.Unit.Root.As_Compilation_Unit);

         --  Subunit
         Subunit_Withed_Units  : constant Compilation_Unit_Array :=
           Separate_Body.Unit.Root.As_Compilation_Unit.P_Withed_Units;
         Subunit_Used_Units    : constant Compilation_Unit_Array :=
           Get_Used_Units (Separate_Body.Unit.Root.As_Compilation_Unit);

         use Compilation_Unit_Hashed_Sets;

      begin
         Missing_With_Clauses :=
           (As_Compilation_Unit_Set (Subunit_Withed_Units)
            - (As_Compilation_Unit_Set (Pkg_Spec_Withed_Units)
              or As_Compilation_Unit_Set (Pkg_Body_Withed_Units)));

         Missing_Used_Clauses :=
           (As_Compilation_Unit_Set (Subunit_Used_Units)
            - (As_Compilation_Unit_Set (Pkg_Spec_Used_Units)
              or As_Compilation_Unit_Set (Pkg_Body_Used_Units)));
      end;

      --  Compute the necessary edits for the prelude ('with' and 'use'
      --  clauses).

      declare
         Prelude : constant Ada_Node_List :=
           Get_Compilation_Unit (Separate_Stub).F_Prelude;

      begin
         New_Text_Slocs.Start_Line   := Prelude.Sloc_Range.End_Line;
         New_Text_Slocs.Start_Column := Prelude.Sloc_Range.End_Column;
         New_Text_Slocs.End_Line     := Prelude.Sloc_Range.End_Line;
         New_Text_Slocs.End_Column   := Prelude.Sloc_Range.End_Column;

         New_Text := Null_Unbounded_Wide_Wide_String;
         Append (New_Text, Chars.LF);
         for C_Unit of Missing_With_Clauses loop
            Append (New_Text, "with ");
            Append (New_Text,
                    C_Unit.P_Top_Level_Decl (C_Unit.Unit)
                    .P_Fully_Qualified_Name);
            Append (New_Text, ";");
            Append (New_Text, Chars.LF);
         end loop;
      end;

      Safe_Insert
        (Edits.Text_Edits,
         Separate_Stub.Unit.Get_Filename,
         (New_Text_Slocs,
         To_Unbounded_String (To_UTF8 (To_Text (New_Text)))));

      --  Compute the necessary edits for the subprogram body declarative part
      --  and statements.

      Separate_Stub_Spec := Separate_Stub.As_Subp_Body_Stub.F_Subp_Spec;

      New_Text_Slocs.Start_Line   := Separate_Stub_Spec.Sloc_Range.End_Line;
      New_Text_Slocs.Start_Column := Separate_Stub_Spec.Sloc_Range.End_Column;
      New_Text_Slocs.End_Line     := Separate_Stub.Sloc_Range.End_Line;
      New_Text_Slocs.End_Column   := Separate_Stub.Sloc_Range.End_Column;

      Indentation := Natural (Separate_Stub_Spec.Sloc_Range.Start_Column - 1);

      --  Declarative Part
      New_Text := Null_Unbounded_Wide_Wide_String;
      Append (New_Text, Chars.LF);
      Append (New_Text, Indentation * " " & "is");
      if not Missing_Used_Clauses.Is_Empty then
         Append (New_Text, Chars.LF);
      end if;
      for C_Unit of Missing_Used_Clauses loop
         Append (New_Text, 2 * Indentation * " " & "use ");
         Append
           (New_Text,
            C_Unit.P_Top_Level_Decl (C_Unit.Unit).P_Fully_Qualified_Name);
         Append (New_Text, ";");
         Append (New_Text, Chars.LF);
      end loop;

      declare
         Subp_Body_Sloc_Range : constant Source_Location_Range :=
           Separate_Body.As_Subp_Body.Sloc_Range;
         Decls_Sloc_Range     : constant Source_Location_Range :=
           Separate_Body.As_Subp_Body.F_Decls.Sloc_Range;

      begin
         if Decls_Sloc_Range.Start_Line = Subp_Body_Sloc_Range.End_Line then
            declare
               Line  : constant Text_Type :=
                 Separate_Body.Unit.Get_Line
                   (Positive (Decls_Sloc_Range.Start_Line));
               Slice : constant Text_Type :=
                 Line (Line'First
                         + Positive (Decls_Sloc_Range.Start_Column) - 1
                       .. Line'First
                            + Positive (Subp_Body_Sloc_Range.End_Column) - 2);

            begin
               Append (New_Text, Indentation * " " & Slice & Chars.LF);
            end;

         elsif Subp_Body_Sloc_Range.End_Line =
                 Decls_Sloc_Range.Start_Line + 1
         then
            declare
               Start_Line  : constant Text_Type :=
                 Separate_Body.Unit.Get_Line
                   (Positive (Decls_Sloc_Range.Start_Line));
               Start_Slice : constant Text_Type :=
                 Start_Line (Start_Line'First
                             + Positive (Decls_Sloc_Range.Start_Column) - 1 ..
                             Start_Line'Last);
               End_Line    : constant Text_Type :=
                 Separate_Body.Unit.Get_Line
                   (Positive (Decls_Sloc_Range.Start_Line));
               End_Slice   : constant Text_Type :=
                 End_Line (End_Line'First ..
                           End_Line'First
                           + Positive (Subp_Body_Sloc_Range.End_Column) - 2);

            begin
               Append (New_Text, Indentation * " " & Start_Slice & Chars.LF);
               Append (New_Text, Indentation * " " & End_Slice & Chars.LF);
            end;

         else
            declare
               Line  : constant Text_Type :=
                 Separate_Body.Unit.Get_Line
                   (Positive (Decls_Sloc_Range.Start_Line));
               Slice : constant Text_Type :=
                 Line (Line'First +
                         Positive (Decls_Sloc_Range.Start_Column) - 1 ..
                           Line'Last);
            begin
               Append (New_Text, Slice & Chars.LF);
            end;
            for Line_Number in
              Decls_Sloc_Range.Start_Line + 1 ..
                Subp_Body_Sloc_Range.End_Line - 1
            loop
               declare
                  Line : constant Text_Type :=
                    Separate_Body.Unit.Get_Line (Positive (Line_Number));

               begin
                  Append (New_Text, Indentation * " " & Line & Chars.LF);
               end;
            end loop;
            declare
               Line  : constant Text_Type :=
                 Separate_Body.Unit.Get_Line
                   (Positive (Subp_Body_Sloc_Range.End_Line));
               Slice : constant Text_Type :=
                 Line (Line'First ..
                         Line'First +
                           Positive (Subp_Body_Sloc_Range.End_Column) - 2);
            begin
               Append (New_Text, Indentation * " " & Slice & Chars.LF);
            end;
         end if;
      end;

      Safe_Insert
        (Edits.Text_Edits,
         Separate_Stub.Unit.Get_Filename,
         (New_Text_Slocs,
         To_Unbounded_String (To_UTF8 (To_Text (New_Text)))));
      Edits.File_Deletions.Insert
        (To_Unbounded_String (Separate_Body.Unit.Get_Filename));

      return Edits;
   end Suppress_Separate;

   --------------
   -- Refactor --
   --------------

   overriding
   function Refactor
     (Self           : Separate_Suppressor;
      Analysis_Units : access function return Analysis_Unit_Array := null)
      return Refactoring_Edits
   is
      pragma Unreferenced (Analysis_Units);

   begin
      return Suppress_Separate (Self.Target_Separate);

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));
         return No_Refactoring_Edits;
   end Refactor;

   ------------
   -- Create --
   ------------

   function Create
     (Target_Separate : Basic_Decl)
      return Separate_Suppressor
   is (Separate_Suppressor'(Target_Separate => Target_Separate));

end Laltools.Refactor.Suppress_Separate;
