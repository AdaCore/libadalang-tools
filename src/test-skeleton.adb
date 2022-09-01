------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2011-2022, AdaCore                    --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;

with Libadalang.Common; use Libadalang.Common;
with Langkit_Support.Errors;
with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with GNAT.OS_Lib;
with GNAT.SHA1;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Strings;
with GNAT.Traceback.Symbolic;

with Test.Common; use Test.Common;
with Test.Harness;
with Test.Skeleton.Source_Table; use Test.Skeleton.Source_Table;
with Test.Mapping; use Test.Mapping;
with Test.Stub;
with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Environment;
with Utils_Debug; use Utils_Debug;

with TGen.Gen_Strategies;

package body Test.Skeleton is
   Me                : constant Trace_Handle :=
     Create ("Skeletons", Default => Off);
   Me_Direct_Callees : constant Trace_Handle :=
     Create ("Skeletons.Direct_Callees", Default => Off);

   -------------------
   --  Minded Data  --
   -------------------

   New_Tests_Counter : Natural := 0;
   All_Tests_Counter : Natural := 0;

   package Tests_Per_Unit is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Natural);
   use Tests_Per_Unit;

   Test_Info : Tests_Per_Unit.Map;

   type Data_Kind_Type is
     (Declaration_Data,
      Instantiation);

   type Base_Type_Info is tagged record
      Main_Type_Elem            : Ada_Node := No_Ada_Node;
      Main_Type_Abstract        : Boolean;
      Main_Type_Text_Name       : String_Access;

      Has_Argument_Father       : Boolean;
      Argument_Father_Unit_Name : String_Access;
      Argument_Father_Type_Name : String_Access;
      Argument_Father_Nesting   : String_Access;

      Nesting                   : String_Access;

      Type_Number               : Positive;

      No_Default_Discriminant   : Boolean;
   end record;

   package Type_Info_Vect is new
     Ada.Containers.Indefinite_Vectors (Positive, Base_Type_Info);
   use Type_Info_Vect;

   use String_Set;

   type Test_Case_Mode is (Normal, Robustness);

   type Test_Case_Info is record
      Pre  : Expr;
      Post : Expr;

      Elem : Ada_Node;
      Name : String_Access;
      Mode : Test_Case_Mode;
      Req  : Expr;
      Ens  : Expr;

      Req_Image : String_Access;
      Ens_Image : String_Access;

      Params_To_Temp : String_Set.Set;

      Req_Line : String_Access;
      Ens_Line : String_Access;

      TC_Hash : String_Access;
   end record;

   type Subp_Info is record
      Subp_Declaration : Ada_Node;
      Subp_Text_Name   : String_Access;
      Subp_Name_Image  : String_Access;
      Subp_Mangle_Name : String_Access;
      Subp_Full_Hash   : String_Access;

      --  Those versions of hash are stored for compatibility reasons.
      --  Transitions from older versions of hash should be performed
      --  automatically.

      Subp_Hash_V1    : String_Access;
      --  Case-sensitive hash.
      Subp_Hash_V2_1  : String_Access;
      --  Non-controlling parameters with same root type as controlling ones
      --  are replaced with root type before hashing.

      Corresp_Type     : Natural;
      Nesting          : String_Access;

      Has_TC_Info      : Boolean := False;
      TC_Info          : Test_Case_Info;

      Is_Overloaded    : Boolean;
   end record;

   package Subp_Data_List is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Subp_Info);
   use Subp_Data_List;

   type Package_Info is record
      Name       : String_Access;
      Is_Generic : Boolean;
      Data_Kind  : Data_Kind_Type;
      Element    : Ada_Node;

      --  only used for instantiations
      Generic_Containing_Package : String_Access;
   end record;

   package Package_Info_List is new
     Ada.Containers.Doubly_Linked_Lists (Package_Info);
   use Package_Info_List;

   --  Info on overloading subprograms
   package Name_Frequency is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Natural);
   use Name_Frequency;

   type Data_Holder (Data_Kind : Data_Kind_Type := Declaration_Data) is record

      Unit : Compilation_Unit;
      --  CU itself.

      Unit_Full_Name : String_Access;
      --  Fully expanded Ada name of the CU

      Unit_File_Name : String_Access;
      --  Full name of the file, containing the CU

      case Data_Kind is
         --  Indicates which data storing structures are used, determines the
         --  way of suite generation.

         when Declaration_Data =>

            Is_Generic       : Boolean;
            --  Indicates if given argument package declaration is generic.

            Has_Simple_Case  : Boolean := False;
            --  Indicates if we have routines that are not primitives of any
            --  tagged type.

            Needs_Set_Up     : Boolean := False;
            --  Indicates if we need the Set_Up routine for at least one test
            --  type;

            Needs_Assertions : Boolean := False;
            --  Indicates if we need to include AUnit.Assertions into the body
            --  of the test package.

            Subp_List : Subp_Data_List.List;
            --  List of subprograms declared in the argument package
            --  declaration.

            Type_Data_List : Type_Info_Vect.Vector;
            --  Stores info on tagged records in the argument package
            --  declaration.

            Package_Data_List : Package_Info_List.List;
            --  Stores info of nested packages

            Units_To_Stub : Ada_Nodes_List.List;
            --  List of direct dependancies of current unit

            Subp_Name_Frequency : Name_Frequency.Map;

         when Instantiation =>

            Gen_Unit_Full_Name : String_Access;
            --  Fully expanded Ada name of the generic CU

            Gen_Unit_File_Name : String_Access;
            --  Name of file containing the generic CU

      end case;

   end record;

   ----------------
   -- Suite Data --
   ----------------

   type Test_Type_Info_Wrapper is record
      TT_Info       : Test.Harness.Test_Type_Info;
      Test_Package  : String_Access;
      Original_Type : Ada_Node := No_Ada_Node;
   end record;

   package TT_Info is new
     Ada.Containers.Indefinite_Vectors (Positive, Test_Type_Info_Wrapper);
   use TT_Info;

   type Test_Routine_Info_Wrapper is record
      TR_Info       : Test.Harness.Test_Routine_Info;
      Test_Package  : String_Access;
      Original_Type : Ada_Node := No_Ada_Node;
      Original_Subp : Ada_Node := No_Ada_Node;

      From_Generic  : Boolean := False;
   end record;

   package TR_Info is new
     Ada.Containers.Indefinite_Vectors (Positive, Test_Routine_Info_Wrapper);
   use TR_Info;

   type Test_Routine_Info_Enhanced_Wrapper is record
      TR_Info       : Test.Harness.Test_Routine_Info_Enhanced;
      Test_Package  : String_Access;
      Original_Type : Ada_Node := No_Ada_Node;
   end record;

   package TR_Info_Enhanced is new
     Ada.Containers.Indefinite_Vectors (Positive,
                                        Test_Routine_Info_Enhanced_Wrapper);
   use TR_Info_Enhanced;

   type Suites_Data_Type is record
      Test_Types   : TT_Info.Vector;
      TR_List      : TR_Info.Vector;
      ITR_List     : TR_Info_Enhanced.Vector;
      LTR_List     : TR_Info_Enhanced.Vector;
   end record;

   ------------------
   -- Test Mapping --
   ------------------

   use TC_Mapping_List;
   use TR_Mapping_List;
   use DT_Mapping_List;
   use TP_Mapping_List;

   procedure Add_TR
     (TP_List : in out TP_Mapping_List.List;
      TPtarg  : String;
      Test_F  : String;
      Test_T  : String;
      Subp    : Subp_Info;
      TR_Line : Natural := 1);

   procedure Add_DT
     (TP_List : in out TP_Mapping_List.List;
      TPtarg  : String;
      Test_F  : String;
      Line    : Natural;
      Column  : Natural);

   --------------
   -- Generics --
   --------------

   type Generic_Tests is record
      Gen_Unit_Full_Name : String_Access;
      Tested_Type_Names  : List_Of_Strings.List;
      Has_Simple_Case    : Boolean := False;
   end record;
   --  Stores info necessary to calculate names of test packages that
   --  correspond to the generic UUT: names of tagged types and
   --  absence/presense of simple case.

   package Generic_Tests_Storage is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Generic_Tests);

   Gen_Tests_Storage : Generic_Tests_Storage.List;
   --  List of data on all the generic tests created during the processing of
   --  generic UUTs.

   type Generic_Package is record
      Name : String_Access;
      Sloc : String_Access := null;

      Has_Instantiation : Boolean := False;
   end record;

   package Generic_Package_Storage is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Generic_Package);

   Gen_Package_Storage : Generic_Package_Storage.List;
   --  Used to detect processed generic packages that do not have
   --  instantiations in the scope of argument sources and, therefore, won't be
   --  included into final harness.

   procedure Update_Generic_Packages (Instantiation : String);
   --  Updates Gen_Package_Storage with a name of processed instantiation
   procedure Update_Generic_Packages (Gen_Pack      : Generic_Package);
   --  Updates Gen_Package_Storage with a new generic package info

   -----------------------
   -- Marker Processing --
   -----------------------

   package String_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, String);

   type Markered_Data is record
      Commented_Out   : Boolean := False;
      Short_Name_Used : Boolean := False;
      Short_Name      : String_Access := new String'("");
      TR_Text         : String_Vectors.Vector;
      Issue_Warning   : Boolean := False;
   end record;

   type Unique_Hash is record
      Version : String_Access;
      Hash    : String_Access;
      TC_Hash : String_Access;
   end record;

   function "<" (L, R : Unique_Hash) return Boolean;

   package Markered_Data_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Unique_Hash, Markered_Data);
   use Markered_Data_Maps;

   Markered_Data_Map : Markered_Data_Maps.Map;

   procedure Gather_Data
     (The_Unit          :     Compilation_Unit;
      Data              : out Data_Holder;
      Suite_Data_List   : out Suites_Data_Type;
      Apropriate_Source : out Boolean);
   --  Iterates through the given unit and gathers all the data needed for
   --  generation of test package. All the iterations are done here.
   --  Checks if given unit is of the right kind and if it is appropriate.
   --  Marks unappropriate sources in the source table.

   procedure Gather_Test_Cases
     (Subp            :        Subp_Info;
      TR_Info         :        Test_Routine_Info_Wrapper;
      Data            : in out Data_Holder;
      Suite_Data_List : in out Suites_Data_Type;
      TC_Found        :    out Boolean;
      Instance_Sloc   :        String := "");
   --  Adds one subprogram-to-test per each test case.
   --  Sets TC_Found if at least one Test_Case aspect or pragma has been found
   --  for given subprogram.

   procedure Generate_Nested_Hierarchy (Data : Data_Holder);
   --  Creates dummy child packages copying nested packages from tested package

   procedure Generate_Test_Package (Data : Data_Holder);
   --  Generates test package spec and body

   procedure Generate_Procedure_Wrapper (Current_Subp : Subp_Info);
   --  Prints a test-case specific wrapper for tested procedure

   procedure Generate_Function_Wrapper (Current_Subp : Subp_Info);
   --  Prints a test-case specific wrapper for tested function

   procedure Print_Comment_Declaration (Subp : Subp_Info; Span : Natural := 0);
   --  Prints the file containing the tested subprogram as well as the line
   --  coloumn numbers of the tested subprogram declaration.

   procedure Get_Subprograms_From_Package (File : String);

   procedure Get_Subprogram_From_Separate
     (File : String;
      UH   : Unique_Hash;
      Subp : Subp_Info);

   procedure Put_Opening_Comment_Section
     (Subp           : Subp_Info;
      Overloading_N  : Natural;
      Commented_Out  : Boolean := False;
      Use_Short_Name : Boolean := True;
      Type_Name      : String  := "");

   procedure Put_Closing_Comment_Section
     (Subp           : Subp_Info;
      Overloading_N  : Natural;
      Commented_Out  : Boolean := False;
      Use_Short_Name : Boolean := True);

   function Sanitize_TC_Name (TC_Name : String) return String;
   --  Processes the name of the test case in such a way that it could be used
   --  as a part of test routine name. the name is trimmed, then all sequences
   --  of whitespace characters are replaced with an underscore, all other
   --  illegal characters are omitted.

   procedure Put_Wrapper_Rename (Span : Natural; Current_Subp : Subp_Info);
   --  Puts subprogram renaming declaration, which renames generated wrapper
   --  into original tested subprogram's name.

   function Find_Same_Short_Name
     (MD_Map : Markered_Data_Maps.Map;
      Subp   : Subp_Info) return Markered_Data_Maps.Cursor;
   --  Searches for the test with given short name

   function Uncomment_Line (S : String) return String;
   --  Removes two dashes and two spaces from the beginning of the line.
   --  Returns argument string if commenting prefix not found.

   function Format_Time (Time : GNAT.OS_Lib.OS_Time) return String;
   --  Returns image of given time in 1901-01-01 00:00:00 format.

   procedure Get_Units_To_Stub
     (The_Unit :        Compilation_Unit;
      Data     : in out Data_Holder);
   --  Populates the list of units that should be stubbed.

   procedure Process_Stubs (List : Ada_Nodes_List.List);

   function Is_Declared_In_Regular_Package
     (Elem : Ada_Node'Class) return Boolean;
   --  Checks that all enclosing elements for the given element are regular
   --  package declarations.

   function Get_Direct_Callees_Setters
     (Subp : Basic_Decl) return String_Set.Set;
   --  Returns the list of possible setters for all subprograms called from
   --  the body of given subprogram.

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Unique_Hash) return Boolean is
   begin
      if L.Version.all = R.Version.all then
         if L.Hash.all = R.Hash.all then
            return L.TC_Hash.all < R.TC_Hash.all;
         else
            return L.Hash.all < R.Hash.all;
         end if;
      else
         return L.Version.all < R.Version.all;
      end if;
   end "<";

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source (The_Unit : Analysis_Unit) is
      Data              : Data_Holder;
      Suite_Data_List   : Suites_Data_Type;
      Suite_Data        : Test.Harness.Data_Holder;

      Subp_Cur : Subp_Data_List.Cursor;

      Apropriate_Source : Boolean;

      CU : Compilation_Unit;

      Test_Packages : String_Set.Set;
      Cur : String_Set.Cursor;

      procedure Get_Test_Packages_List (S_Data : Suites_Data_Type);
      --  Fills suite data sorting out routines from generic packages

      function Get_Suite_Components
        (S_Data       : Suites_Data_Type;
         Package_Name : String)
         return Test.Harness.Data_Holder;

      procedure Cleanup;
      --  Frees Data components

      procedure Report (Ex : Ada.Exceptions.Exception_Occurrence);
      --  Reports problematic source with exception information

      procedure Cleanup is
      begin
         if Data.Data_Kind = Declaration_Data then
            Clear (Data.Type_Data_List);
            Clear (Data.Subp_List);
            Clear (Data.Package_Data_List);
            Clear (Data.Subp_Name_Frequency);
         end if;

         Suite_Data.Test_Types.Clear;
         Suite_Data.TR_List.Clear;
         Suite_Data.ITR_List.Clear;
         Suite_Data.LTR_List.Clear;
      end Cleanup;

      ----------------------------
      -- Get_Test_Packages_List --
      ----------------------------

      procedure Get_Test_Packages_List (S_Data : Suites_Data_Type) is
      begin
         for K in S_Data.TR_List.First_Index .. S_Data.TR_List.Last_Index loop

            if not S_Data.TR_List.Element (K).From_Generic then
               Test_Packages.Include
                 (S_Data.TR_List.Element (K).Test_Package.all);
            end if;
         end loop;

         for
           K in S_Data.ITR_List.First_Index .. S_Data.ITR_List.Last_Index
         loop
            Test_Packages.Include
              (S_Data.ITR_List.Element (K).Test_Package.all);
         end loop;

      end Get_Test_Packages_List;

      function Get_Suite_Components
        (S_Data       : Suites_Data_Type;
         Package_Name : String)
         return Test.Harness.Data_Holder
      is
         Suite_Data   : Test.Harness.Data_Holder;
         Test_Routine : Test.Harness.Test_Routine_Info;
         TT   : Test.Harness.Test_Type_Info;
         TR_E : Test.Harness.Test_Routine_Info_Enhanced;

         package Test_Type_Origins is new
           Ada.Containers.Vectors (Positive, Ada_Node);
         use Test_Type_Origins;

         TT_Origins : Test_Type_Origins.Vector;
         --  Used to set test type numbers.

         Original_Type : Ada_Node;

         Type_Found : Boolean;
      begin

         Suite_Data.Test_Unit_Full_Name := new String'(Package_Name);

         for
           K in S_Data.Test_Types.First_Index .. S_Data.Test_Types.Last_Index
         loop

            if
              S_Data.Test_Types.Element (K).Test_Package.all = Package_Name
            then
               TT := S_Data.Test_Types.Element (K).TT_Info;
               TT.Tested_Type := S_Data.Test_Types.Element (K).Original_Type;
               Suite_Data.Test_Types.Append (TT);
               TT_Origins.Append (S_Data.Test_Types.Element (K).Original_Type);
            end if;
         end loop;

         for K in S_Data.TR_List.First_Index .. S_Data.TR_List.Last_Index loop

            if S_Data.TR_List.Element (K).Test_Package.all = Package_Name then

               Test_Routine := S_Data.TR_List.Element (K).TR_Info;

               --  Setting test type number;

               Original_Type := S_Data.TR_List.Element (K).Original_Type;
               Type_Found := False;

               for L in TT_Origins.First_Index .. TT_Origins.Last_Index loop
                  if TT_Origins.Element (L) = Original_Type then
                     Test_Routine.Test_Type_Numb := L;
                     Type_Found := True;
                     exit;
                  end if;
               end loop;

               if Type_Found then
                  Suite_Data.TR_List.Append (Test_Routine);
                  Suite_Data.Good_For_Suite := True;
               end if;
            end if;
         end loop;

         for
           K in S_Data.ITR_List.First_Index .. S_Data.ITR_List.Last_Index
         loop
            if S_Data.ITR_List.Element (K).Test_Package.all = Package_Name then

               TR_E := S_Data.ITR_List.Element (K).TR_Info;

               --  Setting up test type number

               Original_Type := S_Data.ITR_List.Element (K).Original_Type;
               Type_Found := False;

               for L in TT_Origins.First_Index .. TT_Origins.Last_Index loop
                  if TT_Origins.Element (L) = Original_Type then
                     TR_E.Test_Type_Numb := L;
                     Type_Found := True;
                     exit;
                  end if;
               end loop;

               if Type_Found then
                  Suite_Data.ITR_List.Append (TR_E);
                  Suite_Data.Good_For_Suite := True;
               end if;

            end if;
         end loop;

         for
           K in S_Data.LTR_List.First_Index .. S_Data.LTR_List.Last_Index
         loop
            if S_Data.LTR_List.Element (K).Test_Package.all = Package_Name then

               TR_E := S_Data.LTR_List.Element (K).TR_Info;

               --  Setting up test type number

               Original_Type := S_Data.LTR_List.Element (K).Original_Type;
               Type_Found := False;

               for L in TT_Origins.First_Index .. TT_Origins.Last_Index loop
                  if TT_Origins.Element (L) = Original_Type then
                     TR_E.Test_Type_Numb := L;
                     Type_Found := True;
                     exit;
                  end if;
               end loop;

               if Type_Found then
                  TR_E.Tested_Type := Original_Type;
                  Suite_Data.LTR_List.Append (TR_E);
                  Suite_Data.Good_For_Substitution  := True;
               end if;
            end if;
         end loop;

         TT_Origins.Clear;

         return Suite_Data;

      end Get_Suite_Components;

      ------------
      -- Report --
      ------------

      procedure Report (Ex : Ada.Exceptions.Exception_Occurrence) is
      begin
         if Strict_Execution then
            Report_Err
              (Ada.Exceptions.Exception_Name (Ex)
               & " : "
               & Ada.Exceptions.Exception_Message (Ex)
               & ASCII.LF
               & GNAT.Traceback.Symbolic.Symbolic_Traceback (Ex));
         end if;
      end Report;

   begin
      if The_Unit.Root.Kind /= Ada_Compilation_Unit then
         --  For example, it can be a Pragma_Node_List for a body source
         --  containing pragma No_Body.
         return;
      end if;

      CU := Root (The_Unit).As_Compilation_Unit;

      if P_Unit_Kind (CU) = Unit_Body then
         --  Only interested in specs
         return;
      end if;
      Gather_Data
        (CU, Data, Suite_Data_List, Apropriate_Source);

      if Apropriate_Source then

         --  First, create stubs if needed. This will allow to import stub_data
         --  packages into test packages only for actually stubbed
         --  dependencies.
         if Stub_Mode_ON then
            Process_Stubs (Data.Units_To_Stub);
         end if;

         if Generate_Test_Vectors and then not Data.Is_Generic then
            Subp_Cur := Data.Subp_List.First;
            while Subp_Cur /= Subp_Data_List.No_Element loop
               begin
                  case Kind (Element (Subp_Cur).Subp_Declaration) is
                     when Ada_Subp_Decl =>
                     TGen.Gen_Strategies.Generate_Test_Vectors
                     (TGen_Ctx,
                        Test.Common.TGen_Num_Tests,
                        Element (Subp_Cur).Subp_Declaration.As_Subp_Decl,
                        Ada.Strings.Unbounded.To_Unbounded_String
                        (Element (Subp_Cur).Subp_Full_Hash.all));
                  when others =>
                     if Debug_Flag_1 then
                        Report_Std ("Warning: (TGen) "
                                    & Base_Name (Data.Unit_File_Name.all)
                                    & " : Could not generate test vectors for "
                                    & Element (Subp_Cur).Subp_Text_Name.all
                                    & " : " & ASCII.LF
                                    & "Unsupported subprogram declaration kind"
                                    & ": "
                                    & Kind_Name
                                       (Element (Subp_Cur).Subp_Declaration));
                     end if;
                  end case;
               exception
                  when Exc : Program_Error =>
                     if Debug_Flag_1 then
                        Report_Std ("Warning: (TGen) "
                                    & Base_Name (Data.Unit_File_Name.all)
                                    & " : Unexpected error generating test"
                                    & " vectors for "
                                    & Element (Subp_Cur).Subp_Text_Name.all
                                    & " : " & ASCII.LF
                                    & Ada.Exceptions.Exception_Message (Exc));
                     end if;
                  when Exc : others =>
                     if Debug_Flag_1 then
                        Report_Std ("Warning: (TGen) "
                                    & Base_Name (Data.Unit_File_Name.all)
                                    & " : Unexpected error generating test"
                                    & " vectors for "
                                    & Element (Subp_Cur).Subp_Text_Name.all
                                    & " : " & ASCII.LF
                                    & Ada.Exceptions.Exception_Information
                                        (Exc));
                     end if;
               end;
               Subp_Data_List.Next (Subp_Cur);
            end loop;
            TGen.Gen_Strategies.Generate_Artifacts (TGen_Ctx);
         end if;

         declare
            F : File_Array_Access;
         begin
            Append
              (F,
               GNATCOLL.VFS.Create
                 (+(Get_Source_Output_Dir (CU.Unit.Get_Filename))));
            Create_Dirs (F);
         end;

         if Data.Data_Kind = Declaration_Data then
            Generate_Nested_Hierarchy (Data);
            Generate_Test_Package (Data);

            Get_Test_Packages_List (Suite_Data_List);
            Cur := Test_Packages.First;
            loop
               exit when Cur = String_Set.No_Element;

               Suite_Data := Get_Suite_Components
                 (Suite_Data_List,
                  String_Set.Element (Cur));

               if Suite_Data.Good_For_Suite then
                  if not Stub_Mode_ON and then not Separate_Drivers then

                     Test.Harness.Generate_Suite (Suite_Data);
                     if Substitution_Suite
                       and then Suite_Data.Good_For_Substitution
                     then
                        Test.Harness.Generate_Substitution_Suite_From_Tested
                          (Suite_Data);
                     end if;
                  end if;
               end if;

               String_Set.Next (Cur);
            end loop;

            if Stub_Mode_ON or else Separate_Drivers then

               Cur := Test_Packages.First;
               while Cur /= String_Set.No_Element loop

                  Suite_Data := Get_Suite_Components
                    (Suite_Data_List,
                     String_Set.Element (Cur));

                  if Suite_Data.Good_For_Suite then
                     Test.Harness.Generate_Test_Drivers
                       (Suite_Data,
                        Data.Unit_File_Name.all,
                        Data.Units_To_Stub);
                  end if;
                  if Suite_Data.Good_For_Substitution
                    and then not Driver_Per_Unit
                  then
                     Test.Harness.Generate_Substitution_Test_Drivers
                       (Suite_Data);
                  end if;

                  String_Set.Next (Cur);
               end loop;
            end if;

         end if;
      end if;

      Cleanup;

   exception
      when Ex : Langkit_Support.Errors.Property_Error =>

         Source_Processing_Failed := True;

         Report_Err ("lal error while creating test package for "
                     & Base_Name (The_Unit.Get_Filename));
         Report_Err ("source file may be incomplete/invalid");

         Report (Ex);
         Cleanup;

      when Ex : others =>

         Source_Processing_Failed := True;

         Report_Err ("unexpected error while creating test package for "
                     & Base_Name (The_Unit.Get_Filename));

         Report (Ex);
         Cleanup;
   end Process_Source;

   -----------------
   -- Gather_Data --
   -----------------

   procedure Gather_Data
     (The_Unit          :     Compilation_Unit;
      Data              : out Data_Holder;
      Suite_Data_List   : out Suites_Data_Type;
      Apropriate_Source : out Boolean)
   is

      Bod : constant Library_Item := The_Unit.F_Body.As_Library_Item;

      Unit : Ada_Node;

      Type_Counter       : Positive := 1;
      Dummy_Type_Counter : Natural  := 0;

      function Get_Nested_Packages (Node : Ada_Node'Class) return Visit_Status;
      function Get_Records (Node : Ada_Node'Class) return Visit_Status;
      function Get_Subprograms (Node : Ada_Node'Class) return Visit_Status;

      Inside_Inst : Boolean := False;
      --  Indicates that we are parsing the generic package in place of its
      --  instantiation to populate Data for suite creation. In that mode
      --  the nestings gathered by Get_Records and Get_Subprograms must be
      --  replaced with the real nesting of instantiation.

      Instance_Nesting : String_Access;
      --  Stores the nesting of instantiation and its name

      Instance_Sloc : String_Access;
      --  Stores sloc of instance that is used for test routine output

      procedure Gather_Inherited_Subprograms
        (Dummy_Types     : Natural;
         Suite_Data_List : in out Suites_Data_Type);
      --  Populates the list of inherited subprograms. Dummy_Types indicates
      --  the number of Test types created for non-primitives.

      procedure Gather_Substitution_Data
        (Suite_Data_List : in out Suites_Data_Type);
      --  Populates the list of overridden subprograms

      function Is_Callable_Subprogram (Subp : Basic_Decl) return Boolean is
         (Subp.Kind not in Ada_Abstract_Subp_Decl | Ada_Null_Subp_Decl);
      --  Checks that given subprogram is neither an abstract subprogram
      --  nor a null procedure. This ensures that corresponding test routine
      --  is created for such subprogram.

      function Is_Fully_Private
        (Arg : Base_Type_Decl) return Boolean;
      --  Detects if Arg and its incomplete declaration (if present)
      --  are both in private part.

      function Is_Ghost_Code (Decl : Basic_Decl) return Boolean;
      --  Detects if given declaration is ghost.
      --  If some defining name of Decl is not ghost, then consider the whole
      --  decl as not ghost. This approximation should be fine given that we
      --  do not process ghost code. This means that we may be processing a bit
      --  more code than necessary, but we won't be missing any non-ghost
      --  cases.

      procedure Check_Type_For_Elaboration (Type_Dec : Base_Type_Decl);
      --  Checking if is any of parent types have pragma
      --  Preelaborable_Initialization. This might cause
      --  elaboration conflicts in the harness, so a warning
      --  should be isued.

      function Check_Type_For_Unset_Discriminants
        (Type_Dec : Base_Type_Decl) return Boolean;
      --  Returns True if given type or any of its ancestors have
      --  a discriminant without a default value.

      function Test_Types_Linked
        (Inheritance_Root_Type  : Base_Type_Decl;
         Inheritance_Final_Type : Base_Type_Decl)
         return Boolean;
      --  Checks that there is no fully private types between the root type
      --  and the final descendant, so that corresponding test types are
      --  members of same hierarchy.

      function No_Inheritance_Through_Generics
        (Inheritance_Root_Type  : Base_Type_Decl;
         Inheritance_Final_Type : Base_Type_Decl)
         return Boolean;
      --  Checks that all types between the root type and the final descendant
      --  are declared in regular packages.

      function Is_Node_From_Generic (Node : Ada_Node'Class) return Boolean;
      --  Checks that there are no enclosing generic package declarations for
      --  Node, but takes into account the value of Inside_Inst, so that nodes
      --  from instantiations could be distinguished from same nodes from
      --  corresponding generics.

      -------------------------
      -- Get_Nested_Packages --
      --------------------------

      function Get_Nested_Packages (Node : Ada_Node'Class) return Visit_Status
      is
         Package_Data : Package_Info;
      begin
         if Node.Kind in Ada_Basic_Decl
           and then Is_Ghost_Code (Node.As_Basic_Decl)
         then
            return Over;
         end if;

         if Node.Kind = Ada_Private_Part then
            return Over;
         end if;

         if Node.Kind = Ada_Generic_Formal_Part then
            return Over;
         end if;

         case Kind (Node) is
            when Ada_Package_Decl =>
               if Get_Nesting (Node) = "" then
                  Package_Data.Name := new String'
                    (Node_Image (Node.As_Basic_Decl.P_Defining_Name));
               else
                  Package_Data.Name := new String'
                    (Get_Nesting (Node) & "."
                     & Node_Image (Node.As_Basic_Decl.P_Defining_Name));
               end if;

               Package_Data.Is_Generic := False;
               Package_Data.Data_Kind  := Declaration_Data;
               Package_Data.Element    := Node.As_Ada_Node;
               Data.Package_Data_List.Append (Package_Data);

            when Ada_Generic_Package_Decl =>

               if Stub_Mode_ON then
                  return Over;
               end if;

               --  Only library level generics are processed
               if Node.Parent.Kind = Ada_Library_Item then
                  if Get_Nesting (Node) = "" then
                     Package_Data.Name := new String'
                       (Node_Image (Node.As_Basic_Decl.P_Defining_Name));
                  else
                     Package_Data.Name := new String'
                       (Get_Nesting (Node) & "."
                        & Node_Image (Node.As_Basic_Decl.P_Defining_Name));
                  end if;

                  Package_Data.Is_Generic  := True;
                  Package_Data.Data_Kind   := Declaration_Data;
                  Package_Data.Element     := Node.As_Ada_Node;
                  Data.Package_Data_List.Append (Package_Data);
               end if;

            when Ada_Generic_Package_Instantiation =>

               if Stub_Mode_ON or else Is_Node_From_Generic (Node) then
                  return Over;
               end if;

               declare
                  Gen_Name : constant Libadalang.Analysis.Name :=
                    Node.As_Generic_Package_Instantiation.F_Generic_Pkg_Name;
                  Gen_Decl : Basic_Decl :=
                    Gen_Name.P_Relative_Name.As_Name.P_Referenced_Decl;
               begin
                  if Gen_Decl.Is_Null then
                     return Over;
                  end if;

                  Gen_Decl := Gen_Decl.P_Get_Uninstantiated_Node.As_Basic_Decl;

                  --  No processing for instantiations of nested generics,
                  --  also if corresponding generic is not processed (or going
                  --  to be) there is no corresponding generic test package.
                  if not Source_Present (Gen_Decl.Unit.Get_Filename)
                    or else Gen_Decl.Parent.Kind /= Ada_Library_Item
                  then
                     return Over;
                  end if;

                  Package_Data.Name := new String'
                    (Get_Nesting (Node)
                     & "."
                     & Node_Image (Node.As_Basic_Decl.P_Defining_Name));
                  Package_Data.Data_Kind := Instantiation;
                  Package_Data.Is_Generic := False;
                  Package_Data.Generic_Containing_Package := new String'
                    (Node_Image (Gen_Decl.P_Defining_Name));
                  Package_Data.Element := Node.As_Ada_Node;
                  Data.Package_Data_List.Append (Package_Data);
                  return Over;
               end;

            when others =>
               null;
         end case;

         return Into;
      end Get_Nested_Packages;

      -----------------
      -- Get_Records --
      -----------------

      function Get_Records (Node : Ada_Node'Class) return Visit_Status is
         Cur_Node : Ada_Node;

         Type_Data     : Base_Type_Info;
         Test_Type     : Test.Harness.Test_Type_Info;
         Test_Package  : String_Access;

         procedure Get_Type_Parent_Data (Type_Data : in out Base_Type_Info);
         --  Gathers data on parent type

         --------------------------
         -- Get_Type_Parent_Data --
         --------------------------

         procedure Get_Type_Parent_Data (Type_Data : in out Base_Type_Info) is

            Parent_Type : Base_Type_Decl;

            procedure Set_No_Parent (Type_Data : in out Base_Type_Info);
            --  Sets all data relevant to parent type to null/false

            -------------------
            -- Set_No_Parent --
            -------------------

            procedure Set_No_Parent (Type_Data : in out Base_Type_Info) is
            begin
               Type_Data.Argument_Father_Type_Name := null;
               Type_Data.Argument_Father_Nesting   := null;
               Type_Data.Argument_Father_Unit_Name := null;

               Type_Data.Has_Argument_Father       := False;
            end Set_No_Parent;
         begin

            if Stub_Mode_ON then
               Set_No_Parent (Type_Data);
               return;
            end if;

            if Data.Is_Generic or else Inside_Inst then
               Set_No_Parent (Type_Data);
               return;
            end if;

            Parent_Type :=
              Parent_Type_Declaration (Cur_Node.As_Base_Type_Decl);

            if Parent_Type.Is_Null then
               Set_No_Parent (Type_Data);
               return;
            end if;

            if
              not Is_Declared_In_Regular_Package (Parent_Type.As_Ada_Node)
              or else Parent_Type.As_Type_Decl.P_Is_Interface_Type
              or else Is_Fully_Private (Parent_Type)
            then
               Set_No_Parent (Type_Data);
               return;
            end if;

            if not Source_Present (Parent_Type.Unit.Get_Filename) then
               Set_No_Parent (Type_Data);
               return;
            end if;

            Type_Data.Argument_Father_Type_Name := new
              String'(Node_Image (Parent_Type.P_Defining_Name));
            Type_Data.Argument_Father_Nesting   := new
              String'(Get_Nesting (Parent_Type));
            Type_Data.Argument_Father_Unit_Name := new String'
                (Enclosing_Unit_Name (Parent_Type));

            Type_Data.Has_Argument_Father       := True;
         end Get_Type_Parent_Data;
      begin
         if Node.Kind = Ada_Generic_Package_Decl
           and then (Node.Parent.Kind /= Ada_Library_Item
                     or else Stub_Mode_ON)
         then
            --  Nested generics are not supported
            return Over;
         end if;

         if Node.Kind in Ada_Basic_Decl
           and then Is_Ghost_Code (Node.As_Basic_Decl)
         then
            return Over;
         end if;

         if Node.Kind = Ada_Private_Part then
            return Over;
         end if;

         if Node.Kind = Ada_Generic_Formal_Part then
            return Over;
         end if;

         if Node.Kind = Ada_Package_Decl and then Inside_Inst then
            --  No processing for packages nested inside generic ones
            return Over;
         end if;

         if Node.Kind = Ada_Generic_Package_Instantiation
           and then not Inside_Inst and then not Data.Is_Generic
         then
            if Stub_Mode_ON then
               return Over;
            end if;

            declare
               Gen_Name : constant Libadalang.Analysis.Name :=
                 Node.As_Generic_Package_Instantiation.F_Generic_Pkg_Name;
               Gen_Decl : Basic_Decl :=
                 Gen_Name.P_Relative_Name.As_Name.P_Referenced_Decl;
            begin
               if Gen_Decl.Is_Null then
                  return Over;
               end if;

               Gen_Decl := Gen_Decl.P_Get_Uninstantiated_Node.As_Basic_Decl;

               --  No processing for instantiations of nested generics,
               --  also if corresponding generic is not processed (or going
               --  to be) there is no corresponding generic test package.
               if not Source_Present (Gen_Decl.Unit.Get_Filename)
                 or else Gen_Decl.Parent.Kind /= Ada_Library_Item
               then
                  return Over;
               end if;

               Inside_Inst := True;
               Instance_Nesting := new String'
                 (Encode
                    (Node.As_Basic_Decl.P_Fully_Qualified_Name,
                     Node.Unit.Get_Charset));
               Instance_Sloc := new String'
                 (Base_Name (Data.Unit_File_Name.all)
                  & ":"
                  & Trim (First_Line_Number (Node)'Img, Both)
                  & ":"
                  & Trim (First_Column_Number (Node)'Img, Both)
                  & ":");

               Increase_Indent
                 (Me,
                  "traversing instantiation " & Node.Image);
               Traverse (Gen_Decl, Get_Records'Access);
               Decrease_Indent (Me);

               Inside_Inst := False;
               Free (Instance_Nesting);
               Free (Instance_Sloc);
               return Over;
            end;
         end if;

         if Kind (Node) not in Ada_Type_Decl then
            return Into;
         end if;

         if not Node.As_Type_Decl.P_Is_Tagged_Type then
            return Over;
         end if;

         if Node.As_Type_Decl.P_Is_Interface_Type then
            return Over;
         end if;

         if Node.As_Base_Type_Decl.P_Is_Private then
            Cur_Node :=
              Node.As_Base_Type_Decl.P_Private_Completion.As_Ada_Node;
         else
            Cur_Node := Node.As_Ada_Node;
         end if;

         --  Gathering basic data about type
         Type_Data.Main_Type_Elem := Cur_Node;
         Type_Data.Main_Type_Text_Name := new
           String'(Node_Image (Cur_Node.As_Basic_Decl.P_Defining_Name));
         if Inside_Inst then
            Type_Data.Nesting := new String'(Instance_Nesting.all);
         else
            Type_Data.Nesting := new String'(Get_Nesting (Cur_Node));
         end if;

         --  Checking for duplicating types
         declare
            Stored_Type : Base_Type_Info;
         begin

            for I in Data.Type_Data_List.First_Index ..
              Data.Type_Data_List.Last_Index
            loop
               Stored_Type := Data.Type_Data_List.Element (I);
               if Stored_Type.Main_Type_Elem = Cur_Node
                 and then Stored_Type.Nesting.all = Type_Data.Nesting.all
               then
                  Free (Type_Data.Main_Type_Text_Name);
                  Free (Type_Data.Nesting);
                  return Over;
               end if;
            end loop;
         end;

         Check_Type_For_Elaboration (Cur_Node.As_Base_Type_Decl);

         --  Checking if any of ancestor types had a discriminant part
         Type_Data.No_Default_Discriminant :=
           Check_Type_For_Unset_Discriminants (Cur_Node.As_Base_Type_Decl);

         Get_Type_Parent_Data (Type_Data);

         Type_Data.Main_Type_Abstract :=
           Abstract_Type (Cur_Node.As_Base_Type_Decl);

         Type_Data.Type_Number := Type_Counter;
         Type_Counter          := Type_Counter + 1;

         Data.Type_Data_List.Append (Type_Data);

         if
           Type_Data.Nesting.all = Data.Unit_Full_Name.all
         then
            Test_Package := new String'
              (Data.Unit_Full_Name.all
               & "."
               & Type_Data.Main_Type_Text_Name.all
               & Test_Data_Unit_Name_Suff
               & "."
               & Type_Data.Main_Type_Text_Name.all
               & Test_Unit_Name_Suff);

         else
            Test_Package := new String'
              (Data.Unit_Full_Name.all
                & "." & Test_Data_Unit_Name & "."
                & Test_Unit_Name & "."
                & Nesting_Difference
                 (Type_Data.Nesting.all,
                  Data.Unit_Full_Name.all)
                & "."
                & Type_Data.Main_Type_Text_Name.all
                & Test_Data_Unit_Name_Suff
                & "."
                & Type_Data.Main_Type_Text_Name.all
                & Test_Unit_Name_Suff);
         end if;

         Test_Type.Test_Type := No_Ada_Node;
         Test_Type.Test_Type_Name := new String'
           ("Test_" &
              Type_Data.Main_Type_Text_Name.all);
         Test_Type.Nesting := new String'
           (Test_Package.all);

         if not Type_Data.Main_Type_Abstract then
            Suite_Data_List.Test_Types.Append
              (Test_Type_Info_Wrapper'
                 (TT_Info       => Test_Type,
                  Test_Package  => Test_Package,
                  Original_Type => Type_Data.Main_Type_Elem));
         end if;

         return Over;
      end Get_Records;

      ---------------------
      -- Get_Subprograms --
      ---------------------

      function Get_Subprograms (Node : Ada_Node'Class) return Visit_Status is
         Subp       : Subp_Info;
         Owner_Decl : Base_Type_Decl;

         Type_Found           : Boolean := False;
         Test_Routine         : Test.Harness.Test_Routine_Info;
         Test_Routine_Wrapper : Test_Routine_Info_Wrapper;
         Test_Package_Name    : String_Access;

         Original_Type : Base_Type_Decl;

         Has_TC : Boolean;

         procedure Update_Name_Frequency (Subp_Name : String);

         ---------------------------
         -- Update_Name_Frequency --
         ---------------------------

         procedure Update_Name_Frequency (Subp_Name : String) is
            Cur : Name_Frequency.Cursor;
         begin
            Cur := Data.Subp_Name_Frequency.Find (To_Lower (Subp_Name));

            if Cur = Name_Frequency.No_Element then
               Data.Subp_Name_Frequency.Include (To_Lower (Subp_Name), 1);
            else
               Data.Subp_Name_Frequency.Replace_Element
                 (Cur, (Name_Frequency.Element (Cur)) + 1);
            end if;

         end Update_Name_Frequency;
      begin

         if Node.Kind in Ada_Basic_Decl
           and then Is_Ghost_Code (Node.As_Basic_Decl)
         then
            return Over;
         end if;

         if Node.Kind = Ada_Generic_Package_Decl
           and then (Node.Parent.Kind /= Ada_Library_Item
                     or else Stub_Mode_ON)
         then
            --  Nested generics are not supported
            return Over;
         end if;

         if Node.Kind = Ada_Package_Decl and then Inside_Inst then
            --  No processing for packages nested inside generic ones
            return Over;
         end if;

         if
           Node.Kind in Ada_Protected_Type_Decl | Ada_Single_Protected_Decl
         then
            return Over;
         end if;

         if Node.Kind = Ada_Generic_Formal_Part then
            return Over;
         end if;

         if Node.Kind = Ada_Generic_Package_Instantiation
           and then not Inside_Inst and then not Data.Is_Generic
         then

            if Stub_Mode_ON then
               return Over;
            end if;

            declare
               Gen_Name : constant Libadalang.Analysis.Name :=
                 Node.As_Generic_Package_Instantiation.F_Generic_Pkg_Name;
               Gen_Decl : Basic_Decl :=
                 Gen_Name.P_Relative_Name.As_Name.P_Referenced_Decl;
            begin
               if Gen_Decl.Is_Null then
                  return Over;
               end if;

               Gen_Decl := Gen_Decl.P_Get_Uninstantiated_Node.As_Basic_Decl;

               --  No processing for instantiations of nested generics,
               --  also if corresponding generic is not processed (or going
               --  to be) there is no corresponding generic test package
               if not Source_Present (Gen_Decl.Unit.Get_Filename)
                 or else Gen_Decl.Parent.Kind /= Ada_Library_Item
               then
                  return Over;
               end if;

               Inside_Inst := True;
               Instance_Nesting := new String'
                 (Encode
                    (Node.As_Basic_Decl.P_Fully_Qualified_Name,
                     Node.Unit.Get_Charset));
               Instance_Sloc := new String'
                 (Base_Name (Data.Unit_File_Name.all)
                  & ":"
                  & Trim (First_Line_Number (Node)'Img, Both)
                  & ":"
                  & Trim (First_Column_Number (Node)'Img, Both)
                  & ":");

               Increase_Indent
                 (Me, "traversing instantiation " & Node.Image);
               Traverse (Gen_Decl, Get_Subprograms'Access);
               Decrease_Indent (Me);

               Inside_Inst := False;
               Free (Instance_Nesting);
               Free (Instance_Sloc);
               return Over;
            end;
         end if;

         if
           Node.Kind = Ada_Expr_Function
           and then not Node.As_Base_Subp_Body.P_Previous_Part_For_Decl.Is_Null
         then
            --  It will be treated at spec.
            return Over;
         end if;

         if
           Node.Kind not in Ada_Subp_Decl
             | Ada_Subp_Renaming_Decl | Ada_Expr_Function
         then
            return Into;
         end if;

         if Node.Kind = Ada_Subp_Renaming_Decl
           and then not Node.As_Basic_Decl.P_Previous_Part_For_Decl.Is_Null
         then
            --  A subprogram renaming in this case is a renaming-as-body
            --  corresponding declaration has already been processed.
            return Over;
         end if;

         Subp.Subp_Declaration := Node.As_Ada_Node;
         Subp.Subp_Text_Name   := new String'(Get_Subp_Name (Node));
         Subp.Subp_Name_Image   := new String'
           (Node_Image (Node.As_Basic_Decl.P_Defining_Name));
         if Inside_Inst then
            Subp.Nesting := new String'(Instance_Nesting.all);
         else
            Subp.Nesting := new String'(Get_Nesting (Node));
         end if;

         --  Setting tested subprogram sloc for suite info
         declare
            Subp_Span : constant Source_Location_Range :=
              Subp.Subp_Declaration.Sloc_Range;
         begin
            if Inside_Inst then
               Test_Routine.Tested_Sloc := new String'
                 (Base_Name (Subp.Subp_Declaration.Unit.Get_Filename)
                  & ":"
                  & Trim (Subp_Span.Start_Line'Img, Both)
                  & ":"
                  & Trim (Subp_Span.Start_Column'Img, Both)
                  & " instance at "
                  & Instance_Sloc.all);
            else
               Test_Routine.Tested_Sloc := new String'
                 (Base_Name (Data.Unit_File_Name.all)
                  & ":"
                  & Trim (Subp_Span.Start_Line'Img, Both)
                  & ":"
                  & Trim (Subp_Span.Start_Column'Img, Both)
                  & ":");
            end if;
         end;

         if Node.Kind = Ada_Expr_Function then
            Owner_Decl := P_Primitive_Subp_Tagged_Type
              (Node.As_Base_Subp_Body.F_Subp_Spec.As_Base_Subp_Spec);
         elsif Node.Kind = Ada_Subp_Renaming_Decl then
            Owner_Decl := P_Primitive_Subp_Tagged_Type
              (Node.As_Subp_Renaming_Decl.F_Subp_Spec.As_Base_Subp_Spec);
         else
            Owner_Decl := P_Primitive_Subp_Tagged_Type
              (Node.As_Basic_Subp_Decl.P_Subp_Decl_Spec);
         end if;

         if Owner_Decl /= No_Base_Type_Decl
         --  If owner is incomplete private declaration (without "tagged"
         --  keyword) subp should be treated as non-dispatching.
         then
            if Owner_Decl.As_Base_Type_Decl.P_Is_Private then
               Owner_Decl :=
                 Owner_Decl.As_Base_Type_Decl.P_Private_Completion;
            end if;

            Type_Found := False;
            for
              I in Data.Type_Data_List.First_Index ..
                Data.Type_Data_List.Last_Index
            loop

               if
                 Data.Type_Data_List.Element (I).Main_Type_Elem = Owner_Decl
               then
                  Subp.Corresp_Type :=
                    Data.Type_Data_List.Element (I).Type_Number;

                  Subp.Subp_Mangle_Name := new
                    String'(Mangle_Hash (Node));
                  Subp.Subp_Full_Hash := new
                    String'(Mangle_Hash_Full (Node));
                  Subp.Subp_Hash_V1 := new
                    String'(Mangle_Hash_Full (Node, True, True));
                  Subp.Subp_Hash_V2_1 := new
                    String'(Mangle_Hash_Full
                            (Node,
                               N_Controlling => True));

                  Type_Found := True;
                  exit;
               end if;
            end loop;
         end if;

         --  Setting suite info
         if Type_Found then
            Test_Routine.TR_Declaration := No_Ada_Node;
            Test_Routine.TR_Text_Name := new String'
              (Subp.Subp_Mangle_Name.all);
            --  Not setting test type number since it will be reset
            --  during suite_data generation.
            Original_Type := Owner_Decl;

            if
              Nesting_Difference
                (Data.Unit_Full_Name.all, Subp.Nesting.all) = ""
            then
               Test_Package_Name := new String'
                 (Data.Unit_Full_Name.all
                  & "."
                  & Node_Image
                    (Original_Type.As_Basic_Decl.P_Defining_Name)
                  & Test_Data_Unit_Name_Suff & "."
                  & Node_Image
                    (Original_Type.As_Basic_Decl.P_Defining_Name)
                  & Test_Unit_Name_Suff);

            else
               Test_Package_Name := new String'
                 (Data.Unit_Full_Name.all & "."
                  & Test_Data_Unit_Name & "."
                  & Test_Unit_Name
                  & "."
                  & Nesting_Difference
                    (Data.Unit_Full_Name.all,
                     Subp.Nesting.all)
                  & "."
                  & Node_Image
                    (Original_Type.As_Basic_Decl.P_Defining_Name)
                  & Test_Data_Unit_Name_Suff & "."
                  & Node_Image
                    (Original_Type.As_Basic_Decl.P_Defining_Name)
                  & Test_Unit_Name_Suff);
            end if;

            Test_Routine.Nesting := new String'(Test_Package_Name.all);
         else

            --  In case when owner tagged type is declared in the private part
            --  the check for Elaboration control is not performed
            --  for the type in Get_Records so we need to launch it here.

            if Node.Kind = Ada_Expr_Function then
               Owner_Decl := P_Primitive_Subp_Tagged_Type
                 (Node.As_Base_Subp_Body.F_Subp_Spec.As_Base_Subp_Spec);
            elsif Node.Kind = Ada_Subp_Renaming_Decl then
               Owner_Decl := P_Primitive_Subp_Tagged_Type
                 (Node.As_Subp_Renaming_Decl.F_Subp_Spec.As_Base_Subp_Spec);
            else
               Owner_Decl := P_Primitive_Subp_Tagged_Type
                 (Node.As_Basic_Subp_Decl.P_Subp_Decl_Spec);
            end if;

            if Owner_Decl /= No_Base_Type_Decl then
               Check_Type_For_Elaboration (Owner_Decl);
            end if;

            --  In simple case the type is always found, because in fact
            --  we do not depend on it.
            Type_Found            := True;
            Subp.Corresp_Type     := 0;

            Subp.Subp_Mangle_Name := new
              String'(Mangle_Hash (Node, Unwind_Controlling => False));
            Subp.Subp_Full_Hash := new
              String'(Mangle_Hash_Full (Node, N_Controlling => True));
            Subp.Subp_Hash_V1 := new
              String'(Mangle_Hash_Full (Node, True, True));
            Subp.Subp_Hash_V2_1 := new
              String'(Mangle_Hash_Full
                      (Node,
                         N_Controlling => True));

            Data.Has_Simple_Case  := True;
            Data.Needs_Set_Up     := True;
            Data.Needs_Assertions := True;

            --  Adding corresponding test routines for non-primitives to
            --  the first element of suite data list.

            Test_Routine.TR_Declaration := No_Ada_Node;
            Test_Routine.TR_Text_Name := new String'
              (Subp.Subp_Mangle_Name.all);
            Test_Routine.Test_Type_Numb := 1;

            if
              Nesting_Difference
                (Data.Unit_Full_Name.all, Subp.Nesting.all) = ""
            then
               Test_Routine.Nesting := new String'
                 (Subp.Nesting.all & "." &
                    Test_Data_Unit_Name & "." &
                    Test_Unit_Name);

            else
               Test_Routine.Nesting := new String'
                 (Nesting_Common_Prefix
                    (Data.Unit_Full_Name.all, Subp.Nesting.all) &
                    "." & Test_Data_Unit_Name &
                    "." & Test_Unit_Name & "." &
                    Nesting_Difference
                    (Data.Unit_Full_Name.all, Subp.Nesting.all) &
                    "." & Test_Data_Unit_Name & "." & Test_Unit_Name);
            end if;

            Test_Package_Name := new String'
              (Test_Routine.Nesting.all);
            Original_Type := No_Base_Type_Decl;

         end if;

         if Type_Found then

            Test_Routine_Wrapper :=
              (TR_Info       => Test_Routine,
               Test_Package  => Test_Package_Name,
               Original_Type => Original_Type.As_Ada_Node,
               Original_Subp => Node.As_Ada_Node,
               From_Generic  => Is_Node_From_Generic (Node));

            Gather_Test_Cases
              (Subp,
               Test_Routine_Wrapper,
               Data,
               Suite_Data_List,
               Has_TC,
               (if Instance_Sloc = null then "" else Instance_Sloc.all));

            if Has_TC or else not Test_Case_Only then
               Update_Name_Frequency (Subp.Subp_Text_Name.all);
            end if;
         end if;

         return Over;
      end Get_Subprograms;

      ----------------------------------
      -- Gather_Inherited_Subprograms --
      ----------------------------------

      procedure Gather_Inherited_Subprograms
        (Dummy_Types     : Natural;
         Suite_Data_List : in out Suites_Data_Type)
      is
         Type_Dec : Type_Decl;

         function Is_Overridden
           (Subp : Basic_Decl; Decls : Basic_Decl_Array) return Boolean;
         --  Checks whether given inherited subprogram is hidden by an
         --  overriding one.

         -------------------
         -- Is_Overridden --
         -------------------

         function Is_Overridden
           (Subp : Basic_Decl; Decls : Basic_Decl_Array) return Boolean is
         begin
            for Dec of Decls loop
               if Subp = Dec then
                  return False;
               end if;
            end loop;

            return True;
         end Is_Overridden;

         Test_Routine : Test.Harness.Test_Routine_Info_Enhanced;
         Test_Routine_Wrapper : Test_Routine_Info_Enhanced_Wrapper;

         Tmp_Data        : Data_Holder;
         Tmp_Suites_Data : Suites_Data_Type;
         Tmp_Subp        : Subp_Info;
         Dummy_TR_Info   : Test_Routine_Info_Wrapper;
         Tmp_TR          : Test.Harness.Test_Routine_Info;
         Tmp_Has_TC    : Boolean;
      begin

         --  Creating a stub for Subp_Info object
         Tmp_Subp.Nesting          := new String'("");
         Tmp_Subp.Subp_Text_Name   := new String'("");
         Tmp_Subp.Subp_Full_Hash   := new String'("");
         Tmp_Subp.Subp_Hash_V1     := new String'("");
         Tmp_Subp.Subp_Hash_V2_1   := new String'("");

         for
           K in Suite_Data_List.Test_Types.First_Index + Dummy_Type_Counter ..
             Suite_Data_List.Test_Types.Last_Index
         loop
            if Suite_Data_List.Test_Types.Element (K).Original_Type.Kind in
              Ada_Task_Type_Decl | Ada_Protected_Type_Decl
            then
               goto Skip_Inheritance;
            end if;

            Type_Dec := As_Type_Decl
              (Suite_Data_List.Test_Types.Element (K).Original_Type);
            declare
               ISubs : constant Basic_Decl_Array :=
                 Type_Dec.P_Get_Primitives (Only_Inherited => True);
               ISubs2 : constant Basic_Decl_Array :=
                 Type_Dec.P_Get_Primitives (Only_Inherited => False);
               ISub : Basic_Decl;
               Ancestor_Type : Base_Type_Decl;
            begin

               for ISub_Iter of ISubs loop

                  ISub := ISub_Iter;

                  if
                    Source_Present (ISub.Unit.Get_Filename)
                    and then Is_Callable_Subprogram (ISub)
                    and then not Is_Private (ISub)
                    and then not Is_Overridden (ISub, ISubs2)
                  then

                     --  We need to go to original declaration of the inherited
                     --  subprogram to have same controlling type as specified
                     --  for the parameter to perform root type substitution
                     --  during hash computation.
                     declare
                        Arr : constant Basic_Decl_Array :=
                          P_Base_Subp_Declarations (ISub_Iter);
                     begin
                        ISub := Arr (Arr'Last);
                     end;

                     if ISub.Kind = Ada_Expr_Function then
                        Ancestor_Type :=
                          P_Primitive_Subp_Tagged_Type
                            (ISub.As_Base_Subp_Body.F_Subp_Spec.
                               As_Base_Subp_Spec);
                     elsif ISub.Kind = Ada_Subp_Renaming_Decl then
                        Ancestor_Type :=
                          P_Primitive_Subp_Tagged_Type
                            (ISub.As_Subp_Renaming_Decl.F_Subp_Spec.
                               As_Base_Subp_Spec);
                     else
                        Ancestor_Type :=
                          P_Primitive_Subp_Tagged_Type
                            (ISub.As_Basic_Subp_Decl.P_Subp_Decl_Spec);
                     end if;

                     while not Ancestor_Type.P_Next_Part.Is_Null loop
                        Ancestor_Type := Ancestor_Type.P_Next_Part;
                     end loop;

                     if Test_Types_Linked
                       (Ancestor_Type, Type_Dec.As_Base_Type_Decl)
                       and then No_Inheritance_Through_Generics
                         (Ancestor_Type, Type_Dec.As_Base_Type_Decl)
                     then
                        --  Check if the inherited subprogram had
                        --  Test_Cases. In such case one test per Test_Case
                        --  should be inherited.
                        Tmp_Data.Unit_File_Name := new
                          String'(Base_Name (ISub.Unit.Get_Filename));
                        Tmp_Subp.Subp_Declaration := ISub.As_Ada_Node;
                        Tmp_Subp.Subp_Text_Name :=
                          new String'(Get_Subp_Name (ISub));
                        Tmp_Subp.Subp_Mangle_Name :=
                          new String'(Mangle_Hash (ISub));
                        Tmp_Subp.Subp_Name_Image := new String'
                          (Node_Image (ISub.As_Basic_Decl.P_Defining_Name));
                        Tmp_Subp.Corresp_Type := K;

                        Gather_Test_Cases
                          (Tmp_Subp,
                           Dummy_TR_Info,
                           Tmp_Data,
                           Tmp_Suites_Data,
                           Tmp_Has_TC);

                        if
                          Get_Nesting (ISub) = Enclosing_Unit_Name (ISub)
                        then
                           Test_Routine.TR_Rarent_Unit_Name := new String'
                             (Enclosing_Unit_Name (ISub)
                              & "."
                              & Node_Image
                                (P_Defining_Name
                                     (Ancestor_Type.As_Basic_Decl))
                              & Test_Data_Unit_Name_Suff
                              & "."
                              & Node_Image
                                (P_Defining_Name
                                     (Ancestor_Type.As_Basic_Decl))
                              & Test_Unit_Name_Suff);
                        else
                           Test_Routine.TR_Rarent_Unit_Name := new String'
                             (Enclosing_Unit_Name (ISub)
                              & "."
                              & Test_Data_Unit_Name
                              & "."
                              & Test_Unit_Name
                              & "."
                              & Nesting_Difference
                                (Get_Nesting (ISub),
                                 Enclosing_Unit_Name (ISub))
                              & Node_Image
                                (P_Defining_Name
                                     (Ancestor_Type.As_Basic_Decl))
                              & Test_Data_Unit_Name_Suff
                              & "."
                              & Node_Image
                                (P_Defining_Name
                                     (Ancestor_Type.As_Basic_Decl))
                              & Test_Unit_Name_Suff);
                        end if;
                        Test_Routine.Nesting := new String'
                          (Test_Routine.TR_Rarent_Unit_Name.all);

                        if
                          Get_Nesting (Type_Dec) = Data.Unit_Full_Name.all
                        then
                           Test_Routine_Wrapper.Test_Package := new String'
                             (Data.Unit_Full_Name.all
                              & "."
                              & Node_Image
                                (P_Defining_Name
                                     (Type_Dec.As_Basic_Decl))
                              & Test_Data_Unit_Name_Suff
                              & "."
                              & Node_Image
                                (P_Defining_Name
                                     (Type_Dec.As_Basic_Decl))
                              & Test_Unit_Name_Suff);

                        else
                           Test_Routine_Wrapper.Test_Package := new String'
                             (Data.Unit_Full_Name.all
                              & "."
                              & Test_Data_Unit_Name
                              & "."
                              & Test_Unit_Name
                              & "."
                              & Nesting_Difference
                                (Get_Nesting (Type_Dec),
                                 Data.Unit_Full_Name.all)
                              & "."
                              & Node_Image
                                (P_Defining_Name
                                     (Type_Dec.As_Basic_Decl))
                              & Test_Data_Unit_Name_Suff
                              & "."
                              & Node_Image
                                (P_Defining_Name
                                     (Type_Dec.As_Basic_Decl))
                              & Test_Unit_Name_Suff);
                        end if;

                        --  Type is always the same, test_cases or not
                        Test_Routine_Wrapper.Original_Type :=
                          Type_Dec.As_Ada_Node;

                        if Tmp_Has_TC then

                           --  There were Test_Cases
                           for I in Tmp_Suites_Data.TR_List.First_Index ..
                             Tmp_Suites_Data.TR_List.Last_Index
                           loop
                              Tmp_TR :=
                                Tmp_Suites_Data.TR_List.Element (I).TR_Info;

                              Test_Routine.TR_Text_Name :=
                                new String'(Tmp_TR.TR_Text_Name.all);

                              --  adding sloc info
                              Test_Routine.Tested_Sloc := new String'
                                (Tmp_TR.Tested_Sloc.all
                                 & " inherited at "
                                 & Base_Name
                                   (Type_Dec.Unit.Get_Filename)
                                 & ":"
                                 & Trim
                                   (First_Line_Number (Type_Dec)'Img, Both)
                                 & ":"
                                 & Trim
                                   (First_Column_Number (Type_Dec)'Img,
                                    Both)
                                 & ":");

                              Test_Routine_Wrapper.TR_Info := Test_Routine;

                              Suite_Data_List.ITR_List.Append
                                (Test_Routine_Wrapper);
                           end loop;

                        elsif not Test_Case_Only then
                           --  There were no test_Cases, we just need
                           --  to add the single inherited test.

                           Test_Routine.TR_Text_Name   := new String'
                             (Mangle_Hash (ISub));

                           --  Adding sloc info
                           Test_Routine.Tested_Sloc := new String'
                             (Base_Name (ISub.Unit.Get_Filename)
                              & ":"
                              & Trim
                                (First_Line_Number (ISub)'Img, Both)
                              & ":"
                              & Trim
                                (First_Column_Number (ISub)'Img, Both)
                              & ": inherited at "
                              & Base_Name (Type_Dec.Unit.Get_Filename)
                              & ":"
                              & Trim
                                (First_Line_Number (Type_Dec)'Img, Both)
                              & ":"
                              & Trim
                                (First_Column_Number (Type_Dec)'Img, Both)
                              & ":");

                           Test_Routine_Wrapper.TR_Info := Test_Routine;

                           Suite_Data_List.ITR_List.Append
                             (Test_Routine_Wrapper);
                        end if;
                     end if;
                  end if;

                  Tmp_Data.Subp_List.Clear;
                  Tmp_Suites_Data.TR_List.Clear;
               end loop;
            end;
            <<Skip_Inheritance>>
         end loop;
      end Gather_Inherited_Subprograms;

      ------------------------------
      -- Gather_Substitution_Data --
      ------------------------------

      procedure Gather_Substitution_Data
        (Suite_Data_List : in out Suites_Data_Type)
      is
         OSub          : Basic_Decl := No_Basic_Decl;
         Ancestor_Type : Base_Type_Decl;

         TR    : Test.Harness.Test_Routine_Info;
         LTR   : Test.Harness.Test_Routine_Info_Enhanced;
         LTR_W : Test_Routine_Info_Enhanced_Wrapper;
         Depth : Natural;

         Test_Type_Wrapper : Test_Type_Info_Wrapper;
      begin
         for TR_W of Suite_Data_List.TR_List loop
            if not TR_W.Original_Type.Is_Null then
               declare
                  OSubs : constant Basic_Decl_Array :=
                    P_Base_Subp_Declarations
                      (TR_W.Original_Subp.As_Basic_Decl);
               begin
                  if OSubs'Length > 1 then
                     OSub := No_Basic_Decl;
                     for O in reverse OSubs'First .. OSubs'Last - 1 loop
                        if OSubs (O).Kind /= Ada_Abstract_Subp_Decl then
                           OSub := OSubs (O);
                        end if;
                     end loop;
                  end if;
               end;
            end if;

            if not OSub.Is_Null then

               if OSub.Kind = Ada_Expr_Function then
                  Ancestor_Type :=
                    P_Primitive_Subp_Tagged_Type
                      (OSub.As_Base_Subp_Body.F_Subp_Spec.
                         As_Base_Subp_Spec);
               else
                  Ancestor_Type :=
                    P_Primitive_Subp_Tagged_Type
                      (OSub.As_Basic_Subp_Decl.P_Subp_Decl_Spec);
               end if;

               while not Ancestor_Type.P_Next_Part.Is_Null loop
                  Ancestor_Type := Ancestor_Type.P_Next_Part;
               end loop;

               if Source_Present (Ancestor_Type.Unit.Get_Filename)
                 and then Is_Callable_Subprogram (OSub)
                 and then Test_Types_Linked
                   (Ancestor_Type, TR_W.Original_Type.As_Base_Type_Decl)
                 and then No_Inheritance_Through_Generics
                   (Ancestor_Type, TR_W.Original_Type.As_Base_Type_Decl)
               then
                  Depth := Inheritance_Depth
                    (Ancestor_Type.As_Base_Type_Decl,
                     TR_W.Original_Type.As_Base_Type_Decl);

                  --  Inheritance depth of corresponding test type needs to be
                  --  updated
                  for
                    L in Suite_Data_List.Test_Types.First_Index ..
                      Suite_Data_List.Test_Types.Last_Index
                  loop

                     Test_Type_Wrapper :=
                       Suite_Data_List.Test_Types.Element (L);

                     if Test_Type_Wrapper.Original_Type = TR_W.Original_Type
                     then
                        if Depth >
                          Test_Type_Wrapper.TT_Info.Max_Inheritance_Depth
                        then
                           Test_Type_Wrapper.TT_Info.Max_Inheritance_Depth :=
                             Depth;

                           Suite_Data_List.Test_Types.Replace_Element
                             (L, Test_Type_Wrapper);

                           exit;
                        end if;
                     end if;

                  end loop;

                  --  ATM Test_Cases are not taken into account.
                  TR := TR_W.TR_Info;
                  LTR.TR_Text_Name := new String'(TR.TR_Text_Name.all);
                  LTR.Inheritance_Depth := Depth;
                  LTR_W.TR_Info         := LTR;
                  LTR_W.Original_Type   := TR_W.Original_Type;
                  LTR_W.Test_Package    := new String'(TR_W.Test_Package.all);

                  --  Adding sloc info
                  LTR_W.TR_Info.Tested_Sloc := new String'
                    (Base_Name (OSub.Unit.Get_Filename)
                     & ":"
                     & Trim
                       (First_Line_Number (OSub)'Img,
                        Both)
                     & ":"
                     & Trim
                       (First_Column_Number (OSub)'Img,
                        Both)
                     & ": overridden at "
                     & Base_Name
                       (TR_W.Original_Type.Unit.Get_Filename)
                     & ":"
                     & Trim
                       (First_Line_Number (TR_W.Original_Subp)'Img,
                        Both)
                     & ":"
                     & Trim
                       (First_Column_Number (TR_W.Original_Subp)'Img,
                        Both)
                     & ":");

                  Suite_Data_List.LTR_List.Append (LTR_W);
               end if;
            end if;
         end loop;
      end Gather_Substitution_Data;

      ----------------------
      -- Is_Fully_Private --
      ----------------------

      function Is_Fully_Private (Arg : Base_Type_Decl) return Boolean is
         Type_Part : Base_Type_Decl := Arg;
      begin
         while not Type_Part.P_Previous_Part.Is_Null loop
            Type_Part := Type_Part.P_Previous_Part;
         end loop;

         return Is_Private (Type_Part);
      end Is_Fully_Private;

      -------------------
      -- Is_Ghost_Code --
      -------------------

      function Is_Ghost_Code (Decl : Basic_Decl) return Boolean is
      begin

         if Is_Null (Decl.P_Defining_Name) then
            return Decl.P_Is_Ghost_Code;
         else
            --  We consider that as soon as one of the defining names in
            --  Decl is not ghost, then the whole declaration is not ghost
            --  as well.

            return (for all Name of Decl.P_Defining_Names
                      => Name.P_Is_Ghost_Code);
         end if;
      end Is_Ghost_Code;

      -----------------------
      -- Test_Types_Linked --
      -----------------------

      function Test_Types_Linked
        (Inheritance_Root_Type  : Base_Type_Decl;
         Inheritance_Final_Type : Base_Type_Decl)
            return Boolean
      is
         Intermidiate : Base_Type_Decl := Inheritance_Final_Type;
      begin

         while not Intermidiate.Is_Null loop
            if Is_Fully_Private (Intermidiate) then
               return False;
            end if;

            if Intermidiate = Inheritance_Root_Type then
               return True;
            end if;
            Intermidiate := Parent_Type_Declaration (Intermidiate);
         end loop;

         return False;
      end Test_Types_Linked;

      -------------------------------------
      -- No_Inheritance_Through_Generics --
      -------------------------------------

      function No_Inheritance_Through_Generics
        (Inheritance_Root_Type  : Base_Type_Decl;
         Inheritance_Final_Type : Base_Type_Decl)
         return Boolean
      is
         Intermidiate : Base_Type_Decl := Inheritance_Final_Type;
      begin
         while not Intermidiate.Is_Null loop
            if not Is_Declared_In_Regular_Package (Intermidiate) then
               return False;
            end if;

            if Intermidiate = Inheritance_Root_Type then
               return True;
            end if;
            Intermidiate := Parent_Type_Declaration (Intermidiate);
         end loop;

         return False;
      end No_Inheritance_Through_Generics;

      --------------------------
      -- Is_Node_From_Generic --
      --------------------------

      function Is_Node_From_Generic (Node : Ada_Node'Class) return Boolean
      is
         Elem : Ada_Node := Node.As_Ada_Node;
      begin
         if Inside_Inst then
            return False;
         end if;

         while not Elem.Is_Null loop

            if Elem.Kind = Ada_Generic_Package_Decl then
               return True;
            end if;

            Elem := Elem.Parent;
         end loop;

         return False;
      end Is_Node_From_Generic;

      --------------------------------
      -- Check_Type_For_Elaboration --
      --------------------------------

      procedure Check_Type_For_Elaboration (Type_Dec : Base_Type_Decl) is
         Dec  : Base_Type_Decl := Type_Dec;
         Dec2 : Base_Type_Decl;

         Elab_Name : constant Langkit_Support.Text.Unbounded_Text_Type :=
           To_Unbounded_Text ("preelaborable_initialization");

         Unit_SF_Name : constant String :=
           Base_Name (Type_Dec.Unit.Get_Filename);

         function Check_Pragma (Node : Ada_Node'Class) return Boolean;
         --  Checks for pragma in the following nodes

         function Check_Pragma (Node : Ada_Node'Class) return Boolean is
            Next : Ada_Node := Node.Next_Sibling;
         begin
            while not Next.Is_Null and then Next.Kind = Ada_Pragma_Node loop
               if To_Lower (Node_Image (F_Id (Next.As_Pragma_Node))) =
                 "preelaborable_initialization"
               then
                  return True;
               end if;

               Next := Next.Next_Sibling;
            end loop;

            return False;
         end Check_Pragma;

      begin

         while not Dec.Is_Null loop

            --  We need to check all 3 possible declarations, so first roll
            --  to the topmost one.
            while not Dec.P_Previous_Part.Is_Null loop
               Dec := Dec.P_Previous_Part;
            end loop;

            Dec2 := Dec;

            while not Dec2.Is_Null loop
               if Dec2.P_Has_Aspect (Elab_Name)
                 or else not Dec2.P_Get_Pragma (Elab_Name).Is_Null
                 or else Check_Pragma (Dec2)
               then
                  Report_Std
                    ("warning: (gnattest) "
                     & Unit_SF_Name
                     & ":"
                     & Trim (First_Line_Number (Dec2)'Img, Both)
                     & ":"
                     & Trim (First_Column_Number (Dec2)'Img, Both)
                     & ":"
                     & " elaboration control pragma given"
                     & " for ancestor type of "
                     & Node_Image (Type_Dec.P_Defining_Name));
                  Report_Std
                    ("this can cause circularity in the test harness",
                     1);
                  return;
               end if;

               if Dec2.P_Next_Part.Is_Null then
                  Dec := Parent_Type_Declaration (Dec2);
               end if;

               Dec2 := Dec2.P_Next_Part;
            end loop;

         end loop;
      end Check_Type_For_Elaboration;

      function Check_Type_For_Unset_Discriminants
        (Type_Dec : Base_Type_Decl) return Boolean
      is
         Dec  : Base_Type_Decl := Type_Dec;
         Dec2 : Base_Type_Decl;

         Discr : Discriminant_Part;
      begin

         while not Dec.Is_Null loop

            --  We need to check all 3 possible declarations, so first roll
            --  to the topmost one.
            while not Dec.P_Previous_Part.Is_Null loop
               Dec := Dec.P_Previous_Part;
            end loop;

            Dec2 := Dec;

            while not Dec2.Is_Null loop

               if Dec2.Kind in Ada_Incomplete_Type_Decl
                             | Ada_Incomplete_Tagged_Type_Decl
               then
                  Discr := Dec2.As_Incomplete_Type_Decl.F_Discriminants;

               elsif Dec2.Kind = Ada_Protected_Type_Decl then
                  Discr := Dec2.As_Protected_Type_Decl.F_Discriminants;

               elsif Dec2.Kind = Ada_Task_Type_Decl then
                  Discr := Dec2.As_Task_Type_Decl.F_Discriminants;

               else
                  Discr := Dec2.As_Type_Decl.F_Discriminants;
               end if;

               if not Discr.Is_Null then

                  if Discr.Kind = Ada_Unknown_Discriminant_Part
                  then
                     return True;
                  end if;

                  declare
                     Discr_Specs : constant Discriminant_Spec_List :=
                       Discr.As_Known_Discriminant_Part.F_Discr_Specs;
                  begin
                     for Discr_Spec of Discr_Specs loop
                        if Discr_Spec.F_Default_Expr.Is_Null then
                           return True;
                        end if;
                     end loop;
                  end;
               end if;

               if Dec2.P_Next_Part.Is_Null then
                  Dec := Parent_Type_Declaration (Dec2);
               end if;

               Dec2 := Dec2.P_Next_Part;
            end loop;

         end loop;

         return False;

      end Check_Type_For_Unset_Discriminants;

   begin
      Unit := Bod.F_Item.As_Ada_Node;

      case Unit.Kind is
         when Ada_Package_Decl =>
            Data.Is_Generic := False;

         when Ada_Generic_Package_Decl =>
            Data.Is_Generic := True;

         when Ada_Generic_Package_Instantiation =>
            Report_Std
              ("gnattest: "
               & Base_Name (The_Unit.Unit.Get_Filename)
               & " is a library level instantiation");
            Apropriate_Source := False;
            Set_Source_Status (The_Unit.Unit.Get_Filename, Bad_Content);
            return;

         when others =>
            Report_Std
              ("gnattest: "
               & Base_Name (The_Unit.Unit.Get_Filename)
               & " is an unsupported kind of unit");
            Apropriate_Source := False;
            Set_Source_Status (The_Unit.Unit.Get_Filename, Bad_Content);
            return;
      end case;

      if Unit.As_Basic_Decl.P_Has_Aspect
        (To_Unbounded_Text (To_Text ("Remote_Call_Interface")))
      then
         Apropriate_Source := False;
         Report_Std
           ("gnattest: "
            & Base_Name (The_Unit.Unit.Get_Filename)
            & " is RCI package; skipping");
         Set_Source_Status (The_Unit.Unit.Get_Filename, Processed_In_Vain);
         return;
      end if;

      declare
         Sem_Parent : Ada_Node := Unit;
      begin
         while not Sem_Parent.Is_Null loop

            if Sem_Parent.Kind in Ada_Basic_Decl
              and then Is_Ghost_Code (Sem_Parent.As_Basic_Decl)
            then
               --  The whole UUT is Ghost
               Set_Source_Status (The_Unit.Unit.Get_Filename, Bad_Content);
               Apropriate_Source := False;

               return;
            end if;

            if not Stub_Mode_ON and then not Separate_Drivers
              and then Sem_Parent.Parent.Kind in Ada_Library_Item_Range
              and then Sem_Parent.Parent.As_Library_Item.F_Has_Private
            then
               --  Cannot incorporate test packages of private packages
               --  in monolyth mode.

               Report_Std
                 ("gnattest: "
                  & Enclosing_Unit_Name (The_Unit)
                  & " is private or child of private; skipping");

               Set_Source_Status (The_Unit.Unit.Get_Filename, Bad_Content);
               Apropriate_Source := False;

               return;
            end if;

            Sem_Parent := Sem_Parent.P_Semantic_Parent;
         end loop;
      end;

      Increase_Indent
        (Me,
         "processing " & Node_Image (Unit.As_Basic_Decl.P_Defining_Name)
         &  " (" & Base_Name (The_Unit.Unit.Get_Filename) & ")");

      Check_Unit_For_Elaboration (The_Unit);

      Data.Unit := The_Unit;
      Data.Unit_Full_Name := new String'
        (Node_Image (Unit.As_Basic_Decl.P_Defining_Name));
      Data.Unit_File_Name := new String'(The_Unit.Unit.Get_Filename);

      Trace (Me, "Gathering nested packages");
      Traverse (Unit, Get_Nested_Packages'Access);

      declare
         Test_Type : Test_Type_Info_Wrapper;
         Pack_Cur  : Package_Info_List.Cursor;

         Test_Package : constant String :=
           Data.Unit_Full_Name.all & "." &
           Test_Data_Unit_Name & "." & Test_Unit_Name;

         Nest_Dif : String_Access;
      begin
         Test_Type.TT_Info.Test_Type      := No_Ada_Node;
         Test_Type.TT_Info.Test_Type_Name := new String'("Test");

         Pack_Cur := Data.Package_Data_List.First;

         loop
            exit when Pack_Cur = Package_Info_List.No_Element;

            Nest_Dif := new String'
              (Nesting_Difference
                 (Package_Info_List.Element (Pack_Cur).Name.all,
                  Data.Unit_Full_Name.all));

            if Nest_Dif.all = "" then
               Test_Type.TT_Info.Nesting := new String'(Test_Package);

            else
               Test_Type.TT_Info.Nesting := new String'
                 (Test_Package & "." &
                    Nesting_Difference
                    (Package_Info_List.Element (Pack_Cur).Name.all,
                     Data.Unit_Full_Name.all) &
                    "." & Test_Data_Unit_Name & "." & Test_Unit_Name);
            end if;

            Free (Nest_Dif);
            Test_Type.Test_Package := new String'
              (Test_Type.TT_Info.Nesting.all);

            Suite_Data_List.Test_Types.Append (Test_Type);
            Dummy_Type_Counter := Dummy_Type_Counter + 1;

            Package_Info_List.Next (Pack_Cur);
         end loop;
      end;

      Trace (Me, "Gathering tagged records");
      Traverse (Unit, Get_Records'Access);
      Trace (Me, "Gathering subprograms");
      Traverse (Unit, Get_Subprograms'Access);

      Decrease_Indent (Me, "Traversings finished");

      if Inheritance_To_Suite and then not Stub_Mode_ON then
         Gather_Inherited_Subprograms
           (Dummy_Type_Counter, Suite_Data_List);
      end if;

      if Substitution_Suite and then not Stub_Mode_ON then
         Gather_Substitution_Data (Suite_Data_List);
      end if;

      if Data.Type_Data_List.Is_Empty and Data.Subp_List.Is_Empty then
         Apropriate_Source := False;
         Set_Source_Status (The_Unit.Unit.Get_Filename, Processed_In_Vain);
      else
         Apropriate_Source := True;
      end if;

      declare
         Cur      : Subp_Data_List.Cursor;
         Tmp_Subp : Subp_Info;
      begin
         Cur := Data.Subp_List.First;
         loop
            exit when Cur = Subp_Data_List.No_Element;

            Tmp_Subp := Subp_Data_List.Element (Cur);

            if
              Data.Subp_Name_Frequency.Element
                (To_Lower (Tmp_Subp.Subp_Text_Name.all)) > 1
            then
               Tmp_Subp.Is_Overloaded := True;
            else
               Tmp_Subp.Is_Overloaded := False;
            end if;

            Data.Subp_List.Replace_Element (Cur, Tmp_Subp);

            Subp_Data_List.Next (Cur);
         end loop;
      end;

      if Stub_Mode_ON then
         Get_Units_To_Stub (The_Unit, Data);
      end if;

   end Gather_Data;

   -----------------------
   -- Gather_Test_Cases --
   -----------------------

   procedure Gather_Test_Cases
     (Subp            :        Subp_Info;
      TR_Info         :        Test_Routine_Info_Wrapper;
      Data            : in out Data_Holder;
      Suite_Data_List : in out Suites_Data_Type;
      TC_Found        :    out Boolean;
      Instance_Sloc   :        String := "")
   is

      Me_TC : constant Trace_Handle :=
        Create ("Skeletons.Test_Cases", Default => Off);

      procedure Get_TC_Info_From_Pragma
        (TC_Pragma :     Pragma_Node;
         Name      : out String_Access;
         Mode      : out Test_Case_Mode;
         Requires  : out Expr;
         Ensures   : out Expr);
      --  Processes pragma node and sets values of corresponding parameters

      procedure Get_TC_Info_From_Aspect
        (TC_Aspect :     Aspect_Assoc;
         Name      : out String_Access;
         Mode      : out Test_Case_Mode;
         Requires  : out Expr;
         Ensures   : out Expr);
      --  Processes aspect node and sets values of corresponding parameters

      function Get_Condition_Image (Elem : Expr) return String;
      --  Returns element image as a single line removing all double spaces

      type Old_Attr_Loc is record
         El            : Ada_Node;
         Temp_Var_Name : String_Access;
         Needs_Deref   : Boolean;
      end record;
      Old_Attr_Counter : Positive := 1;

      package Source_Locations is new
        Ada.Containers.Indefinite_Vectors (Positive, Old_Attr_Loc);

      Old_Attr_Ref : Source_Locations.Vector;

      function Replace_Old_Attribute (Elem : Expr) return String;
      --  Replaces all entrances of <expr>'old in Post with
      --  Gnattest_<expr>'Old in Elem's image.

      function Replace_Result_Attribute
        (Post   : String;
         F_Name : String;
         R_Name : String)
         return String;
      --  Replaces all entrances of function'Result in Post with R_Name

      function Get_Old_Attr_Locations
        (Node : Ada_Node'Class) return Visit_Status;
      --  Gathers locations of 'Old attribute references through the given
      --  expression and populates Old_Attr_Ref.

      Subp_Add    : Subp_Info;
      TR_Info_Add : Test_Routine_Info_Wrapper;

      TC : Test_Case_Info;

      Dec : constant Basic_Decl := Subp.Subp_Declaration.As_Basic_Decl;

      Next : Ada_Node;

      Test_Cases : Ada_Nodes_List.List;

      Requiers, Ensures : Expr;
      Mode : Test_Case_Mode;
      Name : String_Access;

      GT_Prefix : constant String := "Gnattest_";

      Params_To_Temp : String_Set.Set;

      Result_Value : String_Access;

      -----------------------------
      -- Get_TC_Info_From_Pragma --
      -----------------------------

      procedure Get_TC_Info_From_Pragma
        (TC_Pragma :     Pragma_Node;
         Name      : out String_Access;
         Mode      : out Test_Case_Mode;
         Requires  : out Expr;
         Ensures   : out Expr)
      is
         Pragma_Params : constant Base_Assoc_List := F_Args (TC_Pragma);
         PP_First      : constant Positive        :=
           Pragma_Params.Base_Assoc_List_First;

         Param_Expr : Expr;
         P_Assoc    : Pragma_Argument_Assoc;
      begin
         --  Name
         Param_Expr :=
           Pragma_Params.List_Child (PP_First).As_Pragma_Argument_Assoc.F_Expr;
         if Param_Expr.Kind = Ada_String_Literal then
            Name := new String'
              (Encode
                 (Text    => Param_Expr.As_String_Literal.P_Denoted_Value,
                  Charset => Param_Expr.Unit.Get_Charset));
         else
            Name     := null;
            Mode     := Robustness;
            Requires := No_Expr;
            Ensures  := No_Expr;
            return;
         end if;

         --  Mode
         Param_Expr :=
           Pragma_Params.List_Child
             (PP_First + 1).As_Pragma_Argument_Assoc.F_Expr;
         if To_Lower (Node_Image (Param_Expr)) = "nominal" then
            Mode := Normal;
         else
            Mode := Robustness;
         end if;

         if Pragma_Params.List_Child (PP_First + 2).Is_Null then
            Requires := No_Expr;
            Ensures  := No_Expr;
            return;
         end if;

         --  Requires and Ensures
         P_Assoc := Pragma_Params.List_Child
           (PP_First + 2).As_Pragma_Argument_Assoc;

         if To_Lower (Node_Image (P_Assoc.F_Expr)) = "requires" then
            Requires := P_Assoc.F_Expr;
         else
            Requires := No_Expr;
            Ensures  := P_Assoc.F_Expr;
            return;
         end if;

         if Pragma_Params.List_Child (PP_First + 3).Is_Null then
            Ensures := No_Expr;
         else
            Ensures :=
              Pragma_Params.List_Child
                (PP_First + 3).As_Pragma_Argument_Assoc.F_Expr;
         end if;
      end Get_TC_Info_From_Pragma;

      -----------------------------
      -- Get_TC_Info_From_Aspect --
      -----------------------------

      procedure Get_TC_Info_From_Aspect
        (TC_Aspect :     Aspect_Assoc;
         Name      : out String_Access;
         Mode      : out Test_Case_Mode;
         Requires  : out Expr;
         Ensures   : out Expr)
      is
         Aspect_Params : constant Basic_Assoc_List :=
           TC_Aspect.F_Expr.As_Aggregate.F_Assocs.As_Basic_Assoc_List;
         AP_First      : constant Positive   :=
           Aspect_Params.Basic_Assoc_List_First;

         Param_Expr : Expr;
         A_Assoc    : Aggregate_Assoc;
      begin
         --  Name
         Param_Expr :=
           Aspect_Params.List_Child (AP_First).As_Aggregate_Assoc.F_R_Expr;
         if Param_Expr.Kind = Ada_String_Literal then
            Name := new String'
              (Encode
                 (Text    => Param_Expr.As_String_Literal.P_Denoted_Value,
                  Charset => Param_Expr.Unit.Get_Charset));
         else
            Name     := null;
            Mode     := Robustness;
            Requires := No_Expr;
            Ensures  := No_Expr;
            return;
         end if;

         --  Mode
         Param_Expr :=
           Aspect_Params.List_Child
             (AP_First + 1).As_Aggregate_Assoc.F_R_Expr;
         if To_Lower (Node_Image (Param_Expr)) = "nominal" then
            Mode := Normal;
         else
            Mode := Robustness;
         end if;

         if Aspect_Params.List_Child (AP_First + 2).Is_Null then
            Requires := No_Expr;
            Ensures  := No_Expr;
            return;
         end if;

         --  Requires and Ensures
         A_Assoc := Aspect_Params.List_Child
           (AP_First + 2).As_Aggregate_Assoc;

         declare
            Des : constant Ada_Node_List :=
              A_Assoc.F_Designators.As_Ada_Node_List;
         begin

            if To_Lower (Node_Image (Des.Ada_Node_List_Element
                         (Des.Ada_Node_List_First))) = "requires"
            then
               Requires := A_Assoc.F_R_Expr;
            else
               Requires := No_Expr;
               Ensures  := A_Assoc.F_R_Expr;
               return;
            end if;
         end;

         if Aspect_Params.List_Child (AP_First + 3).Is_Null then
            Ensures := No_Expr;
         else
            Ensures :=
              Aspect_Params.List_Child
                (AP_First + 3).As_Aggregate_Assoc.F_R_Expr;
         end if;
      end Get_TC_Info_From_Aspect;

      ---------------------------
      -- Replace_Old_Attribute --
      ---------------------------

      function Replace_Old_Attribute (Elem : Expr) return String
      is
         Unprocessed_Start : Token_Reference;
         Expression_End    : Token_Reference;

         Result : Unbounded_String;
      begin
         Trace (Me_TC, "Replace_Old_Attribute");
         if Verbose then
            Trace (Me_TC, "called for: " & Image (Elem));
         end if;

         if Elem.Is_Null then
            return "";
         end if;

         Traverse (Elem, Get_Old_Attr_Locations'Access);

         if Old_Attr_Ref.Is_Empty then
            return Node_Image (Elem);
         end if;

         --  ??? While there is no proper name resolution for Identifiers from
         --  Test_Case expressions that come from pragma Test_Case, it is not
         --  possible to properly handle 'Old.
         --  For now replace the whole expression with True.
         for Par of Parents (Elem) loop
            if Par.Kind = Ada_Pragma_Node then
               Old_Attr_Ref.Clear;
               return "True";
            end if;
         end loop;

         Unprocessed_Start := Elem.Token_Start;
         Expression_End    := Elem.Token_End;

         for Attr_Ref of Old_Attr_Ref loop
            Append
              (Result,
               Encode
                 (Text
                      (Unprocessed_Start, Previous (Attr_Ref.El.Token_Start)),
                  Elem.Unit.Get_Charset));

            if Attr_Ref.Needs_Deref then
               Append (Result, Attr_Ref.Temp_Var_Name.all & ".all");
            else
               Append (Result, Attr_Ref.Temp_Var_Name.all);
            end if;
            Free (Attr_Ref.Temp_Var_Name);

            Unprocessed_Start :=
              Libadalang.Common.Next (Attr_Ref.El.Token_End);
         end loop;

         Append
           (Result,
            Encode
              (Text (Unprocessed_Start, Expression_End),
               Elem.Unit.Get_Charset));

         Old_Attr_Ref.Clear;
         return To_String (Result);

      end Replace_Old_Attribute;

      ----------------------------
      -- Get_Old_Attr_Locations --
      ----------------------------

      function Get_Old_Attr_Locations
        (Node : Ada_Node'Class) return Visit_Status
      is
         Loc      : Old_Attr_Loc;
         Nm       : Libadalang.Analysis.Name;
         Id       : Identifier;
         Dec      : Basic_Decl;
         Type_Dec : Basic_Decl;
         Def      : Type_Def;
         Res, Obj : Type_Expr;

      begin
         if Node.Kind /= Ada_Attribute_Ref
           or else To_Lower
             (Node_Image (Node.As_Attribute_Ref.F_Attribute)) /= "old"
         then
            return Into;
         end if;

         Nm := Node.As_Attribute_Ref.F_Prefix;
         Id := Nm.P_Relative_Name.As_Identifier;

         Trace (Me_TC, "Resolving name " & Id.Image);

         --  ??? While there is no proper name resolution for Identifiers from
         --  Test_Case expressions that come from pragma Test_Case, it is not
         --  possible to properly handle 'Old.
         --  No need to continue processing the expression since it will be
         --  replaced with "True" in Replace_Old_Attribute, just add one
         --  dummy Loc.

         for Par of Parents (Node) loop
            if Par.Kind = Ada_Pragma_Node then
               Loc.El := No_Ada_Node;
               Loc.Temp_Var_Name := null;
               Old_Attr_Ref.Append (Loc);
               return Stop;
            end if;
         end loop;

         Dec := Id.P_Referenced_Decl;

         --  Constructing temp variable assignments
         if Nm.Kind = Ada_Explicit_Deref then
            Nm := Nm.As_Explicit_Deref.F_Prefix;
            Loc.Needs_Deref := True;
         else
            Loc.Needs_Deref := False;
         end if;

         case Dec.Kind is
            when Ada_Subp_Decl =>

               Loc.Temp_Var_Name := new String'
                 (GT_Prefix
                  & Trim (Positive'Image (Old_Attr_Counter), Both)
                  & "_"
                  & Get_Subp_Name (Dec));
               Res := Dec.As_Subp_Decl.F_Subp_Spec.P_Returns;

               if Res.Kind = Ada_Anonymous_Type then
                  Def := F_Type_Def
                    (Res.As_Anonymous_Type.F_Type_Decl.As_Type_Decl);
                  if Def.Kind = Ada_Type_Access_Def then
                     Type_Dec := Def.As_Type_Access_Def.F_Subtype_Indication.
                       F_Name.P_Relative_Name.P_Referenced_Decl;
                     Params_To_Temp.Include
                       (Loc.Temp_Var_Name.all
                        & " : constant access "
                        & Encode
                          (Type_Dec.P_Fully_Qualified_Name,
                           Type_Dec.Unit.Get_Charset)
                        & " := "
                        & Node_Image (Nm)
                        & ";");
                  else
                     Params_To_Temp.Include
                       (Loc.Temp_Var_Name.all
                        & " : constant "
                        & Node_Image (Dec.As_Subp_Decl.F_Subp_Spec.P_Returns)
                        & " := "
                        & Node_Image (Nm)
                        & ";");
                  end if;
               else
                  Type_Dec := Res.As_Subtype_Indication.F_Name.
                    P_Relative_Name.P_Referenced_Decl;
                  Params_To_Temp.Include
                    (Loc.Temp_Var_Name.all
                     & " : constant "
                     & Encode
                       (Type_Dec.P_Fully_Qualified_Name,
                        Type_Dec.Unit.Get_Charset)
                     & " := "
                     & Node_Image (Nm)
                     & ";");
               end if;

            when Ada_Param_Spec =>

               Loc.Temp_Var_Name := new String'
                 (GT_Prefix
                  & Trim (Positive'Image (Old_Attr_Counter), Both)
                  & "_"
                  & Node_Image (Id));
               Params_To_Temp.Include
                 (Loc.Temp_Var_Name.all
                  & " : constant "
                  & Node_Image (Dec.As_Param_Spec.F_Type_Expr)
                  & " := "
                  & Node_Image (Nm)
                  & ";");

            when Ada_Object_Decl =>

               Loc.Temp_Var_Name := new String'
                 (GT_Prefix
                  & Trim (Positive'Image (Old_Attr_Counter), Both)
                  & "_"
                  & Node_Image (Id));

               Obj := Dec.As_Object_Decl.F_Type_Expr;

               if Obj.Kind = Ada_Anonymous_Type then
                  Def := F_Type_Def
                    (Obj.As_Anonymous_Type.F_Type_Decl.As_Type_Decl);
                  if Def.Kind = Ada_Type_Access_Def then
                     Type_Dec := Def.As_Type_Access_Def.F_Subtype_Indication.
                       F_Name.P_Relative_Name.P_Referenced_Decl;
                     Params_To_Temp.Include
                       (Loc.Temp_Var_Name.all
                        & " : constant access "
                        & Encode
                          (Type_Dec.P_Fully_Qualified_Name,
                           Type_Dec.Unit.Get_Charset)
                        & " := "
                        & Node_Image (Nm)
                        & ";");
                  else
                     Params_To_Temp.Include
                       (Loc.Temp_Var_Name.all
                        & " : constant "
                        & Node_Image (Dec.As_Subp_Decl.F_Subp_Spec.P_Returns)
                        & " := "
                        & Node_Image (Nm)
                        & ";");
                  end if;
               else
                  Type_Dec := Obj.As_Subtype_Indication.F_Name.
                    P_Relative_Name.P_Referenced_Decl;
                  Params_To_Temp.Include
                    (Loc.Temp_Var_Name.all
                     & " : constant "
                     & Encode
                       (Type_Dec.P_Fully_Qualified_Name,
                        Type_Dec.Unit.Get_Charset)
                     & " := "
                     & Node_Image (Nm)
                     & ";");
               end if;

            when others =>
               null;
         end case;

         Loc.El := Node.As_Ada_Node;
         Old_Attr_Counter := Old_Attr_Counter + 1;
         Old_Attr_Ref.Append (Loc);

         return Over;
      end Get_Old_Attr_Locations;

      ------------------------------
      -- Replace_Result_Attribute --
      ------------------------------

      function Replace_Result_Attribute
        (Post   : String;
         F_Name : String;
         R_Name : String)
         return String
      is
         Res : String_Access := new String'("");
         Tmp : String_Access;

         Quote : Boolean := False;

         Subp_Is_Operator : Boolean := False;
         Trying_Quote     : Boolean := False;

         F_Name_Length : constant Integer := F_Name'Length + 7;
         Idx           :          Integer := Post'First;

         function Eq (L, R : String) return Boolean is
           (To_Lower (L) = To_Lower (R));
         --  Case insensitive comparision
      begin
         if F_Name (F_Name'First) = '"' then
            Subp_Is_Operator := True;
         end if;

         for I in Post'Range loop
            if Post (I) = '"' then
               if Quote then
                  if I = Post'Last or else Post (I + 1) /= '"' then
                     Quote := False;
                  end if;

               else
                  Quote := True;
                  if Subp_Is_Operator then
                     Trying_Quote := True;
                  end if;
               end if;
            end if;

            if not Quote or else Trying_Quote then
               Trying_Quote := False;

               if Post'Last >= I + F_Name_Length - 1 then
                  if Eq (Post (I .. I + F_Name_Length - 1), F_Name & "'Result")
                  then
                     Tmp := new String'
                       (Res.all
                        & Post (Idx .. I - 1)
                        & R_Name);
                     Free (Res);
                     Res := new String'(Tmp.all);
                     Free (Tmp);
                     Idx := I + F_Name_Length;
                  end if;
               end if;

               if Post'Last >= I + F_Name_Length then
                  if Eq (Post (I .. I + F_Name_Length), F_Name & "' Result")
                    or else Eq (Post (I .. I + F_Name_Length),
                                F_Name & " 'Result")
                  then
                     Tmp := new String'
                       (Res.all
                        & Post (Idx .. I - 1)
                        & R_Name);
                     Free (Res);
                     Res := new String'(Tmp.all);
                     Free (Tmp);
                     Idx := I + F_Name_Length + 1;
                  end if;

               end if;

               if Post'Last >= I + F_Name_Length + 1 then
                  if Eq (Post (I .. I + F_Name_Length + 1),
                         F_Name & " ' Result")
                  then
                     Tmp := new String'
                       (Res.all
                        & Post (Idx .. I - 1)
                        & R_Name);
                     Free (Res);
                     Res := new String'(Tmp.all);
                     Free (Tmp);
                     Idx := I + F_Name_Length + 2;
                  end if;

               end if;

               if Post'Last = I then
                  Tmp := new String'(Res.all & Post (Idx .. I));
                  Free (Res);
                  Res := new String'(Tmp.all);
                  Free (Tmp);
               end if;
            end if;
         end loop;

         return Res.all;
      end Replace_Result_Attribute;

      -------------------------
      -- Get_Condition_Image --
      -------------------------

      function Get_Condition_Image (Elem : Expr) return String
      is

         Res, Tmp, Packed : String_Access;

         Idx   : Integer;
         Space : Boolean;

      begin
         Res := new String'(Replace_Old_Attribute (Elem));

         Tmp := new String'(Trim (Res.all, Both));
         Free (Res);
         Res := new String'(Tmp.all);
         Free (Tmp);

         Space := False;
         Packed := new String'("");
         Idx := Res'First;

         for I in Res'Range loop
            if Res (I) in ' ' | ASCII.CR | ASCII.LF then
               if not Space then
                  Space := True;
                  Tmp := new String'(Packed.all & " " & Res (Idx .. I - 1));
                  Free (Packed);
                  Packed := new String'(Tmp.all);
                  Free (Tmp);
               end if;

            else
               if Space then
                  Idx   := I;
                  Space := False;
               end if;
            end if;

            if I = Res'Last then
               Tmp := new String'(Packed.all & " " & Res (Idx .. I));
               Free (Packed);
               Packed := new String'(Tmp.all);
               Free (Tmp);
            end if;
         end loop;

         return Trim (Packed.all, Both);
      end Get_Condition_Image;

   begin
      Increase_Indent
        (Me_TC, "Looking for test cases of " & Subp.Subp_Text_Name.all);

      TC_Found := False;

      Next := Dec.Next_Sibling;
      while not Next.Is_Null and then Next.Kind = Ada_Pragma_Node loop
         declare
            Pragma_Name : constant String :=
              To_Lower (Node_Image (F_Id (Next.As_Pragma_Node)));
         begin
            if Pragma_Name = "test_case" then

               Get_TC_Info_From_Pragma
                 (Next.As_Pragma_Node,
                  Name, Mode, Requiers, Ensures);

               if Name = null then
                  Report_Std
                    ("warning: (gnattest) "
                     & Base_Name (Next.Unit.Get_Filename)
                     & ":"
                     & Trim (First_Line_Number (Next)'Img, Both)
                     & ":"
                     & Trim (First_Column_Number (Next)'Img, Both)
                     & ": Test_Case has complex name; skipping");
               else
                  Free (Name);
                  Test_Cases.Append (Next);
               end if;

            elsif Pragma_Name = "pre" then

               TC.Pre := List_Child
                 (Next.As_Pragma_Node.F_Args,
                  Next.As_Pragma_Node.F_Args.Base_Assoc_List_First)
                 .As_Pragma_Argument_Assoc.F_Expr;

            elsif Pragma_Name = "post" then

               TC.Post := List_Child
                 (Next.As_Pragma_Node.F_Args,
                  Next.As_Pragma_Node.F_Args.Base_Assoc_List_First)
                 .As_Pragma_Argument_Assoc.F_Expr;

            end if;
         end;

         Next := Next.Next_Sibling;
      end loop;

      if not Dec.F_Aspects.Is_Null then
         for Assoc of Dec.F_Aspects.F_Aspect_Assocs loop

            declare
               Aspect_Name : constant String :=
                 To_Lower (Node_Image (Assoc.F_Id));
            begin
               if Aspect_Name = "test_case" then

                  Get_TC_Info_From_Aspect
                    (Assoc.As_Aspect_Assoc,
                     Name, Mode, Requiers, Ensures);

                  if Name = null then
                     Report_Std
                       ("warning: (gnattest) "
                        & Base_Name (Assoc.Unit.Get_Filename)
                        & ":"
                        & Trim (First_Line_Number (Assoc)'Img, Both)
                        & ":"
                        & Trim (First_Column_Number (Assoc)'Img, Both)
                        & ": Test_Case has complex name; skipping");
                  else
                     Free (Name);
                     Test_Cases.Append (Assoc.As_Ada_Node);
                  end if;

               elsif Aspect_Name = "pre" then
                  TC.Pre := Assoc.F_Expr;
               elsif Aspect_Name = "post" then
                  TC.Post := Assoc.F_Expr;
               end if;
            end;

         end loop;
      end if;

      if Test_Cases.Is_Empty then
         if not Test_Case_Only then
            Data.Subp_List.Append (Subp);
            Suite_Data_List.TR_List.Append (TR_Info);
         end if;
         Decrease_Indent
           (Me_TC, "No test case found for " & Subp.Subp_Text_Name.all);
         return;
      end if;

      --  At this point we are pretty sure that at least one Test_Case exists.
      TC_Found := True;
      Common.Has_Test_Cases := True;

      for Test_Case of Test_Cases loop
         Subp_Add.Has_TC_Info := True;
         TC.Elem := Test_Case;

         Subp_Add.Subp_Declaration := Subp.Subp_Declaration;
         Subp_Add.Corresp_Type     := Subp.Corresp_Type;
         Subp_Add.Nesting          := new String'(Subp.Nesting.all);
         Subp_Add.Subp_Text_Name   := new String'(Subp.Subp_Text_Name.all);
         Subp_Add.Subp_Name_Image  := new String'(Subp.Subp_Name_Image.all);
         Subp_Add.Subp_Full_Hash   := new String'(Subp.Subp_Full_Hash.all);
         Subp_Add.Subp_Hash_V1     := new String'(Subp.Subp_Hash_V1.all);
         Subp_Add.Subp_Hash_V2_1   := new String'(Subp.Subp_Hash_V2_1.all);

         TC.Req := No_Expr;
         TC.Ens := No_Expr;

         if Test_Case.Kind = Ada_Pragma_Node then
            Get_TC_Info_From_Pragma
              (Test_Case.As_Pragma_Node,
               TC.Name, TC.Mode, TC.Req, TC.Ens);
         else
            Get_TC_Info_From_Aspect
              (Test_Case.As_Aspect_Assoc,
               TC.Name, TC.Mode, TC.Req, TC.Ens);
         end if;

         --  Creating second part of hash code for test routine name
         if TC.Mode = Normal then
            declare
               Full_Hash : constant String :=
                 GNAT.SHA1.Digest
                   (TC.Name.all
                    & "#"
                    & Get_Condition_Image (TC.Pre)
                    & "#"
                    & Get_Condition_Image (TC.Post)
                    & "#"
                    & Get_Condition_Image (TC.Req)
                    & "#"
                    & Get_Condition_Image (TC.Ens));
            begin
               TC.TC_Hash := new String'
                 (Full_Hash (Full_Hash'First .. Full_Hash'First + 15));
            end;
         else
            declare
               Full_Hash : constant String :=
                 GNAT.SHA1.Digest
                 (TC.Name.all
                  & "#"
                  & Get_Condition_Image (TC.Req)
                  & "#"
                  & Get_Condition_Image (TC.Ens));
            begin
               TC.TC_Hash := new String'
                 (Full_Hash (Full_Hash'First .. Full_Hash'First + 15));
            end;
         end if;

         if Is_Function (Subp.Subp_Declaration.As_Basic_Decl) then
            Result_Value := new String'
              (Subp.Subp_Mangle_Name.all
               & "_"
               & TC.TC_Hash (TC.TC_Hash'First .. TC.TC_Hash'First + 5)
               & "_Result");
         end if;

         Subp_Add.Subp_Mangle_Name := new String'
           (Subp.Subp_Mangle_Name.all
            & "_"
            & TC.TC_Hash (TC.TC_Hash'First .. TC.TC_Hash'First + 5));

         Params_To_Temp.Clear;
         Old_Attr_Counter := 1;

         if TC.Mode = Normal then

            --  Requires and Pre
            if TC.Req.Is_Null then
               if TC.Pre.Is_Null then
                  TC.Req_Image := new String'("");
               else
                  TC.Req_Image := new String'(Get_Condition_Image (TC.Pre));
               end if;
            else
               if TC.Pre.Is_Null then
                  TC.Req_Image := new String'(Get_Condition_Image (TC.Req));
               else
                  TC.Req_Image := new String'
                    ("("
                     & Get_Condition_Image (TC.Pre)
                     & ") and ("
                     & Get_Condition_Image (TC.Req)
                     & ")");
               end if;
            end if;

            --  Ensures and Post
            if TC.Ens.Is_Null then
               if TC.Post.Is_Null then
                  TC.Ens_Image := new String'("");
               else
                  TC.Ens_Image := new String'(Get_Condition_Image (TC.Post));
               end if;
            else
               if TC.Post.Is_Null then
                  if Result_Value = null then
                     TC.Ens_Image := new String'(Get_Condition_Image (TC.Ens));
                  else
                     TC.Ens_Image := new String'
                       (Replace_Result_Attribute
                          (Get_Condition_Image (TC.Ens),
                           Subp.Subp_Name_Image.all,
                           Result_Value.all));
                  end if;
               else
                  if Result_Value = null then
                     TC.Ens_Image := new String'
                       ("("
                        & Get_Condition_Image (TC.Post)
                        & ") and ("
                        & Get_Condition_Image (TC.Ens)
                        & ")");
                  else
                     TC.Ens_Image := new String'
                       (Replace_Result_Attribute
                          ("("
                           & Get_Condition_Image (TC.Post)
                           & ") and ("
                           & Get_Condition_Image (TC.Ens)
                           & ")",
                           Subp.Subp_Name_Image.all,
                           Result_Value.all));
                  end if;
               end if;
            end if;

         else

            --  Requires
            if TC.Req.Is_Null then
               TC.Req_Image := new String'("");
            else
               TC.Req_Image := new String'(Get_Condition_Image (TC.Req));
            end if;

            --  Ensures
            if TC.Ens.Is_Null then
               TC.Ens_Image := new String'("");
            else
               if Result_Value = null then
                  TC.Ens_Image := new String'(Get_Condition_Image (TC.Ens));
               else
                  TC.Ens_Image := new String'
                    (Replace_Result_Attribute
                       (Get_Condition_Image (TC.Ens),
                        Subp.Subp_Name_Image.all,
                        Result_Value.all));
               end if;
            end if;

         end if;

         Free (Result_Value);

         TC.Params_To_Temp := Params_To_Temp;
         Params_To_Temp.Clear;

         --  adding requiers and ensures slocs
         TC.Req_Line := new String'
           (Base_Name (Data.Unit_File_Name.all)
            & ":"
            & (if TC.Req.Is_Null then "0" else
                   Trim (First_Line_Number (TC.Req)'Img, Both))
            & ":");
         TC.Ens_Line := new String'
           (Base_Name (Data.Unit_File_Name.all)
            & ":"
            & (if TC.Ens.Is_Null then "0" else
                   Trim (First_Line_Number (TC.Ens)'Img, Both))
            & ":");

         Subp_Add.TC_Info := TC;

         Data.Subp_List.Append (Subp_Add);

         TR_Info_Add := TR_Info;
         TR_Info_Add.TR_Info.TR_Text_Name := new String'
           (Subp_Add.Subp_Mangle_Name.all);

         --  Changing tested sloc so it corresponds to test case instead
         --  of tested subprogram

         if Instance_Sloc = "" then
            TR_Info_Add.TR_Info.Tested_Sloc := new String'
              (Base_Name (Data.Unit_File_Name.all)
               & ":"
               & Trim (First_Line_Number (TC.Elem)'Img, Both)
               & ":"
               & Trim (First_Column_Number (TC.Elem)'Img, Both)
               & ":");
         else
            TR_Info_Add.TR_Info.Tested_Sloc := new String'
              (Base_Name (Subp.Subp_Declaration.Unit.Get_Filename)
               & ":"
               & Trim (First_Line_Number (TC.Elem)'Img, Both)
               & ":"
               & Trim (First_Column_Number (TC.Elem)'Img, Both)
               & " instance at "
               & Instance_Sloc);
         end if;

         Suite_Data_List.TR_List.Append (TR_Info_Add);
      end loop;

      Decrease_Indent (Me_TC, "done");

   end Gather_Test_Cases;

   -------------------------------
   -- Generate_Nested_Hierarchy --
   -------------------------------

   procedure Generate_Nested_Hierarchy (Data : Data_Holder)
   is
      use GNAT.OS_Lib;
      Cur : Package_Info_List.Cursor := Data.Package_Data_List.First;
      Output_Dir  : constant String :=
        Get_Source_Output_Dir (Data.Unit_File_Name.all);
   begin
      loop
         exit when Cur = Package_Info_List.No_Element;

         declare
            S           : constant String :=
              Package_Info_List.Element (Cur).Name.all;
            S_Pack : constant String :=
              Data.Unit_Full_Name.all & "." &
              Test_Data_Unit_Name & "." &
              Test_Unit_Name & "." &
              Nesting_Difference (Data.Unit_Full_Name.all, S);
         begin
            if
              Data.Unit_Full_Name.all /= S
            then
               Create
                 (Output_Dir & Directory_Separator &
                  Unit_To_File_Name (S_Pack) & ".ads");

               S_Put (0, "package " & S_Pack & " is");
               Put_New_Line;
               S_Put (0, "end " & S_Pack & ";");
               Put_New_Line;

               Close_File;
            end if;
         end;

         Package_Info_List.Next (Cur);
      end loop;

      if not Data.Has_Simple_Case then
         Create
           (Output_Dir & Directory_Separator &
            Unit_To_File_Name
              (Data.Unit_Full_Name.all & "." &
               Test_Data_Unit_Name & "." &
               Test_Unit_Name) &
            ".ads");

         S_Put
           (0,
            "package " & Data.Unit_Full_Name.all &
            "." & Test_Data_Unit_Name & "." & Test_Unit_Name & " is");
         Put_New_Line;
         S_Put
           (0,
            "end " & Data.Unit_Full_Name.all &
            "." & Test_Data_Unit_Name & "." & Test_Unit_Name  & ";");
         Put_New_Line;

         Close_File;

         Excluded_Test_Package_Bodies.Include
           (Unit_To_File_Name
              (Data.Unit_Full_Name.all & "."
               & Test_Data_Unit_Name & "."
               & Test_Unit_Name)
            & ".adb");

         Create
           (Output_Dir & Directory_Separator &
            Unit_To_File_Name
              (Data.Unit_Full_Name.all & "." &
               Test_Data_Unit_Name) &
            ".ads");

         S_Put
           (0,
            "package " & Data.Unit_Full_Name.all &
            "." & Test_Data_Unit_Name & " is");
         Put_New_Line;
         S_Put
           (0,
            "end " & Data.Unit_Full_Name.all &
            "." & Test_Data_Unit_Name  & ";");
         Put_New_Line;

         Close_File;

         Excluded_Test_Package_Bodies.Include
           (Unit_To_File_Name
              (Data.Unit_Full_Name.all & "."
               & Test_Data_Unit_Name)
            & ".adb");
      end if;

   end Generate_Nested_Hierarchy;

   -----------------------------
   --  Generate_Test_Package  --
   -----------------------------

   procedure Generate_Test_Package (Data : Data_Holder) is

      Output_Dir             : constant String :=
        Get_Source_Output_Dir (Data.Unit_File_Name.all);

      Tmp_File_Name      : constant String :=
        Ada.Directories.Compose
          (Utils.Environment.Tool_Temp_Dir.all, "gnattest_tmp_test_package");

      Test_File_Name : String_Access;
      Data_Unit_Name : String_Access;
      Unit_Name      : String_Access;
      Unit_Pref      : String_Access;

      Setters_Set : String_Set.Set;
      Set_Cur     : String_Set.Cursor;

      Subp_Cur     : Subp_Data_List.Cursor;
      Pack_Cur     : Package_Info_List.Cursor;

      Current_Type : Base_Type_Info;
      --  The test type for which the primitives are
      --  put togather in the corresponding test package

      Test_Unit_Suffix : String_Access;
      --  Generic or non-generic test package suffix or.

      Actual_Test : Boolean;
      --  Indicates if current test package has at least one non-abstract test
      --  routine. In that case we need to include AUnit.Assertions.

      Gen_Tests : Generic_Tests;
      --  Used to store all test type names in case of generic tested package.
      --  They are to be added at generic test storage.

      Nesting_Add : String_Access;

      UH     : Unique_Hash;
      MD     : Markered_Data;
      MD_Cur : Markered_Data_Maps.Cursor;

      Subp_List : Subp_Data_List.List;
      Current_Subp : Subp_Info;
      Current_Pack : Package_Info;

      TP_Map  : TP_Mapping;
      TP_List : TP_Mapping_List.List;

      Tear_Down_Line_Add : Natural := 0;

      Short_Names_Used : String_Set.Set;

      package Elements_Set is new
        Ada.Containers.Indefinite_Hashed_Sets (Ada_Node, Hash, "=", "=");
      use Elements_Set;

      Shortnamed_Subps : Elements_Set.Set;

      --  overlodaing number counting
      Name_Numbers : Name_Frequency.Map;
      package Elem_Number_Maps is new
        Ada.Containers.Indefinite_Hashed_Maps (Ada_Node, Natural, Hash, "=");
      use Elem_Number_Maps;
      Elem_Numbers : Elem_Number_Maps.Map;

      Test_Data_Package_Name : String_Access;

      --  temporary storage for slocs of test routines
      type TR_SLOC_Buffer_Type is record
         TPtarg  : String_Access;
         Test_F  : String_Access;
         Test_T  : String_Access;
         Subp    : Subp_Info;
         TR_Line : Natural := 1;
      end record;

      package TR_SLOC_Buffer_Lists is new
        Ada.Containers.Doubly_Linked_Lists (TR_SLOC_Buffer_Type);
      use TR_SLOC_Buffer_Lists;

      TR_SLOC_Buffer : TR_SLOC_Buffer_Lists.List;

      procedure Add_Buffered_TR_Slocs
        (TP_List     : in out TP_Mapping_List.List;
         Common_Time : String);
      --  Pushes buffered test routine slocs into main mapping container.

      function Is_Unimplemented_Test
        (TR_Text : String_Vectors.Vector) return Boolean;
      --  Searches for specific text pattern which indicates that given test
      --  skeleton was not modified by user after generation.

      procedure Put_Test_Data_Header;

      procedure Put_TP_Header (TD_Package_Name : String);

      type Persistent_Section_Type is
        (With_Clauses,      -- /00/
         Body_Declarations, -- /01/
         Body_Statements);  -- /02/

      function Unparse_Test_Vectors
        (Unit_Data : Data_Holder; Subp : Subp_Info) return Boolean;
      --  Lookup test vectors in JSON format and try to unparse them in the
      --  test wrappers. Returns false if there is no test vectors to unparse.

      procedure Put_Persistent_Section (PS_Type : Persistent_Section_Type);
      --  Puts persistent section of given kind surrounded with read-only
      --  markers and corresponding specific Id.

      function Markered_Data_Map_Is_Empty return Boolean;
      --  Check if Markered_Data_Map is empty or the only element present is
      --  actually the Body_Statements persistent block.

      procedure Put_Stub_Data_Import;
      --  Put with and use clauses for Stub_Data packages of units stubbed
      --  for current UUT.

      function Unparse_Test_Vectors
        (Unit_Data : Data_Holder;
         Subp      : Subp_Info)
         return Boolean
      is
         pragma Unreferenced (Unit_Data);
         JSON_Unit_File : constant Virtual_File := GNATCOLL.VFS.Create
           (+(Test.Common.JSON_Test_Dir.all & Data.Unit_Full_Name.all
            & ".json"));

         Unit_File_Content : GNAT.Strings.String_Access;

         Unit_Content : JSON_Array;
         Subp_Content : JSON_Value := JSON_Null;
         Subp_Vectors : JSON_Array;
         Single_Vec   : JSON_Array;
         Test_Count   : Positive := 1;
         Is_Function  : Boolean;
         Generation_Complete : Boolean := False;

         procedure Pp_Subp_Call (Initial_Pad : Natural := 0);

         ------------------
         -- Pp_Subp_Call --
         ------------------

         procedure Pp_Subp_Call (Initial_Pad : Natural := 0) is
         begin
            S_Put
              (Initial_Pad, Subp_Content.Get ("fully_qualified_name") & " (");
            for Param_Id in Single_Vec loop
               S_Put (0, Array_Element (Single_Vec, Param_Id).Get ("name"));
               if Array_Has_Element (Single_Vec, Param_Id + 1) then
                  S_Put (0, ", ");
               end if;
            end loop;
            S_Put (0, ");");
         end Pp_Subp_Call;

      begin

         if not Is_Regular_File (JSON_Unit_File)
           and then not Is_Readable (JSON_Unit_File)
         then
            return False;
         end if;

         Unit_File_Content := GNATCOLL.VFS.Read_File (JSON_Unit_File);

         if Unit_File_Content in null then
            return False;
         end if;

         Unit_Content := Read
           (Unit_File_Content.all, +JSON_Unit_File.Full_Name).Get;

         if Unit_Content = Empty_Array then
            return False;
         end if;
         --  Look for the correct subprogram in the unit JSON value using the
         --  full hash of the subprogram. If not found, return false.
         for Val of Unit_Content loop
            if String'(Val.Get ("UID")) = Subp.Subp_Full_Hash.all then
               Subp_Content := Val;
               exit;
            end if;
         end loop;

         if Subp_Content = JSON_Null
           or else Subp_Content.Get ("values") = Empty_Array
         then
            return False;
         end if;

         Generation_Complete := Subp_Content.Get ("generation_complete");

         Is_Function := Subp_Content.Has_Field ("return_type");

         declare
            Com : constant String :=
              (if Generation_Complete then "" else "--  ");
         begin

            --  unparse each test vector

            S_Put (6, "--  Autogenerated test vectors");
            New_Line_Count;
            New_Line_Count;

            Subp_Vectors := Subp_Content.Get ("values");

            for Test_Vec of Subp_Vectors loop
               Single_Vec := Test_Vec.Get;
               S_Put (6, Com);
               S_Put (0, "Test_" & Trim (Test_Count'Image, Both) & ":");
               New_Line_Count;
               S_Put (6, Com);
               S_Put (0, "declare");
               New_Line_Count;
               for Param of Single_Vec loop
                  S_Put (6, Com);
                  S_Put (3, Param.Get ("name") & " : "
                            & Param.Get ("type_name")
                            & (if Param.Get ("mode") in 0 | 1
                               then " := " & Param.Get ("value") & ";"
                               else ";"));
                  New_Line_Count;
               end loop;
               S_Put (6, Com);
               S_Put (0, "begin");
               New_Line_Count;
               if Is_Function then
                  S_Put (6, Com);
                  S_Put (3, "declare");
                  New_Line_Count;
                  S_Put (6, Com);
                  S_Put (6, Com & "Ret_Val : "
                            & Subp_Content.Get ("return_type") & ":=");
                  New_Line_Count;
                  S_Put (6, Com);
                  Pp_Subp_Call (8);
                  New_Line_Count;
                  S_Put (6, Com);
                  S_Put (3, "begin");
                  New_Line_Count;
                  S_Put (6, Com);
                  S_Put
                    (6, "--  Insert function call result validation here");
                  New_Line_Count;
                  S_Put (6, Com);
                  S_Put (6, "null;");
                  New_Line_Count;
                  S_Put (6, Com);
                  S_Put (3, "end;");
               else
                  S_Put (6, Com);
                  Pp_Subp_Call (3);
               end if;
               New_Line_Count;
               S_Put (6, Com);
               S_Put (0, "exception");
               New_Line_Count;
               S_Put (6, Com);
               S_Put (3, "when Exc : others =>");
               New_Line_Count;
               S_Put (6, Com);
               S_Put (6, "AUnit.Assertions.Assert");
               New_Line_Count;
               S_Put (6, Com);
               S_Put (8, "(False,");
               New_Line_Count;
               S_Put (6, Com);
               S_Put (9, """Test_" & Trim (Test_Count'Image, Both)
                          & " crashed: "" & Ada.Exceptions.Exception_"
                          & "Information (Exc));");
               New_Line_Count;
               S_Put (6, Com);
               S_Put (0, "end Test_" & Trim (Test_Count'Image, Both)
                         & ";");
               New_Line_Count;
               New_Line_Count;
               Test_Count := Test_Count + 1;
            end loop;
         end;

         if not Generation_Complete then
            S_Put (6, "AUnit.Assertions.Assert");
            New_Line_Count;
            S_Put (9, "(False,");
            New_Line_Count;
            S_Put (9, """Missing test values for " & Subp.Subp_Name_Image.all
                      & ". Please fill out manually."");");
            New_Line_Count;
         end if;

         GNAT.Strings.Free (Unit_File_Content);

         return True;

      end Unparse_Test_Vectors;

      procedure Put_Persistent_Section (PS_Type : Persistent_Section_Type) is
         UH     : Unique_Hash;
         MD     : Markered_Data;
         MD_Cur : Markered_Data_Maps.Cursor;
      begin
         S_Put (0, "--  begin read only");
         New_Line_Count;
         case PS_Type is
            when With_Clauses =>
               S_Put (0, "--  id:" & Hash_Version & "/00/");
            when Body_Declarations =>
               S_Put (0, "--  id:" & Hash_Version & "/01/");
            when Body_Statements =>
               S_Put (0, "--  id:" & Hash_Version & "/02/");
         end case;
         New_Line_Count;
         S_Put (0, "--");
         New_Line_Count;
         case PS_Type is
            when With_Clauses =>
               S_Put (0, "--  This section can be used to add with "
                      & "clauses if necessary.");
            when Body_Declarations =>
               S_Put (0, "--  This section can be used to add global "
                      & "variables and other elements.");
            when Body_Statements =>
               S_Put (0, "--  This section can be used to add "
                      & "elaboration code for the global state.");
         end case;
         New_Line_Count;
         S_Put (0, "--");
         New_Line_Count;
         if PS_Type = Body_Statements then
            S_Put (0, "begin");
            New_Line_Count;
         end if;
         S_Put (0, "--  end read only");
         New_Line_Count;

         UH.Version := new String'(Hash_Version);
         case PS_Type is
            when With_Clauses =>
               UH.Hash := new String'("00");
            when Body_Declarations =>
               UH.Hash := new String'("01");
            when Body_Statements =>
               UH.Hash := new String'("02");
         end case;
         UH.TC_Hash := new String'("");
         MD_Cur := Find (Markered_Data_Map, UH);
         if MD_Cur /= Markered_Data_Maps.No_Element then
            MD := Markered_Data_Maps.Element (MD_Cur);
            for I in MD.TR_Text.First_Index .. MD.TR_Text.Last_Index loop
               S_Put (0, MD.TR_Text.Element (I));
               New_Line_Count;
            end loop;
            Markered_Data_Map.Delete (MD_Cur);
         else
            if PS_Type = Body_Statements then
               S_Put (3, "null;");
            end if;
            New_Line_Count;
         end if;

         S_Put (0, "--  begin read only");
         New_Line_Count;
         S_Put (0, "--  end read only");
         New_Line_Count;
      end Put_Persistent_Section;

      procedure Add_Buffered_TR_Slocs
        (TP_List     : in out TP_Mapping_List.List;
         Common_Time : String)
      is
         Cur : TR_SLOC_Buffer_Lists.Cursor := TR_SLOC_Buffer.First;
      begin
         loop
            exit when Cur = TR_SLOC_Buffer_Lists.No_Element;

            if TR_SLOC_Buffer_Lists.Element (Cur).Test_T /= null then
               Add_TR
                 (TP_List,
                  TR_SLOC_Buffer_Lists.Element (Cur).TPtarg.all,
                  TR_SLOC_Buffer_Lists.Element (Cur).Test_F.all,
                  "modified",
                  TR_SLOC_Buffer_Lists.Element (Cur).Subp,
                  TR_SLOC_Buffer_Lists.Element (Cur).TR_Line);
            else
               Add_TR
                 (TP_List,
                  TR_SLOC_Buffer_Lists.Element (Cur).TPtarg.all,
                  TR_SLOC_Buffer_Lists.Element (Cur).Test_F.all,
                  Common_Time,
                  TR_SLOC_Buffer_Lists.Element (Cur).Subp,
                  TR_SLOC_Buffer_Lists.Element (Cur).TR_Line);
            end if;
            TR_SLOC_Buffer_Lists.Next (Cur);
         end loop;

         TR_SLOC_Buffer.Clear;

      end Add_Buffered_TR_Slocs;

      function Is_Unimplemented_Test
        (TR_Text : String_Vectors.Vector) return Boolean
      is
         Unimplemented_Line : constant String :=
           """Test not implemented.""";
      begin

         if TR_Text.Is_Empty then
            return True;
         end if;

         for I in TR_Text.First_Index .. TR_Text.Last_Index loop
            if Index (TR_Text.Element (I), Unimplemented_Line) /= 0 then
               return True;
            end if;
         end loop;

         return False;

      end Is_Unimplemented_Test;

      function Markered_Data_Map_Is_Empty return Boolean is
         use Ada.Containers;
      begin
         if Markered_Data_Map.Is_Empty then
            return True;
         elsif Markered_Data_Map.Length = 1
           and then Markered_Data_Map.First_Key.Hash.all = "02"
         then
            return True;
         else
            return False;
         end if;
      end Markered_Data_Map_Is_Empty;

      procedure Put_Test_Data_Header is
      begin
         S_Put
           (0,
            "--  This package is intended to set up and tear down "
            & " the test environment.");
         Put_New_Line;
         S_Put
           (0,
            "--  Once created by GNATtest, this package will "
            & "never be overwritten");
         Put_New_Line;
         S_Put
           (0,
            "--  automatically. Contents of this package can be "
            & "modified in any way");
         Put_New_Line;
         S_Put
           (0,
            "--  except for sections surrounded by a 'read only' marker.");
         Put_New_Line;
         Put_New_Line;
      end Put_Test_Data_Header;

      procedure Put_TP_Header (TD_Package_Name : String) is
      begin
         S_Put
           (0,
            "--  This package has been generated automatically by GNATtest.");
         New_Line_Count;
         S_Put
           (0,
            "--  You are allowed to add your code to the bodies "
            & "of test routines.");
         New_Line_Count;
         S_Put
           (0,
            "--  Such changes will be kept during further regeneration "
            & "of this file.");
         New_Line_Count;
         S_Put
           (0,
            "--  All code placed outside of test routine bodies "
            & "will be lost. The");
         New_Line_Count;
         S_Put
           (0,
            "--  code intended to set up and tear down the test "
            & "environment should be");
         New_Line_Count;
         S_Put
           (0,
            "--  placed into "
            & TD_Package_Name & ".");
         New_Line_Count;
         New_Line_Count;
      end Put_TP_Header;

      procedure Put_Stub_Data_Import is
         S_Cur : Ada_Nodes_List.Cursor := Data.Units_To_Stub.First;
         Tmp : Unbounded_String;
         Def_Name : Defining_Name;
         use Ada_Nodes_List;
      begin
         while S_Cur /= Ada_Nodes_List.No_Element loop
            Tmp := To_Unbounded_String
              (Element (S_Cur).Unit.Get_Filename);

            if
              Source_Stubbed (To_String (Tmp)) and then
              not Excluded_Test_Data_Files.Contains
                (Base_Name
                   (Get_Source_Stub_Data_Spec (To_String (Tmp))))
            then
               Def_Name :=
                 Ada_Nodes_List.Element
                   (S_Cur).As_Basic_Decl.P_Defining_Name;
               S_Put
                 (0,
                  "with "
                  & Node_Image (Def_Name)
                  & "."
                  & Stub_Data_Unit_Name
                  & "; use "
                  & Node_Image (Def_Name)
                  & "."
                  & Stub_Data_Unit_Name
                  & ";");
               New_Line_Count;
            end if;

            Next (S_Cur);
         end loop;
      end Put_Stub_Data_Import;

      Unparse_Success : Boolean := False;

      use GNAT.OS_Lib;
   begin

      if not Generate_Separates then
         Test_Info.Include (Data.Unit_File_Name.all, 0);
      end if;

      Test_Unit_Suffix := new String'(Test_Unit_Name_Suff);

      if Data.Is_Generic then
         Gen_Tests.Gen_Unit_Full_Name := new String'(Data.Unit_Full_Name.all);
      end if;

      for I in
        Data.Type_Data_List.First_Index .. Data.Type_Data_List.Last_Index
      loop
         Current_Type := Data.Type_Data_List.Element (I);

         --  setting up current package
         Pack_Cur := Data.Package_Data_List.First;
         loop
            exit when Pack_Cur = Package_Info_List.No_Element;

            Current_Pack := Package_Info_List.Element (Pack_Cur);

            if Current_Type.Nesting.all = Current_Pack.Name.all then
               exit;
            end if;

            Pack_Cur := Package_Info_List.Next (Pack_Cur);
         end loop;

         Actual_Test := False;

         if Data.Unit_Full_Name.all = Current_Type.Nesting.all then
            Unit_Pref := new String'(Data.Unit_Full_Name.all);
         else
            Unit_Pref := new String'
              (Data.Unit_Full_Name.all & "." &
               Test_Data_Unit_Name & "." &
               Test_Unit_Name & "." &
               Nesting_Difference
                 (Data.Unit_Full_Name.all,
                  Current_Type.Nesting.all));
         end if;

         Data_Unit_Name := new String'
           (Unit_Pref.all & "."                  &
            Current_Type.Main_Type_Text_Name.all &
            Test_Data_Unit_Name_Suff);

         Test_File_Name := new String'(Unit_To_File_Name (Data_Unit_Name.all));

         --  saving test data package name for further reference
         Test_Data_Package_Name := new String'(Data_Unit_Name.all);

         if not Is_Regular_File
           (Output_Dir & Directory_Separator & Test_File_Name.all & ".ads")
         then

            Create
              (Output_Dir & Directory_Separator & Test_File_Name.all & ".ads");

            Put_Test_Data_Header;

            if not Current_Type.Has_Argument_Father then
               if Current_Pack.Data_Kind = Instantiation then
                  S_Put
                    (0,
                     "with "
                     & Current_Pack.Generic_Containing_Package.all
                     & "."
                     & Current_Type.Main_Type_Text_Name.all
                     & Test_Data_Unit_Name_Suff
                     & ";");
                  Put_New_Line;
                  S_Put
                    (0,
                     "with "
                     & Current_Pack.Generic_Containing_Package.all
                     & "."
                     & Current_Type.Main_Type_Text_Name.all
                     & Test_Data_Unit_Name_Suff
                     & "."
                     & Current_Type.Main_Type_Text_Name.all
                     & Test_Unit_Name_Suff
                     & ";");
               end if;
               Put_New_Line;
               S_Put (0, "with AUnit.Test_Fixtures;");
            else
               if
                 Current_Type.Argument_Father_Unit_Name.all =
                   Current_Type.Argument_Father_Nesting.all
               then
                  S_Put
                    (0,
                     "with "                                    &
                     Current_Type.Argument_Father_Unit_Name.all &
                     "."                                        &
                     Current_Type.Argument_Father_Type_Name.all &
                     Test_Data_Unit_Name_Suff                   &
                     "."                                        &
                     Current_Type.Argument_Father_Type_Name.all &
                     Test_Unit_Suffix.all                       &
                     ";");
               else
                  S_Put
                    (0,
                     "with "                                      &
                     Current_Type.Argument_Father_Unit_Name.all   &
                     "."                                          &
                     Test_Data_Unit_Name                          &
                     "."                                          &
                     Test_Unit_Name                               &
                     "."                                          &
                     Nesting_Difference
                       (Current_Type.Argument_Father_Unit_Name.all,
                        Current_Type.Argument_Father_Nesting.all) &
                     "."                                          &
                     Current_Type.Argument_Father_Type_Name.all   &
                     Test_Data_Unit_Name_Suff                     &
                     "."                                          &
                     Current_Type.Argument_Father_Type_Name.all   &
                     Test_Unit_Suffix.all                         &
                     ";");
               end if;
            end if;
            Put_New_Line;
            Put_New_Line;

            S_Put (0, "with GNATtest_Generated;");
            Put_New_Line;
            Put_New_Line;

            if Current_Pack.Is_Generic then
               S_Put (0, "generic");
               Put_New_Line;
               S_Put
                 (3,
                  "type GNATtest_Test_Type is new "
                  & "AUnit.Test_Fixtures.Test_Fixture");
               Put_New_Line;
               S_Put (5, "with private;");
               Put_New_Line;
            end if;

            S_Put (0, "package " & Data_Unit_Name.all & " is");
            Put_New_Line;
            Put_New_Line;

            if Current_Pack.Data_Kind = Declaration_Data then
               if Current_Type.Has_Argument_Father then
                  --  Declaring test type extension from another test type.
                  S_Put (0, GT_Marker_Begin);
                  Put_New_Line;
                  S_Put
                    (3,
                     "type Test_" &
                       Current_Type.Main_Type_Text_Name.all);
                  if Current_Type.Main_Type_Abstract then
                     S_Put (0, " is abstract new");
                  else
                     S_Put (0, " is new");
                  end if;
                  Put_New_Line;

                  if
                    Current_Type.Argument_Father_Unit_Name.all /=
                      Current_Type.Argument_Father_Nesting.all
                  then
                     Nesting_Add := new String'
                       (Test_Data_Unit_Name & "." &
                          Test_Unit_Name & "." &
                          Nesting_Difference
                          (Current_Type.Argument_Father_Unit_Name.all,
                           Current_Type.Argument_Father_Nesting.all) &
                          ".");
                  else
                     Nesting_Add := new String'("");
                  end if;

                  S_Put
                    (5,
                     "GNATtest_Generated.GNATtest_Standard."    &
                       Current_Type.Argument_Father_Unit_Name.all &
                       "."                                        &
                       Nesting_Add.all                            &
                       Current_Type.Argument_Father_Type_Name.all &
                       Test_Data_Unit_Name_Suff                   &
                       "."                                        &
                       Current_Type.Argument_Father_Type_Name.all &
                       Test_Unit_Suffix.all                       &
                       ".Test_"                                   &
                       Current_Type.Argument_Father_Type_Name.all);
                  Put_New_Line;
                  S_Put (0, GT_Marker_End);
                  Put_New_Line;
                  S_Put (3, "with null record;");

                  Free (Nesting_Add);

               else
                  --  Declaring access type to tested type.
                  S_Put
                    (3,
                     "type "                                 &
                       Current_Type.Main_Type_Text_Name.all    &
                       "_Access is access all "                &
                       "GNATtest_Generated.GNATtest_Standard." &
                       Current_Type.Nesting.all                &
                       "."                                     &
                       Current_Type.Main_Type_Text_Name.all    &
                       "'Class;");
                  Put_New_Line;
                  Put_New_Line;

                  --  Declaring root test type.
                  S_Put (0, GT_Marker_Begin);
                  Put_New_Line;
                  S_Put
                    (3,
                     "type Test_"                         &
                       Current_Type.Main_Type_Text_Name.all &
                       " is");
                  if Current_Type.Main_Type_Abstract then
                     S_Put (0, " abstract");
                  end if;
                  S_Put (0, " new AUnit.Test_Fixtures.Test_Fixture");
                  Put_New_Line;
                  S_Put (0, GT_Marker_End);
                  Put_New_Line;
                  S_Put (3, "with record");
                  Put_New_Line;
                  S_Put
                    (6,
                     "Fixture : "                         &
                       Current_Type.Main_Type_Text_Name.all &
                       "_Access;");
                  Put_New_Line;
                  S_Put (3, "end record;");
               end if;
            else
               S_Put (0, GT_Marker_Begin);
               Put_New_Line;
               S_Put
                 (3,
                  "type Test_"                         &
                    Current_Type.Main_Type_Text_Name.all &
                    " is");
               S_Put (0, " new AUnit.Test_Fixtures.Test_Fixture");
               Put_New_Line;
               S_Put (0, GT_Marker_End);
               Put_New_Line;
               S_Put (3, "with null record;");
            end if;

            Put_New_Line;
            Put_New_Line;

            if not Current_Type.Main_Type_Abstract then
               S_Put
                 (3,
                  "procedure Set_Up (Gnattest_T : in out Test_" &
                  Current_Type.Main_Type_Text_Name.all &
                  ");");
               Put_New_Line;
               S_Put
                 (3,
                  "procedure Tear_Down (Gnattest_T : in out Test_" &
                  Current_Type.Main_Type_Text_Name.all &
                  ");");
               Put_New_Line;
               Put_New_Line;
            end if;

            if Current_Pack.Data_Kind = Instantiation then
               S_Put (0, GT_Marker_Begin);
               Put_New_Line;
               S_Put
                 (3,
                  "package Gnattest_Data_Inst is new "
                  & "GNATtest_Generated.GNATtest_Standard."
                  & Current_Pack.Name.all
                  & "."
                  & Current_Type.Main_Type_Text_Name.all
                  & Test_Data_Unit_Name_Suff
                  & " (Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & ");");
               Put_New_Line;
               S_Put
                 (3,
                  "package Gnattest_Tests_Inst is new Gnattest_Data_Inst."
                  & Current_Type.Main_Type_Text_Name.all
                  & Test_Unit_Name_Suff
                  & ";");
               Put_New_Line;
               Put_New_Line;
               S_Put
                 (3,
                  "type New_Test is new Gnattest_Tests_Inst.Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & " with null record;");
               Put_New_Line;
               S_Put (0, GT_Marker_End);
               Put_New_Line;
               Put_New_Line;
               S_Put
                 (3,
                  "procedure User_Set_Up (Gnattest_T : in out New_Test);");
               Put_New_Line;
               S_Put
                 (3,
                  "procedure User_Tear_Down "
                  & "(Gnattest_T : in out New_Test);");
               Put_New_Line;
               Put_New_Line;
            end if;

            if Current_Pack.Is_Generic then
               S_Put
                 (3,
                  "procedure User_Set_Up (Gnattest_T : in out Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & ");");
               Put_New_Line;
               S_Put
                 (3,
                  "procedure User_Tear_Down (Gnattest_T : in out Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & ");");
               Put_New_Line;
               Put_New_Line;
            end if;

            S_Put (0, "end " & Data_Unit_Name.all & ";");
            Put_New_Line;

            Close_File;

         end if;

         if not Current_Type.Main_Type_Abstract and then
           not Is_Regular_File
           (Output_Dir & Directory_Separator & Test_File_Name.all & ".adb")
         then

            Create
              (Output_Dir & Directory_Separator & Test_File_Name.all & ".adb");

            Put_Test_Data_Header;

            S_Put (0, "package body " & Data_Unit_Name.all & " is");
            Put_New_Line;
            Put_New_Line;

            if Current_Pack.Data_Kind = Declaration_Data then
               if Current_Type.No_Default_Discriminant then
                  S_Put
                    (3,
                     "--  Local_"                            &
                       Current_Type.Main_Type_Text_Name.all    &
                       " : aliased "                           &
                       "GNATtest_Generated.GNATtest_Standard." &
                       Current_Type.Nesting.all                &
                       "."                                     &
                       Current_Type.Main_Type_Text_Name.all &
                       ";");
               else
                  S_Put
                    (3,
                     "Local_"                                &
                       Current_Type.Main_Type_Text_Name.all    &
                       " : aliased "                           &
                       "GNATtest_Generated.GNATtest_Standard." &
                       Current_Type.Nesting.all                &
                       "."                                     &
                       Current_Type.Main_Type_Text_Name.all    &
                       ";");
               end if;
               Put_New_Line;
            end if;

            S_Put
              (3,
               "procedure Set_Up (Gnattest_T : in out Test_" &
               Current_Type.Main_Type_Text_Name.all      &
               ") is");
            Put_New_Line;

            if Current_Pack.Data_Kind = Declaration_Data then
               if Current_Pack.Is_Generic then
                  S_Put
                    (6,
                     "X : Test_"
                     & Current_Type.Main_Type_Text_Name.all
                     & "'Class renames Test_"
                     & Current_Type.Main_Type_Text_Name.all
                     & "'Class (Gnattest_T);");
                  Put_New_Line;
               end if;
            end if;

            S_Put (3, "begin");
            Put_New_Line;

            if Current_Type.Has_Argument_Father then
               if
                 Current_Type.Argument_Father_Unit_Name.all /=
                   Current_Type.Argument_Father_Nesting.all
               then
                  Nesting_Add := new String'
                    (Test_Data_Unit_Name & "." &
                     Test_Unit_Name & "." &
                     Nesting_Difference
                       (Current_Type.Argument_Father_Unit_Name.all,
                        Current_Type.Argument_Father_Nesting.all) &
                     ".");
               else
                  Nesting_Add := new String'("");
               end if;

               S_Put
                 (6,
                  "GNATtest_Generated.GNATtest_Standard."    &
                  Current_Type.Argument_Father_Unit_Name.all &
                  "."                                        &
                  Nesting_Add.all                            &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Data_Unit_Name_Suff                   &
                  "."                                        &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Unit_Suffix.all                       &
                  ".Set_Up");
               Put_New_Line;
               S_Put
                 (8,
                  "(GNATtest_Generated.GNATtest_Standard."    &
                  Current_Type.Argument_Father_Unit_Name.all &
                  "."                                        &
                  Nesting_Add.all                            &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Data_Unit_Name_Suff                   &
                  "."                                        &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Unit_Suffix.all                       &
                  ".Test_"                                   &
                  Current_Type.Argument_Father_Type_Name.all &
                  " (Gnattest_T));");
               Put_New_Line;

               Free (Nesting_Add);
            end if;

            if Current_Pack.Data_Kind = Declaration_Data then
               if Current_Type.No_Default_Discriminant then
                  S_Put
                    (6, "null;");
                  Put_New_Line;
                  S_Put
                    (6, "--  Gnattest_T.Fixture := Local_"         &
                       Current_Type.Main_Type_Text_Name.all &
                       "'Access;");
                  Put_New_Line;
               else
                  S_Put
                    (6, "Gnattest_T.Fixture := Local_"             &
                       Current_Type.Main_Type_Text_Name.all &
                       "'Access;");
                  Put_New_Line;

                  if Current_Pack.Data_Kind = Declaration_Data then
                     if Current_Pack.Is_Generic then
                        S_Put (6, "User_Set_Up (X);");
                        Put_New_Line;
                     end if;
                  end if;
               end if;

            else
               S_Put
                 (6, "null;");
               Put_New_Line;
            end if;
            S_Put (3, "end Set_Up;");
            Put_New_Line;
            Put_New_Line;

            S_Put
              (3,
               "procedure Tear_Down (Gnattest_T : in out Test_" &
               Current_Type.Main_Type_Text_Name.all &
               ") is");
            Put_New_Line;

            if Current_Pack.Data_Kind = Declaration_Data then
               if Current_Pack.Is_Generic then
                  S_Put
                    (6,
                     "X : Test_"
                     & Current_Type.Main_Type_Text_Name.all
                     & "'Class renames Test_"
                     & Current_Type.Main_Type_Text_Name.all
                     & "'Class (Gnattest_T);");
                  Put_New_Line;
               end if;
            end if;

            S_Put (3, "begin");
            Put_New_Line;

            if Current_Type.Has_Argument_Father then
               if
                 Current_Type.Argument_Father_Unit_Name.all /=
                   Current_Type.Argument_Father_Nesting.all
               then
                  Nesting_Add := new String'
                    (Test_Data_Unit_Name & "." &
                     Test_Unit_Name & "." &
                     Nesting_Difference
                       (Current_Type.Argument_Father_Unit_Name.all,
                        Current_Type.Argument_Father_Nesting.all) &
                     ".");
               else
                  Nesting_Add := new String'("");
               end if;

               S_Put
                 (6,
                  "GNATtest_Generated.GNATtest_Standard."    &
                  Current_Type.Argument_Father_Unit_Name.all &
                  "."                                        &
                  Nesting_Add.all                            &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Data_Unit_Name_Suff                   &
                  "."                                        &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Unit_Suffix.all                       &
                  ".Tear_Down");
               Put_New_Line;
               S_Put
                 (8,
                  "(GNATtest_Generated.GNATtest_Standard."    &
                  Current_Type.Argument_Father_Unit_Name.all &
                  "."                                        &
                  Nesting_Add.all                            &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Data_Unit_Name_Suff                   &
                  "."                                        &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Unit_Suffix.all                       &
                  ".Test_"                                   &
                  Current_Type.Argument_Father_Type_Name.all &
                  " (Gnattest_T));");

               Free (Nesting_Add);
            else
               if Current_Pack.Data_Kind = Declaration_Data
                 and then Current_Pack.Is_Generic
               then
                     S_Put (6, "User_Tear_Down (X);");
               else
                  S_Put
                    (6, "null;");
               end if;
            end if;

            Put_New_Line;
            S_Put (3, "end Tear_Down;");

            Put_New_Line;
            Put_New_Line;

            if Current_Pack.Data_Kind = Instantiation then
               S_Put
                 (3,
                  "procedure User_Set_Up "
                  & "(Gnattest_T : in out New_Test) is");
               Put_New_Line;
               S_Put (6, "pragma Unreferenced (Gnattest_T);");
               Put_New_Line;
               S_Put (3, "begin");
               Put_New_Line;
               S_Put (6, "null;");
               Put_New_Line;
               S_Put (3, "end User_Set_Up;");
               Put_New_Line;
               Put_New_Line;
               S_Put
                 (3,
                  "procedure User_Tear_Down "
                  & "(Gnattest_T : in out New_Test) is");
               Put_New_Line;
               S_Put (6, "pragma Unreferenced (Gnattest_T);");
               Put_New_Line;
               S_Put (3, "begin");
               Put_New_Line;
               S_Put (6, "null;");
               Put_New_Line;
               S_Put (3, "end User_Tear_Down;");
               Put_New_Line;
               Put_New_Line;
            end if;

            if Current_Pack.Is_Generic then
               S_Put
                 (3,
                  "procedure User_Set_Up (Gnattest_T : in out Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & ") is");
               Put_New_Line;
               S_Put (3, "begin");
               Put_New_Line;
               S_Put (6, "null;");
               Put_New_Line;
               S_Put (3, "end User_Set_Up;");
               Put_New_Line;
               Put_New_Line;
               S_Put
                 (3,
                  "procedure User_Tear_Down (Gnattest_T : in out Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & ") is");
               Put_New_Line;
               S_Put (3, "begin");
               Put_New_Line;
               S_Put (6, "null;");
               Put_New_Line;
               S_Put (3, "end User_Tear_Down;");
               Put_New_Line;
               Put_New_Line;
            end if;

            S_Put (0, "end " & Data_Unit_Name.all & ";");
            Put_New_Line;
            Close_File;

         end if;

         TP_Map.SetUp_Name    := new String'(Test_File_Name.all & ".adb");
         TP_Map.TearDown_Name := new String'(Test_File_Name.all & ".adb");
         TP_Map.SetUp_Line    := 9;
         TP_Map.SetUp_Column  := 4;

         Tear_Down_Line_Add := 0;
         if Current_Type.No_Default_Discriminant then
            Tear_Down_Line_Add := Tear_Down_Line_Add + 1;
         end if;
         if Current_Type.Has_Argument_Father then
            Tear_Down_Line_Add := Tear_Down_Line_Add + 1;
         end if;
         TP_Map.TearDown_Line := 14 + Tear_Down_Line_Add;
         TP_Map.TearDown_Column := 4;

         Free (Test_File_Name);

         Unit_Name := new
           String'(Unit_Pref.all                        &
                     "."                                  &
                     Current_Type.Main_Type_Text_Name.all &
                     Test_Data_Unit_Name_Suff             &
                     "."                                  &
                     Current_Type.Main_Type_Text_Name.all &
                     Test_Unit_Name_Suff);

         Free (Unit_Pref);

         Test_File_Name := new String'(Unit_To_File_Name (Unit_Name.all));

         ----------------------------------
         --  Creating test package spec  --
         ----------------------------------

         Create
           (Output_Dir & Directory_Separator & Test_File_Name.all & ".ads");

         Put_Harness_Header;
         S_Put (0, GT_Marker_Begin);
         Put_New_Line;

         S_Put (0, "with GNATtest_Generated;");
         Put_New_Line;
         if Stub_Mode_ON then
            S_Put (0, "with AUnit.Test_Caller;");
            Put_New_Line;
         end if;
         Put_New_Line;

         if Current_Pack.Is_Generic then
            S_Put (0, "generic");
            Put_New_Line;

            declare
               GP : Generic_Package;
            begin
               GP.Name := new String'(Current_Pack.Name.all);
               GP.Sloc := new String'
                 (Base_Name (Data.Unit_File_Name.all)
                  & ":"
                  & Trim
                    (First_Line_Number (Current_Pack.Element)'Img,
                     Both)
                  & ":"
                  & Trim
                    (First_Column_Number (Current_Pack.Element)'Img,
                     Both));
               Update_Generic_Packages (GP);
            end;
         end if;

         S_Put (0, "package " & Unit_Name.all & " is");
         Put_New_Line;
         Put_New_Line;

         if Current_Pack.Data_Kind = Declaration_Data then
            S_Put
              (3,
               "type Test_" &
                 Current_Type.Main_Type_Text_Name.all);
            if Current_Type.Main_Type_Abstract then
               S_Put (0, " is abstract new");
            else
               S_Put (0, " is new");
            end if;
            Put_New_Line;

            if Data.Unit_Full_Name.all = Current_Type.Nesting.all then
               S_Put
                 (5,
                  "GNATtest_Generated.GNATtest_Standard."    &
                    Data.Unit_Full_Name.all                    &
                    "."                                        &
                    Current_Type.Main_Type_Text_Name.all &
                    Test_Data_Unit_Name_Suff                   &
                    ".Test_"                                   &
                    Current_Type.Main_Type_Text_Name.all &
                    " with null record;");
            else
               S_Put
                 (5,
                  "GNATtest_Generated.GNATtest_Standard."    &
                    Data.Unit_Full_Name.all                    &
                    "."                                        &
                    Test_Data_Unit_Name                        &
                    "."                                        &
                    Test_Unit_Name                             &
                    "."                                        &
                    Nesting_Difference
                    (Data.Unit_Full_Name.all,
                     Current_Type.Nesting.all)               &
                    "."                                        &
                    Current_Type.Main_Type_Text_Name.all &
                    Test_Data_Unit_Name_Suff                   &
                    ".Test_"                                   &
                    Current_Type.Main_Type_Text_Name.all &
                    " with null record;");
            end if;

         else
            S_Put
              (3,
               "type Test_"
               & Current_Type.Main_Type_Text_Name.all
               & " is new GNATtest_Generated.GNATtest_Standard."
               & Data_Unit_Name.all & ".New_Test with null record;");

            Update_Generic_Packages
              (Current_Pack.Generic_Containing_Package.all);
         end if;

         Put_New_Line;
         Put_New_Line;

         --  Adding test routine declarations.
         if Current_Pack.Data_Kind = Declaration_Data then
            Subp_Cur := Data.Subp_List.First;
            loop
               exit when Subp_Cur = Subp_Data_List.No_Element;

               if
                 Subp_Data_List.Element (Subp_Cur).Corresp_Type =
                 Current_Type.Type_Number
               then

                  S_Put
                    (3,
                     "procedure "
                     & Subp_Data_List.Element
                       (Subp_Cur).Subp_Mangle_Name.all
                     & " (Gnattest_T : in out Test_"
                     & Current_Type.Main_Type_Text_Name.all
                     & ");");
                  Actual_Test := True;

                  Put_New_Line;
                  Print_Comment_Declaration
                    (Subp_Data_List.Element (Subp_Cur), 3);
                  Put_New_Line;
               end if;

               Subp_Data_List.Next (Subp_Cur);
            end loop;
         end if;

         if Stub_Mode_ON then
            S_Put
              (3,
               "package Caller is new AUnit.Test_Caller (Test_"
               & Current_Type.Main_Type_Text_Name.all
               & ");");
            Put_New_Line;
            Put_New_Line;
         end if;

         S_Put (0, "end " & Unit_Name.all & ";");
         Put_New_Line;
         S_Put (0, GT_Marker_End);
         Put_New_Line;
         Close_File;

         if not Current_Type.Main_Type_Abstract then
            TP_Map.TP_Name := new String'(Test_File_Name.all & ".ads");
            TP_List.Append (TP_Map);
         end if;

         ----------------------------------
         --  Creating test package body  --
         ----------------------------------

         if Actual_Test then

            Reset_Line_Counter;

            if Generate_Separates then
               Create
                 (Output_Dir
                  & Directory_Separator
                  & Test_File_Name.all
                  & ".adb");
               Put_Harness_Header;
            else
               Get_Subprograms_From_Package
                 (Output_Dir
                  & Directory_Separator
                  & Test_File_Name.all
                  & ".adb");
               Create (Tmp_File_Name);
               Put_TP_Header (Test_Data_Package_Name.all);

               --  gathering transition data
               if Transition then
                  Subp_Cur := Data.Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if
                       Current_Subp.Corresp_Type = Current_Type.Type_Number
                     then
                        UH.Version := new String'("1");
                        UH.Hash := new String'
                          (Subp_Data_List.Element
                             (Subp_Cur).Subp_Hash_V1.all);
                        if
                          Subp_Data_List.Element (Subp_Cur).Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Subp_Data_List.Element
                                (Subp_Cur).TC_Info.TC_Hash.all);
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        Current_Subp := Subp_Data_List.Element (Subp_Cur);

                        Get_Subprogram_From_Separate
                          (Output_Dir
                           & Directory_Separator
                           & Unit_To_File_Name
                             (Unit_Name.all
                              & "."
                              & Test_Routine_Prefix
                              & Current_Subp.Subp_Text_Name.all
                              & "_"
                              & Current_Subp.Subp_Hash_V1
                                (Current_Subp.Subp_Hash_V1'First ..
                                   Current_Subp.Subp_Hash_V1'First + 5)
                              & (if Current_Subp.Has_TC_Info
                                then "_" & Current_Subp.TC_Info.TC_Hash
                                  (Current_Subp.TC_Info.TC_Hash'First ..
                                     Current_Subp.TC_Info.TC_Hash'First + 5)
                                else ""))
                           & ".adb",
                           UH,
                           Current_Subp);
                     end if;
                     Subp_Data_List.Next (Subp_Cur);
                  end loop;
               end if;

               --  gathering used short names
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                  then
                     UH.Version := new String'(Hash_Version);
                     UH.Hash := new String'
                       (Current_Subp.Subp_Full_Hash.all);
                     if
                       Current_Subp.Has_TC_Info
                     then
                        UH.TC_Hash := new String'
                          (Sanitize_TC_Name (Current_Subp.TC_Info.Name.all));
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);

                     if MD_Cur /= Markered_Data_Maps.No_Element then
                        MD := Markered_Data_Maps.Element (MD_Cur);
                        if MD.Short_Name_Used then
                           Short_Names_Used.Include
                             (To_Lower (MD.Short_Name.all));
                           Shortnamed_Subps.Include
                             (Current_Subp.Subp_Declaration);

                           Name_Numbers.Include
                             (To_Lower (Current_Subp.Subp_Text_Name.all), 1);
                           Elem_Numbers.Include
                             (Current_Subp.Subp_Declaration, 1);
                        end if;
                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  updating hash v.1 to hash v.2 where possible
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                  then
                     UH.Version := new String'("1");
                     UH.Hash := new String'
                       (Current_Subp.Subp_Hash_V1.all);
                     if
                       Current_Subp.Has_TC_Info
                     then
                        UH.TC_Hash := new String'
                          (Current_Subp.TC_Info.TC_Hash.all);
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);

                     if MD_Cur /= Markered_Data_Maps.No_Element then
                        MD := Markered_Data_Maps.Element (MD_Cur);

                        Markered_Data_Map.Delete (MD_Cur);
                        Free (UH.Hash);
                        UH.Hash := new String'
                          (Current_Subp.Subp_Hash_V2_1.all);
                        Free (UH.Version);
                        UH.Version := new String'("2");

                        Markered_Data_Map.Include (UH, MD);
                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  updating hash v.2 to hash v.2.1 where possible
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                  then
                     UH.Version := new String'("2");
                     UH.Hash := new String'
                       (Current_Subp.Subp_Hash_V2_1 .all);

                     if Current_Subp.Has_TC_Info then
                        UH.TC_Hash := new String'
                          (Current_Subp.TC_Info.TC_Hash.all);
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);

                     if MD_Cur /= Markered_Data_Maps.No_Element then
                        MD := Markered_Data_Maps.Element (MD_Cur);

                        Markered_Data_Map.Delete (MD_Cur);
                        Free (UH.Version);
                        UH.Version := new String'("2.1");
                        if UH.TC_Hash.all /= "" then
                           Free (UH.TC_Hash);
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Current_Subp.TC_Info.Name.all));
                        end if;

                        Markered_Data_Map.Include (UH, MD);
                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  updating hash v.2.1 to hash v.2.2
               --  and looking for new short names
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                  then
                     UH.Version := new String'("2.1");
                     UH.Hash := new String'
                       (Current_Subp.Subp_Hash_V2_1 .all);

                     if Current_Subp.Has_TC_Info then
                        UH.TC_Hash := new String'
                          (Sanitize_TC_Name
                             (Current_Subp.TC_Info.Name.all));
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);

                     if MD_Cur /= Markered_Data_Maps.No_Element then
                        MD := Markered_Data_Maps.Element (MD_Cur);

                        if not
                          Short_Names_Used.Contains (MD.Short_Name.all)
                          or else Shortnamed_Subps.Contains
                            (Current_Subp.Subp_Declaration)
                        then
                           Short_Names_Used.Include (MD.Short_Name.all);
                           Shortnamed_Subps.Include
                             (Current_Subp.Subp_Declaration);

                           Name_Numbers.Include
                             (To_Lower (Current_Subp.Subp_Text_Name.all), 1);
                           Elem_Numbers.Include
                             (Current_Subp.Subp_Declaration, 1);

                           MD.Short_Name_Used := True;
                        end if;

                        Markered_Data_Map.Delete (MD_Cur);
                        Free (UH.Hash);
                        UH.Hash := new String'
                          (Current_Subp.Subp_Full_Hash.all);
                        Free (UH.Version);
                        UH.Version := new String'(Hash_Version);
                        Markered_Data_Map.Include (UH, MD);
                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  creating markered_data and deciding on new short names
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                  then
                     UH.Version := new String'(Hash_Version);
                     UH.Hash := new String'
                       (Current_Subp.Subp_Full_Hash.all);
                     if Current_Subp.Has_TC_Info then
                        UH.TC_Hash := new String'
                          (Sanitize_TC_Name (Current_Subp.TC_Info.Name.all));
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);

                     if MD_Cur = Markered_Data_Maps.No_Element then

                        MD.Commented_Out := False;
                        MD.Short_Name_Used := False;
                        MD.Short_Name := new String'
                          (To_Lower (Current_Subp.Subp_Text_Name.all));
                        MD.TR_Text.Clear;

                        if
                          not Short_Names_Used.Contains
                          (To_Lower (Current_Subp.Subp_Text_Name.all))
                          or else Shortnamed_Subps.Contains
                            (Current_Subp.Subp_Declaration)
                        then
                           --  Short name is free, we can use it
                           MD.Short_Name_Used := True;
                           Short_Names_Used.Include
                             (To_Lower (Current_Subp.Subp_Text_Name.all));
                           Shortnamed_Subps.Include
                             (Current_Subp.Subp_Declaration);

                           Name_Numbers.Include
                             (To_Lower (Current_Subp.Subp_Text_Name.all), 1);
                           Elem_Numbers.Include
                             (Current_Subp.Subp_Declaration, 1);

                           --  Looking for a dangling test with same short
                           --  name but different hash.
                           MD_Cur := Find_Same_Short_Name
                             (Markered_Data_Map,
                              Current_Subp);

                           if MD_Cur /= Markered_Data_Maps.No_Element then
                              --  Using corresponding dangling test

                              MD.TR_Text.Clear;
                              MD.TR_Text :=
                                Markered_Data_Maps.Element (MD_Cur).TR_Text;

                              --  also need to copy Commented_Out since
                              --  the test can be dangling for a long time
                              --  or just become dangling
                              MD.Commented_Out :=
                                Markered_Data_Maps.Element
                                  (MD_Cur).Commented_Out;

                              Markered_Data_Map.Delete (MD_Cur);
                              MD.Issue_Warning := True;
                           end if;

                        end if;

                        Markered_Data_Map.Insert (UH, MD);

                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  setting overloading numbers;
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                  then

                     if
                       Name_Numbers.Find
                         (To_Lower (Current_Subp.Subp_Text_Name.all)) =
                       Name_Frequency.No_Element
                     then

                        Name_Numbers.Include
                          (To_Lower (Current_Subp.Subp_Text_Name.all), 1);
                        Elem_Numbers.Include
                          (Current_Subp.Subp_Declaration, 1);

                     else
                        if
                          Elem_Numbers.Find
                            (Current_Subp.Subp_Declaration) =
                            Elem_Number_Maps.No_Element
                        then

                           declare
                              X : constant Natural :=
                                Name_Numbers.Element
                                  (To_Lower
                                       (Current_Subp.Subp_Text_Name.all));
                           begin
                              Name_Numbers.Replace
                                (To_Lower (Current_Subp.Subp_Text_Name.all),
                                 X + 1);
                              Elem_Numbers.Include
                                (Current_Subp.Subp_Declaration, X + 1);
                           end;

                        end if;
                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;
               Name_Numbers.Clear;

            end if;

            S_Put (0, "with AUnit.Assertions; use AUnit.Assertions;");
            New_Line_Count;
            S_Put (0, "with System.Assertions;");
            New_Line_Count;
            if Test.Common.Generate_Test_Vectors and then not Data.Is_Generic
            then
               S_Put (0, "with Ada.Exceptions;");
               New_Line_Count;
            end if;
            if Stub_Mode_ON then
               Put_Stub_Data_Import;
            end if;
            New_Line_Count;

            Put_Persistent_Section (With_Clauses);

            S_Put (0, "package body " & Unit_Name.all & " is");
            New_Line_Count;
            New_Line_Count;

            Put_Persistent_Section (Body_Declarations);

            --  Adding test routine body stubs.
            Subp_Cur := Data.Subp_List.First;
            loop
               exit when Subp_Cur = Subp_Data_List.No_Element;

               if
                 Subp_Data_List.Element (Subp_Cur).Corresp_Type =
                 Current_Type.Type_Number
               then
                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if Subp_Data_List.Element (Subp_Cur).Has_TC_Info then
                     if Is_Function
                       (Subp_Data_List.Element
                          (Subp_Cur).Subp_Declaration.As_Basic_Decl)
                     then
                        Generate_Function_Wrapper
                          (Subp_Data_List.Element (Subp_Cur));
                     else
                        Generate_Procedure_Wrapper
                          (Subp_Data_List.Element (Subp_Cur));
                     end if;
                  end if;

                  if Generate_Separates then
                     S_Put
                       (3,
                        "procedure "                         &
                          Subp_Data_List.Element
                          (Subp_Cur).Subp_Mangle_Name.all    &
                          " (Gnattest_T : in out Test_"        &
                          Current_Type.Main_Type_Text_Name.all &
                          ") is separate;");

                     New_Line_Count;
                     Print_Comment_Declaration
                       (Subp_Data_List.Element (Subp_Cur), 3);
                     New_Line_Count;

                  else

                     Test_Info.Replace
                       (Data.Unit_File_Name.all,
                        Test_Info.Element (Data.Unit_File_Name.all) + 1);

                     All_Tests_Counter := All_Tests_Counter + 1;

                     UH.Version := new String'(Hash_Version);
                     UH.Hash := new String'
                       (Subp_Data_List.Element
                          (Subp_Cur).Subp_Full_Hash.all);
                     if Subp_Data_List.Element (Subp_Cur).Has_TC_Info then
                        UH.TC_Hash := new String'
                          (Sanitize_TC_Name
                             (Subp_Data_List.Element
                                  (Subp_Cur).TC_Info.Name.all));
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);
                     MD := Markered_Data_Maps.Element (MD_Cur);

                     Put_Opening_Comment_Section
                       (Subp_Data_List.Element (Subp_Cur),
                        Elem_Numbers.Element
                          (Current_Subp.Subp_Declaration),
                        Use_Short_Name => MD.Short_Name_Used,
                        Type_Name => Current_Type.Main_Type_Text_Name.all);

                     if Is_Unimplemented_Test (MD.TR_Text) then
                        TR_SLOC_Buffer.Append
                          ((new String'(Test_File_Name.all & ".ads"),
                           new String'(Test_File_Name.all & ".adb"),
                           null,
                           Subp_Data_List.Element (Subp_Cur),
                           New_Line_Counter));
                     else
                        TR_SLOC_Buffer.Append
                          ((new String'(Test_File_Name.all & ".ads"),
                           new String'(Test_File_Name.all & ".adb"),
                           new String'("modified"),
                           Subp_Data_List.Element (Subp_Cur),
                           New_Line_Counter));
                     end if;

                     if MD.TR_Text.Is_Empty then

                        if Stub_Mode_ON then
                           Setters_Set := Get_Direct_Callees_Setters
                             (Current_Subp.Subp_Declaration.As_Basic_Decl);
                        end if;

                        New_Tests_Counter := New_Tests_Counter + 1;
                        New_Line_Count;
                        S_Put (6, "pragma Unreferenced (Gnattest_T);");
                        New_Line_Count;
                        New_Line_Count;
                        S_Put (3, "begin");
                        New_Line_Count;
                        New_Line_Count;
                        if not Setters_Set.Is_Empty then
                           Set_Cur := Setters_Set.First;
                           while Set_Cur /= String_Set.No_Element loop
                              S_Put
                                (3,
                                 "--  "
                                 & String_Set.Element (Set_Cur)
                                 & "( );");
                              New_Line_Count;
                              Next (Set_Cur);
                           end loop;
                           New_Line_Count;
                           Setters_Set.Clear;
                        end if;

                        if Test.Common.Generate_Test_Vectors then
                           Unparse_Success := Unparse_Test_Vectors
                             (Data, Subp_Data_List.Element (Subp_Cur));
                        end if;
                        if not Unparse_Success then
                           if Test.Common.Generate_Test_Vectors
                             and then not Data.Is_Generic
                             and then not Is_Null
                                  (Subp_Data_List.Element (Subp_Cur)
                                   .Subp_Declaration.As_Basic_Decl
                                   .P_Subp_Spec_Or_Null)
                             and then Subp_Data_List.Element (Subp_Cur)
                                      .Subp_Declaration
                                      .As_Basic_Decl.P_Subp_Spec_Or_Null
                                      .P_Params'Length /= 0
                           then
                              Report_Std
                                ("Warning: (TGen) "
                                 & Base_Name (Data.Unit_File_Name.all)
                                 & " : Could not generate test"
                                 & " vectors for "
                                 & Current_Subp.Subp_Text_Name.all);
                           end if;
                           S_Put (6, "AUnit.Assertions.Assert");
                           New_Line_Count;
                           S_Put
                           (8, "(Gnattest_Generated.Default_Assert_Value,");
                           New_Line_Count;
                           S_Put (9,  """Test not implemented."");");
                           New_Line_Count;
                           New_Line_Count;
                        end if;
                     else

                        if MD.Issue_Warning then
                           Report_Std
                             ("warning: (gnattest) "
                              & Base_Name (Data.Unit_File_Name.all)
                              & ":"
                              & Trim
                                (First_Line_Number
                                 (Current_Subp.Subp_Declaration)'Img,
                                 Both)
                              & ":"
                              & Trim
                                (First_Column_Number
                                 (Current_Subp.Subp_Declaration)'Img,
                                 Both)
                              & ": test for "
                              & MD.Short_Name.all
                              & " at "
                              & Unit_Name.all
                              & ":"
                              & Trim
                                (Integer'Image (New_Line_Counter),
                                 Both)
                              & " might be out of date ("
                              & MD.Short_Name.all
                              & " has been changed)");
                        end if;

                        for I in
                          MD.TR_Text.First_Index .. MD.TR_Text.Last_Index
                        loop
                           if MD.Commented_Out then
                              S_Put
                                (0,
                                 Uncomment_Line (MD.TR_Text.Element (I)));
                           else
                              S_Put (0, MD.TR_Text.Element (I));
                           end if;
                           New_Line_Count;
                        end loop;
                     end if;

                     Markered_Data_Map.Delete (MD_Cur);

                     Put_Closing_Comment_Section
                       (Subp_Data_List.Element (Subp_Cur),
                        Elem_Numbers.Element
                          (Current_Subp.Subp_Declaration),
                        Use_Short_Name => MD.Short_Name_Used);
                     New_Line_Count;

                  end if;

               end if;

               Subp_Data_List.Next (Subp_Cur);
            end loop;

            --  printing dangling tests

            if not Markered_Data_Map_Is_Empty then
               Report_Std
                 ("warning: (gnattest) "
                  & Unit_Name.all
                  & " has dangling test(s)");
            end if;

            MD_Cur := Markered_Data_Map.First;
            loop
               exit when MD_Cur = Markered_Data_Maps.No_Element;

               MD := Markered_Data_Maps.Element (MD_Cur);

               if Markered_Data_Maps.Key (MD_Cur).Hash.all /= "02" then
                  declare
                     Stub : Subp_Info;
                  begin

                     Stub.Subp_Full_Hash := new String'
                       (Markered_Data_Maps.Key (MD_Cur).Hash.all);

                     Stub.Subp_Text_Name := new String'
                       (Markered_Data_Maps.Element (MD_Cur).Short_Name.all);

                     Stub.Subp_Mangle_Name := new String'
                       (Test_Routine_Prefix
                        & Stub.Subp_Text_Name.all
                        & "_"
                        & Stub.Subp_Full_Hash
                          (Stub.Subp_Full_Hash'First ..
                               Stub.Subp_Full_Hash'First + 5));

                     if Markered_Data_Maps.Key (MD_Cur).TC_Hash.all = "" then
                        Stub.Has_TC_Info := False;
                     else
                        Stub.Has_TC_Info := True;
                        Stub.TC_Info.TC_Hash := new String'
                          (Markered_Data_Maps.Key (MD_Cur).TC_Hash.all);
                        Stub.TC_Info.Name := Stub.TC_Info.TC_Hash;
                     end if;

                     Put_Opening_Comment_Section
                       (Stub, 0, True, False,
                        Current_Type.Main_Type_Text_Name.all);

                     Add_DT
                       (TP_List,
                        Test_File_Name.all & ".ads",
                        Test_File_Name.all & ".adb",
                        New_Line_Counter,
                        1);

                     for I in
                       MD.TR_Text.First_Index .. MD.TR_Text.Last_Index
                     loop
                        if MD.Commented_Out then
                           S_Put (0, MD.TR_Text.Element (I));
                        else
                           S_Put (0, "--  " & MD.TR_Text.Element (I));
                        end if;
                        New_Line_Count;
                     end loop;

                     Put_Closing_Comment_Section
                       (Stub,
                        0,
                        True,
                        False);
                     New_Line_Count;
                  end;
               end if;

               Markered_Data_Maps.Next (MD_Cur);
            end loop;

            Put_Persistent_Section (Body_Statements);

            S_Put (0, "end " & Unit_Name.all & ";");
            New_Line_Count;

            Close_File;

            Add_Buffered_TR_Slocs
              (TP_List,
               Format_Time
                 (File_Time_Stamp
                    (Tmp_File_Name)));

            if not Generate_Separates then
               declare
                  Old_Package : constant String :=
                    Output_Dir & Directory_Separator
                    & Test_File_Name.all & ".adb";
                  Success : Boolean;
               begin
                  if Is_Regular_File (Old_Package) then
                     Delete_File (Old_Package, Success);
                     if not Success then
                        Cmd_Error_No_Help ("cannot delete " & Old_Package);
                     end if;
                  end if;
                  Copy_File (Tmp_File_Name, Old_Package, Success);
                  if not Success then
                     Cmd_Error_No_Help
                       ("cannot copy tmp test package to " & Old_Package);
                  end if;
                  Delete_File (Tmp_File_Name, Success);
                  if not Success then
                     Cmd_Error_No_Help ("cannot delete tmp test package");
                  end if;
               end;
            end if;

            Markered_Data_Map.Clear;
         end if;

         Short_Names_Used.Clear;
         Shortnamed_Subps.Clear;
         Elem_Numbers.Clear;

      end loop;

      --  Simple case

      if Data.Has_Simple_Case then

         Pack_Cur := Data.Package_Data_List.First;
         loop
            exit when Pack_Cur = Package_Info_List.No_Element;

            Current_Pack := Package_Info_List.Element (Pack_Cur);

            Subp_Cur := Data.Subp_List.First;
            loop
               exit when Subp_Cur = Subp_Data_List.No_Element;

               Current_Subp := Subp_Data_List.Element (Subp_Cur);
               if Current_Subp.Nesting.all = Current_Pack.Name.all then
                  Subp_List.Append (Current_Subp);
               end if;

               Subp_Data_List.Next (Subp_Cur);
            end loop;

            if Current_Pack.Name.all = Data.Unit_Full_Name.all then
               Data_Unit_Name := new String'
                 (Current_Pack.Name.all & "." &  Test_Data_Unit_Name);
            else
               Data_Unit_Name := new String'
                 (Data.Unit_Full_Name.all & "." &
                  Test_Data_Unit_Name & "." &
                  Test_Unit_Name & "." &
                  Nesting_Difference
                    (Current_Pack.Name.all,
                     Data.Unit_Full_Name.all) &
                  "." &  Test_Data_Unit_Name);
            end if;

            Test_File_Name := new String'
              (Unit_To_File_Name (Data_Unit_Name.all));

            --  saving test data package name for further reference
            Test_Data_Package_Name := new String'(Data_Unit_Name.all);

            --  Generating simple test data package spec
            if not Is_Regular_File
              (Output_Dir & Directory_Separator & Test_File_Name.all & ".ads")
            then
               Create
                 (Output_Dir & Directory_Separator &
                  Test_File_Name.all & ".ads");

               Put_Test_Data_Header;

               if Current_Pack.Data_Kind = Instantiation then
                  S_Put
                    (0,
                     "with "
                     & Current_Pack.Generic_Containing_Package.all
                     & "."
                     & Test_Data_Unit_Name
                     & ";");
                  Put_New_Line;
                  S_Put
                    (0,
                     "with "
                     & Current_Pack.Generic_Containing_Package.all
                     & "."
                     & Test_Data_Unit_Name
                     & "."
                     & Test_Unit_Name
                     & ";");
               else
                  S_Put (0, "with AUnit.Test_Fixtures;");
               end if;
               Put_New_Line;
               Put_New_Line;
               if Current_Pack.Is_Generic then
                  S_Put (0, "generic");
                  Put_New_Line;
                  S_Put
                    (3,
                     "type GNATtest_Test_Type is new "
                     & "AUnit.Test_Fixtures.Test_Fixture");
                  Put_New_Line;
                  S_Put (5, "with private;");
                  Put_New_Line;
               end if;
               S_Put (0, "package " & Data_Unit_Name.all & " is");
               Put_New_Line;
               Put_New_Line;
               S_Put (0, GT_Marker_Begin);
               Put_New_Line;
               S_Put
                 (3,
                  "type Test is new AUnit.Test_Fixtures.Test_Fixture");
               Put_New_Line;
               S_Put (0, GT_Marker_End);
               Put_New_Line;
               S_Put (3, "with null record;");
               Put_New_Line;
               Put_New_Line;
               S_Put (3, "procedure Set_Up (Gnattest_T : in out Test);");
               Put_New_Line;
               S_Put (3, "procedure Tear_Down (Gnattest_T : in out Test);");
               Put_New_Line;
               Put_New_Line;

               if Current_Pack.Data_Kind = Instantiation then
                  S_Put (0, GT_Marker_Begin);
                  Put_New_Line;
                  S_Put
                    (3,
                     "package Gnattest_Data_Inst is new "
                     & "GNATtest_Generated.GNATtest_Standard."
                     & Current_Pack.Name.all
                     & "."
                     & Test_Data_Unit_Name
                     & " (Test);");
                  Put_New_Line;
                  S_Put
                    (3,
                     "package Gnattest_Tests_Inst is new Gnattest_Data_Inst."
                     & Test_Unit_Name
                     & ";");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                     "type New_Test is new Gnattest_Tests_Inst.Test"
                     & " with null record;");
                  Put_New_Line;
                  S_Put (0, GT_Marker_End);
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                    "procedure User_Set_Up (Gnattest_T : in out New_Test);");
                  Put_New_Line;
                  S_Put
                    (3,
                     "procedure User_Tear_Down "
                     & "(Gnattest_T : in out New_Test);");
                  Put_New_Line;
                  Put_New_Line;
               end if;

               if Current_Pack.Is_Generic then
                  S_Put
                    (3,
                    "procedure User_Set_Up (Gnattest_T : in out Test);");
                  Put_New_Line;
                  S_Put
                    (3,
                    "procedure User_Tear_Down (Gnattest_T : in out Test);");
                  Put_New_Line;
                  Put_New_Line;
               end if;

               S_Put (0, "end " & Data_Unit_Name.all & ";");
               Put_New_Line;

               Close_File;
            end if;

            if not Is_Regular_File
              (Output_Dir & Directory_Separator & Test_File_Name.all & ".adb")
            then
               Create
                 (Output_Dir & Directory_Separator &
                  Test_File_Name.all & ".adb");

               Put_Test_Data_Header;

               S_Put (0, "package body " & Data_Unit_Name.all & " is");
               Put_New_Line;
               Put_New_Line;
               if Current_Pack.Data_Kind = Declaration_Data then
                  S_Put (3, "procedure Set_Up (Gnattest_T : in out Test) is");
                  Put_New_Line;
                  if Current_Pack.Is_Generic then
                     S_Put
                       (6, "X : Test'Class renames Test'Class (Gnattest_T);");
                     Put_New_Line;
                     S_Put (3, "begin");
                     Put_New_Line;
                     S_Put (6, "User_Set_Up (X);");
                  else
                     S_Put (6, "pragma Unreferenced (Gnattest_T);");
                     Put_New_Line;
                     S_Put (3, "begin");
                     Put_New_Line;
                     S_Put (6, "null;");
                  end if;
                  Put_New_Line;
                  S_Put (3, "end Set_Up;");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3, "procedure Tear_Down (Gnattest_T : in out Test) is");
                  Put_New_Line;
                  if Current_Pack.Is_Generic then
                     S_Put
                       (6, "X : Test'Class renames Test'Class (Gnattest_T);");
                     Put_New_Line;
                     S_Put (3, "begin");
                     Put_New_Line;
                     S_Put (6, "User_Tear_Down (X);");
                  else
                     S_Put (6, "pragma Unreferenced (Gnattest_T);");
                     Put_New_Line;
                     S_Put (3, "begin");
                     Put_New_Line;
                     S_Put (6, "null;");
                  end if;
                  Put_New_Line;
                  S_Put (3, "end Tear_Down;");
                  Put_New_Line;
                  Put_New_Line;
               else
                  S_Put
                    (3,
                     "procedure Set_Up "
                     & "(Gnattest_T : in out Test) is");
                  Put_New_Line;
                  S_Put (6, "pragma Unreferenced (Gnattest_T);");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end Set_Up;");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                     "procedure Tear_Down "
                     & "(Gnattest_T : in out Test) is");
                  Put_New_Line;
                  S_Put (6, "pragma Unreferenced (Gnattest_T);");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end Tear_Down;");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                     "procedure User_Set_Up "
                     & "(Gnattest_T : in out New_Test) is");
                  Put_New_Line;
                  S_Put (6, "pragma Unreferenced (Gnattest_T);");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end User_Set_Up;");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                     "procedure User_Tear_Down "
                     & "(Gnattest_T : in out New_Test) is");
                  Put_New_Line;
                  S_Put (6, "pragma Unreferenced (Gnattest_T);");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end User_Tear_Down;");

                  Put_New_Line;
                  Put_New_Line;
               end if;

               if Current_Pack.Is_Generic then
                  S_Put
                    (3,
                     "procedure User_Set_Up "
                     & "(Gnattest_T : in out Test) is");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end User_Set_Up;");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                     "procedure User_Tear_Down "
                     & "(Gnattest_T : in out Test) is");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end User_Tear_Down;");
                  Put_New_Line;
                  Put_New_Line;
               end if;

               S_Put (0, "end " & Data_Unit_Name.all & ";");
               Put_New_Line;

               Close_File;
            end if;

            TP_Map.SetUp_Name      := new String'(Test_File_Name.all & ".adb");
            TP_Map.TearDown_Name   := new String'(Test_File_Name.all & ".adb");
            TP_Map.SetUp_Line      := 8;
            TP_Map.SetUp_Column    := 4;
            TP_Map.TearDown_Line   := 14;
            TP_Map.TearDown_Column := 4;

            Free (Test_File_Name);

            if Current_Pack.Name.all = Data.Unit_Full_Name.all then
               Unit_Name := new String'
                 (Current_Pack.Name.all & "." &
                  Test_Data_Unit_Name & "." &
                  Test_Unit_Name);
            else
               Unit_Name := new String'
                 (Data.Unit_Full_Name.all & "." &
                  Test_Data_Unit_Name & "." &
                  Test_Unit_Name & "." &
                  Nesting_Difference
                    (Current_Pack.Name.all,
                     Data.Unit_Full_Name.all) &
                  "." & Test_Data_Unit_Name & "." & Test_Unit_Name);
            end if;

            Test_File_Name := new String'(Unit_To_File_Name (Unit_Name.all));

            Actual_Test := False;

            --  Generating simple test package spec.
            Create
              (Output_Dir & Directory_Separator & Test_File_Name.all & ".ads");

            Put_Harness_Header;
            S_Put (0, GT_Marker_Begin);
            Put_New_Line;

            S_Put (0, "with Gnattest_Generated;");
            Put_New_Line;
            if Stub_Mode_ON then
               S_Put (0, "with AUnit.Test_Caller;");
               Put_New_Line;
            end if;
            Put_New_Line;
            if Current_Pack.Is_Generic then
               S_Put (0, "generic");
               Put_New_Line;

               declare
                  GP : Generic_Package;
               begin
                  GP.Name := new String'(Current_Pack.Name.all);
                  GP.Sloc := new String'
                    (Base_Name (Data.Unit_File_Name.all)
                     & ":"
                     & Trim
                       (First_Line_Number (Current_Pack.Element)'Img,
                        Both)
                     & ":"
                     & Trim
                       (First_Column_Number (Current_Pack.Element)'Img,
                        Both));
                  Update_Generic_Packages (GP);
               end;
            end if;

            S_Put (0, "package " & Unit_Name.all & " is");
            Put_New_Line;
            Put_New_Line;

            --  Declaring simple test type.
            if Current_Pack.Data_Kind = Declaration_Data then
               S_Put
                 (3,
                  "type Test is new GNATtest_Generated.GNATtest_Standard." &
                    Data_Unit_Name.all & ".Test");

            else
               S_Put
                 (3,
                  "type Test is new GNATtest_Generated.GNATtest_Standard." &
                    Data_Unit_Name.all & ".New_Test");

               Update_Generic_Packages
                 (Current_Pack.Generic_Containing_Package.all);
            end if;
            Put_New_Line;
            S_Put (3, "with null record;");
            Put_New_Line;
            Put_New_Line;

            --  Adding test routine declarations.

            if Current_Pack.Data_Kind = Declaration_Data then
               Subp_Cur := Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then

                     S_Put
                       (3,
                        "procedure "
                        & Subp_Data_List.Element
                          (Subp_Cur).Subp_Mangle_Name.all
                        & " (Gnattest_T : in out Test);");

                     Put_New_Line;
                     Print_Comment_Declaration
                       (Subp_Data_List.Element (Subp_Cur),
                        3);
                     Put_New_Line;

                     Actual_Test := True;
                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;
            end if;

            if Stub_Mode_ON then
               S_Put (3, "package Caller is new AUnit.Test_Caller (Test);");
               Put_New_Line;
               Put_New_Line;
            end if;

            S_Put (0, "end " & Unit_Name.all & ";");

            Put_New_Line;
            S_Put (0, GT_Marker_End);
            Put_New_Line;

            Close_File;

            TP_Map.TP_Name := new String'(Test_File_Name.all & ".ads");
            TP_List.Append (TP_Map);

            Reset_Line_Counter;

            --  Generating simple test package body
            if Actual_Test then

               if Generate_Separates then
                  Create
                    (Output_Dir
                     & Directory_Separator
                     & Test_File_Name.all
                     & ".adb");
                  Put_Harness_Header;
               else
                  Get_Subprograms_From_Package
                    (Output_Dir
                     & Directory_Separator
                     & Test_File_Name.all
                     & ".adb");

                  --  updating hash v2 to v2.1 and change TC hash to TC names
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                        UH.Version := new String'("2");
                        UH.Hash := new String'
                          (Subp_Data_List.Element
                             (Subp_Cur).Subp_Full_Hash.all);
                        if
                          Subp_Data_List.Element (Subp_Cur).Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Subp_Data_List.Element
                                (Subp_Cur).TC_Info.TC_Hash.all);
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);

                        if MD_Cur /= Markered_Data_Maps.No_Element then
                           MD := Markered_Data_Maps.Element (MD_Cur);

                           Free (UH.Version);
                           UH.Version := new String'(Hash_Version);
                           if UH.TC_Hash.all /= "" then
                              Free (UH.TC_Hash);
                              UH.TC_Hash := new String'
                                (Sanitize_TC_Name
                                   (Subp_Data_List.Element
                                      (Subp_Cur).TC_Info.Name.all));
                           end if;
                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;

                  --  gathering transition data
                  if Transition then
                     Subp_Cur := Subp_List.First;
                     loop
                        exit when Subp_Cur = Subp_Data_List.No_Element;

                        if
                          Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0
                        then
                           UH.Version := new String'("1");
                           UH.Hash := new String'
                             (Subp_Data_List.Element
                                (Subp_Cur).Subp_Hash_V1.all);
                           if
                             Subp_Data_List.Element (Subp_Cur).Has_TC_Info
                           then
                              UH.TC_Hash := new String'
                                (Subp_Data_List.Element
                                   (Subp_Cur).TC_Info.TC_Hash.all);
                           else
                              UH.TC_Hash := new String'("");
                           end if;

                           Current_Subp := Subp_Data_List.Element (Subp_Cur);

                           Get_Subprogram_From_Separate
                             (Output_Dir
                              & Directory_Separator
                              & Unit_To_File_Name
                                (Unit_Name.all
                                 & "."
                                 & Test_Routine_Prefix
                                 & Current_Subp.Subp_Text_Name.all
                                 & "_"
                                 & Current_Subp.Subp_Hash_V1
                                   (Current_Subp.Subp_Hash_V1'First ..
                                      Current_Subp.Subp_Hash_V1'First + 5)
                                 & (if Current_Subp.Has_TC_Info
                                   then "_" & Current_Subp.TC_Info.TC_Hash
                                     (Current_Subp.TC_Info.TC_Hash'First ..
                                        Current_Subp.TC_Info.TC_Hash'First + 5)
                                   else ""))
                              & ".adb",
                              UH,
                              Current_Subp);
                        end if;
                        Subp_Data_List.Next (Subp_Cur);
                     end loop;
                  end if;

                  --  gathering used short names
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                        UH.Version := new String'(Hash_Version);
                        UH.Hash := new String'
                          (Subp_Data_List.Element
                             (Subp_Cur).Subp_Full_Hash.all);
                        if
                          Subp_Data_List.Element (Subp_Cur).Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Subp_Data_List.Element
                                   (Subp_Cur).TC_Info.Name.all));
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);

                        if MD_Cur /= Markered_Data_Maps.No_Element then
                           MD := Markered_Data_Maps.Element (MD_Cur);
                           if MD.Short_Name_Used then
                              Short_Names_Used.Include
                                (To_Lower (MD.Short_Name.all));
                              Shortnamed_Subps.Include
                                (Current_Subp.Subp_Declaration);

                              Name_Numbers.Include
                                (To_Lower (Current_Subp.Subp_Text_Name.all),
                                 1);
                              Elem_Numbers.Include
                                (Current_Subp.Subp_Declaration, 1);
                           end if;
                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;

                  --  updating short names from markered data with hash v.1
                  --  to hash v.2.1 where possible
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                        UH.Version := new String'("1");
                        UH.Hash := new String'(Current_Subp.Subp_Hash_V1.all);

                        if
                          Current_Subp.Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Current_Subp.TC_Info.TC_Hash.all);
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);

                        if MD_Cur /= Markered_Data_Maps.No_Element then
                           MD := Markered_Data_Maps.Element (MD_Cur);

                           Markered_Data_Map.Delete (MD_Cur);
                           Free (UH.Hash);
                           UH.Hash := new String'
                             (Current_Subp.Subp_Hash_V2_1.all);
                           Free (UH.Version);
                           UH.Version := new String'(Hash_Version);
                           if UH.TC_Hash.all /= "" then
                              Free (UH.TC_Hash);
                              UH.TC_Hash := new String'
                                (Sanitize_TC_Name
                                   (Current_Subp.TC_Info.Name.all));
                           end if;
                           Markered_Data_Map.Include (UH, MD);
                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;

                  --  updating short names from markered data with hash v.2.1
                  --  to hash v.2.2 where possible and gnathering short names
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                        UH.Version := new String'("2.1");
                        UH.Hash := new String'
                          (Current_Subp.Subp_Hash_V2_1.all);

                        if
                          Current_Subp.Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Current_Subp.TC_Info.Name.all));
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);

                        if MD_Cur /= Markered_Data_Maps.No_Element then
                           MD := Markered_Data_Maps.Element (MD_Cur);

                           if not
                             Short_Names_Used.Contains (MD.Short_Name.all)
                             or else Shortnamed_Subps.Contains
                               (Current_Subp.Subp_Declaration)
                           then
                              Short_Names_Used.Include (MD.Short_Name.all);
                              Shortnamed_Subps.Include
                                (Current_Subp.Subp_Declaration);

                              Name_Numbers.Include
                                (To_Lower (Current_Subp.Subp_Text_Name.all),
                                 1);
                              Elem_Numbers.Include
                                (Current_Subp.Subp_Declaration, 1);

                              MD.Short_Name_Used := True;
                           end if;

                           Markered_Data_Map.Delete (MD_Cur);
                           Free (UH.Hash);
                           UH.Hash := new String'
                             (Current_Subp.Subp_Full_Hash.all);
                           Free (UH.Version);
                           UH.Version := new String'(Hash_Version);

                           Markered_Data_Map.Include (UH, MD);
                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;

                  --  creating markered_data and deciding on new short names
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                        UH.Version := new String'(Hash_Version);
                        UH.Hash := new String'
                          (Current_Subp.Subp_Full_Hash.all);
                        if
                          Subp_Data_List.Element (Subp_Cur).Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Current_Subp.TC_Info.Name.all));
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);

                        if MD_Cur = Markered_Data_Maps.No_Element then

                           MD.Commented_Out := False;
                           MD.Short_Name_Used := False;
                           MD.Short_Name := new String'
                             (To_Lower (Current_Subp.Subp_Text_Name.all));
                           MD.TR_Text.Clear;

                           if
                             not Short_Names_Used.Contains
                               (To_Lower (Current_Subp.Subp_Text_Name.all))
                             or else Shortnamed_Subps.Contains
                               (Current_Subp.Subp_Declaration)
                           then
                              --  Short name is free, we can use it
                              MD.Short_Name_Used := True;
                              Short_Names_Used.Include
                                (To_Lower (Current_Subp.Subp_Text_Name.all));
                              Shortnamed_Subps.Include
                                (Current_Subp.Subp_Declaration);

                              Name_Numbers.Include
                                (To_Lower (Current_Subp.Subp_Text_Name.all),
                                 1);
                              Elem_Numbers.Include
                                (Current_Subp.Subp_Declaration, 1);

                              --  Looking for a dangling test with same short
                              --  name but different hash.
                              MD_Cur := Find_Same_Short_Name
                                (Markered_Data_Map,
                                 Current_Subp);

                              if MD_Cur /= Markered_Data_Maps.No_Element then
                                 --  Using corresponding dangling test

                                 MD.TR_Text.Clear;
                                 MD.TR_Text :=
                                   Markered_Data_Maps.Element (MD_Cur).TR_Text;

                                 --  also need to copy Commented_Out since
                                 --  the test can be dangling for a long time
                                 --  or just become dangling
                                 MD.Commented_Out :=
                                   Markered_Data_Maps.Element
                                     (MD_Cur).Commented_Out;

                                 Markered_Data_Map.Delete (MD_Cur);
                                 MD.Issue_Warning := True;
                              end if;

                           end if;

                           Markered_Data_Map.Insert (UH, MD);

                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;

                  --  setting overloading numbers;
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then

                        if
                          Name_Numbers.Find
                            (To_Lower (Current_Subp.Subp_Text_Name.all)) =
                            Name_Frequency.No_Element
                        then

                           Name_Numbers.Include
                             (To_Lower (Current_Subp.Subp_Text_Name.all), 1);
                           Elem_Numbers.Include
                             (Current_Subp.Subp_Declaration, 1);

                        else
                           if
                             Elem_Numbers.Find
                               (Current_Subp.Subp_Declaration) =
                               Elem_Number_Maps.No_Element
                           then
                              declare
                                 X : constant Natural :=
                                   Name_Numbers.Element
                                     (To_Lower
                                          (Current_Subp.Subp_Text_Name.all));
                              begin
                                 Name_Numbers.Replace
                                   (To_Lower (Current_Subp.Subp_Text_Name.all),
                                    X + 1);
                                 Elem_Numbers.Include
                                   (Current_Subp.Subp_Declaration, X + 1);
                              end;
                           end if;
                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;
                  Name_Numbers.Clear;

                  Create (Tmp_File_Name);
                  Put_TP_Header (Test_Data_Package_Name.all);
               end if;

               S_Put (0, "with AUnit.Assertions; use AUnit.Assertions;");
               New_Line_Count;
               S_Put (0, "with System.Assertions;");
               New_Line_Count;
               if Test.Common.Generate_Test_Vectors
                 and then not Data.Is_Generic
               then
                  S_Put (0, "with Ada.Exceptions;");
                  New_Line_Count;
               end if;
               if Stub_Mode_ON then
                  Put_Stub_Data_Import;
               end if;
               New_Line_Count;

               Put_Persistent_Section (With_Clauses);

               S_Put (0, "package body " & Unit_Name.all & " is");
               New_Line_Count;
               New_Line_Count;

               Put_Persistent_Section (Body_Declarations);

               --  Adding test routine body stubs.
               Subp_Cur := Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Has_TC_Info then
                        if Is_Function
                          (Subp_Data_List.Element
                             (Subp_Cur).Subp_Declaration.As_Basic_Decl)
                        then
                           Generate_Function_Wrapper
                             (Subp_Data_List.Element (Subp_Cur));
                        else
                           Generate_Procedure_Wrapper
                             (Subp_Data_List.Element (Subp_Cur));
                        end if;
                     end if;

                     if Generate_Separates then
                        S_Put
                          (3,
                           "procedure "
                           & Subp_Data_List.Element
                             (Subp_Cur).Subp_Mangle_Name.all
                           & " (Gnattest_T : in out Test) is separate;");

                        New_Line_Count;
                        Print_Comment_Declaration
                          (Subp_Data_List.Element (Subp_Cur), 3);
                        New_Line_Count;

                     else

                        Test_Info.Replace
                          (Data.Unit_File_Name.all,
                           Test_Info.Element (Data.Unit_File_Name.all) + 1);

                        All_Tests_Counter := All_Tests_Counter + 1;

                        UH.Version := new String'(Hash_Version);
                        UH.Hash := new String'
                          (Subp_Data_List.Element
                             (Subp_Cur).Subp_Full_Hash.all);
                        if Subp_Data_List.Element (Subp_Cur).Has_TC_Info then
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Subp_Data_List.Element
                                   (Subp_Cur).TC_Info.Name.all));
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);
                        MD := Markered_Data_Maps.Element (MD_Cur);

                        Put_Opening_Comment_Section
                          (Subp_Data_List.Element (Subp_Cur),
                           Elem_Numbers.Element
                             (Current_Subp.Subp_Declaration),
                           Use_Short_Name => MD.Short_Name_Used);

                        if Is_Unimplemented_Test (MD.TR_Text) then
                           TR_SLOC_Buffer.Append
                             ((new String'(Test_File_Name.all & ".ads"),
                              new String'(Test_File_Name.all & ".adb"),
                              null,
                              Subp_Data_List.Element (Subp_Cur),
                              New_Line_Counter));
                        else
                           TR_SLOC_Buffer.Append
                             ((new String'(Test_File_Name.all & ".ads"),
                              new String'(Test_File_Name.all & ".adb"),
                              new String'("modified"),
                              Subp_Data_List.Element (Subp_Cur),
                              New_Line_Counter));
                        end if;

                        if MD.TR_Text.Is_Empty then

                           if Stub_Mode_ON then
                              Setters_Set := Get_Direct_Callees_Setters
                                (Current_Subp.Subp_Declaration.As_Basic_Decl);
                           end if;

                           New_Tests_Counter := New_Tests_Counter + 1;
                           New_Line_Count;
                           S_Put (6, "pragma Unreferenced (Gnattest_T);");
                           New_Line_Count;
                           New_Line_Count;
                           S_Put (3, "begin");
                           New_Line_Count;
                           New_Line_Count;
                           if not Setters_Set.Is_Empty then
                              Set_Cur := Setters_Set.First;
                              while Set_Cur /= String_Set.No_Element loop
                                 S_Put
                                   (3,
                                    "--  "
                                    & String_Set.Element (Set_Cur)
                                    & "( );");
                                 New_Line_Count;
                                 Next (Set_Cur);
                              end loop;
                              New_Line_Count;
                              Setters_Set.Clear;
                           end if;
                           if Test.Common.Generate_Test_Vectors
                             and then not Data.Is_Generic
                           then
                              Unparse_Success := Unparse_Test_Vectors
                                (Data, Current_Subp);
                           end if;
                           if not Unparse_Success then
                              if Test.Common.Generate_Test_Vectors
                                and then not Data.Is_Generic
                                and then not Is_Null
                                  (Current_Subp.Subp_Declaration.As_Basic_Decl
                                   .P_Subp_Spec_Or_Null)
                                and then Current_Subp.Subp_Declaration
                                         .As_Basic_Decl.P_Subp_Spec_Or_Null
                                         .P_Params'Length /= 0
                              then
                                 Report_Std
                                   ("Warning: (TGen) "
                                    & Base_Name (Data.Unit_File_Name.all)
                                    & " : Could not generate test"
                                    & " vectors for "
                                    & Current_Subp.Subp_Text_Name.all);
                              end if;
                              S_Put (6, "AUnit.Assertions.Assert");
                              New_Line_Count;
                              S_Put
                              (8, "(Gnattest_Generated.Default_Assert_Value,");
                              New_Line_Count;
                              S_Put (9,  """Test not implemented."");");
                              New_Line_Count;
                              New_Line_Count;
                           end if;
                        else

                           if MD.Issue_Warning then
                              Report_Std
                                ("warning: (gnattest) "
                                 & Base_Name (Data.Unit_File_Name.all)
                                 & ":"
                                 & Trim
                                   (First_Line_Number
                                    (Current_Subp.Subp_Declaration)'Img,
                                    Both)
                                 & ":"
                                 & Trim
                                   (First_Column_Number
                                    (Current_Subp.Subp_Declaration)'Img,
                                    Both)
                                 & ": test for "
                                 & MD.Short_Name.all
                                 & " at "
                                 & Unit_Name.all
                                 & ":"
                                 & Trim
                                   (Integer'Image (New_Line_Counter),
                                    Both)
                                 & " might be out of date ("
                                 & MD.Short_Name.all
                                 & " has been changed)");
                           end if;

                           for I in
                             MD.TR_Text.First_Index .. MD.TR_Text.Last_Index
                           loop
                              if MD.Commented_Out then
                                 S_Put
                                   (0,
                                    Uncomment_Line (MD.TR_Text.Element (I)));
                              else
                                 S_Put (0, MD.TR_Text.Element (I));
                              end if;
                              New_Line_Count;
                           end loop;
                        end if;

                        Markered_Data_Map.Delete (MD_Cur);

                        Put_Closing_Comment_Section
                          (Subp_Data_List.Element (Subp_Cur),
                           Elem_Numbers.Element
                             (Current_Subp.Subp_Declaration),
                           Use_Short_Name => MD.Short_Name_Used);
                        New_Line_Count;

                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  printing dangling tests

               if not Markered_Data_Map_Is_Empty then
                  Report_Std
                    (" warning: (gnattest) "
                     & Unit_Name.all
                     & " has dangling test(s)");
               end if;

               MD_Cur := Markered_Data_Map.First;
               loop
                  exit when MD_Cur = Markered_Data_Maps.No_Element;

                  MD := Markered_Data_Maps.Element (MD_Cur);

                  if Markered_Data_Maps.Key (MD_Cur).Hash.all /= "02" then
                     declare
                        Stub : Subp_Info;
                     begin

                        Stub.Subp_Full_Hash := new String'
                          (Markered_Data_Maps.Key (MD_Cur).Hash.all);
                        Stub.Subp_Text_Name := new String'
                          (MD.Short_Name.all);

                        if Markered_Data_Maps.Key (MD_Cur).TC_Hash.all = ""
                        then
                           Stub.Has_TC_Info := False;

                           Stub.Subp_Mangle_Name := new String'
                             (Test_Routine_Prefix
                              & Markered_Data_Maps.Element
                                (MD_Cur).Short_Name.all
                              & "_"
                              & Stub.Subp_Full_Hash
                                (Stub.Subp_Full_Hash'First ..
                                     Stub.Subp_Full_Hash'First + 5));

                        else
                           Stub.Has_TC_Info := True;
                           Stub.TC_Info.TC_Hash := new String'
                             (Markered_Data_Maps.Key (MD_Cur).TC_Hash.all);

                           Stub.TC_Info.Name := Stub.TC_Info.TC_Hash;
                           Stub.Subp_Mangle_Name := new String'
                             (Test_Routine_Prefix
                              & Markered_Data_Maps.Element
                                (MD_Cur).Short_Name.all
                              & "_"
                              & Stub.Subp_Full_Hash
                                (Stub.Subp_Full_Hash'First ..
                                     Stub.Subp_Full_Hash'First + 5)
                              & "_"
                              & Stub.TC_Info.TC_Hash.all);
                        end if;

                        Put_Opening_Comment_Section
                          (Stub, 0, True, MD.Short_Name_Used);

                        Add_DT
                          (TP_List,
                           Test_File_Name.all & ".ads",
                           Test_File_Name.all & ".adb",
                           New_Line_Counter,
                           1);

                        for I in
                          MD.TR_Text.First_Index .. MD.TR_Text.Last_Index
                        loop
                           if MD.Commented_Out then
                              S_Put (0, MD.TR_Text.Element (I));
                           else
                              S_Put (0, "--  " & MD.TR_Text.Element (I));
                           end if;
                           New_Line_Count;
                        end loop;

                        Put_Closing_Comment_Section
                          (Stub, 0, True, MD.Short_Name_Used);
                        New_Line_Count;
                     end;
                  end if;

                  Markered_Data_Maps.Next (MD_Cur);
               end loop;

               Put_Persistent_Section (Body_Statements);

               S_Put (0, "end " & Unit_Name.all & ";");
               New_Line_Count;

               Close_File;

               Add_Buffered_TR_Slocs
                 (TP_List,
                  Format_Time
                    (File_Time_Stamp
                       (Tmp_File_Name)));

               if not Generate_Separates then
                  declare
                     Old_Package : constant String :=
                       Output_Dir & Directory_Separator
                       & Test_File_Name.all & ".adb";
                     Success : Boolean;
                  begin
                     if Is_Regular_File (Old_Package) then
                        Delete_File (Old_Package, Success);
                        if not Success then
                           Cmd_Error_No_Help ("cannot delete " & Old_Package);
                        end if;
                     end if;
                     Copy_File (Tmp_File_Name, Old_Package, Success);
                     if not Success then
                        Cmd_Error_No_Help
                          ("cannot copy tmp test package to " & Old_Package);
                     end if;
                     Delete_File (Tmp_File_Name, Success);
                     if not Success then
                        Cmd_Error_No_Help ("cannot delete tmp test package");
                     end if;
                  end;
               end if;

               Markered_Data_Map.Clear;

            else
               Excluded_Test_Package_Bodies.Include
                 (Test_File_Name.all & ".adb");
            end if;

            Short_Names_Used.Clear;
            Shortnamed_Subps.Clear;
            Elem_Numbers.Clear;
            Subp_List.Clear;
            Package_Info_List.Next (Pack_Cur);
         end loop;

      end if;

      Add_Test_List (Data.Unit_File_Name.all, TP_List);
      TP_List.Clear;

      if Data.Is_Generic then
         Gen_Tests_Storage.Append (Gen_Tests);
      end if;

   end Generate_Test_Package;

   ------------
   -- Add_DT --
   ------------

   procedure Add_DT
     (TP_List : in out TP_Mapping_List.List;
      TPtarg  : String;
      Test_F  : String;
      Line    : Natural;
      Column  : Natural)
   is
      TP : TP_Mapping;
      TD : DT_Mapping;

      TP_Cur : TP_Mapping_List.Cursor := TP_List.First;
   begin

      TD.File := new String'(Test_F);
      TD.Line := Line;
      TD.Column := Column;

      loop
         exit when TP_Cur = TP_Mapping_List.No_Element;

         if TP_Mapping_List.Element (TP_Cur).TP_Name.all = TPtarg then
            exit;
         end if;

         TP_Mapping_List.Next (TP_Cur);
      end loop;

      TP := TP_Mapping_List.Element (TP_Cur);
      TP.DT_List.Append (TD);
      TP_List.Replace_Element (TP_Cur, TP);

   end Add_DT;

   ------------
   -- Add_TR --
   ------------

   procedure Add_TR
     (TP_List : in out TP_Mapping_List.List;
      TPtarg  : String;
      Test_F  : String;
      Test_T  : String;
      Subp    : Subp_Info;
      TR_Line : Natural := 1)
   is
      TC : TC_Mapping;
      TR : TR_Mapping;
      TP : TP_Mapping;

      TR_Cur : TR_Mapping_List.Cursor;
      TP_Cur : TP_Mapping_List.Cursor := TP_List.First;

      Subp_Span : constant Source_Location_Range :=
        Subp.Subp_Declaration.As_Basic_Decl.P_Defining_Name.Sloc_Range;
      TC_Span   : constant Source_Location_Range := No_Source_Location_Range;
   begin

      loop
         exit when TP_Cur = TP_Mapping_List.No_Element;

         if TP_Mapping_List.Element (TP_Cur).TP_Name.all = TPtarg then
            exit;
         end if;

         TP_Mapping_List.Next (TP_Cur);
      end loop;

      if TP_Cur = TP_Mapping_List.No_Element then
         TP.TP_Name := new String'(TPtarg);
         TR.TR_Name := new String'(Subp.Subp_Text_Name.all);
         TR.Line := Natural (Subp_Span.Start_Line);
         TR.Column := Natural (Subp_Span.Start_Column);
         if Subp.Has_TC_Info then
            TC.T_Name := new String'(Subp.Subp_Mangle_Name.all);
            TC.TC_Name := new String'(Subp.TC_Info.Name.all);
            TC.Line := Natural (TC_Span.Start_Line);
            TC.Column := Natural (TC_Span.Start_Column);
            TC.Test := new String'(Test_F);
            TC.Test_Time := new String'(Test_T);
            TC.TR_Line := TR_Line;
            TR.TC_List.Append (TC);
         else
            TR.Test := new String'(Test_F);
            TR.Test_Time := new String'(Test_T);
            TR.TR_Line := TR_Line;
            TR.T_Name := new String'(Subp.Subp_Mangle_Name.all);
         end if;

         TP.TR_List.Append (TR);
         TP_List.Append (TP);

         return;
      end if;

      TP := TP_Mapping_List.Element (TP_Cur);

      TR_Cur := TP.TR_List.First;
      loop
         exit when TR_Cur = TR_Mapping_List.No_Element;

         if
           TR_Mapping_List.Element (TR_Cur).Line =
           Natural (Subp_Span.Start_Line) and then
           TR_Mapping_List.Element (TR_Cur).Column =
           Natural (Subp_Span.Start_Column)
         then
            exit;
         end if;

         TR_Mapping_List.Next (TR_Cur);
      end loop;

      if TR_Cur = TR_Mapping_List.No_Element then

         TR.TR_Name := new String'(Subp.Subp_Text_Name.all);
         TR.Line := Natural (Subp_Span.Start_Line);
         TR.Column := Natural (Subp_Span.Start_Column);
         if Subp.Has_TC_Info then
            TC.T_Name := new String'(Subp.Subp_Mangle_Name.all);
            TC.TC_Name := new String'(Subp.TC_Info.Name.all);
            TC.Line := Natural (TC_Span.Start_Line);
            TC.Column := Natural (TC_Span.Start_Column);
            TC.Test := new String'(Test_F);
            TC.Test_Time := new String'(Test_T);
            TC.TR_Line := TR_Line;
            TR.TC_List.Append (TC);
         else
            TR.Test := new String'(Test_F);
            TR.Test_Time := new String'(Test_T);
            TR.TR_Line := TR_Line;
            TR.T_Name := new String'(Subp.Subp_Mangle_Name.all);
         end if;

         TP.TR_List.Append (TR);
         TP_List.Replace_Element (TP_Cur, TP);

         return;
      end if;

      TR := TR_Mapping_List.Element (TR_Cur);

      --  The only way that there is same subprogram already is when it has
      --  test_cases. So no need to check if it has TC_Info.
      TC.T_Name := new String'(Subp.Subp_Mangle_Name.all);
      TC.TC_Name := new String'(Subp.TC_Info.Name.all);
      TC.Line := Natural (TC_Span.Start_Line);
      TC.Column := Natural (TC_Span.Start_Column);
      TC.Test := new String'(Test_F);
      TC.Test_Time := new String'(Test_T);
      TC.TR_Line := TR_Line;
      TR.TC_List.Append (TC);

      TP.TR_List.Replace_Element (TR_Cur, TR);
      TP_List.Replace_Element (TP_Cur, TP);

   end Add_TR;

   -------------------------------
   -- Print_Comment_Declaration --
   -------------------------------

   procedure Print_Comment_Declaration (Subp : Subp_Info; Span : Natural := 0)
   is
      File_Name : constant String := Base_Name
        (Subp.Subp_Declaration.Unit.Get_Filename);

      Elem_Span : constant Source_Location_Range :=
        Subp.Subp_Declaration.Sloc_Range;
   begin
      if Omit_Sloc then
         return;
      end if;
      S_Put
        (Span,
         "--  " &
         File_Name &
         ":" &
         Trim (Elem_Span.Start_Line'Img, Both) &
         ":" &
         Trim (Elem_Span.Start_Column'Img, Both) &
         ":" &
         Subp.Subp_Text_Name.all);
      if Subp.Has_TC_Info then
         S_Put (0, ":" & Subp.TC_Info.Name.all);
      end if;
      New_Line_Count;
   end Print_Comment_Declaration;

   --------------------------------
   -- Get_Direct_Callees_Setters --
   --------------------------------

   function Get_Direct_Callees_Setters
     (Subp : Basic_Decl) return String_Set.Set
   is
      Result : String_Set.Set;

      function Get_Callees (Node : Ada_Node'Class) return Visit_Status;
      --  Traverses subprogram body in search for callees

      function Get_Callees (Node : Ada_Node'Class) return Visit_Status is
         Decl : Basic_Decl;
      begin
         --  P_Is_Call may occasionally crash on some constructs, commented out
         --  setter suggestions are not important enough to keep the tool
         --  exposed to possible crashes, so instead we just skip such cases
         --  and issue error data in the traces.
         begin
            if (Node.Kind = Ada_Identifier
                 and then Node.As_Name.P_Is_Call)
              or else Node.Kind in Ada_Op
            then
               Decl := Node.As_Name.P_Referenced_Decl;

               if Decl.Is_Null then
                  return Over;
               end if;
            else
               return Into;
            end if;

         exception
            when Ex : Langkit_Support.Errors.Property_Error =>
               Trace (Me_Direct_Callees,
                      "Error while processing" & Node.Image);
               Trace (Me_Direct_Callees,
                      Ada.Exceptions.Exception_Name (Ex)
                      & " : "
                      & Ada.Exceptions.Exception_Message (Ex));
               return Over;
         end;

         if Decl.Unit = Node.Unit or else Decl.Unit = Subp.Unit then
            --  Callee is from the same unit spec or even from the body,
            --  it won't be stubbed.
            return Over;
         end if;

         if not Source_Present (Decl.Unit.Get_Filename) then
            --
            return Over;
         end if;

         --  Process simple cases for now. Dispatchings, renamings and parts of
         --  instances are not yet supported.

         if Decl.Kind in Ada_Generic_Subp_Instantiation | Ada_Formal_Subp_Decl
           | Ada_Subp_Renaming_Decl | Ada_Enum_Literal_Decl | Ada_Entry_Decl
           | Ada_Null_Subp_Decl | Ada_Subp_Body | Ada_Subp_Body_Stub
         then
            return Over;
         end if;

         for Parent of Decl.Parents loop
            if Parent.Kind = Ada_Generic_Package_Decl then
               return Over;
            end if;
         end loop;

         if Decl.Parent.Kind = Ada_Library_Item then
            --  Library level supprograms are not stubbed
            return Over;
         end if;

         declare
            Suffix : constant String :=
              "_"
              & Head (Mangle_Hash_Full (Decl), 6)
              & "_"
              & Head (GNAT.SHA1.Digest (Get_Nesting (Decl)), 6);
         begin
            Result.Include
              (Get_Nesting (Decl)
               & "."
               & Stub_Data_Unit_Name
               & "."
               & Setter_Prefix
               & Get_Subp_Name (Decl)
               & Suffix);
         end;

         return Into;
      end Get_Callees;

   begin
      Increase_Indent
        (Me_Direct_Callees,
         "Gathering direct callees for " & Subp.Image);

      if Subp.Kind = Ada_Expr_Function then
         Traverse (Subp.As_Expr_Function.F_Expr, Get_Callees'Access);
      elsif Subp.Kind = Ada_Subp_Decl
        and then not Subp.As_Subp_Decl.P_Body_Part.Is_Null
      then
         Traverse (Subp.As_Subp_Decl.P_Body_Part, Get_Callees'Access);
      end if;

      Trace
        (Me_Direct_Callees,
         "Direct callees gathered");
      Decrease_Indent;
      return Result;
   end Get_Direct_Callees_Setters;

   ----------------------------------
   -- Get_Subprogram_From_Separate --
   ----------------------------------

   procedure Get_Subprogram_From_Separate
     (File : String;
      UH   : Unique_Hash;
      Subp : Subp_Info)
   is
      Input_File : Ada.Text_IO.File_Type;
      MD : Markered_Data;
      Line : String_Access;
      Append_Line : Boolean;

      use GNAT.OS_Lib;
   begin
      if not Is_Regular_File (File) then
         return;
      end if;

      MD.Commented_Out := False;
      MD.TR_Text := String_Vectors.Empty_Vector;
      MD.Short_Name := new String'(Subp.Subp_Text_Name.all);

      Open (Input_File, In_File, File);

      loop
         exit when End_Of_File (Input_File);
         Line := new String'(Get_Line (Input_File));
         Append_Line := True;

         if To_Lower (Line.all) = "with gnattest_generated;" then
            Append_Line := False;
         end if;

         --  skipping test routine profile up to declaration section;
         --  depending on line breaks it can take different number of lines
         if Index (To_Lower (Line.all), "separate", Line'First) /= 0 then
            loop
               if
                 Index (To_Lower (Line.all), ") is", Line'First) /= 0
                 or else Trim (To_Lower (Line.all), Both) = "is"
               then
                  Append_Line := False;
                  exit;
               else
                  Free (Line);
                  Line := new String'(Get_Line (Input_File));
               end if;
            end loop;
         end if;

         --  skipping "end test_outine_name;"
         if
           Index
             (To_Lower (Line.all),
              "end "
              & To_Lower
                (Test_Routine_Prefix
                 & Subp.Subp_Text_Name.all
                 & "_"
                 & Subp.Subp_Hash_V1
                   (Subp.Subp_Hash_V1'First .. Subp.Subp_Hash_V1'First + 5))
              & ";",
              Line'First) /= 0
         then
            Append_Line := False;
         end if;

         if Append_Line then
            MD.TR_Text.Append (Line.all);
         end if;

         Free (Line);
      end loop;

      Close (Input_File);

      if Find (Markered_Data_Map, UH) = Markered_Data_Maps.No_Element then
         Markered_Data_Map.Insert (UH, MD);
      else
         Markered_Data_Map.Replace (UH, MD);
      end if;

   end Get_Subprogram_From_Separate;

   ----------------------------------
   -- Get_Subprograms_From_Package --
   ----------------------------------

   procedure Get_Subprograms_From_Package (File : String) is

      Input_File : Ada.Text_IO.File_Type;

      Line_Counter : Natural := 0;

      Line : String_Access;

      Idx, Idx2 : Natural;

      UH : Unique_Hash;
      MD : Markered_Data;

      ID_Found : Boolean;

      type Parsing_Modes is (TR, Marker, Other);

      Parsing_Mode      : Parsing_Modes := Other;
      Prev_Parsing_Mode : Parsing_Modes := Other;

      procedure Report_Corrupted_Marker;
      pragma Unreferenced (Report_Corrupted_Marker);

      procedure Report_Corrupted_Marker is
      begin
         Report_Err
           ("marker corrupted at "
            & Base_Name (File)
            & ":"
            & Natural'Image (Line_Counter));
      end Report_Corrupted_Marker;

      use GNAT.OS_Lib;
   begin

      if not Is_Regular_File (File) then
         return;
      end if;

      MD.Commented_Out   := False;
      MD.Short_Name_Used := False;
      MD.TR_Text := String_Vectors.Empty_Vector;
      UH.Hash    := new String'("");
      UH.TC_Hash := new String'("");

      Open (Input_File, In_File, File);

      loop
         exit when End_Of_File (Input_File);

         Line := new String'(Get_Line (Input_File));
         Line_Counter := Line_Counter + 1;

         case Parsing_Mode is
            when Other =>
               if Index (Line.all, GT_Marker_Begin) /= 0 then
                  Parsing_Mode := Marker;
                  Prev_Parsing_Mode := Other;
                  ID_Found := False;
               end if;

            when Marker =>

               Idx := Index (Line.all, "--  id:");
               if Idx /= 0 then
                  ID_Found := True;

                  Idx  := Idx + 7;
                  Idx2 := Index (Line.all, "/", Idx + 1);
                  UH.Version := new String'(Line (Idx .. Idx2 - 1));

                  Idx  := Idx2 + 1;
                  Idx2 := Index (Line.all, "/", Idx + 1);
                  UH.Hash := new String'(Line (Idx .. Idx2 - 1));

                  Idx  := Idx2 + 1;
                  Idx2 := Index (Line.all, "/", Idx + 1);
                  MD.Short_Name := new String'(Line (Idx .. Idx2 - 1));

                  Idx  := Idx2 + 1;
                  Idx2 := Index (Line.all, "/", Idx + 1);
                  if Line (Idx .. Idx2 - 1) = "1" then
                     MD.Short_Name_Used := True;
                  else
                     MD.Short_Name_Used := False;
                  end if;

                  Idx  := Idx2 + 1;
                  Idx2 := Index (Line.all, "/", Idx + 1);
                  if Line (Idx .. Idx2 - 1) = "1" then
                     MD.Commented_Out := True;
                  else
                     MD.Commented_Out := False;
                  end if;

                  if Idx2 < Line'Last then

                     Idx  := Idx2 + 1;
                     Idx2 := Index (Line.all, "/", Idx + 1);
                     UH.TC_Hash := new String'(Line (Idx .. Idx2 - 1));

                  end if;

               else

                  if Index (Line.all, GT_Marker_End) /= 0 then
                     if Prev_Parsing_Mode = Other then
                        if ID_Found then
                           Parsing_Mode := TR;
                        else
                           Parsing_Mode := Other;
                        end if;
                     end if;
                     if Prev_Parsing_Mode = TR then
                        Parsing_Mode := Other;
                     end if;
                  end if;

               end if;

            when TR =>

               if Index (Line.all, GT_Marker_Begin) /= 0 then
                  Markered_Data_Map.Include (UH, MD);
                  Prev_Parsing_Mode := TR;
                  Parsing_Mode := Marker;

                  MD.Commented_Out   := False;
                  MD.Short_Name_Used := False;
                  MD.TR_Text := String_Vectors.Empty_Vector;
                  UH.Hash    := new String'("");
                  UH.TC_Hash := new String'("");
               else
                  MD.TR_Text.Append (Line.all);
               end if;

         end case;

      end loop;

      Close (Input_File);
   end Get_Subprograms_From_Package;

   -----------------------
   -- Get_Units_To_Stub --
   -----------------------

   procedure Get_Units_To_Stub
     (The_Unit :        Compilation_Unit;
      Data     : in out Data_Holder)
   is
      Body_N : Body_Node;
      Body_Unit   : Compilation_Unit;

      Parent : Ada_Node;

      Already_Stubbing : String_Set.Set := String_Set.Empty_Set;
      --  It is generally easier to store units to stub in a list, however
      --  to avoid duplications we use this local set since it is easier
      --  and faster to check membership in a set.

      function Good_To_Stub (Check_Unit : Analysis_Unit) return Boolean;
      --  Checks that given unit is suitable for stubbing

      procedure Add_Units_To_Stub (The_Unit : Compilation_Unit);
      --  Adds units from with clauses and parent units to the list of
      --  units to stub.

      procedure Iterate_Separates (The_Unit : Compilation_Unit);
      --  Looks for inuts withed in separate bodies

      -----------------------
      -- Add_Units_To_Stub --
      -----------------------

      procedure Add_Units_To_Stub (The_Unit : Compilation_Unit)
      is
         Clauses : constant Ada_Node_List := The_Unit.F_Prelude;
      begin
         for Cl of Clauses loop
            if
              Cl.Kind = Ada_With_Clause
              and then not Cl.As_With_Clause.F_Has_Limited
            then
               declare
                  With_Names : constant Name_List :=
                    Cl.As_With_Clause.F_Packages;

                  Withed_Spec : Basic_Decl;

                  Parent_Unit : Ada_Node;
               begin
                  for WN of With_Names loop
                     Withed_Spec := WN.As_Name.P_Referenced_Decl;

                     if not Withed_Spec.Is_Null then
                        declare
                           Withed_Spec_Image : constant String :=
                             Withed_Spec.Unit.Get_Filename;
                        begin
                           if Good_To_Stub (Withed_Spec.Unit)
                             and then not Already_Stubbing.Contains
                               (Withed_Spec_Image)
                           then
                              Already_Stubbing.Include (Withed_Spec_Image);
                              Data.Units_To_Stub.Append
                                (Withed_Spec.As_Ada_Node);
                              Trace (Me, Withed_Spec_Image);
                           end if;
                        end;

                        --  Gathering parent packages
                        Parent_Unit := Withed_Spec.P_Semantic_Parent;
                        while
                        not Parent_Unit.Is_Null and then
                          Parent_Unit.Unit /= Parent_Unit.P_Standard_Unit
                        loop
                           if Parent_Unit.Kind = Ada_Package_Decl then
                              declare
                                 Parent_File : constant String :=
                                   Parent_Unit.Unit.Get_Filename;
                              begin
                                 if Good_To_Stub (Parent_Unit.Unit)
                                   and then not Already_Stubbing.Contains
                                     (Parent_File)
                                 then
                                    Already_Stubbing.Include (Parent_File);
                                    Data.Units_To_Stub.Append (Parent_Unit);
                                    Trace (Me, Parent_File);
                                 end if;
                              end;
                           end if;
                           Parent_Unit := Parent_Unit.P_Semantic_Parent;
                        end loop;
                     end if;
                  end loop;
               end;
            end if;
         end loop;

      end Add_Units_To_Stub;

      -----------------------
      -- Iterate_Separates --
      -----------------------

      procedure Iterate_Separates (The_Unit : Compilation_Unit) is
         Bod : Ada_Node;

         function Find (Node : Ada_Node'Class) return Visit_Status;
         function Find (Node : Ada_Node'Class) return Visit_Status is
            Separate_A_Unit : Analysis_Unit;
            Separate_C_Unit : Compilation_Unit;
         begin
            if Node.Kind in Ada_Body_Stub then
               Separate_A_Unit := Node.As_Basic_Decl.P_Next_Part_For_Decl.Unit;
               Separate_C_Unit := Separate_A_Unit.Root.As_Compilation_Unit;
               Add_Units_To_Stub (Separate_C_Unit);
               Iterate_Separates (Separate_C_Unit);
            end if;
            return Into;
         end Find;
      begin

         if The_Unit.F_Body.Kind = Ada_Library_Item then
            Bod := The_Unit.F_Body.As_Library_Item.F_Item.As_Ada_Node;
         else
            Bod := The_Unit.F_Body.As_Subunit.F_Body.As_Ada_Node;
         end if;

         Traverse (Bod, Find'Access);
      end Iterate_Separates;

      ------------------
      -- Good_To_Stub --
      ------------------

      function Good_To_Stub (Check_Unit : Analysis_Unit) return Boolean is
         File_Name     : constant String :=
           Base_Name (Check_Unit.Get_Filename);
         Arg_File_Name : constant String :=
           Base_Name (The_Unit.Unit.Get_Filename);
         Lib_Item : constant Library_Item :=
           Check_Unit.Root.As_Compilation_Unit.F_Body.As_Library_Item;
      begin
         if not Source_Present (Check_Unit.Get_Filename) then
            return False;
         end if;
         if Check_Unit = The_Unit.Unit then
            --  No self stubbing
            return False;
         end if;

         if Lib_Item.F_Item.Kind /= Ada_Package_Decl then
            --  Only packages are stubbed
            return False;
         end if;

         if Lib_Item.F_Item.As_Basic_Decl.P_Has_Aspect
           (To_Unbounded_Text (To_Text ("Remote_Call_Interface")))
         then
            return False;
         end if;

         if Default_Stub_Exclusion_List.Contains (File_Name) then
            return False;
         end if;

         if Stub_Exclusion_Lists.Contains (Arg_File_Name) then
            if
              Stub_Exclusion_Lists.Element (Arg_File_Name).Contains (File_Name)
            then
               return False;
            end if;
         end if;
         return True;
      end Good_To_Stub;

   begin
      Trace
        (Me,
         "units to stub for "
         & Base_Name (The_Unit.Unit.Get_Filename));
      Increase_Indent (Me);

      --  Gathering with clauses from spec
      Add_Units_To_Stub (The_Unit);

      Body_N :=
        The_Unit.F_Body.As_Library_Item.F_Item.As_Basic_Decl.
          P_Body_Part_For_Decl;

      --  Gathering with clauses from body
      if Body_N /= No_Body_Node
        and then Body_N.Unit.Root.Kind = Ada_Compilation_Unit
      then
         Body_Unit := Body_N.Unit.Root.As_Compilation_Unit;
         Add_Units_To_Stub (Body_Unit);
         Iterate_Separates (Body_Unit);
      end if;

      --  Gathering parent packages
      Parent :=
        The_Unit.F_Body.As_Library_Item.F_Item.As_Ada_Node.P_Semantic_Parent;
      while
        not Parent.Is_Null and then Parent.Unit /= Parent.P_Standard_Unit
      loop
         if Parent.Kind = Ada_Package_Decl then
            declare
               Parent_File : constant String := Parent.Unit.Get_Filename;
            begin
               if Good_To_Stub (Parent.Unit)
                 and then not Already_Stubbing.Contains (Parent_File)
               then
                  Already_Stubbing.Include (Parent_File);
                  Data.Units_To_Stub.Append (Parent);
                  Trace (Me, Parent_File);
               end if;
            end;
         end if;

         Parent := Parent.P_Semantic_Parent;
      end loop;

      Decrease_Indent (Me);
      Already_Stubbing.Clear;
   end Get_Units_To_Stub;

   ----------------------
   -- Sanitize_TC_Name --
   ----------------------

   function Sanitize_TC_Name (TC_Name : String) return String
   is
      Name : String := Trim (TC_Name, Both);

      Tmp  : String_Access := new String'("");
      Buff : String_Access;

      Underscore : Boolean := True;
   begin

      for I in Name'Range loop

         if Name (I) = ' ' then
            Name (I) := '_';
         end if;

      end loop;

      for I in Name'Range loop

         if Underscore then
            if Name (I) /= '_' then
               Underscore := False;
               if Is_Letter (Name (I)) or else Is_Digit (Name (I)) then
                  Buff := new String'(Tmp.all & Name (I));
                  Free (Tmp);
                  Tmp := Buff;
                  Buff := null;
               end if;
            end if;
         else
            if
              Is_Letter (Name (I))
              or else Is_Digit (Name (I))
              or else Name (I) = '_'
            then
               Buff := new String'(Tmp.all & Name (I));
               Free (Tmp);
               Tmp := Buff;
               Buff := null;
               if Name (I) = '_' then
                  Underscore := True;
               end if;
            end if;
         end if;

      end loop;

      return To_Lower (Tmp.all);
   end Sanitize_TC_Name;

   --------------------------
   -- Find_Same_Short_Name --
   --------------------------

   function Find_Same_Short_Name
     (MD_Map : Markered_Data_Maps.Map;
      Subp   : Subp_Info) return Markered_Data_Maps.Cursor
   is
      Short_Name : constant String := Subp.Subp_Text_Name.all;
      TC_Hash    : constant String :=
        (if Subp.Has_TC_Info then
            Sanitize_TC_Name (Subp.TC_Info.Name.all)
         else "");
      Cur : Markered_Data_Maps.Cursor := MD_Map.First;
      MD  : Markered_Data;
   begin
      Trace
        (Me,
         "Looking for a compatible dangling test for " & Short_Name);

      loop
         exit when Cur = Markered_Data_Maps.No_Element;

         MD := Markered_Data_Maps.Element (Cur);
         if
           MD.Short_Name_Used
           and then MD.Short_Name.all = Short_Name
         --  It is hard to understand what happens when test case name
         --  is changed, so we do not handle this scenario.
           and then Markered_Data_Maps.Key (Cur).TC_Hash.all = TC_Hash
         then
            exit;
         end if;

         Markered_Data_Maps.Next (Cur);
      end loop;
      return Cur;
   end Find_Same_Short_Name;

   ---------------------------------
   -- Put_Closing_Comment_Section --
   ---------------------------------

   procedure Put_Closing_Comment_Section
     (Subp           : Subp_Info;
      Overloading_N  : Natural;
      Commented_Out  : Boolean := False;
      Use_Short_Name : Boolean := True)
   is
      Overloading_Prefix : String_Access;
   begin

      if Overloading_N /= 0 then
         if Subp.Is_Overloaded then
            if Use_Short_Name then
               Overloading_Prefix := new String'("1_");
            else
               Overloading_Prefix := new String'
                 (Trim (Natural'Image (Overloading_N), Both) & "_");
            end if;
         else
            Overloading_Prefix := new String'("");
         end if;
      end if;

      S_Put (0, "--  begin read only");
      New_Line_Count;

      if Commented_Out then
         S_Put
           (3,
            "--  end "
            & Test_Routine_Prefix
            & Subp.Subp_Text_Name.all
            & (if Subp.Has_TC_Info
              then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
              else "")
            & ";");
      else
         S_Put
           (3,
            "end "
            & Test_Routine_Prefix
            & Overloading_Prefix.all
            & Subp.Subp_Text_Name.all
            & (if Subp.Has_TC_Info
              then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
              else "")
            & ";");
      end if;
      New_Line_Count;
      S_Put (0, "--  end read only");
      New_Line_Count;

   end Put_Closing_Comment_Section;

   ---------------------------------
   -- Put_Opening_Comment_Section --
   ---------------------------------

   procedure Put_Opening_Comment_Section
     (Subp           : Subp_Info;
      Overloading_N  : Natural;
      Commented_Out  : Boolean := False;
      Use_Short_Name : Boolean := True;
      Type_Name      : String  := "")
   is
      Hash_Length_Used : constant := 15;
      Hash_First : constant Integer := Subp.Subp_Full_Hash'First;
      Hash_Last  : constant Integer :=
        Subp.Subp_Full_Hash'First + Hash_Length_Used;

      Overloading_Prefix : String_Access;
   begin

      if Overloading_N /= 0 then
         if Subp.Is_Overloaded then
            if Use_Short_Name then
               Overloading_Prefix := new String'("1_");
            else
               Overloading_Prefix := new String'
                 (Trim (Natural'Image (Overloading_N), Both) & "_");
            end if;
         else
            Overloading_Prefix := new String'("");
         end if;
      end if;

      New_Line_Count;
      S_Put (0, "--  begin read only");
      New_Line_Count;

      if Subp.Corresp_Type = 0 then
         if Commented_Out then
            S_Put
              (3,
               "--  procedure "
               & Test_Routine_Prefix
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               &  " (Gnattest_T : in out Test);");
            New_Line_Count;
            S_Put
              (3,
               "--  procedure "
               & Subp.Subp_Mangle_Name.all
               &  " (Gnattest_T : in out Test) renames "
               & Test_Routine_Prefix
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               & ";");
            New_Line_Count;
         else
            S_Put
              (3,
               "procedure "
               & Test_Routine_Prefix
               & Overloading_Prefix.all
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               &  " (Gnattest_T : in out Test);");
            New_Line_Count;
            S_Put
              (3,
               "procedure "
               & Subp.Subp_Mangle_Name.all
               &  " (Gnattest_T : in out Test) renames "
               & Test_Routine_Prefix
               & Overloading_Prefix.all
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               & ";");
            New_Line_Count;
         end if;
      else
         if Commented_Out then
            S_Put
              (3,
               "--  procedure "
               & Test_Routine_Prefix
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               &  " (Gnattest_T : in out Test_"
               & Type_Name
               & ");");
            New_Line_Count;
            S_Put
              (3,
               "--  procedure "
               & Subp.Subp_Mangle_Name.all
               &  " (Gnattest_T : in out Test_"
               & Type_Name
               & ") renames "
               & Test_Routine_Prefix
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               & ";");
            New_Line_Count;
         else
            S_Put
              (3,
               "procedure "
               & Test_Routine_Prefix
               & Overloading_Prefix.all
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               &  " (Gnattest_T : in out Test_"
               & Type_Name
               & ");");
            New_Line_Count;
            S_Put
              (3,
               "procedure "
               & Subp.Subp_Mangle_Name.all
               &  " (Gnattest_T : in out Test_"
               & Type_Name
               & ") renames "
               & Test_Routine_Prefix
               & Overloading_Prefix.all
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               & ";");
            New_Line_Count;
         end if;
      end if;

      S_Put
        (0,
         "--  id:"
         & Hash_Version
         & "/"
         & Subp.Subp_Full_Hash (Hash_First .. Hash_Last)
         & "/"
         & Subp.Subp_Text_Name.all
         & "/"
         & (if Use_Short_Name then "1" else "0")
         & "/"
         & (if Commented_Out then "1" else "0")
         & "/");
      if Subp.Has_TC_Info then
         S_Put
           (0,
            Sanitize_TC_Name (Subp.TC_Info.Name.all)
            & "/");
      end if;
      New_Line_Count;

      if Commented_Out then
         S_Put
           (3,
            "--  procedure "
            & Test_Routine_Prefix
            & Subp.Subp_Text_Name.all
            & (if Subp.Has_TC_Info
              then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
              else "")
            & " (Gnattest_T : in out ");
      else
         S_Put
           (3,
            "procedure "
            & Test_Routine_Prefix
            & Overloading_Prefix.all
            & Subp.Subp_Text_Name.all
            & (if Subp.Has_TC_Info
              then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
              else "")
            & " (Gnattest_T : in out ");
      end if;
      if Subp.Corresp_Type = 0 then
         S_Put (0, "Test) is");
      else
         S_Put
           (0,
            "Test_"
            & Type_Name
            & ") is");
      end if;

      New_Line_Count;

      if not Commented_Out then

         --  we cannot relate to any sloc in case of a dangling test

         if not Omit_Sloc then
            S_Put
              (3,
               "--  "
               & Base_Name
                 (Subp.Subp_Declaration.Unit.Get_Filename)
               & ":"
               & Trim
                 (First_Line_Number (Subp.Subp_Declaration)'Img, Both)
               & ":"
               & Trim
                 (First_Column_Number (Subp.Subp_Declaration)'Img, Both)
               & ":"
               & Subp.Subp_Name_Image.all);
            New_Line_Count;
         end if;

         if Subp.Has_TC_Info then
            Put_Wrapper_Rename (6, Subp);
         end if;
      end if;

      S_Put (0, "--  end read only");
      New_Line_Count;

   end Put_Opening_Comment_Section;

   --------------------
   -- Uncomment_Line --
   --------------------

   function Uncomment_Line (S : String) return String is
   begin
      if S = "--  " then
         return "";
      end if;

      if S'Length < 5 then
         return S;
      end if;

      if S (S'First .. S'First + 3) = "--  " then
         return S (S'First + 4 .. S'Last);
      end if;

      return S;
   end Uncomment_Line;

   -----------------
   -- Format_Time --
   -----------------

   function Format_Time (Time : GNAT.OS_Lib.OS_Time) return String is

      function Prefix_With_Zero (S : String) return String;

      function Prefix_With_Zero (S : String) return String is
         S_Trimmed : constant String := Trim (S, Both);
      begin
         if S_Trimmed'Length = 1 then
            return "0" & S_Trimmed;
         else
            return S_Trimmed;
         end if;
      end Prefix_With_Zero;

      use GNAT.OS_Lib;
   begin
      return
        Trim (Integer'Image (GM_Year (Time)), Both) & "-" &
      Prefix_With_Zero (Integer'Image (GM_Month (Time))) & "-" &
      Prefix_With_Zero (Integer'Image (GM_Day (Time))) & " " &
      Prefix_With_Zero (Integer'Image (GM_Hour (Time))) & ":" &
      Prefix_With_Zero (Integer'Image (GM_Minute (Time))) & ":" &
      Prefix_With_Zero (Integer'Image (GM_Second (Time)));
   end Format_Time;

   ---------------------------
   -- Generate_Project_File --
   ---------------------------

   procedure Generate_Project_File (Source_Prj : String) is
      Tmp_Str : GNAT.OS_Lib.String_Access;
      package Srcs is new
        Ada.Containers.Indefinite_Ordered_Sets (String);
      use Srcs;

      Out_Dirs     : Srcs.Set;
      Out_Dirs_Cur : Srcs.Cursor;

      Output_Prj : GNAT.OS_Lib.String_Access;

      Source_Prj_Name : String :=
        Base_Name (Source_Prj, File_Extension (Source_Prj));

      use GNAT.OS_Lib;
   begin
      for I in Source_Prj_Name'Range loop
         if Source_Prj_Name (I) = '-' then
            Source_Prj_Name (I) := '_';
         end if;
      end loop;

      Reset_Source_Iterator;
      loop
         Tmp_Str := new String'(Next_Source_Name);
         exit when Tmp_Str.all = "";

         if Is_Directory (Get_Source_Output_Dir (Tmp_Str.all)) then
            Include (Out_Dirs, Get_Source_Output_Dir (Tmp_Str.all));
         end if;
         Free (Tmp_Str);
      end loop;

      Output_Prj :=
        new String'(Harness_Dir_Str.all
                    & Directory_Separator
                    & Test_Prj_Prefix
                    & Source_Prj_Name
                    & ".gpr");

      Create (Output_Prj.all);

      S_Put (0, "with ""aunit"";");

      Put_New_Line;
      S_Put (0, "with ""gnattest_common.gpr"";");
      Put_New_Line;
      S_Put (0, "with """);
      S_Put
        (0,
         +Relative_Path
           (Create (+Source_Prj),
            Create (+Harness_Dir_Str.all)) &
           """;");
      Put_New_Line;
      S_Put
        (0,
         "project "
         & Test_Prj_Prefix
         & Base_Name (Source_Prj_Name)
         & " is");
      Put_New_Line;
      Put_New_Line;

      S_Put (3, "for Source_Dirs use");
      Put_New_Line;

      if Out_Dirs.Is_Empty then
         S_Put (5, "(""common"");");

         Put_New_Line;
         Put_New_Line;
      else
         Out_Dirs_Cur := Out_Dirs.First;
         S_Put (5, "(""");
         S_Put
           (0,
            +Relative_Path
              (Create (+Srcs.Element (Out_Dirs_Cur)),
               Create (+Harness_Dir_Str.all)) &
              """");
         loop
            Srcs.Next (Out_Dirs_Cur);
            exit when Out_Dirs_Cur = Srcs.No_Element;

            S_Put (0, ",");
            Put_New_Line;
            S_Put (6, """");
            S_Put
              (0,
               +Relative_Path
                 (Create (+Srcs.Element (Out_Dirs_Cur)),
                  Create (+Harness_Dir_Str.all)) &
                 """");

         end loop;
         S_Put (0, ",");
         Put_New_Line;
         S_Put (6, """common"");");

         Put_New_Line;
         Put_New_Line;
      end if;

      S_Put (3, "for Object_Dir use ""test_obj"";");
      Put_New_Line;
      declare
         Obj_Dir : constant String :=
           Harness_Dir_Str.all
           & Directory_Separator
           & "test_obj";
         Dir     : File_Array_Access;
      begin
         Append (Dir, GNATCOLL.VFS.Create (+Obj_Dir));
         Create_Dirs (Dir);
      exception
         when Directory_Error =>
            Cmd_Error_No_Help
              ("cannot create directory " & Obj_Dir);
      end;

      S_Put (3, "for Languages use Gnattest_Common'Languages & (""Ada"");");
      Put_New_Line;

      S_Put (3, "package Compiler renames Gnattest_Common.Compiler;");
      Put_New_Line;
      Put_New_Line;

      if IDE_Package_Present then
         S_Put
           (3,
            "package Ide renames " &
            Base_Name (Source_Prj, File_Extension (Source_Prj)) &
            ".Ide;");
         Put_New_Line;
         Put_New_Line;
      end if;

      if Make_Package_Present then
         S_Put
           (3,
            "package Make renames " &
            Base_Name (Source_Prj, File_Extension (Source_Prj)) &
            ".Make;");
         Put_New_Line;
         Put_New_Line;
      end if;

      S_Put (3, "package Coverage is");
      Put_New_Line;
      S_Put (6, "for Units use ();");
      Put_New_Line;
      S_Put (3, "end Coverage;");
      Put_New_Line;
      Put_New_Line;

      S_Put
        (0,
         "end "
         & Test_Prj_Prefix
         & Base_Name (Source_Prj_Name)
         & ";");
      Put_New_Line;
      Close_File;

      Tmp_Test_Prj := new String'(Normalize_Pathname
                                  (Name => Output_Prj.all,
                                   Case_Sensitive => False));
   end Generate_Project_File;

   -------------------
   -- Process_Stubs --
   -------------------

   procedure Process_Stubs (List : Ada_Nodes_List.List)
   is
      Cur : Ada_Nodes_List.Cursor;
      Str : String_Access;

      Stub_Success : Boolean;

      use Ada_Nodes_List;
   begin
      if Is_Empty (List) then
         return;
      end if;

      --  Once we change the context, contents of List won't make sense.
      Cur := List.First;
      while Cur /= Ada_Nodes_List.No_Element loop

         Str := new String'(Ada_Nodes_List.Element (Cur).Unit.Get_Filename);

         if Get_Source_Body (Str.all) /= "" then
            if not Source_Stubbed (Str.all) then
               begin
                  Test.Stub.Process_Unit
                    (Ada_Nodes_List.Element (Cur),
                     Get_Source_Stub_Dir (Str.all)
                     & GNAT.OS_Lib.Directory_Separator
                     & Base_Name (Get_Source_Body (Str.all)),
                     Get_Source_Stub_Dir (Str.all)
                     & GNAT.OS_Lib.Directory_Separator
                     & Get_Source_Stub_Data_Spec (Str.all),
                     Get_Source_Stub_Dir (Str.all)
                     & GNAT.OS_Lib.Directory_Separator
                     & Get_Source_Stub_Data_Body (Str.all));
                  Stub_Success := True;

               exception
                  when Test.Stub.Stub_Processing_Error =>
                     --  Error message has been printed already
                     Stub_Success := False;
               end;

               if Stub_Success then
                  Mark_Sourse_Stubbed (Str.all);
               end if;

            end if;
         end if;

         Free (Str);

         Next (Cur);
      end loop;

   end Process_Stubs;

   ------------------------------------
   -- Is_Declared_In_Regular_Package --
   ------------------------------------

   function Is_Declared_In_Regular_Package
     (Elem : Ada_Node'Class) return Boolean
   is
      Nesting : constant Ada_Node_Array := Parents (Elem);
   begin
      for I in Nesting'First + 1 .. Nesting'Last loop
         if Kind (Nesting (I)) = Ada_Generic_Package_Decl then
            return False;
         end if;
      end loop;

      return True;
   end Is_Declared_In_Regular_Package;

   ------------------------
   -- Put_Wrapper_Rename --
   ------------------------

   procedure Put_Wrapper_Rename (Span : Natural; Current_Subp : Subp_Info)
   is
      Spec    : constant Base_Subp_Spec'Class :=
        (if Current_Subp.Subp_Declaration.Kind = Ada_Expr_Function then
            Current_Subp.Subp_Declaration.As_Expr_Function.F_Subp_Spec
         else
            Current_Subp.Subp_Declaration.As_Basic_Subp_Decl.P_Subp_Decl_Spec);
      Params  : constant Param_Spec_Array := Spec.P_Params;
      Is_Func : constant Boolean          :=
        Is_Function (Current_Subp.Subp_Declaration.As_Basic_Decl);
   begin

      if Is_Func then
         S_Put
           (Span,
            "function " & Current_Subp.Subp_Name_Image.all);

      else
         S_Put
           (Span,
            "procedure " & Current_Subp.Subp_Name_Image.all);
      end if;

      if Params'Length /= 0 then
         S_Put (1, "(");
         for P in Params'Range loop
            S_Put (0, Node_Image (Params (P)));
            if P = Params'Last then
               S_Put (0, ")");
            else
               S_Put (0, "; ");
            end if;
         end loop;
      end if;

      if Is_Func then
         S_Put (1, "return " & Node_Image (Spec.P_Returns));
      end if;

      S_Put
        (1,
         "renames "
         & Wrapper_Prefix
         & Current_Subp.Subp_Mangle_Name.all
         & ";");
      New_Line_Count;
   end Put_Wrapper_Rename;

   -------------------------------
   -- Generate_Function_Wrapper --
   -------------------------------

   procedure Generate_Function_Wrapper (Current_Subp : Subp_Info)
   is
      Spec    : constant Base_Subp_Spec'Class :=
        (if Current_Subp.Subp_Declaration.Kind = Ada_Expr_Function then
            Current_Subp.Subp_Declaration.As_Expr_Function.F_Subp_Spec
         else
            Current_Subp.Subp_Declaration.As_Basic_Subp_Decl.P_Subp_Decl_Spec);

      Params  : constant Param_Spec_Array := Spec.P_Params;
      Str_Set : String_Set.Set;
      Cur     : String_Set.Cursor;
   begin
      S_Put (0, GT_Marker_Begin);
      New_Line_Count;
      S_Put
        (3,
         "function " &
           Wrapper_Prefix &
           Current_Subp.Subp_Mangle_Name.all);

      for I in Params'Range loop
         if I = Params'First then
            S_Put (0, " (");
         end if;
         S_Put (0, Node_Image (Params (I)));
         if I = Params'Last then
            S_Put (0, ") ");
         else
            S_Put (0, "; ");
         end if;
      end loop;

      S_Put (0, " return " & Node_Image (Spec.P_Returns));

      New_Line_Count;
      S_Put (3, "is");
      New_Line_Count;

      Str_Set := Current_Subp.TC_Info.Params_To_Temp;
      Cur := Str_Set.First;
      loop
         exit when Cur = String_Set.No_Element;

         S_Put (6, String_Set.Element (Cur));
         New_Line_Count;

         String_Set.Next (Cur);
      end loop;

      S_Put (3, "begin");
      New_Line_Count;

      if Current_Subp.TC_Info.Req_Image.all /= "" then
         S_Put (6, "begin");
         New_Line_Count;
         S_Put (9, "pragma Assert");
         New_Line_Count;
         S_Put
           (11,
            "(" &
              Current_Subp.TC_Info.Req_Image.all &
              ");");
         New_Line_Count;
         S_Put (9, "null;");
         New_Line_Count;
         S_Put (6, "exception");
         New_Line_Count;
         S_Put (12, "when System.Assertions.Assert_Failure =>");
         New_Line_Count;
         S_Put (15, "AUnit.Assertions.Assert");
         New_Line_Count;
         S_Put (17, "(False,");
         New_Line_Count;
         S_Put
           (18,
            """req_sloc("
            & Current_Subp.TC_Info.Req_Line.all
            & "):"
            & Current_Subp.TC_Info.Name.all
            & " test requirement violated"");");
         New_Line_Count;
         S_Put (6, "end;");
         New_Line_Count;
      end if;

      S_Put (6, "declare");
      New_Line_Count;
      S_Put
        (9,
         Current_Subp.Subp_Mangle_Name.all
         & "_Result : constant "
         & Node_Image (Spec.P_Returns)
         & " := GNATtest_Generated.GNATtest_Standard."
         & Current_Subp.Nesting.all
         & "."
         & Current_Subp.Subp_Name_Image.all);

      if Params'Length = 0 then
         S_Put (0, ";");
      else
         S_Put (1, "(");
         for I in Params'Range loop
            declare
               Name_List : constant Defining_Name_List := F_Ids (Params (I));
               Idx       :          Positive :=
                 Name_List.Defining_Name_List_First;
            begin
               while Name_List.Defining_Name_List_Has_Element (Idx) loop
                  S_Put
                    (0,
                     Node_Image (Name_List.Defining_Name_List_Element (Idx)));
                  Idx := Name_List.Defining_Name_List_Next (Idx);
                  if Name_List.Defining_Name_List_Has_Element (Idx) then
                     S_Put (0, ", ");
                  end if;
               end loop;
            end;

            if I = Params'Last then
               S_Put (0, ");");
            else
               S_Put (0, ", ");
            end if;
         end loop;
      end if;

      New_Line_Count;

      S_Put (6, "begin");
      New_Line_Count;

      if Current_Subp.TC_Info.Ens_Image.all /= "" then
         S_Put (9, "begin");
         New_Line_Count;
         S_Put (12, "pragma Assert");
         New_Line_Count;
         S_Put
           (14,
            "(" &
              Current_Subp.TC_Info.Ens_Image.all &
              ");");
         New_Line_Count;
         S_Put (12, "null;");
         New_Line_Count;
         S_Put (9, "exception");
         New_Line_Count;
         S_Put (12, "when System.Assertions.Assert_Failure =>");
         New_Line_Count;
         S_Put (15, "AUnit.Assertions.Assert");
         New_Line_Count;
         S_Put (17, "(False,");
         New_Line_Count;
         S_Put
           (18,
            """ens_sloc("
            & Current_Subp.TC_Info.Ens_Line.all
            & "):"
            & Current_Subp.TC_Info.Name.all
            & " test commitment violated"");");
         New_Line_Count;
         S_Put (9, "end;");
         New_Line_Count;
      end if;

      S_Put
        (9,
         "return " &
           Current_Subp.Subp_Mangle_Name.all &
           "_Result;");
      New_Line_Count;

      S_Put (6, "end;");
      New_Line_Count;

      S_Put
        (3,
         "end " &
           Wrapper_Prefix &
           Current_Subp.Subp_Mangle_Name.all &
           ";");
      New_Line_Count;
      S_Put (0, GT_Marker_End);
      New_Line_Count;

   end Generate_Function_Wrapper;

   --------------------------------
   -- Generate_Procedure_Wrapper --
   --------------------------------

   procedure Generate_Procedure_Wrapper (Current_Subp : Subp_Info)
   is
      Spec    : constant Base_Subp_Spec   :=
        Current_Subp.Subp_Declaration.As_Basic_Subp_Decl.P_Subp_Decl_Spec;
      Params  : constant Param_Spec_Array := Spec.P_Params;
      Str_Set : String_Set.Set;
      Cur     : String_Set.Cursor;
   begin
      S_Put (0, GT_Marker_Begin);
      New_Line_Count;
      S_Put
        (3,
         "procedure " &
           Wrapper_Prefix &
           Current_Subp.Subp_Mangle_Name.all);

      for I in Params'Range loop
         if I = Params'First then
            S_Put (0, " (");
         end if;
         S_Put (0, Node_Image (Params (I)));
         if I = Params'Last then
            S_Put (0, ") ");
         else
            S_Put (0, "; ");
         end if;
      end loop;

      New_Line_Count;
      S_Put (3, "is");
      New_Line_Count;

      Str_Set := Current_Subp.TC_Info.Params_To_Temp;
      Cur := Str_Set.First;
      loop
         exit when Cur = String_Set.No_Element;

         S_Put (6, String_Set.Element (Cur));
         New_Line_Count;

         String_Set.Next (Cur);
      end loop;

      S_Put (3, "begin");
      New_Line_Count;

      if Current_Subp.TC_Info.Req_Image.all /= "" then
         S_Put (6, "begin");
         New_Line_Count;
         S_Put (9, "pragma Assert");
         New_Line_Count;
         S_Put
           (11,
            "(" &
              Current_Subp.TC_Info.Req_Image.all &
              ");");
         New_Line_Count;
         S_Put (9, "null;");
         New_Line_Count;
         S_Put (6, "exception");
         New_Line_Count;
         S_Put (9, "when System.Assertions.Assert_Failure =>");
         New_Line_Count;
         S_Put (12, "AUnit.Assertions.Assert");
         New_Line_Count;
         S_Put (14, "(False,");
         New_Line_Count;
         S_Put
           (15,
            """req_sloc("
            & Current_Subp.TC_Info.Req_Line.all
            & "):"
            & Current_Subp.TC_Info.Name.all
            & " test requirement violated"");");
         New_Line_Count;
         S_Put (6, "end;");
         New_Line_Count;
      end if;

      S_Put
        (6,
         "GNATtest_Generated.GNATtest_Standard." &
           Current_Subp.Nesting.all &
           "." &
           Current_Subp.Subp_Text_Name.all);

      if Params'Length = 0 then
         S_Put (0, ";");
      else
         S_Put (1, "(");
         for I in Params'Range loop
            declare
               Name_List : constant Defining_Name_List := F_Ids (Params (I));
               Idx       :          Positive :=
                 Name_List.Defining_Name_List_First;
            begin
               while Name_List.Defining_Name_List_Has_Element (Idx) loop
                  S_Put
                    (0,
                     Node_Image (Name_List.Defining_Name_List_Element (Idx)));
                  Idx := Name_List.Defining_Name_List_Next (Idx);
                  if Name_List.Defining_Name_List_Has_Element (Idx) then
                     S_Put (0, ", ");
                  end if;
               end loop;
            end;

            if I = Params'Last then
               S_Put (0, ");");
            else
               S_Put (0, ", ");
            end if;
         end loop;
      end if;

      New_Line_Count;

      if Current_Subp.TC_Info.Ens_Image.all /= "" then
         S_Put (6, "begin");
         New_Line_Count;
         S_Put (9, "pragma Assert");
         New_Line_Count;
         S_Put
           (11,
            "(" &
              Current_Subp.TC_Info.Ens_Image.all &
              ");");
         New_Line_Count;
         S_Put (9, "null;");
         New_Line_Count;
         S_Put (6, "exception");
         New_Line_Count;
         S_Put (9, "when System.Assertions.Assert_Failure =>");
         New_Line_Count;
         S_Put (12, "AUnit.Assertions.Assert");
         New_Line_Count;
         S_Put (14, "(False,");
         New_Line_Count;
         S_Put
           (15,
            """ens_sloc("
            & Current_Subp.TC_Info.Ens_Line.all
            & "):"
            & Current_Subp.TC_Info.Name.all
            & " test commitment violated"");");
         New_Line_Count;
         S_Put (6, "end;");
         New_Line_Count;
      end if;

      S_Put
        (3,
         "end " &
           Wrapper_Prefix &
           Current_Subp.Subp_Mangle_Name.all &
           ";");
      New_Line_Count;
      S_Put (0, GT_Marker_End);
      New_Line_Count;

   end Generate_Procedure_Wrapper;

   -----------------------------
   -- Update_Generic_Packages --
   -----------------------------

   procedure Update_Generic_Packages (Gen_Pack : Generic_Package) is
      Cur : Generic_Package_Storage.Cursor := Gen_Package_Storage.First;
      GP  : Generic_Package;

      use Generic_Package_Storage;
   begin
      while Cur /= Generic_Package_Storage.No_Element loop

         GP := Generic_Package_Storage.Element (Cur);

         if GP.Name.all = Gen_Pack.Name.all then
            if GP.Sloc /= null then
               --  Same package can be added several times.
               return;
            end if;
            GP.Sloc := Gen_Pack.Sloc;
            Gen_Package_Storage.Replace_Element (Cur, GP);
            return;
         end if;

         Next (Cur);
      end loop;

      Gen_Package_Storage.Append (Gen_Pack);
   end Update_Generic_Packages;

   -----------------------------
   -- Update_Generic_Packages --
   -----------------------------

   procedure Update_Generic_Packages (Instantiation : String) is
      Cur : Generic_Package_Storage.Cursor := Gen_Package_Storage.First;
      GP  : Generic_Package;

      use Generic_Package_Storage;
   begin
      while Cur /= Generic_Package_Storage.No_Element loop

         GP := Generic_Package_Storage.Element (Cur);

         if GP.Name.all = Instantiation then
            if GP.Has_Instantiation then
               --  Same package can be instantiated multiple times.
               return;
            end if;
            GP.Has_Instantiation := True;
            Gen_Package_Storage.Replace_Element (Cur, GP);
            return;
         end if;

         Next (Cur);
      end loop;

      --  Instantiation is processed ahead of coresponding generic.
      --  Adding a template for it to later fill in the sloc.
      GP.Name := new String'(Instantiation);
      GP.Sloc := null;
      GP.Has_Instantiation := True;
      Gen_Package_Storage.Append (GP);
   end Update_Generic_Packages;

   ------------------------
   -- Report_Tests_Total --
   ------------------------

   procedure Report_Tests_Total is
      Cur : Tests_Per_Unit.Cursor := Test_Info.First;
   begin
      loop
         exit when Cur = Tests_Per_Unit.No_Element;

         Report_Std
           (Natural'Image (Tests_Per_Unit.Element (Cur)) &
              " testable subprograms in " &
              Base_Name (Tests_Per_Unit.Key (Cur)));

         Tests_Per_Unit.Next (Cur);
      end loop;

      Test_Info.Clear;
      Report_Std
        ("gnattest:" &
           Natural'Image (All_Tests_Counter) &
           " testable subprogram(s) processed");
      Report_Std
        ("gnattest:" &
           Natural'Image (New_Tests_Counter) &
           " new skeleton(s) generated");
   end Report_Tests_Total;

   ---------------------------------
   -- Report_Unused_Generic_Tests --
   ---------------------------------

   procedure Report_Unused_Generic_Tests is
   begin
      for GP of Gen_Package_Storage loop
         if not GP.Has_Instantiation then
            Report_Std
              ("warning: (gnattest) "
               & GP.Sloc.all
               & ": no instance of "
               & GP.Name.all);
            Report_Std
              (" corresponding tests are not included into harness");

            Free (GP.Name);
            Free (GP.Sloc);
         end if;
      end loop;

      Gen_Package_Storage.Clear;
   end Report_Unused_Generic_Tests;

end Test.Skeleton;
