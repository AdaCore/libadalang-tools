------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                    G N A T T E S T . S K E L E T O N                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
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

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with GNAT.OS_Lib;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Test.Common; use Test.Common;
with Test.Harness;
with Test.Skeleton.Source_Table; use Test.Skeleton.Source_Table;
with Test.Mapping; use Test.Mapping;
with Utils.Command_Lines; use Utils.Command_Lines;

package body Test.Skeleton is
   Me                : constant Trace_Handle :=
     Create ("Skeletons", Default => Off);
   Me_Direct_Callees : constant Trace_Handle :=
     Create ("Skeletons.Direct_Callees", Default => Off);
   pragma Unreferenced (Me_Direct_Callees);

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
   pragma Unreferenced (Normal, Robustness);

   type Test_Case_Info is record
      Name : String_Access;
      Mode : Test_Case_Mode;

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
      --  Fully expanded Ada name of the CU.

      Unit_File_Name : String_Access;
      --  Full name of the file, containing the CU.

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
            --  Stores info of nested packages.

            Subp_Name_Frequency : Name_Frequency.Map;

         when Instantiation =>

            Gen_Unit_Full_Name : String_Access;
            --  Fully expanded Ada name of the generic CU.

            Gen_Unit_File_Name : String_Access;
            --  Name of file containing the generic CU.

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
--     use SP_Mapping;

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
      TC_Found        :    out Boolean);
   --  Adds one subprogram-to-test per each test case.
   --  Sets TC_Found if at least one Test_Case aspect or pragma has been found
   --  for given subprogram.

   procedure Generate_Nested_Hierarchy (Data : Data_Holder);
   --  Create dummy child packages copying nested packages from tested package.

   procedure Generate_Test_Package (Data : Data_Holder);
   --  Generates test package spec and body.

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

   function Find_Same_Short_Name
     (MD_Map : Markered_Data_Maps.Map;
      Subp   : Subp_Info) return Markered_Data_Maps.Cursor;
   --  Searches for the test with given short name

   function Uncomment_Line (S : String) return String;
   --  Removes two dashes and two spaces from the beginning of the line.
   --  Returns argument string if commenting prefix not found.

   function Format_Time (Time : GNAT.OS_Lib.OS_Time) return String;
   --  Returns image of given time in 1901-01-01 00:00:00 format.

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

      Apropriate_Source : Boolean;

      CU : constant Compilation_Unit := Root (The_Unit).As_Compilation_Unit;

      Test_Packages : String_Set.Set;
      Cur : String_Set.Cursor;

      procedure Get_Test_Packages_List (S_Data : Suites_Data_Type);

      function Get_Suite_Components
        (S_Data       : Suites_Data_Type;
         Package_Name : String)
         return Test.Harness.Data_Holder;

      procedure Get_Test_Packages_List (S_Data : Suites_Data_Type)
      is
         Declared_In_Generic : Boolean;
         Elem : Ada_Node;
      begin
         for K in S_Data.TR_List.First_Index .. S_Data.TR_List.Last_Index loop

            Declared_In_Generic := False;
            Elem := S_Data.TR_List.Element (K).Original_Subp;
            loop
               exit when Elem = No_Ada_Node;

               if Elem.Kind = Ada_Generic_Package_Decl then
                  Declared_In_Generic := True;
                  exit;
               end if;

               Elem := Elem.Parent;
            end loop;

            if not Declared_In_Generic then
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
   begin
      if P_Unit_Kind (CU) = Unit_Body then
         --  Only interested in specs.
         return;
      end if;
      Gather_Data
        (CU, Data, Suite_Data_List, Apropriate_Source);

      if Apropriate_Source then

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

                  end if;
               end if;

               String_Set.Next (Cur);
            end loop;

         end if;
      end if;

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

      -------------------------
      -- Get_Nested_Packages --
      --------------------------

      function Get_Nested_Packages (Node : Ada_Node'Class) return Visit_Status
      is
         Package_Data : Package_Info;
      begin
         --  We should first abandon for Private parts, abandon for ghost
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
               Package_Data.Data_Kind := Declaration_Data;
               Package_Data.Element := Node.As_Ada_Node;
               Data.Package_Data_List.Append (Package_Data);

            when others =>
               null;
         end case;

         return Into;
      end Get_Nested_Packages;

      ---------------------
      -- Get_Subprograms --
      ---------------------

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
            Set_No_Parent (Type_Data);
         end Get_Type_Parent_Data;
      begin
         --  We should first abandon for Private parts, abandon for ghost

         if Kind (Node) /= Ada_Type_Decl then
            return Into;
         end if;

         if not Node.As_Type_Decl.P_Is_Tagged_Type then
            return Over;
         end if;

         if Node.As_Base_Type_Decl.P_Is_Private then
            Cur_Node :=
              Node.As_Base_Type_Decl.P_Private_Completion.As_Ada_Node;
         else
            Cur_Node := Node.As_Ada_Node;
         end if;

         --  Checking for doubled types.
         for I in Data.Type_Data_List.First_Index ..
           Data.Type_Data_List.Last_Index
         loop
            if Data.Type_Data_List.Element (I).Main_Type_Elem = Cur_Node then
               return Over;
            end if;
         end loop;

         --  Gathering basic data about type
         Type_Data.Main_Type_Elem := Cur_Node;
         Type_Data.Main_Type_Text_Name := new
           String'(Node_Image (Cur_Node.As_Basic_Decl.P_Defining_Name));
         Type_Data.Nesting := new String'(Get_Nesting (Cur_Node));

         Get_Type_Parent_Data (Type_Data);

         Type_Data.Main_Type_Abstract :=
           (Cur_Node.Kind = Ada_Record_Type_Def and then
            Cur_Node.As_Record_Type_Def.F_Has_Abstract) or else
           (Cur_Node.Kind = Ada_Derived_Type_Def and then
            Cur_Node.As_Derived_Type_Def.F_Has_Abstract);

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
              ((TT_Info       => Test_Type,
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
         --  We should first abandon for Private parts, abandon for ghost

         if
           Node.Kind in Ada_Protected_Type_Decl | Ada_Single_Protected_Decl
         then
            return Over;
         end if;

         if
           Node.Kind = Ada_Expr_Function and then
           Node.As_Base_Subp_Body.F_Subp_Spec /= No_Subp_Spec
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

         Subp.Subp_Declaration := Node.As_Ada_Node;
         Subp.Subp_Text_Name   := new String'(Get_Subp_Name (Node));
         Subp.Subp_Name_Image   := new String'
           (Node_Image (Node.As_Basic_Decl.P_Defining_Name));
         Subp.Nesting := new String'(Get_Nesting (Node));

         --  Setting tested subprogram sloc for suite info
         declare
            Subp_Span : constant Source_Location_Range :=
              Subp.Subp_Declaration.Sloc_Range;
         begin
            Test_Routine.Tested_Sloc := new String'
              (Base_Name (Data.Unit_File_Name.all)
               & ":"
               & Trim (Subp_Span.Start_Line'Img, Both)
               & ":"
               & Trim (Subp_Span.Start_Column'Img, Both)
               & ":");
         end;

         Owner_Decl := Tagged_Primitive_Owner
           (Node.As_Basic_Subp_Decl.P_Subp_Decl_Spec);

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
            --  In simple case the type is always found, because in fact
            --  we do not depend on it.
            Type_Found            := True;
            Subp.Corresp_Type     := 0;

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
               Original_Subp => Node.As_Ada_Node);

            Gather_Test_Cases
              (Subp,
               Test_Routine_Wrapper,
               Data,
               Suite_Data_List,
               Has_TC);

            Update_Name_Frequency (Subp.Subp_Text_Name.all);
         end if;

         return Over;
      end Get_Subprograms;

   begin
      Unit := Bod.F_Item.As_Ada_Node;

      case Unit.Kind is
         when Ada_Package_Decl =>
            Data.Is_Generic := False;

         when others =>
            Report_Std
              ("gnattest: "
               & Base_Name (The_Unit.Unit.Get_Filename)
               & " is an unsupported kind of unit");
      end case;

      Increase_Indent
        (Me,
         "processing " & Node_Image (Unit.As_Basic_Decl.P_Defining_Name)
         &  " (" & Base_Name (The_Unit.Unit.Get_Filename) & ")");

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

   end Gather_Data;

   -----------------------
   -- Gather_Test_Cases --
   -----------------------

   procedure Gather_Test_Cases
     (Subp            :        Subp_Info;
      TR_Info         :        Test_Routine_Info_Wrapper;
      Data            : in out Data_Holder;
      Suite_Data_List : in out Suites_Data_Type;
      TC_Found        :    out Boolean)
   is

      Me_TC : constant Trace_Handle :=
        Create ("Skeletons.Test_Cases", Default => Off);

   begin
      Trace (Me_TC, "Looking for test cases of " & Subp.Subp_Text_Name.all);

      TC_Found := False;

      Data.Subp_List.Append (Subp);
      Suite_Data_List.TR_List.Append (TR_Info);
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
        "gnattest_tmp_test_package";

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

      procedure Put_Persistent_Section (PS_Type : Persistent_Section_Type);
      --  Puts persistent section of given kind surrounded with read-only
      --  markers and corresponding specific Id.

      function Markered_Data_Map_Is_Empty return Boolean;
      --  Check if Markered_Data_Map is empty or the only element present is
      --  actually the Body_Statements persistent block.

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

      use GNAT.OS_Lib;
   begin

      if not Generate_Separates then
         Test_Info.Include (Data.Unit_File_Name.all, 0);
      end if;

      Test_Unit_Suffix := new String'(Test_Unit_Name_Suff);

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
                        S_Put (6, "AUnit.Assertions.Assert");
                        New_Line_Count;
                        S_Put
                          (8, "(Gnattest_Generated.Default_Assert_Value,");
                        New_Line_Count;
                        S_Put (9,  """Test not implemented."");");
                        New_Line_Count;
                        New_Line_Count;
                     else

                        if MD.Issue_Warning then
                           Report_Std
                             (Base_Name (Data.Unit_File_Name.all)
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
                              & ": warning: test for "
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
                 (" warning: "
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
                        Elem_Numbers.Element
                          (Current_Subp.Subp_Declaration),
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
                           S_Put (6, "AUnit.Assertions.Assert");
                           New_Line_Count;
                           S_Put
                             (8, "(Gnattest_Generated.Default_Assert_Value,");
                           New_Line_Count;
                           S_Put (9,  """Test not implemented."");");
                           New_Line_Count;
                           New_Line_Count;
                        else

                           if MD.Issue_Warning then
                              Report_Std
                                (Base_Name (Data.Unit_File_Name.all)
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
                                 & ": warning: test for "
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
                    (" warning: "
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
           ("gnattest: marker corrupted at "
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

--           if Subp.Has_TC_Info then
--              Put_Wrapper_Rename (6, Subp);
--           end if;
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

end Test.Skeleton;
