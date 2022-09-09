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

with Ada.Text_IO;

with Ada.Containers; use type Ada.Containers.Count_Type;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada;
with Ada.Characters.Handling;
with Interfaces; use type Interfaces.Unsigned_16;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.VFS;
with GNATCOLL.Projects;

with LAL_Extensions; use LAL_Extensions;

with Laltools.Common; use Laltools.Common;

with Utils.Command_Lines.Common; use Utils; use Utils.Command_Lines.Common;
with Utils.Dbg_Out;
with Utils.Formatted_Output;
with Utils.String_Utilities; use Utils.String_Utilities;

with Utils_Debug; use Utils_Debug;
with Utils.Predefined_Symbols; use Utils.Predefined_Symbols;

package body METRICS.Actions is

   Output_To_Standard_Output : Boolean renames Debug_Flag_S;

   Implemented : constant Metrics_Set :=
   --  Set of metrics that are implemented in lalmetric so far.
   --    48 metrics total
   --    44 fully implemented
   --    3 partially implemented
   --    1 not implemented

     [Contract            => True,
      Post                => True,
      Contract_Complete   => False, ---------------- (needs semantic info)
      --  We need to know what each name in pre/etc denotes
      Contract_Complexity => True,
      --  The above 4 are new (not implemented in the old gnatmetric)

      Lines                => True,
      Lines_Code           => True,
      Lines_Comment        => True,
      Lines_Eol_Comment    => True,
      Lines_Ratio          => True,
      Lines_Blank          => True,
      Lines_Average        => True,
      Lines_Spark          => True, -- partial (needs semantic info)
      --  We don't inherit properly from spec to body or parent to child, and
      --  we don't take configuration files into account.
      --  Lines_Spark is new (not implemented in the old gnatmetric).
      Lines_Code_In_Bodies => True,
      Num_Bodies           => True,

      Public_Types         => True,
      Private_Types        => True,
      All_Types            => True,
      Public_Subprograms   => True,
      All_Subprograms      => True,
      Statements           => True,
      Declarations         => True,
      Logical_Source_Lines => True,
      Unit_Nesting         => True,
      Construct_Nesting    => True,
      Current_Construct_Nesting => True,
      Param_Number         => True,
      In_Parameters        => True,
      Out_Parameters       => True,
      In_Out_Parameters    => True,

      Computed_Public_Types       => True,
      Computed_All_Types          => True,
      Computed_Public_Subprograms => True,
      Computed_All_Subprograms    => True,

      Complexity_Statement  => True,
      Complexity_Expression => True,
      Complexity_Cyclomatic => True,
      Complexity_Essential  => True, -- partial (needs semantic info)
      --  We need to hook goto's up to their label, and know which level the
      --  label is at in the tree. Similarly for exit statements with names.
      Complexity_Average    => True,
      Loop_Nesting          => True,
      Extra_Exit_Points     => True, -- partial (needs semantic info)
      --  We need to know which exception is denoted by the name in a raise
      --  statement or in an exception handler.

      Tagged_Coupling_Out    => True,
      Hierarchy_Coupling_Out => True,
      Tagged_Coupling_In     => True,
      Hierarchy_Coupling_In  => True,
      Control_Coupling_Out   => True,
      Control_Coupling_In    => True,
      Unit_Coupling_Out      => True,
      Unit_Coupling_In       => True];

   function Image (X : Integer) return String
     renames Utils.String_Utilities.Image;

   --  Debugging printouts
   --  See also Libadalang.Debug.
   pragma Warnings (Off);
   pragma Style_Checks (Off);
   function Par (X : Ada_Node) return Ada_Node is (Parent (X));

   procedure knd (X : Ada_Node) is
      use Utils.Dbg_Out;
   begin
      Put ("\1\n", Kind (X)'Img);
   end knd;

   procedure pp (X : Ada_Node'Class) is
      use Utils.Dbg_Out;
   begin
      Utils.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", (if X.Is_Null then "null" else X.Image));
   end pp;

   procedure ppp (X : Ada_Node'Class) is
      use Utils.Dbg_Out;
   begin
      pp (X);
      Print (X);
   end ppp;

   procedure Put_Ada_Node_Array (X : Ada_Node_Array) is
      use Utils.Dbg_Out;
   begin
      for N of X loop
         pp (N);
         Put ("----------------\n");
      end loop;
   end Put_Ada_Node_Array;

   procedure Put_Child_Record (C : Child_Record) is
      use Utils.Dbg_Out;
   begin
      case C.Kind is
         when Child =>
            Put ("Child: \1\n", C.Node.Image);
         when Trivia =>
            declare
               Trivia_Data : constant Token_Data_Type := Data (C.Trivia);
            begin
               Put ("Trivia: \1 ""\2"" \3\n",
                    Kind (Trivia_Data)'Img,
                    To_UTF8 (Text_To_W_Str (Text (C.Trivia))),
                    Slocs.Image (Sloc_Range (Trivia_Data)));
            end;
      end case;
   end Put_Child_Record;

   procedure Put_Children_Array (A : Children_Array) is
      use Utils.Dbg_Out;
   begin
      for I in A'Range loop
         Put ("\1: ", Image (I));
         Put_Child_Record (A (I));
      end loop;
   end Put_Children_Array;
   pragma Style_Checks (On);
   pragma Warnings (On);

   use Common_String_Switches;

   use Metrics_Flag_Switches, Metrics_Boolean_Switches,
       Metrics_String_Switches;

   use Utils.Formatted_Output;

   Subunit_Sym : constant Symbol := Intern (" - subunit");
   Library_Item_Sym : constant Symbol := Intern (" - library item");

   type Metrix_Ref_Array is
     array (CU_Symbol range <>) of aliased Metrix_Ref;

   package CU_Metrix is new Utils.Vectors
     (CU_Symbol, Metrix_Ref, Metrix_Ref_Array);
   use CU_Metrix;
   Specs : CU_Metrix.Vector;
   Bodies : CU_Metrix.Vector;
   --  Mappings from Symbols representing compilation unit full names to the
   --  spec and body of that compilation unit. Used for coupling metrics.

   function Pragma_Name (Node : Ada_Node) return W_Str is
     (L_Name (Node.As_Pragma_Node.F_Id));
   --  Name of a pragma node, in lower case

   function Is_Ancestor_CU (Anc, Child : Metrix_Ref) return Boolean is
      (Anc /= null and then Child /= null
         and then (Anc = Child
                   or else Is_Ancestor_CU (Anc, Specs (Child.Child_Parent))));
   --  True if Anc is an ancestor library unit of Child

   function Same_Hierarchy (M1, M2 : Metrix_Ref) return Boolean is
     (Is_Ancestor_CU (M1, M2) or else Is_Ancestor_CU (M2, M1));
   --  True if one is an ancestor of the other. Used for coupling metrics
   --  Hierarchy_Coupling_In/Out. There are ways to do this more efficiently.
   --  If each Metrix has an array of ancestors, we can do it in constant time
   --  rather than chasing up a linked list. Alternatively, if we store a flag
   --  with each dependence that indicates whether that dependence comes from a
   --  chain of only parent dependences, versus including one or more with's,
   --  then we could simply ignore the former when doing
   --  Hierarchy_Coupling_In/Out.

   function Acts_As_Spec (M : Metrix) return Boolean is (M.Acts_As_Spec);
   --  True if M is for a subprogram body that has no spec

   function Get_CU_Access
     (A : in out CU_Metrix.Vector; S : CU_Symbol)
     return CU_Metrix.Element_Access;
   --  Return a pointer to Specs[S] or Bodies[S]

   function Get_CU_Access
     (A : in out CU_Metrix.Vector; S : CU_Symbol)
     return CU_Metrix.Element_Access
   is
      I : constant CU_Symbol := Same_Ignoring_Case (S);
   begin
      --  If A is too short, append nulls until it's long enough

      while Last_Index (A) < I loop
         Append (A, null);
      end loop;

      declare
         El : Metrix_Ref_Array renames Elems_Var (A).all (1 .. Last_Index (A));
      begin
         return El (I)'Unchecked_Access;
      end;
   end Get_CU_Access;

   procedure Validate (M : Metrix);
   procedure Validate_Coupling (Units_For_Coupling : Metrix_Vectors.Vector);
   --  For testing/debugging. Check consistency.

   procedure Destroy (M : in out Metrix_Ref);
   --  Reclaim memory for a tree of Metrix records

   use Ada_Node_Vectors;

   subtype Gnatmetric_Eligible is Ada_Node_Kind_Type with
     Predicate => Gnatmetric_Eligible in
       Ada_Expr_Function |
       Ada_Generic_Package_Decl |
       Ada_Package_Body |
       Ada_Package_Decl |
       Ada_Protected_Body |
       Ada_Single_Protected_Decl |
       Ada_Protected_Type_Decl |
       Ada_Entry_Body |
       Ada_Subp_Body |
       Ada_Subp_Body_Stub |
       Ada_Generic_Subp_Instantiation |
       Ada_Task_Body |
       Ada_Single_Task_Decl |
       Ada_Task_Type_Decl;
   --  These are the node kinds that the gnatmetric documentation calls
   --  "eligible local units". We compute metrics for the outermost node (the
   --  compilation unit, for the file-level metrics), and the library item or
   --  subunit, as well as eligible local units. A procedure declaration, for
   --  example, has metrics only if it is the outer node. There is one
   --  exception: see Contract_Complexity below.

   subtype Contract_Complexity_Eligible is Ada_Node_Kind_Type with
     Predicate => Contract_Complexity_Eligible in
       Ada_Generic_Subp_Decl |
       Ada_Abstract_Subp_Decl |
       Ada_Null_Subp_Decl |
       Ada_Subp_Renaming_Decl |
       Ada_Subp_Decl;
   --  For the new lalmetric tool, we have the --contract-complexity
   --  metric, which is on subprogram declarations, so we need
   --  additional "eligible" nodes.

   subtype Eligible is Ada_Node_Kind_Type with
     Predicate => Eligible in Gnatmetric_Eligible |
       Contract_Complexity_Eligible;

   function Q (S : String) return String is -- quote
     ("""" & S & """");

   function Push_New_Metrix
     (Tool : in out Metrics_Tool'Class;
      Node : Ada_Node;
      Ignore_Assertions : Boolean;
      Source_File_Name : String_Ref := null)
     return Metrix_Ref with
     Pre => (Source_File_Name /= null) =
            (not Node.Is_Null and then Kind (Node) = Ada_Compilation_Unit);
   --  Pushes a new Metrix onto the Metrix_Stack, and returns it

   function Get_Outer_Unit (Node : Ada_Node) return Ada_Node with
     Pre => Kind (Node) = Ada_Compilation_Unit;
   --  Given the Compilation_Unit node, return the program unit (Package_Decl,
   --  Package_Body, or whatever node) that is outermost (i.e. directly within
   --  the library item or subunit).

   function Unit_Is_Subunit (Node : Ada_Node) return Boolean is
     (Kind (Parent (Node)) = Ada_Subunit);
   --  True if this is the body of a subunit

   type Assertion_Enum is (Postcondition, Other_Assertion, Not_An_Assertion);

   function Assertion_Kind (Node : Ada_Node) return Assertion_Enum;
   --  Return what kind of assertion it is, if any. Postconditions have their
   --  own metric, so we have to split that out.

   function Is_Assertion (Node : Ada_Node) return Boolean is
     (case Assertion_Kind (Node) is
        when Postcondition | Other_Assertion => True,
        when Not_An_Assertion => False);
   --  True for pragma Assert and for Pre, Post, Contract_Cases,
   --  Pre'Class, and Post'Class.

   function Has_Complexity_Metrics
     (Node : Ada_Node; Ignore_Assertions : Boolean) return Boolean;
   --  True if complexity metrix should be computed for Node (assuming it's
   --  requested on the command line).

   function File_Name_To_Print
     (Cmd : Command_Line; File_Name : String) return String is
      (if Arg (Cmd, Short_File_Names)
         then Directories.Simple_Name (File_Name)
         else Normalize_Pathname (File_Name));

   function Lines_String
     (Sloc_Range : Slocs.Source_Location_Range) return String is
      (Image (Integer (Sloc_Range.Start_Line)) & ": " &
       Image (Integer (Sloc_Range.End_Line)));

   function Get_Fine_Kind (Node : Ada_Node) return Fine_Kind;
   function Fine_Kind_String (Kind : Fine_Kind) return String;
   function Fine_Kind_String_For_Header (M : Metrix) return String;
   --  Name of the node kind for printing in both XML and text

   procedure Write_XML_Schema (Xsd_File_Name : String);
   --  Write the XSD file

   function XML (X : String) return String;
   --  Returns X, but with replacements like "&" --> "&amp", and quoted.

   function XML_Metric_Name_String (Metric : Metrics_Enum) return String;
   --  Name of the metric for printing in XML

   function Metric_Name_String (Metric : Metrics_Enum) return String;
   --  Name of the metric for printing in text

   procedure XML_Print_Config;
   --  Print out the '<config>' section, which maps XML metric names to text
   --  metric names.

   --  Below, Depth parameters are the nesting depth, starting with 1 for the
   --  global metrics.

   function Should_Print
     (Metric : Metrics_Enum;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Positive;
      XML : Boolean) return Boolean;
   --  Return True if the given Metric should be printed, based on
   --  Metrics_To_Compute, the node associated with M and other
   --  special cases.

   function Should_Print_Any
     (First, Last : Metrics_Enum;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Positive;
      XML : Boolean) return Boolean is
      (for some Metric in First .. Last =>
         Should_Print (Metric, Metrics_To_Compute, M, Depth, XML));
   --  True if at least one metric in the range should be printed

   function Val_To_Print
     (Metric : Metrics_Enum; M : Metrix; XML : Boolean) return String;
   --  Return the string value to print for the metric. This is normally
   --  just the value converted to a string. But if the Metric is a
   --  complexity metric, and we are printing the "totals" for the file, we
   --  don't actually want to print the total. We want to print the average.
   --  We prefix the average with an extra blank if it's not XML.
   --  Lines_Average is also an average.

   procedure Print_Range
     (Name : String;
      Metrics_To_Compute : Metrics_Set;
      First, Last : Metrics_Enum;
      M : Metrix;
      Depth : Positive);
   --  Prints a range of metrics. This is needed because the metrics are
   --  printed in groups (line metrics, contract metrics, etc).  Name is the
   --  name of the group, e.g. "=== Lines metrics ===". Prints the name
   --  followed by metrics First..Last.

   function Skip_Print_Metrix
     (Cmd : Command_Line;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Positive;
      Doing_Coupling_Metrics : Boolean) return Boolean;
   --  Return True if Print_Metrix and XML_Print_Metrix should return
   --  immediately without printing anything.

   procedure Print_Metrix
     (Cmd : Command_Line;
      File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Positive);
   --  Print the metrics for one node in text form

   procedure XML_Print_Metrix_Vals
     (Metrics_To_Compute : Metrics_Set;
      First : Metrics_Enum;
      Last : Metrics_Enum;
      M : Metrix;
      Depth : Positive);

   procedure XML_Print_Metrix
     (Cmd : Command_Line;
      File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Positive);
   --  Print the metrics for one node in XML form

   procedure Print_File_Metrics
     (Cmd : Command_Line;
      XML_File : in out Text_IO.File_Type;
      File_M : Metrix;
      Metrics_To_Compute : Metrics_Set;
      Object_Dir : String);
   --  Print out the metrics for this file, and for all units
   --  within it.

   function Is_Private (Node : Ada_Node) return Boolean;
   --  True if Node is a private library unit. ???This doesn't work for
   --  bodies; we need semantic information.

   procedure Process_CU
     (Tool : in out Metrics_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Unit : Analysis_Unit;
      CU_Node : Ada_Node);
   --  Process one compilation unit

   function Push_New_Metrix
     (Tool : in out Metrics_Tool'Class;
      Node : Ada_Node;
      Ignore_Assertions : Boolean;
      Source_File_Name : String_Ref := null)
     return Metrix_Ref
   is
      K : constant Ada_Node_Kind_Type :=
        (if Node.Is_Null then Null_Kind else Kind (Node));

      procedure Set_CU_Metrix
        (M : Metrix_Ref;
         Outer_Unit_Kind : Ada_Node_Kind_Type;
         Is_Subunit : Boolean);
      --  Set the appropriate elements of Specs or Bodies.
      --  Also sets M.Is_Spec.

      procedure Set_CU_Metrix
        (M : Metrix_Ref;
         Outer_Unit_Kind : Ada_Node_Kind_Type;
         Is_Subunit : Boolean)
      is
         S : Metrix_Ref renames Get_CU_Access (Specs, M.CU_Name).all;
         B : Metrix_Ref renames Get_CU_Access (Bodies, M.CU_Name).all;
         --  M will be placed in either S or B, depending on whether
         --  it's a spec or a body.
      begin
         if Is_Subunit then
            B := M;
            M.Is_Spec := False;
            return;
         end if;

         case Outer_Unit_Kind is
            when Ada_Generic_Package_Decl |
              Ada_Package_Decl |
              Ada_Single_Protected_Decl |
              Ada_Protected_Type_Decl |
              Ada_Single_Task_Decl |
              Ada_Task_Type_Decl |
              Ada_Generic_Subp_Instantiation |
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Renaming_Decl |
              Ada_Package_Renaming_Decl |
              Ada_Abstract_Subp_Decl |
              Ada_Null_Subp_Decl |
              Ada_Subp_Renaming_Decl =>
               pragma Assert (S = null);
               S := M;
               M.Is_Spec := True;

            when Ada_Package_Body |
              Ada_Protected_Body |
              Ada_Entry_Body |
              Ada_Task_Body =>
               pragma Assert (B = null);
               B := M;
               M.Is_Spec := False;

            --  A subprogram body could act as a spec. If we haven't
            --  seen the spec (yet), we assume the body is a spec.
            --  If we later see a spec, we move the body to the Bodies array.

            when Ada_Subp_Body =>
               pragma Assert (B = null);
               if S = null then
                  S := M;
                  M.Is_Spec := True;
                  --  ???Is_Spec could be wrong here. We need semantic
                  --  information to know if this body is acting as a spec.
                  --  It will be fixed up below if the spec comes along later.
               else
                  B := M;
                  M.Is_Spec := False;
               end if;

            when Ada_Subp_Decl | Ada_Generic_Subp_Decl =>
               if S /= null then
                  pragma Assert (B = null);
                  B := S;
                  B.Is_Spec := False;
               end if;
               S := M;
               M.Is_Spec := True;

            when others =>
               raise Program_Error with M.Node.Image;
         end case;
      end Set_CU_Metrix;

   --  Start of processing for Push_New_Metrix

   begin
      return Result : constant Metrix_Ref := new Metrix (Kind => K) do
         Push (Tool.Metrix_Stack, Result);

         Result.Node := Node;
         Result.Knd := Get_Fine_Kind (Node);
         Result.Sloc := (if Node.Is_Null
                           then Slocs.No_Source_Location_Range
                           else Sloc_Range (Node));
         Result.Is_Private_Lib_Unit := Is_Private (Node);
         Result.Has_Complexity_Metrics :=
           Has_Complexity_Metrics (Node, Ignore_Assertions);

         if not Node.Is_Null then
            Result.Comp_Unit := Element (Tool.Metrix_Stack, 2);

            declare
               function Get_Name (N : Ada_Node) return String is
                 (To_UTF8 (Full_Name (Get_Def_Name (N).As_Name)));
            begin
               Result.XML_Name := Intern (Get_Name (Node));

               if K = Ada_Compilation_Unit then
                  declare
                     Outer_Unit : constant Ada_Node := Get_Outer_Unit (Node);
                     Is_Subunit : constant Boolean :=
                       Unit_Is_Subunit (Outer_Unit);
                     Sub_Str : constant String :=
                       (if Is_Subunit
                          then "subunit "
                          else "");
                     Dot : constant String := (if Is_Subunit then "." else "");
                  begin
                     Result.Subunit_Parent :=
                       (if Is_Subunit
                          then W_Intern (Full_Name (Node.As_Compilation_Unit
                                                    .F_Body.As_Subunit.F_Name))
                          else Empty_CU_Sym);
                     Result.Text_Name :=
                       Intern (Sub_Str & Str (Result.Subunit_Parent).S & Dot &
                                 Get_Name (Outer_Unit));
                     Result.CU_Name :=
                       Intern (Str (Result.Subunit_Parent).S & Dot &
                                 Get_Name (Outer_Unit));
                     Set_CU_Metrix (Result, Kind (Outer_Unit), Is_Subunit);
                  end;

               else
                  Result.Text_Name := Result.XML_Name;
                  Result.LI_Sub :=
                    (if Length (Tool.Metrix_Stack) = 3
                       then (if Unit_Is_Subunit (Node)
                               then Subunit_Sym
                               else Library_Item_Sym)
                       else Name_Empty);
               end if;
            end;
         end if;

         if Source_File_Name /= null then
            Result.Source_File_Name := Source_File_Name;
         end if;

         if not Node.Is_Null
           and then Kind (Node) = Ada_Package_Body
           and then not Node.As_Package_Body.F_Stmts.Is_Null
         then
            Result.Statements_Sloc :=
              Node.As_Package_Body.F_Stmts.F_Stmts.Sloc_Range;
         end if;

         if not Node.Is_Null
           and then Kind (Node) in Ada_Subp_Body | Ada_Subp_Body_Stub
         then
            Result.Acts_As_Spec := True;

            declare
               B : constant Body_Node := Node.As_Body_Node;
            begin
               if not P_Decl_Part (B).Is_Null then
                  Result.Acts_As_Spec := False;
               end if;
            exception
               when Property_Error =>
                  --  If the spec is not found, we leave Acts_As_Spec = True
                  null;
            end;
         end if;
      end return;
   end Push_New_Metrix;

   function Get_Outer_Unit (Node : Ada_Node) return Ada_Node is
      Lib_Item_Or_Subunit : constant Ada_Node :=
        Node.As_Compilation_Unit.F_Body;
   begin
      return
        (case Kind (Lib_Item_Or_Subunit) is
           when Ada_Library_Item =>
              Lib_Item_Or_Subunit.As_Library_Item.F_Item.As_Ada_Node,
           when Ada_Subunit =>
              Lib_Item_Or_Subunit.As_Subunit.F_Body.As_Ada_Node,
           when others => raise Program_Error);
   end Get_Outer_Unit;

   function Assertion_Kind (Node : Ada_Node) return Assertion_Enum is
      Contract_Cases : constant W_Str := "contract_cases";
      Pre : constant W_Str := "pre";
      Post : constant W_Str := "post";
      Class : constant W_Str := "class";
   begin
      case Kind (Node) is
         when Ada_Pragma_Node =>
            if Pragma_Name (Node) = "assert" then
               return Other_Assertion;
            end if;

         when Ada_Aspect_Assoc =>
            declare
               Id : constant Name := Node.As_Aspect_Assoc.F_Id;
            begin
               case Kind (Id) is
                  when Ada_Identifier =>
                     declare
                        Text : constant W_Str := L_Name (Id);
                     begin
                        if Text = Pre then
                           return Other_Assertion;
                        elsif Text = Contract_Cases
                          or else Text = Post
                        then
                           return Postcondition;
                        end if;
                     end;

                  when Ada_Attribute_Ref =>
                     declare
                        Prefix : constant W_Str :=
                          L_Name (Id.As_Attribute_Ref.F_Prefix);
                        Attr : constant W_Str :=
                          L_Name (Id.As_Attribute_Ref.F_Attribute);
                     begin
                        if Attr = Class then
                           if Prefix = Pre then
                              return Other_Assertion;
                           elsif Prefix = Post then
                              return Postcondition;
                           end if;
                        end if;
                     end;
                  when others => raise Program_Error;
               end case;
            end;
         when others =>
            null;
      end case;

      return Not_An_Assertion;
   end Assertion_Kind;

   function Has_Complexity_Metrics
     (Node : Ada_Node; Ignore_Assertions : Boolean) return Boolean is
   begin
      if Node.Is_Null then
         return False;
      end if;

      case Kind (Node) is
         when Ada_Compilation_Unit =>
            --  At the file level, we print complexity metrics if the outermost
            --  unit has complexity metrics, or if it's a package decl, etc.

            declare
               Outer_Unit : constant Ada_Node := Get_Outer_Unit (Node);
            begin
               return Has_Complexity_Metrics (Outer_Unit, Ignore_Assertions)
                       or else Kind (Outer_Unit) in
                         Ada_Package_Decl | Ada_Generic_Package_Decl |
                         Ada_Package_Body | Ada_Protected_Body;
            end;

         when Ada_Expr_Function |
           Ada_Entry_Body |
           Ada_Subp_Body |
           Ada_Task_Body =>
            return True;

         when Ada_Package_Body =>
            --  Apparently, gnatmetric doesn't do nested package bodies if
            --  there are no statements.
            return not Node.As_Package_Body.F_Stmts.Is_Null;

         when Contract_Complexity_Eligible =>
            return not Ignore_Assertions;

         when others =>
            return False;
      end case;
   end Has_Complexity_Metrics;

   function Get_Fine_Kind (Node : Ada_Node) return Fine_Kind is
   begin
      if Node.Is_Null then
         return No_Such_Knd;
      end if;

      case Kind (Node) is
         when Ada_Compilation_Unit =>
            return No_Such_Knd;

         when Ada_Generic_Package_Decl =>
            return Generic_Package_Knd;
         when Ada_Package_Body =>
            return Package_Body_Knd;
         when Ada_Package_Decl =>
            return Package_Knd;
         when Ada_Protected_Body =>
            return Protected_Body_Knd;
         when Ada_Single_Protected_Decl =>
            return Protected_Object_Knd;
         when Ada_Protected_Type_Decl =>
            return Protected_Type_Knd;
         when Ada_Entry_Body =>
            return Entry_Body_Knd;
         when Ada_Subp_Body =>
            declare
               R : constant Type_Expr :=
                 F_Subp_Returns (Get_Subp_Spec (Node));
            begin
               return
                 (if R.Is_Null then Procedure_Body_Knd else Function_Body_Knd);
            end;
         when Ada_Subp_Body_Stub =>
            declare
               R : constant Type_Expr :=
                 F_Subp_Returns (Get_Subp_Spec (Node));
            begin
               return
                 (if R.Is_Null then Procedure_Body_Stub_Knd
                  else Function_Body_Stub_Knd);
            end;
         when Ada_Task_Body =>
            return Task_Body_Knd;
         when Ada_Single_Task_Decl =>
            return Task_Object_Knd;
         when Ada_Task_Type_Decl =>
            return Task_Type_Knd;

         when Ada_Generic_Package_Instantiation =>
            return Package_Instantiation_Knd;
         when Ada_Generic_Renaming_Decl =>
            return Generic_Package_Renaming_Knd; -- ???Or proc/func
         when Ada_Generic_Subp_Instantiation =>
            return
              (case Ada_Subp_Kind'
                 (Kind (Node.As_Generic_Subp_Instantiation.F_Kind))
               is
                 when Ada_Subp_Kind_Function =>
                    Function_Instantiation_Knd,
                 when Ada_Subp_Kind_Procedure =>
                    Procedure_Instantiation_Knd);
         when Ada_Generic_Subp_Decl =>
            declare
               R : constant Type_Expr :=
                 F_Subp_Returns (Get_Subp_Spec (Node));
               --  ???R is null here even for functions
            begin
               return
                 (if R.Is_Null
                    then Generic_Procedure_Knd
                    else Generic_Function_Knd);
            end;
         when Ada_Package_Renaming_Decl =>
            return Package_Renaming_Knd;
         when Ada_Abstract_Subp_Decl |
             Ada_Subp_Renaming_Decl |
             Ada_Subp_Decl =>
            declare
               R : constant Type_Expr :=
                 F_Subp_Returns (Get_Subp_Spec (Node));
            begin
               return (if R.Is_Null then Procedure_Knd else Function_Knd);
            end;
         when Ada_Null_Subp_Decl =>
            return Null_Procedure_Knd;

         when Ada_Expr_Function =>
            return Expression_Function_Knd;

         when others => raise Program_Error;
      end case;
   end Get_Fine_Kind;

   function Fine_Kind_String (Kind : Fine_Kind) return String is
      --  For example, we convert Generic_Package_Knd into the string value
      --  "generic package".
      use Ada.Characters.Handling;
   begin
      return Replace_String
        (Strip_Suffix (To_Lower (Kind'Img), Suffix => "_knd"),
         From => "_", To => " ");
   end Fine_Kind_String;

   function Fine_Kind_String_For_Header (M : Metrix) return String is
   begin
      --  Prepend "private " if appropriate.
      --  Also, gnatmetric says "containing package instance" at the top, but
      --  uses "instantiation" elsewhere.

      return ((if M.Is_Private_Lib_Unit then "private " else "") &
        Replace_String
          (Fine_Kind_String (M.Knd),
           From => "instantiation", To => "instance"));
   end Fine_Kind_String_For_Header;

   function Is_Private (Node : Ada_Node) return Boolean is
   begin
      if Node.Is_Null or else Kind (Node) = Ada_Compilation_Unit then
         return False;
      end if;

      declare
         P : constant Ada_Node := Parent (Node);
      begin
         return Kind (P) = Ada_Library_Item
           and then P.As_Library_Item.F_Has_Private;
      end;
   end Is_Private;

   function Metric_Name_String (Metric : Metrics_Enum) return String is
   begin
      --  In many, but not all, cases the name of the metric in the text output
      --  is the same as the name in the XML, replacing "_" with " ".

      case Metric is
         when Statements =>
            return "all statements";
         when Declarations =>
            return "all declarations";
         when Logical_Source_Lines =>
            return "logical SLOC";
         when All_Types =>
            return "all type definitions";
         when Loop_Nesting =>
            return "maximum loop nesting";
         when All_Subprograms =>
            return "all subprogram bodies";
         when Unit_Nesting =>
            return "maximal unit nesting";
         when Construct_Nesting =>
            return "maximal construct nesting";
         when Lines_Eol_Comment =>
            return "end-of-line comments";
         when Lines_Average =>
            return "Average lines in body";
         when In_Parameters =>
            return "IN parameters";
         when Out_Parameters =>
            return "OUT parameters";
         when In_Out_Parameters =>
            return "IN OUT parameters";

         when others =>
            return Replace_String
              (XML_Metric_Name_String (Metric), From => "_", To => " ");
      end case;
   end Metric_Name_String;

   function Should_Print
     (Metric : Metrics_Enum;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Positive;
      XML : Boolean) return Boolean
   is
      pragma Assert ((M.Kind = Ada_Compilation_Unit) = (Depth = 2));
      pragma Assert (M.Node.Is_Null);
   begin
      --  Don't print metrics that weren't requested on the command line (or by
      --  default).

      if not Metrics_To_Compute (Metric) then
         return False;
      end if;

      --  Don't print metrics for nested subprogram declarations unless it's
      --  one of the few metrics that applies to those.

      if Depth > 3
        and then M.Kind in Contract_Complexity_Eligible
        and then Metric not in Contract_Complexity |
          Param_Number | In_Parameters | Out_Parameters | In_Out_Parameters
      then
         return False;
      end if;

      if M.Kind in Ada_Subp_Body_Stub | Ada_Generic_Subp_Instantiation
        and then Metric not in
          Param_Number | In_Parameters | Out_Parameters | In_Out_Parameters
      then
         return False;
      end if;

      case Metric is
         when Lines |
           Lines_Code |
           Lines_Comment |
           Lines_Eol_Comment |
           Lines_Blank |
           Lines_Ratio |
           Lines_Code_In_Bodies |
           Num_Bodies =>
            return True;
         when Lines_Average | Lines_Spark =>
            return Depth in 1 | 2;

         when All_Subprograms =>
            return (Depth = 3
              and then M.Kind in
                      Ada_Package_Body |
                      Ada_Subp_Body |
                      Ada_Task_Body |
                      Ada_Protected_Body)
              or else (XML and then Depth = 1);
         when All_Types | Public_Types =>
            return (Depth = 3 or else (XML and then Depth = 1))
              and then M.Vals (Metric) > 0;
         when Private_Types =>
            return Depth = 3 and then M.Vals (Metric) > 0;
         when Public_Subprograms =>
            if Depth = 3
              and then (M.Is_Private_Lib_Unit or else M.LI_Sub = Subunit_Sym)
            then
               pragma Assert (M.Vals (Metric) = 0);
               return False;
            end if;

            if XML and then Depth = 1 then
               return True;
            end if;

            if Depth = 3 then
               if M.Kind in
                 Ada_Package_Decl |
                 Ada_Generic_Package_Decl |
                 Ada_Subp_Decl |
                 Ada_Generic_Subp_Decl
               then
                  return True;
               end if;

               if M.Kind in Ada_Subp_Body and then Acts_As_Spec (M) then
                  return True;
               end if;
            end if;

            return False;

         when Declarations |
           Statements |
           Logical_Source_Lines =>
            return M.Kind /= Ada_Compilation_Unit;
         when Unit_Nesting | Construct_Nesting =>
            return Depth >= 3 and then M.Vals (Metric) > 0;
         when Current_Construct_Nesting =>
            return False;
         when Param_Number =>
            if M.Kind in Ada_Subp_Decl | Ada_Generic_Subp_Instantiation |
              Ada_Expr_Function | Ada_Null_Subp_Decl
            then
               return True;
            end if;

            if M.Kind in Ada_Subp_Body |  Ada_Subp_Body_Stub
              and then Acts_As_Spec (M)
            then
               return True;
            end if;

            return False;

         when In_Parameters |
           Out_Parameters |
           In_Out_Parameters =>
            return M.Vals (Param_Number) > 0
              and then
                Should_Print (Param_Number, Metrics_To_Compute, M, Depth, XML);

         when Contract_Complexity =>
            return M.Kind in Contract_Complexity_Eligible and then M.Visible;
         when Contract | Post | Contract_Complete =>
            return M.Visible;

         when Complexity_Statement |
           Complexity_Expression |
           Complexity_Cyclomatic |
           Complexity_Essential |
           Loop_Nesting =>
            return M.Has_Complexity_Metrics;
         when Complexity_Average =>
            return XML and Depth = 1;
         when Extra_Exit_Points =>
            return M.Kind in Ada_Subp_Body | Ada_Task_Body | Ada_Entry_Body
                or else
              (M.Kind = Ada_Package_Body
               and then M.Statements_Sloc /= Slocs.No_Source_Location_Range);

         when Coupling_Metrics =>
            pragma Assert (Depth <= 3);
            return Result : Boolean do
               if Depth = 3 then
                  case Coupling_Metrics'(Metric) is
                     when Tagged_Coupling_Out | Tagged_Coupling_In |
                       Hierarchy_Coupling_Out | Hierarchy_Coupling_In =>
                        Result := M.Comp_Unit.Has_Tagged_Type;
                     when Control_Coupling_Out | Control_Coupling_In =>
                        Result := M.Comp_Unit.Has_Subp;
                     when Unit_Coupling_Out | Unit_Coupling_In =>
                        Result := True;
                  end case;
               else
                  Result := False;
               end if;

               pragma Assert (if not Result then M.Vals (Metric) = 0);
            end return;

         when Computed_Metrics =>
            raise Program_Error;
      end case;
   end Should_Print;

   Metric_Found : Metrics_Set := [others => False];
   --  Metric_Found(M) is True if we computed some non-default value for M.

   function Val_To_Print
     (Metric : Metrics_Enum; M : Metrix; XML : Boolean) return String is
      type Fixed is delta 0.01 digits 8;
   begin
      if M.Vals (Metric) /= Initial_Metrics_Values (Metric) then
         Metric_Found (Metric) := True;
      end if;

      if not Implemented (Metric) then
         pragma Assert (M.Vals (Metric) = Initial_Metrics_Values (Metric));
         return "nyi";
      end if;

      if Metric in Complexity_Metrics | Lines_Average
        and then M.Kind in Ada_Compilation_Unit | Null_Kind
      then
         if Metric = Loop_Nesting then
            --  We want the total here, not the average.

            declare
               Img : constant String := Fixed (Float (M.Vals (Metric)))'Img;
            begin
               pragma Assert (Img'First = 1 and then Img (1) = ' ');
               return (if XML then "" else " ") & Img (2 .. Img'Last);
            end;
         end if;

         declare
            --  Calculate average in type Float, then convert to
            --  fixed-point for printing in a form like "12.34".
            --  We need to adjust M.Vals (Metric) by
            --  M.Num_With_Complexity for certain metrics, because
            --  those were initialized to 1 in the subnodes, and not
            --  incremented in the file-level data.

            Num : constant Metric_Nat :=
              (if Metric = Lines_Average
                 then (if M.Vals (Num_Bodies) = 0
                         then 1
                         else M.Vals (Num_Bodies))
                 else M.Num_With_Complexity);
            --  Number of items (divide by this to compute average)
            pragma Assert (Num > 0);
            Adjust : constant Metric_Nat :=
              (if Metric in Complexity_Statement |
                            Complexity_Cyclomatic |
                            Complexity_Essential
                 then Num - 1
                 else 0);
            Numerator_Metric : constant Metrics_Enum :=
              (if Metric = Lines_Average
                then Lines_Code_In_Bodies
                else Metric);
            --  Metric whose value is used as the numerator when computing the
            --  average. For complexity metrics, that is the Metric itself, but
            --  for Lines_Average, we need to divide Lines_Code_In_Bodies by
            --  something.
            Numerator : constant Metric_Nat := M.Vals (Numerator_Metric);
            Av : constant Float :=
              Float (Numerator + Adjust) / Float (Num);
            Img : constant String := Fixed (Av)'Img;
         begin
            pragma Assert (Img'First = 1 and then Img (1) = ' ');
            return (if XML or else Metric = Lines_Average then "" else " ") &
                   Img (2 .. Img'Last);
         end;

      elsif Metric = Lines_Ratio then
         declare
            --  Mimic gnatmetric, here:
            Comments : constant Float :=
              Float (M.Vals (Lines_Comment)) +
              Float (M.Vals (Lines_Eol_Comment));
            Code : constant Float :=
              Float (M.Vals (Lines_Comment)) +
              Float (M.Vals (Lines_Code));
         begin
            if Code = 0.0 then
               return "0/0"; -- avoid division by zero
            else
               declare
                  Img : constant String := Fixed (Comments / Code * 100.0)'Img;
               begin
                  pragma Assert (Img'First = 1 and then Img (1) = ' ');
                  return Img (2 .. Img'Last);
               end;
            end if;
         end;

      else
         return Image (M.Vals (Metric));
      end if;
   end Val_To_Print;

   Average_Complexity_Metrics : constant String :=
     "=== Average complexity metrics ===";
   --  Used to indicate we should indent an extra level, and that we should
   --  put underscores in the metric name.

   First_Printed_Range : Boolean := True;

   procedure Print_Range
     (Name : String;
      Metrics_To_Compute : Metrics_Set;
      First, Last : Metrics_Enum;
      M : Metrix;
      Depth : Positive)
   is
      First_Printed_Metric : Boolean := True;
   begin
      if not Should_Print_Any
        (First, Last, Metrics_To_Compute, M, Depth, XML => False)
      then
         return;
      end if;

      if Depth /= 1 then
         Put ("\n");
      end if;

      if First_Printed_Range then
         First_Printed_Range := False;
      else
         Put ("\n");
      end if;

      Put ("\1\n", Name);

      for I in First .. Last loop
         if Should_Print
           (I, Metrics_To_Compute, M, Depth, XML => False)
         then
            declare
               Metric_Name : constant String :=
                 (if Name = Average_Complexity_Metrics
                    then XML_Metric_Name_String (I)
                    else Metric_Name_String (I));
               Indentation_Amount : constant Natural :=
                 (if I = Lines_Average
                    then 0
                  elsif Depth = 1
                    then 2
                  elsif Depth = 2 and then First in Lines_Metrics
                    then 2
                  elsif Name = Average_Complexity_Metrics
                    then 2 * Default_Indentation_Amount
                  else Default_Indentation_Amount);
               --  Indentation_Amount, and Tab below, are
               --  intended to mimic some partially arbitrary
               --  behavior of gnatmetric.
               Tab : constant Positive :=
                 (if Depth = 1 and then I in Lines_Metrics
                    then 22
                  elsif Depth = 1 or else I in Lines_Metrics
                    then 21
                  else 26);
            begin
               Indent (Indentation_Amount);
               if First_Printed_Metric then
                  First_Printed_Metric := False;
               else
                  Put ("\n");
               end if;
               if I = Lines_Average then -- gnatmetric puts extra line
                  Put ("\n");
               end if;
               Put ("\1", Metric_Name);
               Tab_To_Column (Indentation_Level + Tab);
               Put (": \1", Val_To_Print (I, M, XML => False));
               Outdent (Indentation_Amount);
            end;
         end if;
      end loop;

      if Depth = 1 and then First in Lines_Metrics then
         Put ("\n");
      end if;
   end Print_Range;

   function Skip_Print_Metrix
     (Cmd : Command_Line;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Positive;
      Doing_Coupling_Metrics : Boolean) return Boolean is
   begin
      --  Skip if we're doing coupling metrics, and this is not a
      --  compilation unit spec, or if M is for a Contract_Complexity_Eligible
      --  node, and we're not going to print. Also don't print metrics for
      --  "eligible local program units" if the -nolocal switch was given.

      if Depth > 3 then
         if Doing_Coupling_Metrics then
            return True;
         end if;

         if M.Kind in Contract_Complexity_Eligible
           and then not
             (Should_Print
               (Contract_Complexity, Metrics_To_Compute, M, Depth, XML => True)
                or else
              Should_Print
               (Param_Number, Metrics_To_Compute, M, Depth, XML => True))
         then
            return True;
         end if;

         if Arg (Cmd, No_Local_Metrics) then
            return True;
         end if;
      end if;

      --  For subprogram stubs and instantiations, Param_Number is the only
      --  metric printed.

      if M.Kind in Ada_Subp_Body_Stub | Ada_Generic_Subp_Instantiation
        and then not Metrics_To_Compute (Param_Number)
      then
         return True;
      end if;

      --  Coupling metrics are only printed for specs

      if M.Kind = Ada_Compilation_Unit
        and then Doing_Coupling_Metrics
        and then not M.Is_Spec
      then
         return True;
      end if;

      return False;
   end Skip_Print_Metrix;

   procedure Print_Metrix
     (Cmd : Command_Line;
      File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Positive)
   is
      pragma Debug (Validate (M));
      pragma Assert (M.Node.Is_Null);

      Doing_Coupling_Metrics : constant Boolean :=
        (Metrics_To_Compute and not Coupling_Only) = Empty_Metrics_Set;
   begin
      pragma Assert (Metrics_To_Compute /= Empty_Metrics_Set);

      if Skip_Print_Metrix
        (Cmd, Metrics_To_Compute, M, Depth, Doing_Coupling_Metrics)
      then
         return;
      end if;

      if Depth > 3 then
         Indent;
      end if;

      if Doing_Coupling_Metrics then
         if Depth = 3 then
            Put ("Unit \1 (\2)\n", Str (M.Text_Name).S,
                 File_Name_To_Print (Cmd, File_Name));
         end if;
      elsif M.Kind = Ada_Compilation_Unit then
         Put ("Metrics computed for \1\n",
              File_Name_To_Print (Cmd, File_Name));
         Put ("containing \1 \2\n",
              Fine_Kind_String_For_Header (Element (M.Submetrix, 1).all),
              Str (M.Text_Name).S);
      else
         Put ("\n\n\1 (\2\3 at lines  \4)\n",
              Str (M.Text_Name).S,
              Fine_Kind_String (M.Knd), Str (M.LI_Sub).S,
              Lines_String (M.Sloc));
      end if;

      if Doing_Coupling_Metrics and then Depth = 3 then
         if Should_Print_Any
           (Coupling_Metrics'First, Coupling_Metrics'Last,
            Metrics_To_Compute, M, Depth, XML => False)
         then
            Indent;
            for I in Coupling_Metrics loop
               if Should_Print
                 (I, Metrics_To_Compute, M, Depth, XML => False)
               then
                  Put ("\1", Metric_Name_String (I));
                  Tab_To_Column (33);
                  Put (": \1\n", Val_To_Print (I, M, XML => False));
               end if;
            end loop;
            Outdent;
         end if;
      end if;

      --  Print metrix for this unit

      First_Printed_Range := True;

      Print_Range
        ("=== Code line metrics ===",
         Metrics_To_Compute,
         Lines_Metrics'First, Lines_Metrics'Last, M, Depth);

      Print_Range
        ("=== Contract metrics ===",
         Metrics_To_Compute,
         Contract_Metrics'First, Contract_Metrics'Last, M, Depth);

      Print_Range
        ("=== Element metrics ===",
         Metrics_To_Compute,
         Syntax_Metrics'First, Syntax_Metrics'Last, M, Depth);

      if M.Kind /= Ada_Compilation_Unit then
         Print_Range
           ("=== Complexity metrics ===",
            Metrics_To_Compute,
            Complexity_Metrics'First, Complexity_Metrics'Last, M, Depth);
      end if;

      --  Then recursively print metrix of nested units

      for Child of M.Submetrix loop
         Print_Metrix
           (Cmd, File_Name, Metrics_To_Compute, Child.all, Depth + 1);
      end loop;

      --  At the file level, average complexity metrics come last:

      if M.Kind = Ada_Compilation_Unit
        and then M.Num_With_Complexity > 0
      then
         Print_Range
           (Average_Complexity_Metrics,
            Metrics_To_Compute,
            Complexity_Metrics'First, Complexity_Metrics'Last, M, Depth);
      end if;

      if Depth > 3 then
         Outdent;
      end if;
   end Print_Metrix;

   function XML (X : String) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for C of X loop
         case C is
            when '&' => Append (Result, "&amp;");
            when '<' => Append (Result, "&lt;");
            when '>' => Append (Result, "&gt;");
            when '"' => Append (Result, "&quot;");
            when ''' => Append (Result, "&apos;");
            when others => Append (Result, C);
         end case;
      end loop;
      return Q (To_String (Result));
   end XML;

   function XML_Metric_Name_String (Metric : Metrics_Enum) return String is
   begin
      case Metric is
         when Contract =>
            return "contract";
         when Post =>
            return "post";
         when Contract_Complete =>
            return "contract_complete";
         when Contract_Complexity =>
            return "contract_complexity";
         when Complexity_Statement =>
            return "statement_complexity";
         when Complexity_Expression =>
            return "expression_complexity";
         when Complexity_Cyclomatic =>
            return "cyclomatic_complexity";
         when Complexity_Essential =>
            return "essential_complexity";
         when Complexity_Average =>
            return "average_complexity";
         when Loop_Nesting =>
            return "max_loop_nesting";
         when Extra_Exit_Points =>
            return "extra_exit_points";
         when Lines =>
            return "all_lines";
         when Lines_Code =>
            return "code_lines";
         when Lines_Comment =>
            return "comment_lines";
         when Lines_Eol_Comment =>
            return "eol_comments";
         when Lines_Blank =>
            return "blank_lines";
         when Lines_Spark =>
            return "spark_lines";
         when Lines_Average =>
            return "average_lines_in_bodies";
         when Lines_Code_In_Bodies =>
            return "lines_code_in_bodies";
         when Num_Bodies =>
            return "num_bodies";
         when Lines_Ratio =>
            return "comment_percentage";
         when Public_Types =>
            return "public_types";
         when Private_Types =>
            return "private_types";
         when All_Types =>
            return "all_types";
         when Statements =>
            return "all_stmts";
         when Declarations =>
            return "all_dcls";
         when Logical_Source_Lines =>
            return "lsloc";
         when Public_Subprograms =>
            return "public_subprograms";
         when All_Subprograms =>
            return "all_subprograms";
         when Unit_Nesting =>
            return "unit_nesting";
         when Construct_Nesting =>
            return "construct_nesting";
         when Current_Construct_Nesting =>
            raise Program_Error;
         when Param_Number =>
            return "all_parameters";
         when In_Parameters =>
            return "in_parameters";
         when Out_Parameters =>
            return "out_parameters";
         when In_Out_Parameters =>
            return "in_out_parameters";
         when Tagged_Coupling_Out =>
            return "tagged_fan-out_coupling";
         when Hierarchy_Coupling_Out =>
            return "hierarchy_fan-out_coupling";
         when Tagged_Coupling_In =>
            return "tagged_fan-in_coupling";
         when Hierarchy_Coupling_In =>
            return "hierarchy_fan-in_coupling";
         when Control_Coupling_Out =>
            return "control_fan-out_coupling";
         when Control_Coupling_In =>
            return "control_fan-in_coupling";
         when Unit_Coupling_Out =>
            return "unit_fan-out_coupling";
         when Unit_Coupling_In =>
            return "unit_fan-in_coupling";
         when Computed_Metrics =>
            raise Program_Error;
      end case;
   end XML_Metric_Name_String;

   procedure XML_Print_Config is
   begin
      Indent;
      Put ("<config>\n");
      Indent;

      for Metric in Metrics_Enum loop
         case Metric is
            when Lines_Code_In_Bodies | Num_Bodies |
              Current_Construct_Nesting | Computed_Metrics =>
               null;
               --  The above are not documented, and not printed unless
               --  explicitly specified on the command line, so we don't
               --  include them in the <config>.

            when others =>
               Put ("<metric name=""\1"" display_name=""\2""/>\n",
                    XML_Metric_Name_String (Metric),
                    Metric_Name_String (Metric));
         end case;
      end loop;

      Outdent;
      Put ("</config>\n");
      Outdent;
   end XML_Print_Config;

   procedure XML_Print_Metrix_Vals
     (Metrics_To_Compute : Metrics_Set;
      First : Metrics_Enum;
      Last : Metrics_Enum;
      M : Metrix;
      Depth : Positive)
   is
   begin
      Indent;

      for I in First .. Last loop
         if Should_Print (I, Metrics_To_Compute, M, Depth, XML => True) then
            if True or else M.Vals (I) /= 0 then -- ???
               if not (I = Complexity_Average
                 and then M.Kind in Ada_Compilation_Unit | Null_Kind
                 and then M.Num_With_Complexity = 0)
               then
                  Put ("<metric name=\1>\2</metric>\n",
                       Q (XML_Metric_Name_String (I)),
                       Val_To_Print
                         ((if I = Complexity_Average
                             then Complexity_Cyclomatic
                             else I),
                          M, XML => True));
               end if;
            end if;
         end if;

         case I is
            when Param_Number => Indent;
            when In_Out_Parameters => Outdent;
            when others => null;
         end case;
      end loop;

      Outdent;
   end XML_Print_Metrix_Vals;

   procedure XML_Print_Metrix
     (Cmd : Command_Line;
      File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Positive)
   is
      pragma Debug (Validate (M));
      pragma Assert (M.Node.Is_Null);
      pragma Assert (Cur_Column = 1);

      To_Print_First : constant Metrics_Set :=
        Metrics_To_Compute and
          (if M.Kind = Ada_Compilation_Unit
             then not Complexity_Only
             else All_Metrics_Set);
      --  Set of metrics to print first, before printing subtrees. Same as
      --  Metrics_To_Compute, except at the top level, we leave out complexity
      --  metrics, because they will be printed last.

      To_Print_Last : constant Metrics_Set :=
        Metrics_To_Compute and
          (if M.Kind = Ada_Compilation_Unit
               and then M.Num_With_Complexity > 0
             then Complexity_Only
             else Empty_Metrics_Set);
      --  Set of metrics to print last, after printing subtrees. This is
      --  normally empty. At the top level it is the intersection of
      --  Metrics_To_Compute and Complexity_Only, but only if we have
      --  some with complexity metrics.

      Doing_Coupling_Metrics : constant Boolean :=
        (Metrics_To_Compute and Coupling_Only) /= Empty_Metrics_Set;
   begin
      if Metrics_To_Compute = Empty_Metrics_Set then
         return;
      end if;

      if Skip_Print_Metrix
        (Cmd, Metrics_To_Compute, M, Depth, Doing_Coupling_Metrics)
      then
         return;
      end if;

      Indent;

      if M.Kind = Ada_Compilation_Unit then
         Put ("<file name=\1>\n", Q (File_Name_To_Print (Cmd, File_Name)));
      else
         Put ("<unit name=\1\2 line=\3 col=\4>\n",
              XML (Str (M.XML_Name).S),
              (if Doing_Coupling_Metrics
                 then ""
                 else " kind=" & Q (Fine_Kind_String (M.Knd))),
              Q (Image (Integer (M.Sloc.Start_Line))),
              Q (Image (Integer (M.Sloc.Start_Column))));
      end if;

      --  Print metrics for this unit

      XML_Print_Metrix_Vals
        (To_Print_First, M.Vals'First, M.Vals'Last, M, Depth);

      --  Then recursively print metrix of nested units

      for Child of M.Submetrix loop
         XML_Print_Metrix
           (Cmd, File_Name, Metrics_To_Compute, Child.all, Depth + 1);
      end loop;

      --  At the file level, average complexity metrics go at the end:

      if M.Kind = Ada_Compilation_Unit then
         if Debug_Flag_V and Doing_Coupling_Metrics then
            for Sym of M.Depends_On loop
               Put ("<dependence dep=""\1 \2 depends on \3"">\n",
                    Str (M.CU_Name).S,
                    (if M.Is_Spec then "spec" else "body"),
                    Str (Sym).S);
            end loop;
            Put ("\n");
         end if;

         XML_Print_Metrix_Vals
           (To_Print_Last, M.Vals'First, M.Vals'Last, M, Depth);
         Put ("</file>\n");
      else
         Put ("</unit>\n");
      end if;

      Outdent;
   end XML_Print_Metrix;

   procedure Print_File_Metrics
     (Cmd : Command_Line;
      XML_File : in out Text_IO.File_Type;
      File_M : Metrix;
      Metrics_To_Compute : Metrics_Set;
      Object_Dir : String)
   is
      Suffix : constant String :=
        (if Arg (Cmd, Output_Suffix) = null
           then ".metrix"
           else Arg (Cmd, Output_Suffix).all);
      use Ada.Text_IO;
      Text : File_Type;
      File_Name : String renames File_M.Source_File_Name.all;
      Text_File_Name : constant String :=
        (if Arg (Cmd, Output_Directory) = null then
          (if Object_Dir = "" then File_Name & Suffix
           else Directories.Compose
             (Object_Dir, Directories.Simple_Name (File_Name) & Suffix))
         else Directories.Compose
           (Arg (Cmd, Output_Directory).all,
            Directories.Simple_Name (File_Name) & Suffix));
      --  Can't pass Suffix as Extension, because that inserts an extra "."
   begin
      if Text_File_Name = File_Name then
         --  Otherwise, we could overwrite the input!
         raise Program_Error with "empty suffix";
      end if;

      if Gen_Text (Cmd) then
         if not Output_To_Standard_Output then
            Create (Text, Name => Text_File_Name);
            Set_Output (Text);
         end if;

         Print_Metrix
           (Cmd, File_Name, Metrics_To_Compute, File_M, Depth => 2);
         Formatted_Output.Put ("\n");

         if not Output_To_Standard_Output then
            Set_Output (Standard_Output);
            Close (Text);
         end if;
      end if;

      pragma Assert (Indentation_Level = 0);
      if Gen_XML (Cmd) then
         if not Output_To_Standard_Output then
            Set_Output (XML_File);
         end if;
         XML_Print_Metrix
           (Cmd, File_Name, Metrics_To_Compute, File_M, Depth => 2);
         if not Output_To_Standard_Output then
            Set_Output (Standard_Output);
         end if;
      end if;
   end Print_File_Metrics;

   procedure Destroy (M : in out Metrix_Ref) is
      procedure Free is new Ada.Unchecked_Deallocation (Metrix, Metrix_Ref);
   begin
      for Index in 1 .. Last_Index (M.Submetrix) loop
         declare
            Sub : Metrix_Ref := Element (M.Submetrix, Index);
         begin
            Destroy (Sub);
         end;
      end loop;
      Clear (M.Submetrix);

      Free (M);
   end Destroy;

   ----------------------
   -- Write_XML_Schema --
   ----------------------

   procedure Write_XML_Schema (Xsd_File_Name : String) is
      XSD_Out_File : Text_IO.File_Type;
   begin
      if not Output_To_Standard_Output then
         Text_IO.Create (XSD_Out_File, Name => Xsd_File_Name);
         Text_IO.Set_Output (XSD_Out_File);
      end if;

      pragma Style_Checks ("M200"); -- Allow long lines
      Put ("<?xml version=""1.0"" encoding=""UTF-8""?>\n");
      Put ("<xs:schema xmlns:xs=""http://www.w3.org/2001/XMLSchema"">\n");
      Put ("        <xs:element name=""global"">\n");
      Put ("                <xs:complexType>\n");
      Put ("                        <xs:sequence>\n");
      Put ("                                <xs:element ref=""file"" minOccurs=""0"" maxOccurs=""unbounded""/>\n");
      Put ("                                <xs:element ref=""metric"" minOccurs=""0"" maxOccurs=""unbounded""/>\n");
      Put ("                                <xs:element ref=""coupling"" minOccurs=""0"" maxOccurs=""1""/>\n");
      Put ("                        </xs:sequence>\n");
      Put ("                </xs:complexType>\n");
      Put ("        </xs:element>\n");
      Put ("        <xs:element name=""file"">\n");
      Put ("                <xs:complexType>\n");
      Put ("                        <xs:sequence>\n");
      Put ("                                <xs:element ref=""metric"" minOccurs=""0"" maxOccurs=""unbounded""/>\n");
      Put ("                                <xs:element ref=""unit"" minOccurs=""0"" maxOccurs=""unbounded""/>\n");
      Put ("                        </xs:sequence>\n");
      Put ("                        <xs:attribute name=""name"" use=""required"" type=""xs:string""/>\n");
      Put ("                </xs:complexType>\n");
      Put ("        </xs:element>\n");
      Put ("        <xs:element name=""unit"">\n");
      Put ("                <xs:complexType>\n");
      Put ("                        <xs:sequence>\n");
      Put ("                                <xs:element ref=""metric"" minOccurs=""0"" maxOccurs=""unbounded""/>\n");
      Put ("                                <xs:element ref=""unit"" minOccurs=""0"" maxOccurs=""unbounded""/>\n");
      Put ("                        </xs:sequence>\n");
      Put ("                        <xs:attribute name=""name"" use=""required"" type=""xs:string""/>\n");
      Put ("                        <xs:attribute name=""line"" use=""required"" type=""xs:decimal""/>\n");
      Put ("                        <xs:attribute name=""kind"" type=""xs:string""/>\n");
      Put ("                        <xs:attribute name=""col"" use=""required"" type=""xs:byte""/>\n");
      Put ("                </xs:complexType>\n");
      Put ("        </xs:element>\n");
      Put ("        <xs:element name=""metric"">\n");
      Put ("                <xs:complexType>\n");
      Put ("                        <xs:simpleContent>\n");
      Put ("                                <xs:extension base=""xs:decimal"">\n");
      Put ("                                        <xs:attribute name=""name"" use=""required"" type=""xs:string""/>\n");
      Put ("                                </xs:extension>\n");
      Put ("                        </xs:simpleContent>\n");
      Put ("                </xs:complexType>\n");
      Put ("        </xs:element>\n");
      Put ("        <xs:element name=""coupling"">\n");
      Put ("                <xs:complexType>\n");
      Put ("                        <xs:sequence>\n");
      Put ("                                <xs:element ref=""file"" minOccurs=""0"" maxOccurs=""unbounded""/>\n");
      Put ("                        </xs:sequence>\n");
      Put ("                </xs:complexType>\n");
      Put ("        </xs:element>\n");
      Put ("</xs:schema>\n");
      pragma Style_Checks ("M79");

      if not Output_To_Standard_Output then
         Text_IO.Set_Output (Text_IO.Standard_Output);
         Text_IO.Close (XSD_Out_File);
      end if;
   end Write_XML_Schema;

   ----------
   -- Init --
   ----------

   procedure Init
     (Tool : in out Metrics_Tool; Cmd : in out Command_Line)
   is

      function To_Compute return Metrics_Set;
      --  Computes which metrics we should compute. We actually compute all
      --  metrics, but this controls which ones we print. If some metrics are
      --  expensive to compute, we could suppress their computation when we're
      --  not going to print, but most metrics are so cheap that it's not worth
      --  it.

      function To_Compute return Metrics_Set is
      begin
         return Result : Metrics_Set do
            --  Set Result components True for all metrics requested on the
            --  command line:

            for Metric in Metrics_Enum loop
               Result (Metric) := Arg (Cmd, Metric);
            end loop;

            --  "all" switches:

            if Arg (Cmd, Contract_All) then
               Result (Contract_Metrics) := [others => True];
            end if;

            if Arg (Cmd, Complexity_All) then
               Result (Complexity_Metrics) := [others => True];
            end if;

            if Arg (Cmd, Lines_All) then
               Result (Lines_Metrics) := [others => True];
            end if;

            if Arg (Cmd, Syntax_All) then
               Result (Syntax_Metrics) := [others => True];
            end if;

            if Arg (Cmd, Coupling_All) then
               Result (Coupling_Metrics) := [others => True];
            end if;

            --  Special cases

            if Arg (Cmd, Complexity_Cyclomatic) then
               Result (Complexity_Statement) := True;
               Result (Complexity_Expression) := True;
            end if;

            if Arg (Cmd, Public_Types) then
               Result (Private_Types) := True;
            end if;

            if Arg (Cmd, Statements) and then Arg (Cmd, Declarations) then
               Result (Logical_Source_Lines) := True;
            end if;

            if Arg (Cmd, Param_Number) then
               Result (In_Parameters) := True;
               Result (Out_Parameters) := True;
               Result (In_Out_Parameters) := True;
            end if;

            --  If no metrics were requested on the command line, or the
            --  --metrics-all switch was specified, we compute all metrics
            --  except "computed" metrics and the Lines_Code_In_Bodies and
            --  Num_Bodies (unless explicitly specified on the command
            --  Line).

            if Result = Empty_Metrics_Set or else Arg (Cmd, Metrics_All) then
               Result := [Computed_Metrics => False, others => True];
               Result (Lines_Code_In_Bodies) :=
                 Arg (Cmd, Lines_Code_In_Bodies);
               Result (Num_Bodies) := Arg (Cmd, Num_Bodies);

               declare
                  Explicit_False_Switches : Metrics_Set := Empty_Metrics_Set;
               begin
                  for Metric in Metrics_Enum loop
                     Explicit_False_Switches (Metric) :=
                       Explicit (Cmd, Metric);
                  end loop;

                  if Explicit_False_Switches /= Empty_Metrics_Set then
                     Result := Result and not Explicit_False_Switches;
                  end if;
               end;
            end if;
         end return;
      end To_Compute;

      Metrics_To_Compute : Metrics_Set renames Tool.Metrics_To_Compute;

      Ignored : constant Metrix_Ref :=
         Push_New_Metrix (Tool, No_Ada_Node, Arg (Cmd, Ignore_Assertions));
      --  Initialize the Metrix_Stack by pushing the outermost Metrix, which is
      --  for totals for all the files together.

   --  Start of processing for Init

   begin
      Tool.Treat_Exit_As_Goto := not Arg (Cmd, No_Treat_Exit_As_Goto);
      Metrics_To_Compute := To_Compute; -- Decide what metrics to print
   end Init;

   -----------
   -- Final --
   -----------

   procedure Compute_Indirect_Dependencies (Global_M : Metrix) with
     Pre => Global_M.Kind = Null_Kind;
   --  Depends_On contains direct dependencies (as computed by
   --  Gather_Dependencies). This computes the indirect dependencies
   --  for all compilation units by walking the dependency graph.

   procedure Compute_Coupling
     (Tool : in out Metrics_Tool; Global_M : Metrix;
      Metrics_To_Compute : Metrics_Set);
   --  This uses the dependency information to compute the coupling metrics.
   --  Metrics_To_Compute is for debugging only.

   function Get_Spec (M : Metrix_Ref) return Metrix_Ref with
     Pre => M.Kind = Ada_Compilation_Unit;
   --  If M is a spec, return M. If it's a library unit body, return the
   --  corresponding spec. If it's a subunit, return the spec of the innermost
   --  enclosing library unit. If the spec is not present, return M.

   procedure Print_Coupling
     (Cmd : Command_Line;
      Metrics_To_Compute : Metrics_Set);
   procedure XML_Print_Coupling
     (Cmd : Command_Line;
      Metrics_To_Compute : Metrics_Set);
   --  Print the metrics computed by Compute_Coupling

   Units_For_Coupling : Metrix_Vectors.Vector;
   --  Compilation units for which coupling metrics will be printed

   procedure Compute_Indirect_Dependencies (Global_M : Metrix) is

      procedure Remove_Nonexistent_Units (M : Metrix_Ref);
      --  Removes units from M.Depends_On if those units are not being
      --  processed by this run of the tool. So "with X;" is ignored if x.ads
      --  is not being processed. We can't do this in the first place, because
      --  we need to wait until all the Metrix exist.

      procedure Visit
        (M : Metrix_Ref; Depends_On : in out CU_Symbol_Sets.Set);
      --  Visit one node in the dependency graph of compilation units.
      --  Recursively calls Visit to visit compilation units that M depends
      --  upon. Depends_On is that of the caller (some unit that depends on M);
      --  this unions-in M and everything M depends upon directly or
      --  indirectly.

      procedure Remove_Nonexistent_Units (M : Metrix_Ref) is
         Nonexistent_Units : CU_Symbol_Sets.Set;
      begin
         if M = null then
            return;
         end if;

         --  Remove nonexistent units:

         for Sym of M.Depends_On loop
            if Same_Ignoring_Case (Sym) > Last_Index (Specs)
              or else Specs (Same_Ignoring_Case (Sym)) = null
            then
               Insert (Nonexistent_Units, Sym);
            end if;
         end loop;

         Difference (M.Depends_On, Nonexistent_Units);
      end Remove_Nonexistent_Units;

      procedure Visit
        (M : Metrix_Ref; Depends_On : in out CU_Symbol_Sets.Set) is
      begin
         --  The "limited with" clauses aare not included in
         --  Depends_On. Therefore, there cannot be cycles in the
         --  graph.

         if not M.Indirect_Dependences_Computed then
            M.Indirect_Dependences_Computed := True;
            if M.Is_Spec then
               Append (Units_For_Coupling, M);
            end if;

            for Sym of Copy (M.Depends_On) loop
               --  Copy is necessary, because we are passing M.Depends_On to
               --  the recursive Visit, which is going to modify it, so we
               --  can't be iterating over it directly.

               Visit (Specs (Same_Ignoring_Case (Sym)), M.Depends_On);
            end loop;
         end if;

         Include (Depends_On, M.CU_Name);
         Union (Depends_On, M.Depends_On);
      end Visit;

      Ignored : CU_Symbol_Sets.Set;

   --  Start of processing for Compute_Indirect_Dependencies

   begin
      --  First remove nonexistent units from the Depends_On sets, because
      --  we're not supposed to count those in the coupling metrics.

      for File_M of Global_M.Submetrix loop
         Remove_Nonexistent_Units (File_M);
      end loop;

      --  Then compute indirect dependences

      for File_M of Global_M.Submetrix loop
         Visit (File_M, Ignored);
      end loop;

      --  A compilation unit can depend on itself at this point. For example,
      --  if body-A says "with B;", and spec-B says "with A;", A will end up
      --  depending on itself, which we don't want. So we remove such
      --  self-referential dependences at this point, if present.

      for File_M of Global_M.Submetrix loop
         Exclude (File_M.Depends_On, File_M.CU_Name);
      end loop;
   end Compute_Indirect_Dependencies;

   function Get_Spec (M : Metrix_Ref) return Metrix_Ref is
   begin
      if M.Is_Spec then
         return M;
      elsif M.Subunit_Parent = Empty_CU_Sym then
         declare
            Spec : constant Metrix_Ref :=
              Specs (Same_Ignoring_Case (M.CU_Name));
         begin
            return (if Spec = null then M else Spec);
         end;
      else
         declare
            Parent_Body : Metrix_Ref :=
              Bodies (Same_Ignoring_Case (M.Subunit_Parent));
         begin
            --  The parent could be a body acting as spec, in which case it's
            --  in Specs, not Bodies.

            if Parent_Body = null then
               Parent_Body := Specs (Same_Ignoring_Case (M.Subunit_Parent));
            end if;

            return (if Parent_Body = null then M else Get_Spec (Parent_Body));
            --  This recursion will climb up a chain of nested subunits until
            --  it reaches a library unit, and then we'll get the spec of that
            --  library unit.
         end;
      end if;
   end Get_Spec;

   procedure Compute_Coupling
     (Tool : in out Metrics_Tool; Global_M : Metrix;
      Metrics_To_Compute : Metrics_Set)
   is

      procedure Do_Edge (Metric : Coupling_Metrics; From, To : Metrix_Ref);
      --  Process one edge in the dependency graph for the given Metric.
      --  If the Metric is Coupling_Out, then From depends on To.
      --  If the Metric is Coupling_In, then To depends on From.
      --  Thus, the metric is always recorded in the Outer_Unit of From.

      --  The ASIS-based version computes Hierarchy_Coupling as follows:
      --
      --  Crown in computed for each unit. This is the set of leaf descendants
      --  of this unit. Crown is a local variable. (Better: in
      --  Gather_Dependencies, if we see a leaf unit, add it Crown(?name) of
      --  each ancestor.)
      --
      --  Crown is used only for Hierarchy_Coupling_Out. Loop through Crown,
      --  Union its OO_Supporters into Category_Supporters (local var). Remove
      --  descendents of the unit (including itself) from Category_Supporters.
      --  Hierarchy_Coupling_Out is then Length (Category_Supporters).
      --
      --  Length(OO_Supporters) = Tagged_Coupling_Out!
      --  OO_Dependents is the reverse of OO_Supporters.
      --
      --  For Hierarchy_Coupling_In, loop through the unit's descendents
      --  (including itself), and union its OO_Dependents into
      --  Category_Dependents (local var).  Remove descendents of the unit
      --  (including itself) from Category_Dependents.  Hierarchy_Coupling_Out
      --  is then Length (Category_Dependents).

      procedure Do_Edge (Metric : Coupling_Metrics; From, To : Metrix_Ref) is
         Outer_Unit : Metrix renames Element (From.Submetrix, 1).all;
         --  Outer_Unit is the outermost package spec, procedure spec, etc.

         procedure Do_Inc;
         --  Call Inc, with some extra debugging output

         procedure Do_Inc is
         begin
            if Debug_Flag_8 and then Metrics_To_Compute (Metric) then
               declare
                  Metric_Name : constant String :=
                    XML_Metric_Name_String (Metric);
                  Longest_Metric_Name : constant String :=
                    XML_Metric_Name_String (Hierarchy_Coupling_Out);
                  Spaces : constant String :=
                    [Metric_Name'Length .. Longest_Metric_Name'Length => ' '];
               begin
                  Put ("\1\2: \3 --> \4\n", Metric_Name, Spaces,
                       Str (From.XML_Name).S, Str (To.XML_Name).S);
               end;
            end if;

            Inc (Outer_Unit.Vals (Metric));
         end Do_Inc;

      begin
         case Metric is
            when Tagged_Coupling_Out | Tagged_Coupling_In =>
               if From.Has_Tagged_Type and To.Has_Tagged_Type then
                  Do_Inc;
               end if;
            when Hierarchy_Coupling_Out | Hierarchy_Coupling_In =>
               if From.Has_Tagged_Type and To.Has_Tagged_Type then
                  if not Same_Hierarchy (From, To) then
                     Do_Inc;
                  end if;
               end if;
            when Control_Coupling_Out | Control_Coupling_In =>
               if From.Has_Subp and To.Has_Subp then
                  Do_Inc;
               end if;
            when Unit_Coupling_Out | Unit_Coupling_In =>
               Do_Inc;
         end case;
      end Do_Edge;

   --  Start of processing for Compute_Coupling

   begin
      --  Union the Depends_On set for each body (including subunits) into that
      --  of the corresponding spec. We can't put it there in the first place
      --  (during Compute_Indirect_Dependencies), because that would compute
      --  too-high numbers. For example, if A-body with's B, we want to count
      --  that in the metrics for A, but not in the metrics of things that
      --  depend on A.

      for B of Bodies loop
         if B /= null then
            declare
               pragma Assert (B.Indirect_Dependences_Computed);
               S : constant Metrix_Ref := Get_Spec (B);
               pragma Assert (S.Indirect_Dependences_Computed);
               --  S is the compilation unit node for the spec; dependences of
               --  bodies (including subunits) should be counted on the spec.
            begin
               if S /= B then
                  Union (S.Depends_On, B.Depends_On);
               end if;
            end;
         end if;
      end loop;

      Dump (Tool, Global_M, "After bodies:");

      --  Compute metrics for the specs

      for S of Specs loop
         if S /= null then
            pragma Assert (S.Indirect_Dependences_Computed);

            for Sym of S.Depends_On loop
               declare
                  Dep : constant Metrix_Ref :=
                    Specs (Same_Ignoring_Case (Sym));
                  pragma Assert (Dep /= null);
                  --  It can't be null, because we removed nonexistent units
               begin
                  --  Compute *_Coupling_Out From this unit To what it depends
                  --  on:

                  Do_Edge (Tagged_Coupling_Out, S, Dep);
                  Do_Edge (Hierarchy_Coupling_Out, S, Dep);
                  Do_Edge (Control_Coupling_Out, S, Dep);
                  Do_Edge (Unit_Coupling_Out, S, Dep);

                  --  Compute *_Coupling_In To this unit From what it depends
                  --  on (noting that To and From are reversed from the above):

                  Do_Edge (Tagged_Coupling_In, Dep, S);
                  Do_Edge (Hierarchy_Coupling_In, Dep, S);
                  Do_Edge (Control_Coupling_In, Dep, S);
                  Do_Edge (Unit_Coupling_In, Dep, S);
               end;
            end loop;
         end if;
      end loop;

      --  Sort the compilation units by name, so they will get printed in
      --  alphabetical order, rather than some order caused by the graph walk.

      declare
         function Lt (X, Y : Metrix_Ref) return Boolean;
         pragma Inline (Lt);

         package Sorting is new Metrix_Vectors.Generic_Sorting (Lt);

         function Lt (X, Y : Metrix_Ref) return Boolean is
            U1 : constant String := Str (X.CU_Name).S;
            U2 : constant String := Str (Y.CU_Name).S;
         begin
            return U1 < U2;
         end Lt;
      begin
         Sorting.Sort (Units_For_Coupling);
      end;
   end Compute_Coupling;

   procedure Print_Coupling
     (Cmd : Command_Line;
      Metrics_To_Compute : Metrics_Set)
   is
   begin
      if Metrics_To_Compute = Empty_Metrics_Set then
         return;
      end if;

      Put ("\n\nCoupling metrics:\n");
      Put ("=================\n");
      Indent;

      for Index in 1 .. Units_For_Coupling.Last_Index loop
         declare
            File_M : Metrix renames Element (Units_For_Coupling, Index).all;
            Outer_Unit : Metrix renames Element (File_M.Submetrix, 1).all;
         begin
            if Should_Print_Any
              (Coupling_Metrics'First, Coupling_Metrics'Last,
               Metrics_To_Compute, Outer_Unit, Depth => 3, XML => False)
            then
               Print_Metrix
                 (Cmd,
                  File_M.Source_File_Name.all,
                  Metrics_To_Compute,
                  File_M,
                  Depth => 2);
               if Index /= Units_For_Coupling.Last_Index then
                  Put ("\n");
               end if;
            end if;
         end;
      end loop;

      Outdent;
   end Print_Coupling;

   procedure XML_Print_Coupling
     (Cmd : Command_Line;
      Metrics_To_Compute : Metrics_Set)
   is
   begin
      if Metrics_To_Compute = Empty_Metrics_Set then
         return;
      end if;

      Indent;
      Put ("<coupling>\n");

      for File_M of Units_For_Coupling loop
         pragma Assert (Indentation_Level = 3);
         declare
            Outer_Unit : Metrix renames Element (File_M.Submetrix, 1).all;
         begin
            if Should_Print_Any
              (Coupling_Metrics'First, Coupling_Metrics'Last,
               Metrics_To_Compute, Outer_Unit, Depth => 3, XML => True)
            then
               XML_Print_Metrix
                 (Cmd,
                  File_M.Source_File_Name.all,
                  Metrics_To_Compute,
                  File_M.all,
                  Depth => 2);
            end if;
         end;
      end loop;

      Put ("</coupling>\n");
      Outdent;
   end XML_Print_Coupling;

   procedure Final (Tool : in out Metrics_Tool; Cmd : Command_Line) is
      Metrics_To_Compute : Metrics_Set renames Tool.Metrics_To_Compute;
      Without_Coupling : constant Metrics_Set :=
        Metrics_To_Compute and not Coupling_Only;
      With_Coupling : constant Metrics_Set :=
        Metrics_To_Compute and Coupling_Only;
      Metrix_Stack : Metrix_Vectors.Vector renames Tool.Metrix_Stack;
      Summed : constant String :=
        "summed over " & Image (Num_File_Names (Cmd)) & " units";
      pragma Assert (Length (Metrix_Stack) = 1);
      Global_M : Metrix_Ref := Element (Metrix_Stack, 1);

      Global : Text_IO.File_Type;
      --  File for global information in text form, if --global-file-name
      --  switch was given. By default, this information is sent to standard
      --  output.

      function Get_Object_Dir return String;
      --  Name of the Object_Dir specified in the project file, if any

      function Get_Object_Dir return String is
         use GNATCOLL.Projects, GNATCOLL.VFS;
      begin
         if Status (Tool.Project_Tree.all) = Empty then
            return "";
         else
            declare
               Prj : constant Project_Type := Tool.Project_Tree.Root_Project;
               Name : constant Filesystem_String :=
                 Full_Name (Object_Dir (Prj));
            begin
               return String (Name);
            end;
         end if;
      end Get_Object_Dir;

      Object_Dir : constant String := Get_Object_Dir;

      Xml_F_Name : constant String :=
        (if Arg (Cmd, Xml_File_Name) = null
           then "metrix.xml"
           else Arg (Cmd, Xml_File_Name).all);
      --  ASIS-based gnatmetric ignores Output_Dir for the xml.

      Has_Dir : constant Boolean :=
        Directories.Simple_Name (Xml_F_Name) /= Xml_F_Name;
      --  True if Xml_F_Name contains directory information

      Xml_FD_Name : constant String :=
        (if Object_Dir = "" or else Has_Dir then Xml_F_Name
         else Directories.Compose (Object_Dir, Xml_F_Name));

      XML_File : Text_IO.File_Type;
      --  All XML output for all source files goes to this file.

      function Xsd_File_Name return String;
      --  Return the name of the XSD (schema) file name, which is based
      --  on the XML file name. In particular if the XML file name
      --  ends in ".xml", that is replaced by ".xsd"; otherwise,
      --  ".xsd" is appended. So "foo.xml" --> "foo.xsd", but
      --  "foo.bar" --> "foo.bar.xsd".
      --
      --  Note that this name is written to the XML file, in addition
      --  to being used to open the XSD file.

      function Xsd_File_Name return String is
         Norm : constant String  := Normalize_Pathname (Xml_FD_Name);
         Xml : constant String := ".xml";
         Xsd : constant String := ".xsd";
      begin
         if Has_Suffix (Norm, Suffix => Xml) then
            return Replace_String (Norm, Xml, Xsd);
         else
            return Norm & Xsd;
         end if;
      end Xsd_File_Name;

      procedure Print_Computed_Metric
        (T : Template; Metric : Metrics_Enum; C_Metric : Computed_Metrics);
      --  Print a "computed metric". Metric is the base metric, C_Metric is the
      --  computed metric based on that.

      procedure Print_Computed_Metric
        (T : Template; Metric : Metrics_Enum; C_Metric : Computed_Metrics) is
      begin
         if Gen_Text (Cmd) and then Metrics_To_Compute (Metric) then
            if Global_M.Vals (Metric) > 0 then
               Put (T,
                    Val_To_Print (Metric, Global_M.all, XML => False),
                    Val_To_Print (C_Metric, Global_M.all, XML => False));

               if Metric = Public_Types then
                  if Global_M.Vals (Private_Types) > 0 then
                     Put ("\n including");
                     Put ("\n    \1 private types",
                          Val_To_Print
                            (Private_Types, Global_M.all, XML => False));
                  end if;
               end if;
            end if;
         end if;
      end Print_Computed_Metric;

   --  Start of processing for Final

   begin
      pragma Assert (Global_M.Vals (Complexity_Cyclomatic) =
                       Global_M.Vals (Complexity_Statement) +
                       Global_M.Vals (Complexity_Expression));

      --  We're done with Metrix_Stack at this point. Printing uses the tree
      --  formed by Submetrix.

      Global_M.Node := No_Ada_Node;
      Pop (Metrix_Stack);
      Clear (Metrix_Stack);

      if Object_Dir /= "" and then not Directories.Exists (Object_Dir) then
         begin
            Directories.Create_Path (Object_Dir);
         exception
            when Directories.Name_Error | Directories.Use_Error =>
               Cmd_Error ("cannot create directory " & Object_Dir);
         end;
      end if;

      Dump (Tool, Global_M.all, "Initial:");
      Compute_Indirect_Dependencies (Global_M.all);
      Dump (Tool, Global_M.all, "After Compute_Indirect_Dependencies");
      Compute_Coupling (Tool, Global_M.all, With_Coupling);
      Validate_Coupling (Units_For_Coupling);

      if Gen_XML (Cmd) then
         --  Generate schema (XSD file), if requested

         if Arg (Cmd, Generate_XML_Schema) then
            Write_XML_Schema (Xsd_File_Name);
         end if;

         --  Put initial lines of XML

         if not Output_To_Standard_Output then
            Text_IO.Create (XML_File, Name => Xml_FD_Name);
            Text_IO.Set_Output (XML_File);
         end if;
         Put ("<?xml version=\1?>\n", Q ("1.0"));

         if Arg (Cmd, Generate_XML_Schema) then
            Put ("<global xmlns:xsi=" &
                 """http://www.w3.org/2001/XMLSchema-instance"" " &
                 "xsi:noNamespaceSchemaLocation=""\1"">\n",
                 Xsd_File_Name);
         else
            Put ("<global>\n");
         end if;

         if Arg (Cmd, XML_Config) then
            XML_Print_Config;
         end if;
      end if;

      --  Print the metrics for each file in text and XML form

      for File_M of Global_M.Submetrix loop
         pragma Assert (Debug_Flag_V or else Indentation_Level = 0);
         Print_File_Metrics
           (Cmd, XML_File, File_M.all, Without_Coupling, Object_Dir);
         pragma Assert (Debug_Flag_V or else Indentation_Level = 0);
--         Destroy (File_M);
      end loop;

      --  Print the totals in text form. These go to standard output, unless
      --  --global-file-name was specified. Note that Output_Dir is ignored by
      --  gnatmetric for this output.

      if Gen_Text (Cmd) then
         if Arg (Cmd, Global_File_Name) /= null then
            if not Output_To_Standard_Output then
               Text_IO.Create
                 (Global, Name => Arg (Cmd, Global_File_Name).all);
               Text_IO.Set_Output (Global);
            end if;
         end if;

         First_Printed_Range := True;

         Print_Range
           ("Line metrics " & Summed,
            Metrics_To_Compute,
            Lines_Metrics'First, Lines_Metrics'Last, Global_M.all,
            Depth => 1);
         Print_Range
           ("Contract metrics " & Summed,
            Metrics_To_Compute,
            Contract_Metrics'First, Contract_Metrics'Last, Global_M.all,
            Depth => 1);
         Print_Range
           ("Element metrics " & Summed,
            Metrics_To_Compute,
            Syntax_Metrics'First, Syntax_Metrics'Last, Global_M.all,
            Depth => 1);

         Print_Computed_Metric
           ("\n\n \1 public types in \2 units",
            Public_Types, Computed_Public_Types);
         Print_Computed_Metric
           ("\n\n \1 type declarations in \2 units",
            All_Types, Computed_All_Types);
         Print_Computed_Metric
           ("\n\n \1 public subprograms in \2 units",
            Public_Subprograms, Computed_Public_Subprograms);
         Print_Computed_Metric
           ("\n\n \1 subprogram bodies in \2 units",
            All_Subprograms, Computed_All_Subprograms);

         --  For Complexity_Average, the actual metric value is in
         --  Complexity_Cyclomatic, and Val_To_Print will compute the
         --  average. We set XML to True, even though it's not XML to
         --  avoid an annoying extra space.

         if Metrics_To_Compute (Complexity_Average)
           and then Global_M.Num_With_Complexity > 0
           and then not Arg (Cmd, No_Local_Metrics)
         then
            declare
               All_But_Complexity_Average : constant Metrics_Set :=
                 [Complexity_Average => False, others => True];
            begin
               --  If Complexity_Average is the only one we're printing,
               --  then suppress two NLs, so we don't get NLs at the beginning
               --  of the output.

               if (Metrics_To_Compute and All_But_Complexity_Average) /=
                 Empty_Metrics_Set
               then
                  Put ("\n\n");
               end if;
            end;

            Put ("Average cyclomatic complexity: \1",
                 Val_To_Print (Complexity_Cyclomatic,
                               Global_M.all, XML => True));
         end if;

         if Arg (Cmd, Global_File_Name) /= null then
            if not Output_To_Standard_Output then
               Text_IO.Set_Output (Text_IO.Standard_Output);
               Text_IO.Close (Global);
            end if;
         end if;
      end if;

      --  Print the totals in XML form

      if Gen_Text (Cmd) then
         Print_Coupling (Cmd, With_Coupling);
      end if;

      if Gen_XML (Cmd) then
         if not Output_To_Standard_Output then
            Text_IO.Set_Output (XML_File);
         end if;
         XML_Print_Metrix_Vals
           (Metrics_To_Compute, Global_M.Vals'First, Global_M.Vals'Last,
            Global_M.all, Depth => 1);
         XML_Print_Coupling (Cmd, With_Coupling);
         Put ("</global>\n");
         if not Output_To_Standard_Output then
            Text_IO.Set_Output (Text_IO.Standard_Output);
            Text_IO.Close (XML_File);
         end if;
      end if;

      for Metric in Metrics_Enum loop
         if Metric_Found (Metric) and then not Implemented (Metric) then
            Put ("Metric is implemented: \1\n", Capitalize (Metric'Img));
         end if;
      end loop;

      Destroy (Global_M);
   end Final;

   ---------------------
   -- Process_CU --
   ---------------------

   procedure Process_CU
     (Tool : in out Metrics_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Unit : Analysis_Unit;
      CU_Node : Ada_Node)
   is
      pragma Assert (Kind (CU_Node) = Ada_Compilation_Unit);
      Outer_Unit : constant Ada_Node := Get_Outer_Unit (CU_Node);

      Cumulative : constant Cumulative_Counts_Array :=
        Get_Cumulative_Counts (Unit);
      Metrix_Stack : Metrix_Vectors.Vector renames Tool.Metrix_Stack;

      procedure Inc_All (Metric : Metrics_Enum; By : Metric_Nat := 1);
      --  Increment all values on the stack for a given Metric

      procedure Inc_All (Metric : Metrics_Enum; By : Metric_Nat := 1) is
      begin
         for M of Metrix_Stack loop
            Inc (M.Vals (Metric), By);
         end loop;
      end Inc_All;

      procedure Dec_All (Metric : Metrics_Enum; By : Metric_Nat := 1);
      --  Decrement all values on the stack for a given Metric

      procedure Dec_All (Metric : Metrics_Enum; By : Metric_Nat := 1) is
      begin
         for M of Metrix_Stack loop
            Dec (M.Vals (Metric), By);
         end loop;
      end Dec_All;

      Node_Stack : Ada_Node_Vector;
      --  Stack of all nodes currently being walked

      function Ancestor_Node (N : Natural) return Ada_Node;
      --  Returns the N'th ancestor of the current node. Ancestor (0) is the
      --  current node, Ancestor (1) is the parent of the current node,
      --  Ancestor (2) is the grandparent of the current node, and so on.

      function Ancestor_Node (N : Natural) return Ada_Node is
      begin
         pragma Assert (Last_Index (Node_Stack) >= N);
         return Element (Node_Stack, Last_Index (Node_Stack) - N);
      end Ancestor_Node;

      function Parent_Node return Ada_Node is (Ancestor_Node (1));
      pragma Unreferenced (Parent_Node); -- ???

      Exception_Handler_Count : Natural := 0;
      --  Number of exception handlers we are nested within. Used to
      --  suppress computing complexity metrics within handlers.

      Quantified_Expr_Count : Natural := 0;
      --  Number of quantified expressions we are nested within. This is
      --  used to get around the fact that For_Loop_Spec is used for both
      --  for loops and quantified expressions. For complexity metrics, the
      --  former increments the statement metric, and the latter the
      --  expression metric.

      Expr_Function_Count : Natural := 0;
      --  Apparently, gnatmetric doesn't walk expression functions for
      --  complexity metrics.

      Loop_Count : Natural := 0;
      --  Number of loop statements we are nested within, within the
      --  current program unit. So if a loop contains a block, which
      --  contains a body, the outer loop doesn't count within that
      --  body. Used to compute the maximum loop nesting level.

      --  Maybe count all kinds, via an array indexed by kind???

      Private_Part_Count : Natural := 0;
      --  Number of private parts we are nested within. Used for contract
      --  metrics, which are only supposed to be shown for visible subprograms.

      Non_Package_Body_Count : Natural := 0;
      --  Number of bodies we are nested within, except package bodies
      --  don't count.

      Prev_Subp_Decl : Ada_Node := No_Ada_Node;
      --  When we're walking a pragma, this is the most recently seen
      --  subprogram declaration, if any. Used in the implementation of pragma
      --  SPARK_Mode, which can immediately follow a subprogram declaration.

      In_Generic_Formal_Part : Boolean := False;
      --  True when we are processing a generic formal part. ???It would be
      --  better to have a Generic_Formal_Part node, similar to Visible_Part
      --  and Private_Part.

      function In_Visible_Part return Boolean is
         (Last_Index (Metrix_Stack) >= 3
            and then Element (Metrix_Stack, 3).Kind in
              Ada_Package_Decl | Ada_Generic_Package_Decl
            and then not Element (Metrix_Stack, 3).Is_Private_Lib_Unit
            and then Private_Part_Count = 0);
      --  True if we're within only visible parts. Note that it is possible to
      --  be in a visible part that is within a private part; we return False
      --  in that case.

      In_Assertion : Boolean := False;
      --  True if we are nested within an assertion (pragma Assert,
      --  or a pre/post/etc aspect).

      function In_Ignored_Assertion return Boolean is
        (In_Assertion and then Arg (Cmd, Ignore_Assertions));
      --  True if we are nested within an assertion, and --ignore-assertions is
      --  in force, which is the default. If --no-ignore-assertions was
      --  specified, then this is False. 'gnatmetric' skips such constructs.
      --  However, we need to process those, but only for the
      --  --contract-complexity metric.

      procedure Cyclomate (Node : Ada_Node; M : in out Metrix);
      --  Compute McCabe Cyclomatic Complexity metrics. This also handles the
      --  Contract_Complexity metric, even though that's considered a "contract
      --  metric".

      procedure Gather_Essential_Complexity
        (Node : Ada_Node; M : in out Metrix);

      procedure Gather_Extra_Exit_Points
        (Node : Ada_Node; M : in out Metrix);

      procedure Gather_Contract_Metrics (Node : Ada_Node);
      --  Compute contract metrics, except for Contract_Complexity, which is
      --  handled by Cyclomate.

      procedure Gather_Line_Metrics (Node : Ada_Node; M : in out Metrix);
      --  Compute line metrics

      procedure Gather_SPARK_Line_Metrics (Node : Ada_Node; M : in out Metrix);

      procedure Gather_Syntax_Metrics (Node : Ada_Node; M : in out Metrix);
      --  Compute syntax element metrics

      procedure Gather_Dependencies (Node : Ada_Node);
      --  Set M.Depends_On to include names as appropriate. For example, if
      --  Node is "with X;", we include X.

      procedure Rec (Node : Ada_Node);
      --  Recursive tree walk. Rec and Gather_Metrics_And_Walk_Children are
      --  mutually recursive.

      procedure Gather_Metrics_And_Walk_Children (Node : Ada_Node);

      procedure Cyclomate (Node : Ada_Node; M : in out Metrix) is
         Global_M : Metrix renames Element (Metrix_Stack, 1).all;
         File_M : Metrix renames Element (Metrix_Stack, 2).all;
         --  We don't walk up the stack as for other metrics. We increment
         --  the current Metrix (M) and the one for the file as a whole
         --  (File_M). Thus, the one for the file ends up being the total,
         --  which is then used to compute the average.

         --  Complexity_Cyclomatic is the sum of Complexity_Statement and
         --  Complexity_Expression. Therefore, we increment the former every
         --  time we increment one of the latter.

         --  How we compute the cyclomatic complexity:
         --
         --  1. Control statements:
         --
         --     IF adds 1 + the number of ELSIF paths
         --
         --     CASE statement adds the number of alternatives minus 1
         --
         --     WHILE loop always adds 1
         --
         --     FOR loop adds 1 unless we can detect that in any case this
         --          loop will be executes at least once
         --
         --     LOOP (condition-less) adds nothing
         --
         --     EXIT statement adds 1 if contains the exit condition, otherwise
         --          adds nothing
         --
         --     GOTO statement adds nothing
         --
         --     RETURN statement adds nothing
         --
         --     SELECT STATEMENTS:
         --
         --        SELECTIVE_ACCEPT is treaded as a CASE statement (number of
         --           alternatives minus 1). Opposite to IF statement, ELSE
         --           path adds 1 to the complexity (that is, for IF,
         --           both IF ... END IF; and IF ... ELSE ... END IF; adds 1,
         --           whereas
         --              SELECT
         --                 ...
         --              OR
         --                 ...
         --              END SELECT;
         --           adds 1, but
         --
         --              SELECT
         --                 ...
         --              OR
         --                 ...
         --              ELSE
         --                 ...
         --              END SELECT;
         --           adds 2
         --
         --        TIMED_ENTRY_CALL, CONDITIONAL_ENTRY_CALL and
         --        ASYNCHRONOUS_SELECT add 1 (they are considered as an IF
         --           statement with no ELSIF parts
         --
         --  2. We do not check if some code or some path is dead (unreachable)
         --
         --  3. We do not take into account the code in the exception handlers
         --     (only the main statement sequence is analyzed). RAISE statement
         --     adds nothing
         --
         --  4. A short-circuit control form add to the complexity value the
         --     number of AND THEN or OR ELSE at the given level (that is, if
         --     we have
         --
         --       Bool := A and then (B and then C) and then E;
         --
         --     we consider this as two short-circuit control forms: the outer
         --     adds to the complexity 2 and the inner adds 1.
         --
         --     Any short-circuit control form is taken into account, including
         --     expressions being parts of type and object definitions.
         --
         --  5. Conditional expressions.
         --
         --  5.1 An IF expression is treated in the same way as an IF
         --      statement: it adds 1 + the number of ELSIF paths, but to the
         --      expression complexity.
         --
         --  5.2 A CASE expression is treated in the same way as an CASE
         --      statement: it adds the number of CASE paths minus 1, but to
         --      the expression complexity.
         --
         --  6. Quantified expressions are treated as the equivalent loop
         --     construct:
         --
         --        for some X in Y => Z (X)
         --
         --     is considered as a shortcut for
         --
         --        Result := False;
         --        Tmp := First (X);
         --
         --        while Present (Tmp) loop
         --           if Z (Tmp) then
         --              Result := True;
         --              exit;
         --           end if;
         --
         --           Tmp := Next (Tmp);
         --        end loop;
         --
         --     That is, it adds 2 (1 as WHILE loop and 1 as IF statement with
         --     no ELSIF parts.
         --
         --     'for all' expression is treated in a similar way.
         --
         --     For essential complexity, quantified expressions add 1 if
         --     Treat_Exit_As_Goto is set ON.
         --
         --  7. Any enclosed body is just skipped and is not taken into
         --     account. The only situation that is not completely clear is
         --     an enclosed package body with statement sequence part. When
         --     enclosing body is executed, this enclosed package body will
         --     also be executed inconditionally and exactly once - this is the
         --     reason to count it when computing the complexity of enclosing
         --     body. From the other side, the enclosed package body is similar
         --     to enclosed local procedures, and we for sure do not want to
         --     count enclosed procedures...

         procedure Inc_Cyc (Metric : Metrics_Enum; By : Metric_Nat := 1) with
           Pre => Metric in Complexity_Statement | Complexity_Expression;
         --  Increment the specified complexity metric, and also
         --  Complexity_Cyclomatic. Increment the current unit's metrics, as
         --  well as the file-level ones. However, if this is something like a
         --  subprogram declaration, we actually increment the
         --  Contract_Complexity.

         procedure Inc_Cyc (Metric : Metrics_Enum; By : Metric_Nat := 1) is
         begin
            if M.Kind in Contract_Complexity_Eligible then
               Inc (M.Vals (Contract_Complexity), By);
               Inc (File_M.Vals (Contract_Complexity), By);
            elsif not In_Ignored_Assertion then
               Inc (M.Vals (Metric), By);
               Inc (M.Vals (Complexity_Cyclomatic), By);
               Inc (File_M.Vals (Metric), By);
               Inc (File_M.Vals (Complexity_Cyclomatic), By);
               Inc (Global_M.Vals (Metric), By);
               Inc (Global_M.Vals (Complexity_Cyclomatic), By);
            end if;
         end Inc_Cyc;

      --  Start of processing for Cyclomate

      begin
         --  Don't compute these metrics within exception handlers.

         if Exception_Handler_Count > 0 then
            return;
         end if;

         case Kind (Node) is
            when Ada_If_Stmt |
              Ada_Elsif_Stmt_Part |
              Ada_While_Loop_Spec =>
               Inc_Cyc (Complexity_Statement);

            when Ada_For_Loop_Spec =>
               if Quantified_Expr_Count = 0 then
                  --  We want to increment the statement count only for real
                  --  for loops, not for quantified expressions, which use the
                  --  same node kind.

                  --  ????Disable No_Static_Loop for now, pending updates to
                  --  libadalang that will enable us to do this right.

--                  if Arg (Cmd, No_Static_Loop) then
--                     declare
--                        Subtype_Decl : constant Basic_Decl :=
--                         Node.As_For_Loop_Spec.F_Iter_Expr.P_Referenced_Decl;
--                     begin
--                        if not Subtype_Decl.Is_Null
--                          and then Subtype_Decl.P_Is_Static_Decl
--                        then
--                           --  Ignore "static loops" if the --no-static-loop
--                           --  switch was given.
--                           goto Ignore_Static_Loop;
--                        end if;
--                     end;
--                  end if;
--
                  Inc_Cyc (Complexity_Statement);
--                  <<Ignore_Static_Loop>>
               end if;

            when Ada_Case_Stmt =>
               Inc_Cyc (Complexity_Statement,
                 By => Node.As_Case_Stmt.F_Alternatives.Children_Count - 1);

            when Ada_Exit_Stmt =>
               if not Node.As_Exit_Stmt.F_Cond_Expr.Is_Null then
                  Inc_Cyc (Complexity_Statement);
               end if;

            when Ada_Select_Stmt =>
               declare
                  S : constant Select_Stmt := Node.As_Select_Stmt;
                  Num_Alts : constant Metric_Nat :=
                     Children_Count (F_Guards (S));
                  Num_Else : constant Metric_Nat :=
                    (if Children_Count (F_Else_Stmts (S)) = 0 then 0 else 1);
                  Num_Abort : constant Metric_Nat :=
                    (if Children_Count (F_Abort_Stmts (S)) = 0 then 0 else 1);
               begin
                  Inc_Cyc (Complexity_Statement,
                           By => Num_Alts + Num_Else + Num_Abort - 1);
               end;

            when Ada_Expr_Function =>
               null; -- It's already set to 1

            when Ada_Bin_Op =>
               if Node.As_Bin_Op.F_Op in Ada_Op_Or_Else | Ada_Op_And_Then then
                  Inc_Cyc (Complexity_Expression);
               end if;

            when Ada_If_Expr | Ada_Elsif_Expr_Part =>
               Inc_Cyc (Complexity_Expression);

            when Ada_Case_Expr =>
               Inc_Cyc (Complexity_Expression,
                        By => Node.As_Case_Expr.F_Cases.Children_Count - 1);

            when Ada_Quantified_Expr =>
               Inc_Cyc (Complexity_Expression, By => 2);

            when Ada_For_Loop_Stmt | Ada_Loop_Stmt | Ada_While_Loop_Stmt =>
               --  Compute M.Vals (Loop_Nesting) as the maximum loop
               --  nesting level for this unit. We only set it for the
               --  innermost unit and at the file level.

               if not In_Ignored_Assertion then
                  if Loop_Count > M.Vals (Loop_Nesting) then
                     M.Vals (Loop_Nesting) := Loop_Count;
                  end if;

                  if Loop_Count > File_M.Vals (Loop_Nesting) then
                     File_M.Vals (Loop_Nesting) := Loop_Count;
                  end if;
               end if;

            when others => null;
         end case;
      end Cyclomate;

      --  To compute the Complexity_Essential metric, we keep a stack of nodes
      --  currently being processed, which includes all the Gnatmetric_Eligible
      --  nodes, and all the compound statements we are counting. When we see a
      --  jump, we walk up the stack and increment the count for each compound
      --  statement we're jumping out of. We stop when we get to a
      --  non-compound-statement (i.e. a Gnatmetric_Eligible node). Exit
      --  statements are treated as jumps by default, and we stop when we get
      --  to a loop.  If the No_Treat_Exit_As_Goto command-line option is
      --  given, exits are ignored. There seem to be some bugs in the
      --  gnatmetric version, which we are not mimicing here. Each compound
      --  statement in the stack has a Counted flag, which is set to True
      --  when we encounter a jump out of that statement; this prevents us from
      --  counting multiple jumps out of the same statement. For example, if a
      --  loop contains 3 exit statements, that counts as 1, not 3.

      type EC_Rec is record
         Node : Ada_Node;
         Counted : Boolean;
      end record;

      type EC_Index is new Positive;
      type EC_Array is array (EC_Index range <>) of EC_Rec;
      package EC_Vectors is
         new Utils.Vectors (EC_Index, EC_Rec, EC_Array);
      use EC_Vectors;
      EC_Stack : EC_Vectors.Vector; -- "essential complexity" stack

      procedure Gather_Essential_Complexity
        (Node : Ada_Node; M : in out Metrix)
      is
         File_M : Metrix renames Element (Metrix_Stack, 2).all;
      begin
         --  Don't compute this metric within exception handlers.

         if Exception_Handler_Count > 0 then
            return;
         end if;

         --  Push the stack if appropriate

         if Kind (Node) in Gnatmetric_Eligible |
           Ada_If_Stmt | Ada_Case_Stmt | Ada_For_Loop_Stmt | Ada_Loop_Stmt |
           Ada_While_Loop_Stmt | Ada_Select_Stmt
         then
            Push (EC_Stack, EC_Rec'(Node, Counted => False));
            --  (The corresponding Pop is at the end of
            --  Gather_Metrics_And_Walk_Children.)
         end if;

         --  If this is a jump, increment appropriate counts

         if Kind (Node) in -- Not Ada_Extended_Return_Stmt
             Ada_Return_Stmt |
             Ada_Raise_Stmt |
             Ada_Terminate_Alternative |
             Ada_Goto_Stmt
           or else
             (Kind (Node) = Ada_Exit_Stmt and then Tool.Treat_Exit_As_Goto)
         then
            declare
               X : EC_Index := Last_Index (EC_Stack);
               K : Ada_Node_Kind_Type;
            begin
               loop
                  K := Kind (Ada_Node'(EC_Stack (X).Node));

                  exit when K in Gnatmetric_Eligible;

                  if not EC_Stack (X).Counted then
                     EC_Stack (X).Counted := True;
                     Inc (M.Vals (Complexity_Essential));
                     Inc (File_M.Vals (Complexity_Essential));
                  end if;

                  exit when X = 1;
                  exit when Kind (Node) = Ada_Exit_Stmt
                    and then K in
                      Ada_For_Loop_Stmt | Ada_Loop_Stmt | Ada_While_Loop_Stmt;

                  X := X - 1;
               end loop;
            end;
         end if;
      end Gather_Essential_Complexity;

      procedure Gather_Extra_Exit_Points
        (Node : Ada_Node; M : in out Metrix)
      is

         function Is_Handled_By
           (H : Exception_Handler; Exc : Defining_Name) return Boolean;
         --  True if exception Exc is handled by H, and no exception is raised
         --  by H.
         function Is_Handled_In
           (HS : Handled_Stmts; Exc : Defining_Name) return Boolean;
         --  True if Is_Handled_By is True for one or more handlers of the
         --  handled sequence of statements.
         function Handled_Locally return Boolean;
         --  True if Is_Handled_In is True for one or more handled sequences of
         --  statements within the subprogram, and enclosing the raise
         --  statement.

         function Is_Handled_By
           (H : Exception_Handler; Exc : Defining_Name) return Boolean is
         begin
            for X of H.F_Handled_Exceptions loop
               if (X.Kind = Ada_Others_Designator or else Exc = Xref (X))
                 and then not Contains_Kind (H, Ada_Raise_Stmt)
               then
                  return True;
               end if;
            end loop;
            return False;

         exception
            when Property_Error =>
               --  If we can't find the exception declaration, return False
               return False;
         end Is_Handled_By;

         function Is_Handled_In
           (HS : Handled_Stmts; Exc : Defining_Name) return Boolean is
         begin
            for H of HS.F_Exceptions loop
               if H.Kind /= Ada_Pragma_Node
                 and then Is_Handled_By (H.As_Exception_Handler, Exc)
               then
                  return True;
               end if;
            end loop;

            return False;
         end Is_Handled_In;

         function Handled_Locally return Boolean is
            Exc : Defining_Name;
         begin
            begin
               Exc := (Node.As_Raise_Stmt.F_Exception_Name
                       .P_Referenced_Defining_Name);
            exception
               when Property_Error =>
                  --  If we can't find the exception declaration, return False
                  return False;
            end;

            for Anc_Index in 1 .. Last_Index (Node_Stack) loop
               declare
                  Anc : constant Ada_Node := Ancestor_Node (Anc_Index);
               begin
                  exit when Anc = M.Node; ----------------

                  if Anc.Kind = Ada_Handled_Stmts then
                     if Is_Handled_In (Anc.As_Handled_Stmts, Exc) then
                        return True;
                     end if;
                  end if;
               end;
            end loop;
            return False;
         end Handled_Locally;

      --  Start of processing for Gather_Extra_Exit_Points

      begin
         --  Don't compute this metric within exception handlers.

         if Exception_Handler_Count > 0 then
            return;
         end if;

         case Kind (Node) is
            when Ada_Return_Stmt | Ada_Extended_Return_Stmt =>
               Inc (M.Vals (Extra_Exit_Points));
            when Ada_Raise_Stmt =>
               --  We do some additional analysis to determine whether the
               --  exception will be handled locally, as best we can
               --  statically. If the exception is handled locally by these
               --  heuristics, we don't count it as an extra exit point.

               if not Handled_Locally then
                  Inc (M.Vals (Extra_Exit_Points));
               end if;
            when others => null;
         end case;
      end Gather_Extra_Exit_Points;

      procedure Gather_Line_Metrics (Node : Ada_Node; M : in out Metrix) is
         function Should_Gather_Body_Lines return Boolean;
         --  True if we should gather the Lines_Code_In_Bodies for
         --  this node. True for entry, subprogram, and task bodies,
         --  and True for package bodies if there is a statement part.

         function Should_Gather_Body_Lines return Boolean is
         begin
            case Kind (Node) is
               when Ada_Package_Body =>
                  return not Node.As_Package_Body.F_Stmts.Is_Null;
               when Ada_Entry_Body | Ada_Subp_Body | Ada_Task_Body =>
                  return True;
               when others =>
                  return False;
            end case;
         end Should_Gather_Body_Lines;

         Global_M : Metrix renames Element (Metrix_Stack, 1).all;
         File_M : Metrix renames Element (Metrix_Stack, 2).all;

      --  Start of processing for Gather_Line_Metrics

      begin
         if Node = M.Node then
            declare
               use type Slocs.Line_Number;

               Start : Slocs.Line_Number;
               Stop : Slocs.Line_Number;
               Lines_Count : Metric_Nat;
            begin

               --  If this is a compilation unit and the only one in the
               --  analysis unit, then we want to include the entire analysis
               --  unit.

               if M.Node.Kind = Ada_Compilation_Unit
                 and then M.Node.Parent.Is_Null
               then
                  Start := 1;

                  Stop := Last (Cumulative);

                  Lines_Count := Metric_Nat (Stop - Start + 1);

               --  If this is a compilation unit but not the only one in the
               --  analysis unit, then Line_Count must be computed accordingly.

               elsif M.Node.Kind = Ada_Compilation_Unit
                 and then not M.Node.Parent.Is_Null
               then
                  declare
                     Comp_Units : constant Ada_Node_Array :=
                       M.Node.Parent.As_Compilation_Unit_List.Children;

                  begin

                     --  If this is the first compilation unit of the file
                     --  then, we want to include all the comments and blank
                     --  lines preceding and following the compilation unit
                     --  until the next one.

                     if M.Node = Comp_Units (Comp_Units'First) then
                        Start := 1;

                        Stop := Comp_Units (Comp_Units'First + 1).
                          Sloc_Range.Start_Line - 1;

                        Lines_Count := Metric_Nat (Stop - Start + 1);

                     --  If this is the last compilation unit of the analysis
                     --  unit then, we want to include all the comments and
                     --  blank lines following the compilation unit until the
                     --  end of the analysis unit.

                     elsif M.Node = Comp_Units (Comp_Units'Last) then
                        Start := M.Node.Sloc_Range.Start_Line;

                        Stop := Last (Cumulative);

                        Lines_Count := Metric_Nat (Stop - Start + 1);

                     --  If this compilation unit is in the middle of the
                     --  compilation unit list of this analysis unit, then
                     --  we want to include all the comments and blank lines
                     --  following the compilation unit until the next one.

                     else
                        for Idx in Comp_Units'First .. Comp_Units'Length loop
                           if M.Node = Comp_Units (Idx) then
                              Start := Comp_Units (Idx).Sloc_Range.Start_Line;

                              Stop :=
                                Comp_Units (Idx + 1).Sloc_Range.Start_Line - 1;

                              Lines_Count := Metric_Nat (Stop - Start + 1);
                              exit;
                           end if;
                        end loop;
                     end if;
                  end;
               else
                  Start := M.Sloc.Start_Line;

                  Stop := M.Sloc.End_Line;

                  Lines_Count := Metric_Nat (Stop - Start + 1);
               end if;
               Lines_Count := Metric_Nat (Stop - Start + 1);

               pragma Assert (M.Vals (Lines) = 0);
               M.Vals (Lines) := Lines_Count;
               if Kind (Node) = Ada_Compilation_Unit then
                  Inc (Global_M.Vals (Lines), By => Lines_Count);
               end if;

               for Metric in Cumulative_Metrics loop
                  declare
                     Range_Count : Metric_Nat :=
                       Line_Range_Count
                         (Cumulative,
                          First_Line => Start, Last_Line => Stop,
                          Metric => Metric);
                  begin
                     M.Vals (Metric) := Range_Count;
                     if Kind (Node) = Ada_Compilation_Unit then
                        Inc (Global_M.Vals (Metric), By => Range_Count);
                     end if;

                     --  If we're doing Lines_Code, gather the
                     --  Lines_Code_In_Bodies metric as well. For
                     --  appropriate nodes, Lines_Code_In_Bodies is
                     --  the same a Lines_Code, except for package
                     --  bodies, where we just count the lines in
                     --  statements (so we overwrite Range_Count in
                     --  that case).

                     if Metric = Lines_Code
                       and then Should_Gather_Body_Lines
                     then
                        if Kind (Node) = Ada_Package_Body then
                           Range_Count :=
                             Line_Range_Count
                               (Cumulative,
                                First_Line => M.Statements_Sloc.Start_Line,
                                Last_Line => M.Statements_Sloc.End_Line,
                                Metric => Metric);
                        end if;

                        --  Mimic gnatmetric, which counts procedures
                        --  within procedures, but not their lines.

                        if Node = Outer_Unit
                          or else Non_Package_Body_Count <= 1
                          or else Kind (Node) = Ada_Package_Body
                        then
                           Inc (Global_M.Vals (Lines_Code_In_Bodies),
                                By => Range_Count);
                        end if;

                        Inc (File_M.Vals (Lines_Code_In_Bodies),
                             By => Range_Count);
                        Inc (File_M.Vals (Num_Bodies));
                        Inc (M.Vals (Num_Bodies));
                        Inc (Global_M.Vals (Num_Bodies));
                     end if;
                  end;
               end loop;
            end;
         end if;

      end Gather_Line_Metrics;

      procedure Gather_SPARK_Line_Metrics
        (Node : Ada_Node; M : in out Metrix)
      is
         --  Note that "pragma SPARK_Mode (ON);" is always redundant except on
         --  the first "section" of root library unit. However, we're not yet
         --  implementing that correctly.

         --  Not yet implemented:
         --     Pragma SPARK_Mode as a configuration pragma.
         --     Inheritance of SPARK_Mode e.g. from parent to child.
         --     Redundant OFF's.
         --  Basically, we're doing a purely syntactic analysis, and anything
         --  else is not yet implemented.

         function Find_Section return Ada_Node;
         --  Find a node representing the appropriate section for Node (which
         --  must be a pragma SPARK_Mode). For a pragma that applies to the
         --  first declaration list (e.g. the visible part of a package), we
         --  return the declaration or body node (e.g. the package
         --  declaration). If it applies to a private part or to the statements
         --  of a package body, we return just that. If it applies to a
         --  subprogram declaration, we return that declaration.

         function Find_Pragma
           (L : Ada_Node_List'Class) return Boolean;
         --  True if L contains Node at the start (preceded only by other
         --  pragmas). Node must be a pragma SPARK_Mode.

         function Find_Section return Ada_Node is
            P : constant Ada_Node := M.Node;
         begin
            case Kind (P) is
               when Ada_Compilation_Unit =>
                  return P;

               when Ada_Package_Decl
                 | Ada_Single_Protected_Decl
                 | Ada_Protected_Type_Decl
                 | Ada_Single_Task_Decl
                 | Ada_Task_Type_Decl
                 | Ada_Generic_Package_Decl
               =>
                  if Find_Pragma (F_Decls (Vis_Part (P))) then
                     return P;
                  elsif Find_Pragma (F_Decls (Priv_Part (P))) then
                     return Ada_Node (Priv_Part (P));
                  end if;

               when Ada_Body_Node =>
                  if Kind (Node) = Ada_Package_Body
                    and then
                      Find_Pragma
                        (P.As_Package_Body.F_Stmts.F_Stmts)
                  then
                     return P.As_Package_Body.F_Stmts.As_Ada_Node;
                  elsif Find_Pragma (F_Decls (Body_Decls (P))) then
                     return P;
                  end if;

               when others => raise Program_Error;
            end case;

            --  We didn't find the pragma at the start of a declaration list,
            --  so it must be immediately following a subprogram declaration
            --  (preceded only by other pragmas).

            pragma Assert (not Prev_Subp_Decl.Is_Null);
            return Prev_Subp_Decl;
         end Find_Section;

         function Find_Pragma
           (L : Ada_Node_List'Class) return Boolean is
         begin
            for C in 1 .. Children_Count (L) loop
               if Kind (Childx (L, C)) /= Ada_Pragma_Node then
                  return False;
               end if;

               if Childx (L, C) = Node then
                  return True;
               end if;
            end loop;

            return False;
         end Find_Pragma;

         ON : Boolean;
         Range_Count : Metric_Nat;
         Global_M : Metrix renames Element (Metrix_Stack, 1).all;
         File_M : Metrix renames Element (Metrix_Stack, 2).all;
         Section : Ada_Node;

         Spark_Mode : constant W_Str := "spark_mode";
         On_Str : constant W_Str := "on";

      --  Start of processing for Gather_SPARK_Line_Metrics

      begin
         case Kind (Node) is
            when Ada_Basic_Subp_Decl =>
               Prev_Subp_Decl := Node;
            when Ada_Pragma_Node =>
               null; -- Leave Prev_Subp_Decl alone
            when others =>
               Prev_Subp_Decl := No_Ada_Node;
         end case;

         if Kind (Node) = Ada_Pragma_Node
           and then Pragma_Name (Node) = Spark_Mode
         then
            if Node.As_Pragma_Node.F_Args.Is_Null then
               ON := True;
            else
               case Node.As_Pragma_Node.F_Args.Children_Count is
                  when 0 => ON := True; pragma Assert (False);
                  when 1 =>
                     declare
                        I : constant Base_Assoc :=
                           Node.As_Pragma_Node.F_Args.Child (1).As_Base_Assoc;
                     begin
                        ON := L_Name (I.P_Assoc_Expr) = "on";
                     end;
                  when others => raise Program_Error;
               end case;
            end if;
            Section := Find_Section;

         elsif Kind (Node) = Ada_Aspect_Assoc
           and then Kind (Node.As_Aspect_Assoc.F_Id) = Ada_Identifier
           and then L_Name (Node.As_Aspect_Assoc.F_Id) = Spark_Mode
         then
            if Node.As_Aspect_Assoc.F_Expr.Is_Null then
               ON := True;
            else
               ON :=
                 L_Name (Node.As_Aspect_Assoc.F_Expr.As_Identifier) = On_Str;
            end if;
            Section := M.Node;

         else
            return;
         end if;

         --  We ignore the pragma if ON is True and it applies to a private
         --  part, statement part of a package, or a subprogram
         --  declaration. Such a pragma is necessarily redundant.
         --  A non-ignored ON applies to the whole syntactic construct
         --  (e.g. for ON in a visible part, we increment by the size of the
         --  whole package declaration, including the private part).
         --
         --  OFF pragmas are not ignored.

         Range_Count := Line_Range_Count
           (Cumulative,
            First_Line => Sloc_Range (Section).Start_Line,
            Last_Line => Sloc_Range (Section).End_Line,
            Metric => Lines_Code);
         if ON then
            case Kind (Section) is
               when Ada_Compilation_Unit
                 | Ada_Package_Decl
                 | Ada_Single_Protected_Decl
                 | Ada_Protected_Type_Decl
                 | Ada_Single_Task_Decl
                 | Ada_Task_Type_Decl
                 | Ada_Generic_Package_Decl
                 | Ada_Body_Node
                 | Ada_Abstract_Subp_Decl
                 | Ada_Subp_Decl
                 | Ada_Generic_Subp_Decl
               =>
                  Inc (File_M.Vals (Lines_Spark), By => Range_Count);
                  Inc (Global_M.Vals (Lines_Spark), By => Range_Count);
               when Ada_Private_Part | Ada_Handled_Stmts =>
                  null;
               when others => raise Program_Error;
            end case;
         else
            --  Protect against seeing "OFF" with no previous "ON".

            if File_M.Vals (Lines_Spark) > Range_Count then
               Dec (File_M.Vals (Lines_Spark), By => Range_Count);
            else
               File_M.Vals (Lines_Spark) := 0;
            end if;

            if Global_M.Vals (Lines_Spark) > Range_Count then
               Dec (Global_M.Vals (Lines_Spark), By => Range_Count);
            else
               Global_M.Vals (Lines_Spark) := 0;
            end if;
         end if;
      end Gather_SPARK_Line_Metrics;

      procedure Gather_Contract_Metrics (Node : Ada_Node) is
         Vis_Decls : constant Ada_Node_List := F_Decls (Vis_Part (Node));

         --  At some point we might want to compute the ratio of
         --  --contracts and --public-subprograms.

         procedure Search_Aspects
           (Subp_Decl : Ada_Node;
            Has_Contracts, Has_Post : out Boolean);
         --  Search through the subprogram's aspects, and set
         --  Has_Contracts and/or Has_Post as appropriate.

         procedure Search_Aspects
           (Subp_Decl : Ada_Node;
            Has_Contracts, Has_Post : out Boolean)
         is
            Aspects : constant Aspect_Spec :=
              Get_Aspects (Subp_Decl.As_Basic_Decl);
         begin
            Has_Contracts := False;
            Has_Post := False;

            if Kind (Subp_Decl) = Ada_Expr_Function then
               --  Expression functions are considered to have contracts and
               --  postconditions.
               Has_Contracts := True;
               Has_Post := True;

            elsif not Aspects.Is_Null then
               declare
                  Assocs : constant Aspect_Assoc_List :=
                    F_Aspect_Assocs (Aspects);
               begin
                  for I in 1 .. Children_Count (Assocs) loop
                     case Assertion_Kind (Childx (Assocs, I)) is
                        when Postcondition =>
                           Has_Contracts := True;
                           Has_Post := True;
                        when Other_Assertion =>
                           Has_Contracts := True;
                        when Not_An_Assertion =>
                           null;
                     end case;
                  end loop;
               end;
            end if;
         end Search_Aspects;

      --  Start of processing for Gather_Contract_Metrics

      begin
         if not Vis_Decls.Is_Null then -- Shouldn't it be empty list???
            for I in 1 .. Children_Count (Vis_Decls) loop
               declare
                  Decl : constant Ada_Node := Childx (Vis_Decls, I);
                  Has_Contracts, Has_Post : Boolean;
               begin
                  if Decl.Kind in Ada_Basic_Subp_Decl | Ada_Expr_Function then
                     --  Shouldn't Ada_Expr_Function be in
                     --  Ada_Basic_Subp_Decl???  It used to be.
                     Search_Aspects (Decl, Has_Contracts, Has_Post);

                     if Has_Contracts then
                        Inc_All (Contract);
                     end if;

                     if Has_Post then
                        Inc_All (Post);
                     end if;
                  end if;
               end;
            end loop;
         end if;
      end Gather_Contract_Metrics;

      procedure Gather_Syntax_Metrics (Node : Ada_Node; M : in out Metrix) is

         procedure Count_Params (Spec : Subp_Spec);
         --  For each Param_Spec of the form "Id1, Id2, ..., IdN : [mode] T",
         --  increment value for Param_Number and the value for the mode
         --  by N (the number of identifiers).

         procedure Count_Params (Spec : Subp_Spec) is
         begin
            if Spec.F_Subp_Params.Is_Null then
               return;
            end if;

            for Param_Spec of Spec.F_Subp_Params.F_Params loop
               declare
                  Num : constant Metric_Nat :=
                    Children_Count (Param_Spec.F_Ids);
               begin
                  Inc (M.Vals (Param_Number), By => Num);

                  case Ada_Mode'(Param_Spec.F_Mode) is
                     when Ada_Mode_Default | Ada_Mode_In =>
                        Inc (M.Vals (In_Parameters), By => Num);
                     when Ada_Mode_Out =>
                        Inc (M.Vals (Out_Parameters), By => Num);
                     when Ada_Mode_In_Out =>
                        Inc (M.Vals (In_Out_Parameters), By => Num);
                  end case;
               end;
            end loop;
         end Count_Params;

      --  Start of processing for Gather_Syntax_Metrics

      begin
         --  Param_Number and friends

         if Kind (Node) in
           Ada_Subp_Decl |
           Ada_Expr_Function |
           Ada_Abstract_Subp_Decl |
           Ada_Null_Subp_Decl |
           Ada_Subp_Renaming_Decl |
           Ada_Subp_Body |
           Ada_Subp_Body_Stub
         then
            declare
               pragma Assert (Node = M.Node);
               Spec : Subp_Spec;
            begin
               begin
                  Spec := Node.As_Basic_Decl.P_Subp_Spec_Or_Null.As_Subp_Spec;
                  Count_Params (Spec);
               exception
                  when Property_Error =>
                     --  If we can't find the spec, ignore it
                     null;
               end;
            end;

         --  For a subprogram instantiation, we have to find the generic
         --  subprogram, and call Count_Params on that spec.

         elsif Kind (Node) = Ada_Generic_Subp_Instantiation then
            begin
               declare
                  pragma Assert (Node = M.Node);
                  G : constant Basic_Decl :=
                    Node.As_Generic_Instantiation.P_Designated_Generic_Decl;
                  Spec : constant Subp_Spec :=
                    G.As_Generic_Subp_Decl.F_Subp_Decl
                      .P_Subp_Spec_Or_Null.As_Subp_Spec;
               begin
                  Count_Params (Spec);
               end;
            exception
               when Property_Error =>
                  --  If the generic unit being instantiated is not present, we
                  --  simply ignore this instantiation.
                  null;
            end;
         end if;

         --  Public_Subprograms

         if Last_Index (Metrix_Stack) = 3 or else In_Visible_Part then
            if Node = M.Node and then not M.Is_Private_Lib_Unit then
               if M.Kind in Ada_Subp_Decl |
                 Ada_Abstract_Subp_Decl |
                 Ada_Generic_Subp_Decl
                 --  Not Ada_Subp_Renaming_Decl
               then
                  Inc_All (Public_Subprograms);
               end if;

               --  A library subprogram body should be counted only if there is
               --  no corresponding spec.

               if Last_Index (Metrix_Stack) = 3
                 and then not Unit_Is_Subunit (Node)
                 and then M.Kind = Ada_Subp_Body
                 and then Acts_As_Spec (M)
               then
                  Inc_All (Public_Subprograms);
               end if;
            end if;
         end if;

         --  All_Subprograms

         if Kind (Node) = Ada_Subp_Body then
            Inc_All (All_Subprograms);
         end if;

         --  Statements

         --  Labels and terminate alternatives are classified as statements by
         --  libadalang, but they are not actually statements, so shouldn't be
         --  counted in the metric.

         if Node.Kind in Ada_Stmt
           and then Node.Kind not in Ada_Label
           and then Node.Kind not in Ada_Terminate_Alternative
           and then Node.Kind not in Ada_Named_Stmt
         then
            if Debug_Flag_W then
               Put ("Statement: \1\n", Node.Image);
            end if;
            Inc_All (Statements);
            Inc_All (Logical_Source_Lines);
         end if;

         --  Declarations

         pragma Assert
           (if Kind (Node) = Ada_Generic_Package_Internal then
              Kind (Parent (Node)) = Ada_Generic_Package_Decl);

         if Kind (Node) in
             Ada_Basic_Decl | Ada_Entry_Index_Spec
           and then Kind (Node) not in
             Ada_Generic_Formal |
             Ada_Generic_Formal_Obj_Decl |
             Ada_Generic_Formal_Package | Ada_Generic_Formal_Subp_Decl |
             Ada_Generic_Formal_Type_Decl |
             Ada_Generic_Package_Internal | Ada_Anonymous_Type_Decl |
             Ada_Named_Stmt_Decl | Ada_Label_Decl |
             Ada_Single_Task_Type_Decl |
             Ada_Generic_Subp_Internal |
             Ada_Exception_Handler
         then
            Inc_All (Declarations);
            Inc_All (Logical_Source_Lines);
         end if;
         if Kind (Node) = Ada_Exception_Handler
           and then not Node.As_Exception_Handler.F_Exception_Name.Is_Null
         then
            Inc_All (Declarations);
            Inc_All (Logical_Source_Lines);
         end if;

         --  Public_Types

         if In_Visible_Part then
            if not M.Is_Private_Lib_Unit and not In_Generic_Formal_Part then
               if Kind (Node) in Ada_Concrete_Type_Decl |
                 Ada_Formal_Type_Decl | Ada_Protected_Type_Decl |
                 Ada_Task_Type_Decl | Ada_Incomplete_Type_Decl_Range
               then
                  Inc_All (Public_Types);
               end if;

               if Kind (Node) in
                 Ada_Concrete_Type_Decl | Ada_Formal_Type_Decl
               then
                  declare
                     Def : constant Type_Def := Node.As_Type_Decl.F_Type_Def;
                  begin
                     if Kind (Def) = Ada_Private_Type_Def
                       or else
                       (Kind (Def) = Ada_Derived_Type_Def
                          and then Def.As_Derived_Type_Def.F_Has_With_Private)
                     then
                        Inc_All (Private_Types);
                     end if;
                  end;
               end if;
            end if;
         end if;

         --  All_Types

         if Kind (Node) in Ada_Concrete_Type_Decl | Ada_Formal_Type_Decl |
           Ada_Protected_Type_Decl | Ada_Task_Type_Decl |
           Ada_Incomplete_Type_Decl_Range
         then
            begin
               if P_Previous_Part (Node.As_Base_Type_Decl).Is_Null
                 and then Node.Parent.Kind /= Ada_Generic_Formal_Type_Decl
               then
                  Inc_All (All_Types);
               end if;
            exception
               when Property_Error =>
                  --  If we can't find the previous part, ignore this
                  null;
            end;
         end if;

         --  Unit_Nesting

         if Is_Program_Unit (Node) then
            if Last_Index (Metrix_Stack) >= 3 then
               declare
                  Top : constant Metrix_Index := Last_Index (Metrix_Stack);
                  Start : constant Metrix_Index :=
                    (if Node = M.Node then Top - 1 else Top);
               begin
                  for X in reverse 3 .. Start loop
                     declare
                        MM : Metrix renames Element (Metrix_Stack, X).all;
                     begin
                        MM.Vals (Unit_Nesting) := Metric_Nat'Max
                          (MM.Vals (Unit_Nesting), Metric_Nat (Start - X) + 1);
                     end;
                  end loop;
               end;
            end if;
         end if;

         --  Construct_Nesting

         if Kind (Node) in
           Ada_Subp_Decl | Ada_Expr_Function | Ada_Generic_Subp_Decl |
           Ada_Task_Type_Decl | Ada_Single_Task_Decl |
           Ada_Protected_Type_Decl | Ada_Single_Protected_Decl |
           Ada_Generic_Package_Instantiation | Ada_Generic_Subp_Instantiation
         then
            pragma Assert
              (M.Vals (Construct_Nesting) = 0
                 or else Kind (Node) in
                   Ada_Generic_Package_Instantiation |
                   Ada_Generic_Subp_Instantiation);
            if M.Vals (Construct_Nesting) = 0 then
               M.Vals (Construct_Nesting) := 1;
            end if;
         end if;

         if Adds_New_Nesting_Level (Node) then
            if Last_Index (Metrix_Stack) >= 3 then
               declare
                  Top : constant Metrix_Index := Last_Index (Metrix_Stack);
               begin
                  for X in reverse 3 .. Top loop
                     declare
                        MM : Metrix renames Element (Metrix_Stack, X).all;
                     begin
                        MM.Vals (Construct_Nesting) := Metric_Nat'Max
                          (MM.Vals (Construct_Nesting),
                           MM.Vals (Current_Construct_Nesting));
                     end;
                  end loop;
               end;
            end if;
         end if;
      end Gather_Syntax_Metrics;

      procedure Gather_Dependencies (Node : Ada_Node) is
         File_M : Metrix renames Element (Metrix_Stack, 2).all;

         procedure Set_Flags (Node : Ada_Node);
         --  Set the Has_Tagged_Type and/or Has_Subp flags if appropriate. In
         --  case of a package instantiation, we recursively walk the generic
         --  unit.

         function Visit (Node : Ada_Node'Class) return Visit_Status;
         --  Visit one subnode of a generic being instantiated

         function Visit (Node : Ada_Node'Class) return Visit_Status is
         begin
            Set_Flags (Ada_Node (Node));
            return Into;
            --  ???This could be more efficient if we return Over in cases
            --  where we know there are no interesting subnodes.
         end Visit;

         procedure Set_Flags (Node : Ada_Node) is
         begin
            case Kind (Node) is
               when Ada_Generic_Package_Instantiation =>
                  begin
                     Traverse
                       (P_Designated_Generic_Decl
                          (Node.As_Generic_Instantiation),
                        Visit'Access);
                     --  Set one or both flags according to whether the generic
                     --  package contains tagged types or subprograms.
                  exception
                     when Property_Error => null;
                        --  ???Ignore errors in P_Designated_Generic_Decl,
                        --  which occur when the generic being instantiated is
                        --  declared by a generic_renaming_declaration.
                  end;

               when Ada_Interface_Type_Def =>
                  File_M.Has_Tagged_Type := True;
               when Ada_Incomplete_Tagged_Type_Decl =>
                  File_M.Has_Tagged_Type := True;
               when Ada_Private_Type_Def =>
                  if Node.As_Private_Type_Def.F_Has_Tagged then
                     File_M.Has_Tagged_Type := True;
                  end if;
               when Ada_Record_Type_Def =>
                  if Node.As_Record_Type_Def.F_Has_Tagged then
                     File_M.Has_Tagged_Type := True;
                  end if;
               when Ada_Derived_Type_Def =>
                  if not Node.As_Derived_Type_Def.F_Record_Extension.Is_Null
                    or else Node.As_Derived_Type_Def.F_Has_With_Private
                  then
                     File_M.Has_Tagged_Type := True;
                  end if;

               when Ada_Generic_Subp_Instantiation |
                 Ada_Subp_Renaming_Decl |
                 Ada_Subp_Decl |
                 Ada_Subp_Body =>
                  File_M.Has_Subp := True;

               when others => null;
            end case;
         end Set_Flags;

      --  Start of processing for Gather_Dependencies

      begin
         case Kind (Node) is
            --  For "with P, Q;" include P and Q

            when Ada_With_Clause =>
               declare
                  Names : constant Name_List :=
                    Node.As_With_Clause.F_Packages;
               begin
                  for I in 1 .. Children_Count (Names) loop
                     Include
                       (File_M.Depends_On,
                        W_Intern (Full_Name (Childx (Names, I).As_Name)));
                  end loop;
               end;

            --  A child unit depends on its parent. We don't need to bother
            --  with bodies here (except subprogram bodies), because the
            --  coupling metrics treat spec and body as one.

            when Ada_Generic_Subp_Instantiation |
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Renaming_Decl |
              Ada_Package_Renaming_Decl |
              Ada_Subp_Renaming_Decl |
              Ada_Package_Decl |
              Ada_Generic_Package_Decl |
              Ada_Subp_Decl |
              Ada_Subp_Body | -- could be acting as spec
              Ada_Generic_Subp_Decl =>
               declare
                  Def_Name : constant Name := Get_Def_Name (Node).F_Name;
               begin
                  if Kind (Def_Name) = Ada_Dotted_Name then
                     pragma Assert (File_M.Child_Parent = Empty_CU_Sym);
                     File_M.Child_Parent :=
                       W_Intern (Full_Name (Def_Name.As_Dotted_Name.F_Prefix));
                     Include (File_M.Depends_On, File_M.Child_Parent);
                  end if;
               end;

            when others => null;
         end case;

         Set_Flags (Node);
      end Gather_Dependencies;

      procedure Rec (Node : Ada_Node) is
      begin
         if Debug_Flag_V then
            Put ("-->Walk: \1\n", Node.Image);
            Indent;
         end if;

         case Kind (Node) is
            when Ada_Exception_Handler =>
               Inc (Exception_Handler_Count);
            when Ada_Quantified_Expr =>
               Inc (Quantified_Expr_Count);
            when Ada_Expr_Function =>
               Inc (Expr_Function_Count);
            when Ada_For_Loop_Stmt | Ada_Loop_Stmt | Ada_While_Loop_Stmt =>
               Inc (Loop_Count);
            when Ada_Private_Part =>
               Inc (Private_Part_Count);
            when Ada_Entry_Body | Ada_Subp_Body | Ada_Task_Body =>
               Inc (Non_Package_Body_Count);
            when others => null;
         end case;

         if Is_Assertion (Node) then
            pragma Assert (not In_Assertion);
            In_Assertion := True;
         end if;

         Push (Node_Stack, Node);

         --  For the library item and eligible local units, we push and pop the
         --  Metrix_Stack around the call to Gather_Metrics_And_Walk_Children;
         --  otherwise we just call Gather_Metrics_And_Walk_Children.

         if Node = Outer_Unit or else Kind (Node) in Eligible then
            declare
               Global_M : Metrix renames Element (Metrix_Stack, 1).all;
               File_M : Metrix renames Element (Metrix_Stack, 2).all;
               Parent : Metrix renames
                 Element (Metrix_Stack, Last_Index (Metrix_Stack)).all;
               M : constant Metrix_Ref :=
                 Push_New_Metrix (Tool, Node, Arg (Cmd, Ignore_Assertions));
               Saved_Loop_Count : constant Natural := Loop_Count;
            begin
               Loop_Count := 0;
               M.Visible := In_Visible_Part; -- must be after Push_New_Metrix
               Append (Parent.Submetrix, M);
               Gather_Metrics_And_Walk_Children (Node);
               if M.Has_Complexity_Metrics then
                  Inc (File_M.Num_With_Complexity);
                  Inc (Global_M.Num_With_Complexity);
               end if;
               if M.Knd = Function_Body_Knd then
                  if M.Vals (Extra_Exit_Points) >= 1 then
                     Dec (M.Vals (Extra_Exit_Points));
                  end if;
               end if;
               Validate (M.all);
               M.Node := No_Ada_Node;
               Pop (Metrix_Stack);
               Loop_Count := Saved_Loop_Count;
            end;
         else
            Gather_Metrics_And_Walk_Children (Node);
         end if;

         Pop (Node_Stack);

         if Is_Assertion (Node) then
            pragma Assert (In_Assertion);
            In_Assertion := False;
         end if;

         case Kind (Node) is
            when Ada_Exception_Handler =>
               Dec (Exception_Handler_Count);
            when Ada_Quantified_Expr =>
               Dec (Quantified_Expr_Count);
            when Ada_Expr_Function =>
               Dec (Expr_Function_Count);
            when Ada_For_Loop_Stmt | Ada_Loop_Stmt | Ada_While_Loop_Stmt =>
               Dec (Loop_Count);
            when Ada_Private_Part =>
               Dec (Private_Part_Count);
            when Ada_Entry_Body | Ada_Subp_Body | Ada_Task_Body =>
               Dec (Non_Package_Body_Count);
            when others => null;
         end case;

         if Debug_Flag_V then
            Outdent;
            Put ("<--Walk: \1\n", Node.Image);
         end if;
      end Rec;

      procedure Gather_Metrics_And_Walk_Children (Node : Ada_Node) is
         M : Metrix renames
           Element (Metrix_Stack, Last_Index (Metrix_Stack)).all;
         With_Trivia : constant Children_Array := Children_And_Trivia (Node);
         B           : Boolean;

      --  Start of processing for Gather_Metrics_And_Walk_Children

      begin
         for Trivium of With_Trivia loop
            if Trivium.Kind = Trivia then
               declare
                  K : constant Token_Kind := Kind (Data (Trivium.Trivia));
               begin
                  pragma Assert
                    (K in Ada_Comment | Ada_Whitespace | Ada_Prep_Line);
               end;
            end if;
         end loop;

         --  Gather Current_Construct_Nesting

         if Adds_New_Nesting_Level (Node) then
            if Last_Index (Metrix_Stack) >= 3 then
               Inc_All (Current_Construct_Nesting);
            end if;
         end if;

         B := Node.Parents'Length > 2;
         if B
           and then False -- ???See P907-045
         then
            pragma Assert
              (Node.Parents (3) =
                 Element (Node_Stack, Last_Index (Node_Stack) - 2));
         end if;

         if not In_Ignored_Assertion then
            Gather_Line_Metrics (Node, M);
            Gather_SPARK_Line_Metrics (Node, M);
            Gather_Syntax_Metrics (Node, M);

            if Kind (Node) in
                Ada_Package_Decl | Ada_Generic_Package_Decl |
                Ada_Task_Def | Ada_Protected_Def
              and then In_Visible_Part
            then
               --  We gather contract metrics only for public subprograms

               Gather_Contract_Metrics (Node);
            end if;

            Gather_Dependencies (Node);
         end if;

         if (M.Has_Complexity_Metrics
               and then M.Kind /= Ada_Compilation_Unit)
           or else (M.Kind in Contract_Complexity_Eligible
                      and then In_Visible_Part)
         then
            Cyclomate (Node, M);
            Gather_Essential_Complexity (Node, M);
            Gather_Extra_Exit_Points (Node, M);
         end if;

         for I in 1 .. Children_Count (Node) loop
            declare
               Cur_Child : constant Ada_Node := Child (Node, I);
               In_Generic_Formal_Part_Set : Boolean := False;
            begin
               if not Cur_Child.Is_Null then
                  if Kind (Node) in
                    Ada_Generic_Package_Decl | Ada_Generic_Subp_Decl
                  then
                     pragma Assert
                       ((I = 1) =
                        (Cur_Child = G_Formal_Part (Node).As_Ada_Node));
                     pragma Assert (not In_Generic_Formal_Part);
                     if I = 1 then
                        In_Generic_Formal_Part := True;
                        In_Generic_Formal_Part_Set := True;
                     end if;
                  end if;

                  Rec (Cur_Child);

                  if In_Generic_Formal_Part_Set then
                     pragma Assert (In_Generic_Formal_Part);
                     In_Generic_Formal_Part := False;
                  end if;
               end if;
            end;
         end loop;

         if Adds_New_Nesting_Level (Node) then
            if Last_Index (Metrix_Stack) >= 3 then
               Dec_All (Current_Construct_Nesting);
            end if;
         end if;

         if not Is_Empty (EC_Stack) -- See Gather_Essential_Complexity
           and then Last_Element (EC_Stack).Node = Node
         then
            Pop (EC_Stack);
         end if;
      end Gather_Metrics_And_Walk_Children;

      pragma Assert (Length (Metrix_Stack) = 1);
      Global_M : Metrix renames Element (Metrix_Stack, 1).all;
      File_M : constant Metrix_Ref :=
        Push_New_Metrix
          (Tool, CU_Node, Arg (Cmd, Ignore_Assertions),
           Source_File_Name => new String'(File_Name));

   --  Start of processing for Process_CU

   begin
      if Debug_Flag_V then
         Print (Unit);
         Put ("With trivia\n");
         PP_Trivia (Unit);
      end if;

      if Debug_Flag_V then
         Put ("-->Walk: \1\n", CU_Node.Image);
         Indent;
      end if;

      Push (Node_Stack, CU_Node);
      Append (Global_M.Submetrix, File_M);

      Gather_Metrics_And_Walk_Children (CU_Node);

      --  Gather "Computed_" metrics.

      if File_M.Vals (Public_Types) /= 0 then
         Inc (Global_M.Vals (Computed_Public_Types));
      end if;

      if File_M.Vals (All_Types) /= 0 then
         Inc (Global_M.Vals (Computed_All_Types));
      end if;

      if File_M.Vals (Public_Subprograms) /= 0 then
         Inc (Global_M.Vals (Computed_Public_Subprograms));
      end if;

      if File_M.Vals (All_Subprograms) /= 0 then
         Inc (Global_M.Vals (Computed_All_Subprograms));
      end if;

      File_M.Node := No_Ada_Node;
      Pop (Metrix_Stack);
      Pop (Node_Stack);
      pragma Assert (Length (Metrix_Stack) = 1);
      pragma Assert (Length (Node_Stack) = 0);
      pragma Assert (File_M.Vals (Complexity_Cyclomatic) =
                       File_M.Vals (Complexity_Statement) +
                       File_M.Vals (Complexity_Expression));

      if Debug_Flag_V then
         Outdent;
         Put ("<--Walk: \1\n", CU_Node.Image);
      end if;
   end Process_CU;

   ---------------------
   -- Per_File_Action --
   ---------------------

   procedure Per_File_Action
     (Tool : in out Metrics_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit)
   is
      pragma Unreferenced (Input, BOM_Seen);
      Unit_Root : constant Ada_Node := Root (Unit);
      pragma Assert (not Unit_Root.Is_Null);

   begin
      --  The root node of an empty file is of Ada_Compilation_Unit_List
      --  kind. Therefore, do nothing for empty files.

      if Unit_Root.Kind = Ada_Compilation_Unit_List
        and then Length (Unit_Root.As_Compilation_Unit_List) = 0
      then
         return;
      end if;

      if Debug_Flag_V then
         Print (Unit);
         Put ("With trivia\n");
         PP_Trivia (Unit);
      end if;

      if Unit_Root.Kind = Ada_Pragma_Node_List then
         Put ("Skipping \1 (pragmas only)\n", File_Name);
      elsif Unit_Root.Kind = Ada_Compilation_Unit_List then
         for Unit_Node of Unit_Root.As_Compilation_Unit_List loop
            Process_CU (Tool, Cmd, File_Name, Unit, Unit_Node.As_Ada_Node);
         end loop;
      else
         Process_CU (Tool, Cmd, File_Name, Unit, Unit_Root);
      end if;
   end Per_File_Action;

   ---------------
   -- Tool_Help --
   ---------------

   procedure Tool_Help (Tool : Metrics_Tool) is
      pragma Unreferenced (Tool);
   begin
      pragma Style_Checks ("M200"); -- Allow long lines
      Put ("usage: gnatmetric [options] {filename}\n");
      Put (" options:\n");
      Put (" --version - Display version and exit\n");
      Put (" --help    - Display usage and exit\n");
      Put ("\n");

      Put (" -Pproject        - Use project file project. Only one such switch can be used\n");
      Put (" -U               - process all sources of the argument project\n");
      Put (" -U main          - process the closure of units rooted at unit main\n");
      Put (" --no-subprojects - Process sources of root project only\n");
      Put (" -Xname=value     - specify an external reference for argument project file\n");
      Put (" --subdirs=dir    - specify subdirectory to place the result files into\n");
      Put (" -eL              - follow all symbolic links when processing project files\n");
      Put ("\n");

      if False then -- Disable this for now
         Put (" --incremental -- incremental processing on a per-file basis\n");
      end if;
      Put (" --verbose    - verbose mode\n");
      Put (" --quiet      - quiet mode\n");
      Put ("\n");

      Put (" --no-local-metrics - do not compute detailed metrics for local program units\n");
      Put ("\n");

      Put ("Options to control metrics to compute. An option --<metric> turns computing\n");
      Put ("the metric ON, the corresponding --no-<metric> option turns computing the\n");
      Put ("metric OFF. If no metric option is specified, all the metrics are computed\n");
      Put ("and reported. If at least one positive option is  specified, only explicitly\n");
      Put ("selected metrics are computed.\n");
      Put ("\n");

      Put ("Contract metrics:\n");
      Put ("  --contract-all        - all contract metrics\n");
      Put ("  --contract            - subprograms with contracts\n");
      Put ("  --post                - subprograms with postconditions\n");
      Put ("  --contract-complete   - subprograms with complete contracts\n");
      Put ("  --contract-cyclomatic - McCabe Cyclomatic Complexity of contracts\n");
      Put ("\n");

      Put ("Complexity metrics:\n");
      Put ("  --complexity-all        - all complexity metrics\n");
      Put ("  --complexity-cyclomatic - McCabe Cyclomatic Complexity\n");
      Put ("  --complexity-essential  - Essential Complexity\n");
      Put ("  --complexity-average    - average McCabe Cyclomatic Complexity of a body\n");
      Put ("  --loop-nesting          - maximal loop nesting level\n");
      Put ("  --no-static-loop        - do not count static loops for cyclomatic complexity\n");
      Put ("  --no-treat-exit-as-goto - do not consider exit statements as gotos when\n");
      Put ("                            computing Essential Complexity\n");
      Put ("  --extra-exit-points     - extra exit points in subprograms\n");
      Put ("\n");

      Put ("Line metrics:\n");
      Put ("  --lines-all         - all line metrics\n");
      Put ("  --lines             - number of all lines\n");
      Put ("  --lines-code        - number of code lines\n");
      Put ("  --lines-comment     - number of comment lines\n");
      Put ("  --lines-eol-comment - number of code lines also containing comments\n");
      Put ("  --lines-ratio       - comment/code lines percentage\n");
      Put ("  --lines-blank       - number of blank lines\n");
      Put ("  --lines-average     - average number of code lines in a body\n");
      Put ("  --lines-spark       - code lines with SPARK_Mode (not included in --lines-all)\n");
      Put ("\n");

      Put ("Syntax element metrics:\n");
      Put ("  --syntax-all         - all syntax element metrics\n");
      Put ("  --declarations       - total number of declarations\n");
      Put ("  --statements         - total number of statements\n");
      Put ("  --public-subprograms - number of public subprograms in a compilation unit\n");
      Put ("  --all-subprograms    - number of subprograms in a compilation unit\n");
      Put ("  --public-types       - number of public types in a compilation unit\n");
      Put ("  --all-types          - number of types in a compilation unit\n");
      Put ("  --unit-nesting       - maximal unit nesting level\n");
      Put ("  --construct-nesting  - maximal construct nesting level\n");
      Put ("\n");

      Put ("Coupling metrics:\n");
      Put ("  --coupling-all           - all coupling metrics\n");
      Put ("  --tagged-coupling-out    - tagged (class) fan-out coupling\n");
      Put ("  --tagged-coupling-in     - tagged (class) fan-in coupling\n");
      Put ("  --hierarchy-coupling-out - hierarchy (category) fan-out coupling\n");
      Put ("  --hierarchy-coupling-in  - hierarchy (category) fan-in coupling\n");
      Put ("  --unit-coupling-out      - unit fan-out coupling\n");
      Put ("  --unit-coupling-in       - unit fan-in coupling\n");
      Put ("  --control-coupling-out   - control fan-out coupling\n");
      Put ("  --control-coupling-in    - control fan-in coupling\n");
      Put ("\n");

      Put ("output file control:\n");
      Put ("  --output-dir=dirname     - put files with detailed metrics into 'dirname'\n");
      Put ("  --generate-xml-output    - generate XML output\n");
      Put ("  --generate-xml-schema    - generate XML output and corresponding schema file\n");
      Put ("  --no-text-output         - do not generate output in text form\n");
      Put ("  --output-suffix=file-suffix - suffix for the detailed metrics file\n");
      Put ("  --global-file-name=filename - name of the global metrics file\n");
      Put ("  --xml-file-name=filename    - name of the XML output file\n");
      Put ("  --short-file-names          - use short source file names in output\n");
      Put ("\n");

      Put (" filename         - name of Ada source file for which metrics\n");
      Put ("                    should be computed\n");
      Put (" --files=filename - name of a text file containing a list of Ada\n");
      Put ("                    source files for which metrics should be computed\n");
      pragma Style_Checks ("M79");
   end Tool_Help;

   --------------
   -- Validate --
   --------------

   procedure Validate (M : Metrix) is
      L : constant Metric_Nat := M.Vals (Lines);
   begin
      pragma Assert (M.Vals (Lines_Code) <= L);
      pragma Assert (M.Vals (Lines_Comment) <= L);
      pragma Assert (M.Vals (Lines_Eol_Comment) <= L);
      pragma Assert (M.Vals (Lines_Blank) <= L);

      pragma Assert (M.Vals (Logical_Source_Lines) =
                       M.Vals (Statements) + M.Vals (Declarations));

      pragma Assert
        (M.Vals (Hierarchy_Coupling_Out) <= M.Vals (Tagged_Coupling_Out));
      pragma Assert
        (M.Vals (Hierarchy_Coupling_In) <= M.Vals (Tagged_Coupling_In));
   end Validate;

   procedure Validate_Coupling (Units_For_Coupling : Metrix_Vectors.Vector) is
      Totals : array (Coupling_Metrics) of Metric_Nat := [others => 0];
   begin
      for File_M of Units_For_Coupling loop
         Validate (File_M.all);

         for Metric in Coupling_Metrics loop
            Totals (Metric) := Totals (Metric) + File_M.Vals (Metric);
         end loop;
      end loop;

      pragma Assert
        (Totals (Tagged_Coupling_Out) = Totals (Tagged_Coupling_In));
      pragma Assert
        (Totals (Hierarchy_Coupling_Out) = Totals (Hierarchy_Coupling_In));
      pragma Assert
        (Totals (Control_Coupling_Out) = Totals (Control_Coupling_In));
      pragma Assert
        (Totals (Unit_Coupling_Out) = Totals (Unit_Coupling_In));
   end Validate_Coupling;

   procedure Dump_Metrix (M : Metrix) is
   begin
      Put ("\1 \2:\n",
           Str (M.CU_Name).S,
           (if M.Is_Spec then "spec" else "body"));
      Indent;

      for Sym of M.Depends_On loop
         Put ("<dependence dep=""\1 \2 depends on \3"">\n",
              Str (M.CU_Name).S,
              (if M.Is_Spec then "spec" else "body"),
                Str (Sym).S);
      end loop;

      Outdent;
      Put ("\n");
   end Dump_Metrix;

   procedure Dump
     (Tool : in out Metrics_Tool;
      Global_M : Metrix;
      Message : String := "")
   is
      pragma Unreferenced (Tool);
   begin
      if Debug_Flag_V then
         Put ("\1\n", Message);
         for File_M of Global_M.Submetrix loop
            Dump_Metrix (File_M.all);
         end loop;
      end if;
   end Dump;

end METRICS.Actions;
