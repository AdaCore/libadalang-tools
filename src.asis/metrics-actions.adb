with Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO; use Ada;
with Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Vectors;
with Langkit_Support.Tokens; use Langkit_Support;

with Libadalang;     use Libadalang;
with Libadalang.AST; use Libadalang.AST;
with Libadalang.AST.Types; use Libadalang.AST.Types;
with LAL_Extensions; use LAL_Extensions;

with LAL_UL.Common; use LAL_UL; use LAL_UL.Common;
with LAL_UL.Formatted_Output; use LAL_UL.Formatted_Output;
with LAL_UL.String_Utilities; use LAL_UL.String_Utilities;

with ASIS_UL.Debug; use ASIS_UL.Debug;

pragma Warnings (Off);
with LAL_UL.Projects;
with LAL_UL.Drivers;
pragma Warnings (On);

with METRICS.Command_Lines; use METRICS.Command_Lines;

package body METRICS.Actions is

   function Image (X : Integer) return String
     renames String_Utilities.Image;

   pragma Warnings (Off); -- ????????????????
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   use Metrics_Flag_Switches, Metrics_Boolean_Switches,
     Metrics_String_Switches, Metrics_String_Seq_Switches;
   pragma Warnings (On);

   function Has_Contracts (Subp_Decl : Ada_Node) return Boolean;
   --  True if the subprogram has contract aspects

   subtype Percent is Float range 0.0 .. 100.0;

   function Get_Coverage (Pkg_Decl : Base_Package_Decl) return Percent;
   --  Returns the percentage of subprograms with contracts, so if all
   --  subprograms have contracts, 100.0 is returned. 100.0 is returned if
   --  there are no subprograms.

   procedure Compute_Contract_Metrics
     (File_Name : String; Unit : Analysis_Unit);

   procedure Walk
     (Cmd : Command_Line;
      File_Name : String;
      CU_Node : Ada_Node;
      Metrics_To_Compute : Metrics_Set);

   function Has_Contracts (Subp_Decl : Ada_Node) return Boolean is
      Aspects : constant Aspect_Specification :=
        F_Aspects (Subprogram_Decl (Subp_Decl));
   begin
      --  Search through the aspects, and return True if we find one of the
      --  relevant ones.

      if Aspects /= null then
         declare
            Assocs : constant List_Aspect_Assoc := F_Aspect_Assocs (Aspects);
         begin
            for I in 0 .. Child_Count (Assocs) - 1 loop
               declare
                  Assoc : constant Ada_Node := Childx (Assocs, I);
                  use Ada.Wide_Wide_Characters.Handling;
                  Id : constant Expr := F_Id (Aspect_Assoc (Assoc));
               begin
                  if Kind (Id) = Identifier_Kind then
                     declare
                        Text : constant Text_Type :=
                          To_Lower (F_Tok (Single_Tok_Node (Id)).Text.all);
                     begin
                        if Text = "contract_cases"
                          or else Text = "pre"
                          or else Text = "post"
                        then
                           return True;
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end;
      end if;

      return False;
   end Has_Contracts;

   function Get_Coverage (Pkg_Decl : Base_Package_Decl) return Percent is
      Decls : constant List_Ada_Node := F_Decls (Pkg_Decl);

      Has_Contracts_Count : Natural := 0;
      --  Number of subprograms in the package with contracts

      Subp_Count : Natural := 0;
      --  Number of subprograms in the package.

      --  The result will be the ratio of the above two.

   begin
      if Decls /= null then -- Shouldn't it be empty list???
         for I in 0 .. Child_Count (Decls) - 1 loop
            declare
               Decl : constant Ada_Node := Childx (Decls, I);
            begin
               if Kind (Decl) = Subprogram_Decl_Kind then
                  if Debug_Flag_V then
                     Put ("    Doing subprogram ");
                     Wide_Wide_Text_IO.Put
                       (Full_Name
                          (F_Name (F_Subp_Spec (Subprogram_Decl (Decl)))));
                  end if;

                  Subp_Count := Subp_Count + 1;

                  if Has_Contracts (Decl) then
                     if Debug_Flag_V then
                        Put (": yes\n");
                     end if;

                     Has_Contracts_Count := Has_Contracts_Count + 1;

                  elsif Debug_Flag_V then
                     Put (": no\n");
                  end if;
               end if;
            end;
         end loop;
      end if;

      if Subp_Count = 0 then -- Don't divide by zero!
         return 100.0;
      else
         return (100.0 * Float (Has_Contracts_Count)) / Float (Subp_Count);
      end if;
   end Get_Coverage;

   procedure Compute_Contract_Metrics
     (File_Name : String; Unit : Analysis_Unit) is

      pragma Unreferenced (File_Name);

      function Is_Pkg_Or_Gen (Node : Ada_Node) return Boolean is
        (Kind (Node) in Base_Package_Decl_Kind | Package_Decl_Kind);
      --  True if the node is a generic package declaration or a
      --  package declaration.

      Packs : constant Ada_Node_Vectors.Elements_Array :=
        Find_All (Root (Unit), Is_Pkg_Or_Gen'Access);

   begin
      if Debug_Flag_V then
         Print (Unit);
      end if;

      --  Go through all the [generic] package declarations in the
      --  file, calculate the coverage, and print it out.

      for P : Ada_Node of Packs loop
         declare
            Pkg_Decl : constant Base_Package_Decl :=
              Base_Package_Decl (P);
            Coverage : constant Percent := Get_Coverage (Pkg_Decl);
         begin
            Put ("Contract coverage for package ");
            Wide_Wide_Text_IO.Put
              (Full_Name (F_Package_Name (Pkg_Decl)));
            Put (":\1%\n", Integer (Coverage)'Img);
         end;
      end loop;
   end Compute_Contract_Metrics;

   use Ada_Node_Vectors;

   type Metric_Int is new Natural;
   procedure Inc (X : in out Metric_Int);

   procedure Inc (X : in out Metric_Int) is
   begin
      X := X + 1;
   end Inc;

   type Metrics_Values is array (Metrics_Enum) of Metric_Int;

   type Metrix is record
      Node : Ada_Node;
      --  Node to which the metrics are associated

      Nesting : Natural;
      --  Nesting level. 0 for the compilation unit, and 0 for the library item
      --  therein. Then increasing by 1. So Metrix_Stack[0] and Metrix_Stack[1]
      --  both have Nesting = 0, and Metrix_Stack[2].Nesting = 1.

      Vals : Metrics_Values;
   end record;

   type Metrix_Ref is access all Metrix;

   package Metrix_Vectors is new Langkit_Support.Vectors (Metrix_Ref);
   use Metrix_Vectors;

   subtype Eligible is Ada_Node_Type_Kind with
     Predicate => Eligible in
       Generic_Package_Decl_Kind |
       Package_Body_Kind |
       Package_Decl_Kind |
       Protected_Body_Kind |
       Protected_Decl_Kind |
       Protected_Type_Decl_Kind |
       Subprogram_Body_Kind |
       Task_Body_Kind |
       Task_Decl_Kind |
       Task_Type_Decl_Kind;
   --  These are the node kinds that the gnatmetric documentation calls
   --  "eligible local units". We compute metrics for the outermost node (the
   --  compilation unit), as well as eligible local units. A procedure
   --  declaration, for example, has metrics only if it is the outer node.

   function Q (S : String) return String is -- quote
     ("""" & S & """");

   function File_Name_To_Print
     (Cmd : Command_Line; File_Name : String) return String is
      (if Arg (Cmd, Short_File_Name_In_Output)
         then File_Name
         else Normalize_Pathname (File_Name));

   function Lines_String
     (Sloc_Range : Tokens.Source_Location_Range) return String is
      (Image (Integer (Sloc_Range.Start_Line)) & ": " &
       Image (Integer (Sloc_Range.End_Line)));

   function Node_Kind_String (Node : Ada_Node) return String;
   --  Name of the node kind for printing in both XML and text

   function XML_Metric_Name_String (Metric : Metrics_Enum) return String;
   --  Name of the metric for printing in XML

   function Metric_Name_String (Metric : Metrics_Enum) return String;
   --  Name of the metric for printing in text

   procedure Print_Range
     (Name : String;
      Metrics_To_Compute : Metrics_Set;
      First, Last : Metrics_Enum;
      M : Metrix);
   --  Prints a range of metrics. This is needed because the metrics are
   --  printed in groups (line metrics, contract metrics, etc).  Name is the
   --  name of the group, e.g. "=== Lines metrics ===". Prints the name
   --  followed by metrics First..Last. (????Leave out the "===")

   procedure Print_Metrix
     (File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      Node_Metrix : Metrix_Vectors.Vector;
      Index : Natural);
   --  Print the metrics for one node

   procedure XML_Print_Metrix
     (File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      Node_Metrix : Metrix_Vectors.Vector;
      Index : Natural);

   function Node_Kind_String (Node : Ada_Node) return String is
   begin
      case Kind (Node) is
         when Generic_Package_Decl_Kind =>
            return "generic package";
         when Package_Body_Kind =>
            return "package body";
         when Package_Decl_Kind =>
            return "package";
         when Protected_Body_Kind =>
            return "protected body";
         when Protected_Decl_Kind =>
            return "single protected object";
         when Protected_Type_Decl_Kind =>
            return "protected type";
         when Subprogram_Body_Kind =>
            declare
               R : constant Type_Expression :=
                 F_Returns (F_Subp_Spec (Subprogram_Body (Node)));
            begin
               return (if R = null then "procedure" else "function") & " body";
            end;
         when Task_Body_Kind =>
            return "task body";
         when Task_Decl_Kind =>
            return "single task";
         when Task_Type_Decl_Kind =>
            return "task type";

         when Generic_Instantiation_Kind =>
            return "generic instantiation kind";
         when Generic_Renaming_Decl_Kind =>
            return "generic renaming decl_kind";
         when Generic_Subprogram_Decl_Kind =>
            return "generic subprogram decl_kind";
         when Package_Renaming_Decl_Kind =>
            return "package renaming decl_kind";
         when Subprogram_Decl_Kind =>
            return "subprogram decl kind";

         when others => raise Program_Error;
      end case;
   end Node_Kind_String;

   function Metric_Name_String (Metric : Metrics_Enum) return String is
   begin
      --  In many, but not all, cases the name of the metric in the text output
      --  is the same as the name in the XML, replacing "_" with " ".

      case Metric is
         when Statements =>
            return "all statements";
         --  ????More 'when's go here.

         when others =>
            return Replace_String
              (XML_Metric_Name_String (Metric), From => "_", To => " ");
      end case;
   end Metric_Name_String;

   procedure Print_Range
     (Name : String;
      Metrics_To_Compute : Metrics_Set;
      First, Last : Metrics_Enum;
      M : Metrix)
   is
   begin
      if Metrics_To_Compute (First .. Last) /= (First .. Last => False) then
         Put ("\n\1\n", Name);
         Indent;

         for I in First .. Last loop
            if Metrics_To_Compute (I) then
               if M.Vals (I) /= 0 then -- ????????????????
                  Put ("\1           : \2\n",
                       Metric_Name_String (I),
                       Image (Integer (M.Vals (I))));
               end if;
            end if;
         end loop;

         Outdent;
      end if;
   end Print_Range;

   procedure Print_Metrix
     (File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      Node_Metrix : Metrix_Vectors.Vector;
      Index : Natural)
   is
      M : Metrix renames Get (Node_Metrix, Index).all;
   begin
      Indent (M.Nesting * Default_Indentation_Amount);

      if Index = 0 then
         pragma Assert (Kind (M.Node) = Compilation_Unit_Kind);
         declare
            pragma Assert (Last_Index (Node_Metrix) >= 1); -- ????????????????
            Lib_Item : constant Ada_Node := Get (Node_Metrix, 1).Node;
            --  Node_Metrix[0] is the compilation unit, and Node_Metrix[1] is
            --  the library item therein. Note this isn't what libadalang calls
            --  Library_Item; this is the package body or whatever node.
            --  ????????????????Lib_Item could be a subunit.
         begin
            Put ("Metrics computed for \1\n",
                 File_Name_To_Print (Cmd, File_Name));
            Put ("containing \1 \2\n",
                 Node_Kind_String (Lib_Item),
                 To_UTF8 (Full_Name (Get_Name (Lib_Item))));
         end;

      else
         declare
            LI : constant String :=
              (if Index = 1 then " - library item" else "");
         begin
            Put ("\n\1 (\2\3 at lines  \4)\n",
                 To_UTF8 (Full_Name (Get_Name (M.Node))),
                 Node_Kind_String (M.Node), LI,
                 Lines_String (Sloc_Range (M.Node)));
         end;
      end if;

      Print_Range
        ("=== Lines metrics ===",
         Metrics_To_Compute,
         Lines_Metrics'First, Lines_Metrics'Last, M);

      if Index /= 0 then
         Print_Range
           ("=== Contract metrics ===",
            Metrics_To_Compute,
            Contract_Metrics'First, Contract_Metrics'Last, M);
         Print_Range
           ("=== Element metrics ===",
            Metrics_To_Compute,
            Syntax_Metrics'First, Syntax_Metrics'Last, M);
         Print_Range
           ("=== Complexity metrics ===",
            Metrics_To_Compute,
            Complexity_Metrics'First, Complexity_Metrics'Last, M);
      end if;

      Outdent (M.Nesting * Default_Indentation_Amount);
   end Print_Metrix;

   function XML_Metric_Name_String (Metric : Metrics_Enum) return String is
   begin
      case Metric is
         when Contract =>
            return "contract";
         when Post =>
            return "post";
         when Contract_Complete =>
            return "contract_complete";
         when Contract_Cyclomatic =>
            return "contract_cyclomatic";
         when Complexity_Cyclomatic =>
            return "complexity_cyclomatic";
         when Complexity_Essential =>
            return "complexity_essential";
         when Complexity_Average =>
            return "complexity_average";
         when Loop_Nesting =>
            return "loop_nesting";
         when Extra_Exit_Points =>
            return "extra_exit_points";
         when Lines =>
            return "lines";
         when Lines_Code =>
            return "lines_code";
         when Lines_Comment =>
            return "lines_comment";
         when Lines_Eol_Comment =>
            return "lines_eol_comment";
         when Lines_Ratio =>
            return "lines_ratio";
         when Lines_Blank =>
            return "lines_blank";
         when Lines_Average =>
            return "lines_average";
         when Declarations =>
            return "declarations";
         when Statements =>
            return "all_stmts";
         when Public_Subprograms =>
            return "public_subprograms";
         when All_Subprograms =>
            return "all_subprograms";
         when Public_Types =>
            return "public_types";
         when All_Types =>
            return "all_types";
         when Unit_Nesting =>
            return "unit_nesting";
         when Construct_Nesting =>
            return "construct_nesting";
         when Param_Number =>
            return "param_number";
         when Tagged_Coupling_Out =>
            return "tagged_coupling_out";
         when Tagged_Coupling_In =>
            return "tagged_coupling_in";
         when Hierarchy_Coupling_Out =>
            return "hierarchy_coupling_out";
         when Hierarchy_Coupling_In =>
            return "hierarchy_coupling_in";
         when Unit_Coupling_Out =>
            return "unit_coupling_out";
         when Unit_Coupling_In =>
            return "unit_coupling_in";
         when Control_Coupling_Out =>
            return "control_coupling_out";
         when Control_Coupling_In =>
            return "control_coupling_in";
      end case;
   end XML_Metric_Name_String;

   procedure XML_Print_Metrix
     (File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      Node_Metrix : Metrix_Vectors.Vector;
      Index : Natural)
   is
      M : Metrix renames Get (Node_Metrix, Index).all;
   begin
      Indent (M.Nesting * Default_Indentation_Amount);

      if Index = 0 then
         pragma Assert (Kind (M.Node) = Compilation_Unit_Kind);
         Put ("<file name=\1>\n", Q (File_Name_To_Print (Cmd, File_Name)));
         Indent;

      else
         declare
            Sloc : constant Tokens.Source_Location_Range :=
              Sloc_Range (M.Node);
         begin
            Put ("<unit name=\1 kind=\2 line=\3 col=\4>\n",
                 Q (To_UTF8 (Full_Name (Get_Name (M.Node)))),
                 Q (Node_Kind_String (M.Node)),
                 Q (Image (Integer (Sloc.Start_Line))),
                 Q (Image (Integer (Sloc.Start_Column))));
         end;
      end if;

      Indent;

      for I in M.Vals'Range loop
         if Metrics_To_Compute (I) then
            if M.Vals (I) /= 0 then -- ????????????????
               Put ("<metric name=\1>\2</metric>\n",
                    Q (XML_Metric_Name_String (I)),
                    Image (Integer (M.Vals (I))));
            end if;
         end if;
      end loop;

      Outdent;

      if Index = 0 then
         Outdent;
         Put ("</file>\n");
      else
         Put ("</unit>\n");
      end if;

      Outdent (M.Nesting * Default_Indentation_Amount);
   end XML_Print_Metrix;

   procedure Walk
     (Cmd : Command_Line;
      File_Name : String;
      CU_Node : Ada_Node;
      Metrics_To_Compute : Metrics_Set)
   is
      pragma Assert (CU_Node /= null);
      pragma Assert (Kind (CU_Node) = Compilation_Unit_Kind);
--    pragma Assert (Child_Count (F_Bodies (Compilation_Unit (CU_Node))) = 1);

      First_Body : constant Ada_Node :=
        Childx (F_Bodies (Compilation_Unit (CU_Node)), 0);
      Lib_Item : constant Ada_Node :=
        (case Kind (First_Body) is
           when Library_Item_Kind => F_Item (Library_Item (First_Body)),
           when Subunit_Kind => Ada_Node (F_Body (Subunit (First_Body))),
           when others => raise Program_Error);
      --  ????????????????Could be subunit

      Node_Stack : Ada_Node_Vectors.Vector;
      --  Stack of all nodes currently being walked

      Metrix_Stack : Metrix_Vectors.Vector;
      --  Metrix_Stack[0] is the Metrix for the Compilation_Unit node.
      --
      --  Metrix_Stack[1] is the Metrix for the library item within that; this
      --  is a Package_Decl, Package_Body, or whatever node.
      --
      --  The rest are Metrix for the nested nodes that are "eligible" for
      --  computing metrics. These nodes are [generic] package specs, single
      --  task/protected declarations, task/protected type declarations, and
      --  proper bodies other than entry bodies.
      --
      --  This stack contains the relevant nodes currently being processed by
      --  the recursive walk.

      Node_Metrix : Metrix_Vectors.Vector;
      --  This contains all the computed metrix (not just the ones currently
      --  being processed, as Metrix_Stack does).

      procedure Rec (Node : Ada_Node);
      --  Recursive tree walk. Rec and Gather_Metrics_And_Walk_Children are
      --  mutually recursive.

      procedure Gather_Metrics_And_Walk_Children (Node : Ada_Node);

      procedure Print;
      --  Print out the per-file metrics

      procedure Rec (Node : Ada_Node) is
      begin
         if Debug_Flag_V then
            Put ("-->Walk: \1\n", Short_Image (Node));
            Indent;
         end if;

         Append (Node_Stack, Node); -- push

         if Node = Lib_Item or else Kind (Node) in Eligible then
            declare
               M : constant Metrix_Ref :=
                 new Metrix'(Node => Node,
                             Nesting => Last_Index (Metrix_Stack),
                             Vals => (others => 0));
            begin
               Append (Metrix_Stack, M); -- push
               Append (Node_Metrix, M);
               Gather_Metrics_And_Walk_Children (Node);
               Pop (Metrix_Stack);
            end;
         else
            Gather_Metrics_And_Walk_Children (Node);
         end if;

         Pop (Node_Stack);

         if Debug_Flag_V then
            Outdent;
            Put ("<--Walk: \1\n", Short_Image (Node));
         end if;
      end Rec;

      procedure Gather_Metrics_And_Walk_Children (Node : Ada_Node) is
      begin
         declare
            M : Metrix renames
              Get (Metrix_Stack, Last_Index (Metrix_Stack)).all;
            pragma Unreferenced (M);
         begin
            if Node.all in Statement_Type'Class then
               for X of Metrix_Stack loop
                  Inc (X.Vals (Statements));
               end loop;
            end if;
         end;

         for I in 1 .. Child_Count (Node) loop
            declare
               Cur_Child : constant Ada_Node := Child (Node, I - 1);
            begin
--             pragma Assert (Cur_Child /= null); -- ????
               if Cur_Child /= null then
                  Rec (Cur_Child);
               end if;
            end;
         end loop;
      end Gather_Metrics_And_Walk_Children;

      procedure Print is
         --  ????Compute correct directory.
         Suffix : constant String :=
           (if Arg (Cmd, Output_Suffix) = null
              then ".metrix"
              else Arg (Cmd, Output_Suffix).all);
         use Text_IO;
         Text : File_Type;
         Text_Name : constant String := File_Name & Suffix;
      begin
         if Text_Name = File_Name then
            --  Otherwise, we could overwrite the input!
            raise Program_Error with "empty suffix";
         end if;
         Create (Text, Name => Text_Name);
         Set_Output (Text);

         for I in 1 .. Length (Node_Metrix) loop
            Print_Metrix (File_Name, Metrics_To_Compute, Node_Metrix, I - 1);
         end loop;

         Set_Output (Standard_Output);
         Close (Text);

         if False then -- ????????????????
            for I in 1 .. Length (Node_Metrix) loop
               XML_Print_Metrix
                 (File_Name, Metrics_To_Compute, Node_Metrix, I - 1);
            end loop;
         end if;
      end Print;

      M : constant Metrix_Ref :=
        new Metrix'(Node => CU_Node, Nesting => 0, Vals => (others => 0));

   --  Start of processing for Walk

   begin
      if Debug_Flag_V then
         Put ("-->Walk: \1\n", Short_Image (CU_Node));
         Indent;
      end if;

      Append (Node_Stack, CU_Node); -- push
      Append (Metrix_Stack, M); -- push
      Append (Node_Metrix, M);

      Gather_Metrics_And_Walk_Children (CU_Node);

      Pop (Metrix_Stack);
      Pop (Node_Stack);
      pragma Assert (Length (Metrix_Stack) = 0);
      pragma Assert (Length (Node_Stack) = 0);

      Print;

      if Debug_Flag_V then
         Outdent;
         Put ("<--Walk: \1\n", Short_Image (CU_Node));
      end if;

      --  ????????????????Free all the Node_Metrix.
      Destroy (Metrix_Stack);
      Destroy (Node_Stack);
   end Walk;

   procedure Per_File_Action
     (Cmd : Command_Line; File_Name : String; Unit : Analysis_Unit) is

      function To_Compute return Metrics_Set;
      --  Computes which metrics we should compute

      function To_Compute return Metrics_Set is
      begin
         return Result : Metrics_Set do
            --  Set Result components True for all metrics requested on the
            --  command line:

            for Metric in Metrics_Enum loop
               Result (Metric) := Arg (Cmd, Metric);
            end loop;

            if Arg (Cmd, Contract_All) then
               Result (Contract_Metrics) := (others => True);
            end if;
            if Arg (Cmd, Complexity_All) then
               Result (Complexity_Metrics) := (others => True);
            end if;
            if Arg (Cmd, Lines_All) then
               Result (Lines_Metrics) := (others => True);
            end if;
            if Arg (Cmd, Syntax_All) then
               Result (Syntax_Metrics) := (others => True);
            end if;
            if Arg (Cmd, Coupling_All) then
               Result (Coupling_Metrics) := (others => True);
            end if;

            --  If no metrics were requested on the command line, we compute
            --  all metrics:

            if Result = (Metrics_Enum => False) then
               Result := (Metrics_Enum => True);
            end if;
         end return;
      end To_Compute;

      Metrics_To_Compute : constant Metrics_Set := To_Compute;

   begin
      if Debug_Flag_V then
         Print (Unit);
      end if;

      Walk (Cmd, File_Name, Root (Unit), Metrics_To_Compute);

      if Metrics_To_Compute (Contract_Metrics) /=
        (Contract_Metrics => False)
      then
         Compute_Contract_Metrics (File_Name, Unit); -- ????????????????
      end if;
   end Per_File_Action;

end METRICS.Actions;
