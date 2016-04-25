with Ada.Strings.Unbounded; use Ada;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Tokens; use Langkit_Support;

with Libadalang;     use Libadalang;
with Libadalang.AST.Types; use Libadalang.AST.Types;
with LAL_Extensions; use LAL_Extensions;

with LAL_UL.Common; use LAL_UL; use LAL_UL.Common;
with LAL_UL.Formatted_Output; use LAL_UL.Formatted_Output;
with LAL_UL.String_Utilities; use LAL_UL.String_Utilities;
with LAL_UL.Tool_Names;

with ASIS_UL.Debug; use ASIS_UL.Debug;

pragma Warnings (Off);
with LAL_UL.Projects;
with LAL_UL.Drivers;
pragma Warnings (On);

package body METRICS.Actions is

   Output_To_Standard_Output : Boolean renames Debug_Flag_S;

   Unit : Analysis_Unit;
   --  Make this a global variable for now. Not sure if it should be passed
   --  as parameters all over.????

   function Full_Name (Nm : Name) return Text_Type is (Full_Name (Nm, Unit));

   function Image (X : Integer) return String
     renames String_Utilities.Image;

   pragma Warnings (Off);
   procedure Stop (Node : Ada_Node);
   --  For setting breakpoints in gdb

   procedure Stop (Node : Ada_Node) is
      P : constant Ada_Node_Array_Access := Parents (Node);
   begin
      if True then
         Put ("Node:\n");
         Print (Node);
         if False then
            for X in P.Items'Range loop
               Put ("Parent \1:\n", Image (X));
               Print (P.Items (X));
            end loop;
         end if;
      end if;
   end Stop;

   procedure knd (X : Ada_Node);
   procedure pp (X : Ada_Node);
   --  Debugging printouts

   procedure knd (X : Ada_Node) is
   begin
      Put ("\1\n", Kind (X)'Img);
   end knd;

   procedure pp (X : Ada_Node) is
   begin
      Print (X);
   end pp;
   pragma Warnings (On);

   pragma Warnings (Off); -- ???
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   use Metrics_Flag_Switches, Metrics_Boolean_Switches,
     Metrics_String_Switches, Metrics_String_Seq_Switches;
   pragma Warnings (On);

   procedure Walk
     (Tool : in out Metrics_Tool'Class;
      Cmd : Command_Line;
      File_Name : String;
      CU_Node : Ada_Node;
      Metrics_To_Compute : Metrics_Set);

   use Ada_Node_Vectors;

   procedure Inc (X : in out Metric_Int; By : Metric_Int := 1);
   procedure Dec (X : in out Metric_Int; By : Metric_Int := 1);

   procedure Inc (X : in out Metric_Int; By : Metric_Int := 1) is
   begin
      X := X + By;
   end Inc;

   procedure Dec (X : in out Metric_Int; By : Metric_Int := 1) is
   begin
      X := X - By;
   end Dec;

   subtype Gnatmetric_Eligible is Ada_Node_Type_Kind with
     Predicate => Gnatmetric_Eligible in
       Expression_Function_Kind |
       Generic_Package_Decl_Kind |
       Package_Body_Kind |
       Package_Decl_Kind |
       Protected_Body_Kind |
       Protected_Decl_Kind |
       Protected_Type_Decl_Kind |
       Entry_Body_Kind |
       Subprogram_Body_Kind |
       Task_Body_Kind |
       Task_Decl_Kind |
       Task_Type_Decl_Kind;
   --  These are the node kinds that the gnatmetric documentation calls
   --  "eligible local units". We compute metrics for the outermost node (the
   --  compilation unit), as well as eligible local units. A procedure
   --  declaration, for example, has metrics only if it is the outer node.

   subtype Contract_Complexity_Eligible is Ada_Node_Type_Kind with
     Predicate => Contract_Complexity_Eligible in
       Generic_Subprogram_Decl_Kind |
       Abstract_Subprogram_Decl_Kind |
       Null_Subprogram_Decl_Kind |
       Renaming_Subprogram_Decl_Kind |
       Subprogram_Decl_Kind;
   --  For the new lalmetric tool, we have the --contract-complexity
   --  metric, which is on subprogram declarations, so we need
   --  additional "eligible" nodes.

   subtype Eligible is Ada_Node_Type_Kind with
     Predicate => Eligible in Gnatmetric_Eligible |
       Contract_Complexity_Eligible;

   function Q (S : String) return String is -- quote
     ("""" & S & """");

   type Assertion_Enum is (Postcondition, Other_Assertion, Not_An_Assertion);

   function Assertion_Kind (Node : Ada_Node) return Assertion_Enum;
   --  Return what kind of assertion it it, if any. Postconditions have their
   --  own metric, so we have to split that out.

   function Is_Assertion (Node : Ada_Node) return Boolean is
     (case Assertion_Kind (Node) is
        when Postcondition | Other_Assertion => True,
        when Not_An_Assertion => False);
   --  True for pragma Assert and for Pre, Post, Contract_Cases,
   --  Pre'Class, and Post'Class.

   function Is_Private_Part (Parent, Child : Ada_Node) return Boolean;
   --  True if Parent is a package decl, generic package decl, task decl, or
   --  protected decl, and Child is its private part.

   function Has_Complexity_Metrics
     (Node : Ada_Node; Lib_Item : Boolean) return Boolean;
   --  True if complexity metrix should be computed for Node (assuming it's
   --  requested on the command line).

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

   function XML (X : Text_Type) return String;
   --  Returns X, converted to UTF8, and with replacements like "&" -->
   --  "&amp", and quoted.

   function XML_Metric_Name_String (Metric : Metrics_Enum) return String;
   --  Name of the metric for printing in XML

   function Metric_Name_String (Metric : Metrics_Enum) return String;
   --  Name of the metric for printing in text

   function Should_Print
     (Metric : Metrics_Enum;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural;
      XML : Boolean) return Boolean;
   --  Return True if the given Metric should be printed, based on
   --  Metrics_To_Compute, the node associated with M and other
   --  special cases.

   function Should_Print_Any
     (First, Last : Metrics_Enum;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural;
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

   procedure Print_Range
     (Name : String;
      Metrics_To_Compute : Metrics_Set;
      First, Last : Metrics_Enum;
      M : Metrix;
      Depth : Natural;
      Global : Boolean := False);
   --  Prints a range of metrics. This is needed because the metrics are
   --  printed in groups (line metrics, contract metrics, etc).  Name is the
   --  name of the group, e.g. "=== Lines metrics ===". Prints the name
   --  followed by metrics First..Last. Global is True when printing the
   --  global metrics for all files.

   procedure Print_Metrix
     (File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural);
   --  Print the metrics for one node in text form

   procedure XML_Print_Metrix_Vals
     (Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Do_Complexity_Metrics : Boolean;
      Depth : Natural);
   --  Do_Complexity_Metrics is true if we should print complexity
   --  metrics. This is so we can put average complexity metrics for the
   --  file at the end.

   procedure XML_Print_Metrix
     (File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural);
   --  Print the metrics for one node in XML form

   function Assertion_Kind (Node : Ada_Node) return Assertion_Enum is
      Contract_Cases : constant Wide_Wide_String := "contract_cases";
      Pre : constant Wide_Wide_String := "pre";
      Post : constant Wide_Wide_String := "post";
      Class : constant Wide_Wide_String := "class";
   begin
      case Kind (Node) is
         when Pragma_Node_Kind =>
            declare
               Pragma_Name : constant Text_Type :=
                 L_Name (F_Id (Pragma_Node (Node)), Unit);
            begin
               if Pragma_Name = "assert" then
                  return Other_Assertion;
               end if;
            end;

         when Aspect_Assoc_Kind =>
            declare
               Id : constant Expr := F_Id (Aspect_Assoc (Node));
            begin
               case Kind (Id) is
                  when Identifier_Kind =>
                     declare
                        Text : constant Text_Type := L_Name (Id, Unit);
                     begin
                        if Text = Pre then
                           return Other_Assertion;
                        elsif Text = Contract_Cases
                          or else Text = Post
                        then
                           return Postcondition;
                        end if;
                     end;

                  when Attribute_Ref_Kind =>
                     declare
                        Prefix : constant Text_Type :=
                          L_Name (F_Prefix (Attribute_Ref (Id)), Unit);
                        Attr : constant Text_Type :=
                          L_Name (F_Attribute (Attribute_Ref (Id)), Unit);
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

   function Is_Private_Part (Parent, Child : Ada_Node) return Boolean is
      --  We don't collect metrics on entries????
      --  What about private child units?
   begin
      if Child.all in List_Ada_Node_Type'Class then
         declare
            pragma Warnings (Off);
            C : constant List_Ada_Node := List_Ada_Node (Child);
         begin
            case Kind (Parent) is
--               when Package_Decl_Kind | Base_Package_Decl_Kind =>
--                  return C = F_Private_Part (Base_Package_Decl (Parent));
--               when Task_Def_Kind =>
--                  return C = F_Private_Items (Task_Def (Parent));
--               when Protected_Def =>
--                  return C = F_Private_Components (Protected_Def (Parent));
               when others => null;
            end case;
         end;
      end if;

      return False;
   end Is_Private_Part;

   function Has_Complexity_Metrics
     (Node : Ada_Node; Lib_Item : Boolean) return Boolean is
   begin
      case Kind (Node) is
         when Expression_Function_Kind |
           Entry_Body_Kind |
           Subprogram_Body_Kind |
           Task_Body_Kind =>
            return True;

         when Package_Body_Kind =>
            --  Apparently, gnatmetric doesn't do nested package bodies if
            --  there are no statements.
            return Lib_Item or else F_Statements (Package_Body (Node)) /= null;

         when others =>
            return False;
      end case;
   end Has_Complexity_Metrics;

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
            return "protected object";
         when Protected_Type_Decl_Kind =>
            return "protected type";
         when Entry_Body_Kind =>
            return "entry body";
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
            return "task object";
         when Task_Type_Decl_Kind =>
            return "task type";

         when Generic_Function_Instantiation_Kind =>
            return "function instantiation";
         when Generic_Package_Instantiation_Kind =>
            return "package instantiation";
         when Generic_Procedure_Instantiation_Kind =>
            return "procedure instantiation";
         when Generic_Renaming_Decl_Kind =>
            return "generic package renaming"; -- ????Or proc/func
         when Generic_Subprogram_Decl_Kind =>
            declare
               R : constant Type_Expression :=
                 F_Returns (F_Subp_Spec (Generic_Subprogram_Decl (Node)));
               --  ????R is null here even for functions
            begin
               return "generic " &
                 (if R = null then "procedure" else "function");
            end;
         when Package_Renaming_Decl_Kind =>
            return "package renaming";
         when Abstract_Subprogram_Decl_Kind |
             Null_Subprogram_Decl_Kind |
             Renaming_Subprogram_Decl_Kind |
             Subprogram_Decl_Kind =>
            declare
               R : constant Type_Expression :=
                 F_Returns (F_Subp_Spec (Basic_Subprogram_Decl (Node)));
            begin
               return (if R = null then "procedure" else "function");
            end;

         when Expression_Function_Kind =>
            return "expression function";

         when others => raise Program_Error;
      end case;
   end Node_Kind_String;

   function Is_Private (Node : Ada_Node) return Boolean;
   --  True if Node is a private library unit. ????This doesn't work for
   --  bodies; we need semantic information.

   function Is_Private (Node : Ada_Node) return Boolean is
      P : constant Ada_Node := Parents (Node).Items (1);
   begin
      return Kind (P) = Library_Item_Kind
        and then F_Is_Private (Library_Item (P));
   end Is_Private;

   function Node_Kind_String_For_Header (Node : Ada_Node) return String is
      ((if Is_Private (Node) then "private " else "") &
        Replace_String
         (Node_Kind_String (Node), From => "instantiation", To => "instance"));
   --  Prepend "private " if appropriate.
   --  Also, gnatmetric says "containing package instance" at the top, but
   --  uses "instantiation" elsewhere.

   function Metric_Name_String (Metric : Metrics_Enum) return String is
   begin
      --  In many, but not all, cases the name of the metric in the text output
      --  is the same as the name in the XML, replacing "_" with " ".

      case Metric is
         when Statements =>
            return "all statements";
         when Loop_Nesting =>
            return "maximum loop nesting";
         when All_Subprograms =>
            return "all subprogram bodies";
         --  ????More 'when's go here.

         when others =>
            return Replace_String
              (XML_Metric_Name_String (Metric), From => "_", To => " ");
      end case;
   end Metric_Name_String;

   function Should_Print
     (Metric : Metrics_Enum;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural;
      XML : Boolean) return Boolean
   is
   begin
      if not Metrics_To_Compute (Metric) then
         return False;
      end if;

      --  Don't print metrics for nested subprogram declarations
      --  unless it's Contract_Complexity, which is the only metric
      --  that applies to those.

      if Depth > 2
        and then Kind (M.Node) in Contract_Complexity_Eligible
        and then Metric /= Contract_Complexity
      then
         return False;
      end if;

      case Metric is
         when Lines_Metrics =>
            return True;

         when All_Subprograms =>
            return (Depth = 2
              and then Kind (M.Node) in
                      Package_Body_Kind |
                      Subprogram_Body_Kind)
              or else (XML and then Depth = 0);
         when Public_Subprograms =>
            return (Depth = 2
              and then Kind (M.Node) in
                      Package_Decl_Kind |
                      Generic_Package_Decl_Kind |
                      Subprogram_Decl_Kind |
                      Subprogram_Body_Kind |
                      --  Only if no spec????
                      Generic_Subprogram_Decl_Kind)
              or else (XML and then Depth = 0);
         when Declarations |
           Statements |
           Public_Types |
           All_Types |
           Unit_Nesting |
           Construct_Nesting |
           Param_Number =>
            return M.Node = null
              or else Kind (M.Node) /= Compilation_Unit_Kind;

         when Contract_Complexity =>
            return M.Node /= null
              and then Kind (M.Node) in Contract_Complexity_Eligible
              and then M.Visible;
         when Contract | Post | Contract_Complete =>
            return M.Visible;

         when Complexity_Metrics =>
            if M.Node = null then
               return False;
            end if;

            --  Return True if Has_Complexity_Metrics is True for the node,
            --  or it's a compilation unit containing such a node as its
            --  library item.

            if Kind (M.Node) = Compilation_Unit_Kind then
               declare
                  First_Body : constant Ada_Node :=
                    Childx (F_Bodies (Compilation_Unit (M.Node)), 0);
                  Lib_Item : constant Ada_Node :=
                    (case Kind (First_Body) is
                       when Library_Item_Kind =>
                          Ada_Node (F_Item (Library_Item (First_Body))),
                       when Subunit_Kind =>
                          Ada_Node (F_Body (Subunit (First_Body))),
                       when others => raise Program_Error);
               begin
                  return Has_Complexity_Metrics (Lib_Item, Lib_Item => True)
                    or else Kind (Lib_Item) in
                      Package_Decl_Kind | Generic_Package_Decl_Kind |
                      Protected_Body_Kind;
                  --  Why [generic] pkg and protected????
               end;
            else
               return Has_Complexity_Metrics (M.Node, Lib_Item => False);
            end if;

         when Coupling_Metrics =>
            return True;
      end case;
   end Should_Print;

   function Val_To_Print
     (Metric : Metrics_Enum; M : Metrix; XML : Boolean) return String is
      type Fixed is delta 0.01 digits 8;
   begin
      if Metric in Complexity_Metrics
        and then Kind (M.Node) = Compilation_Unit_Kind
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

            pragma Assert (M.Num_With_Complexity > 0);
            Adjust : constant Metric_Int :=
              (if Metric in Complexity_Statement |
                            Complexity_Cyclomatic |
                            Complexity_Essential
                 then M.Num_With_Complexity - 1
                 else 0);
            Av : constant Float :=
              Float (M.Vals (Metric) + Adjust) / Float (M.Num_With_Complexity);
            Img : constant String := Fixed (Av)'Img;
         begin
            pragma Assert (Img'First = 1 and then Img (1) = ' ');
            return (if XML then "" else " ") & Img (2 .. Img'Last);
         end;
      else
         return Image (M.Vals (Metric));
      end if;
   end Val_To_Print;

   Average_Complexity_Metrics : constant String :=
     "=== Average complexity metrics ===";
   --  Used to indicate we should indent an extra level, and that we should
   --  put underscores in the metric name.

   procedure Print_Range
     (Name : String;
      Metrics_To_Compute : Metrics_Set;
      First, Last : Metrics_Enum;
      M : Metrix;
      Depth : Natural;
      Global : Boolean := False)
   is
      Indentation_Amount : constant Natural :=
        (if Global
           then 2
         elsif Name = Average_Complexity_Metrics
           then 2 * Default_Indentation_Amount
         else Default_Indentation_Amount);
   begin
      if Should_Print_Any
        (First, Last, Metrics_To_Compute, M, Depth, XML => False)
      then
         if not Global then
            Put ("\n");
         end if;

         Put ("\1\n", Name);
         Indent (Indentation_Amount);

         for I in First .. Last loop
            if Should_Print
              (I, Metrics_To_Compute, M, Depth, XML => False)
            then
               if True or else M.Vals (I) /= 0 then -- ???
                  declare
                     Metric_Name : constant String :=
                       (if Name = Average_Complexity_Metrics
                          then XML_Metric_Name_String (I)
                          else Metric_Name_String (I));
                  begin
                     Put ("\1", Metric_Name);
                     Tab_To_Column (Indentation + (if Global then 21 else 26));
                     Put (": \1\n", Val_To_Print (I, M, XML => False));
                  end;
               end if;
            end if;
         end loop;

         Outdent (Indentation_Amount);
      end if;
   end Print_Range;

   procedure Print_Metrix
     (File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural)
   is
   begin
      --  Return immediately if M is for a Contract_Complexity_Eligible
      --  node, and we're not going to print.

      if Depth > 2
        and then Kind (M.Node) in Contract_Complexity_Eligible
        and then not Should_Print
          (Contract_Complexity, Metrics_To_Compute, M, Depth, XML => True)
      then
         return;
      end if;

      if Depth > 2 then
         Indent;
      end if;

      if Kind (M.Node) = Compilation_Unit_Kind then
         declare
            pragma Assert (Length (M.Submetrix) = 1);
            Lib_Item : constant Ada_Node := Get (M.Submetrix, 0).Node;
            --  Note this isn't what libadalang calls
            --  Library_Item; this is the package body or whatever node.
            --  ????????????????Lib_Item could be a subunit.
            First_Body : constant Ada_Node :=
              Childx (F_Bodies (Compilation_Unit (M.Node)), 0);
            Subunit_Parent : constant String :=
              (if Kind (First_Body) = Subunit_Kind
                 then "subunit " &
                   To_UTF8 (Full_Name (F_Name (Subunit (First_Body)))) &
                   "."
                 else "");
         begin
            Put ("Metrics computed for \1\n",
                 File_Name_To_Print (Cmd, File_Name));
            Put ("containing \1 \2\3\n",
                 Node_Kind_String_For_Header (Lib_Item),
                 Subunit_Parent,
                 To_UTF8 (Full_Name (Get_Def_Name (Lib_Item))));
         end;

      else
         declare
            P : constant Ada_Node := Parents (M.Node).Items (1);
            LI_Sub : constant String :=
              (if Depth = 2
                 then (if Kind (P) = Subunit_Kind
                         then " - subunit"
                         else " - library item")
                 else "");
         begin
            Put ("\n\1 (\2\3 at lines  \4)\n",
                 To_UTF8 (Full_Name (Get_Def_Name (M.Node))),
                 Node_Kind_String (M.Node), LI_Sub,
                 Lines_String (Sloc_Range (M.Node)));
         end;
      end if;

      --  Print metrix for this unit

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

      if Kind (M.Node) /= Compilation_Unit_Kind then
         Print_Range
           ("=== Complexity metrics ===",
            Metrics_To_Compute,
            Complexity_Metrics'First, Complexity_Metrics'Last, M, Depth);
      end if;

      --  Then recursively print metrix of nested units

      for Child of M.Submetrix loop
         Print_Metrix (File_Name, Metrics_To_Compute, Child.all, Depth + 1);
      end loop;

      --  At the file level, average complexity metrics come last:

      if Kind (M.Node) = Compilation_Unit_Kind
        and then M.Num_With_Complexity > 0
      then
         Print_Range
           (Average_Complexity_Metrics,
            Metrics_To_Compute,
            Complexity_Metrics'First, Complexity_Metrics'Last, M, Depth);
      end if;

      if Depth > 2 then
         Outdent;
      end if;
   end Print_Metrix;

   function XML (X : Text_Type) return String is
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
            when others => Append (Result, (To_UTF8 ((1 => C))));
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
            return "all_dcls";
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

   procedure XML_Print_Metrix_Vals
     (Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Do_Complexity_Metrics : Boolean;
      Depth : Natural)
   is
   begin
      Indent;

      for I in M.Vals'Range loop
         if Should_Print (I, Metrics_To_Compute, M, Depth, XML => True) then
            if Do_Complexity_Metrics or else I not in Complexity_Metrics then
               if True or else M.Vals (I) /= 0 then -- ???
                  Put ("<metric name=\1>\2</metric>\n",
                       Q (XML_Metric_Name_String (I)),
                       Val_To_Print (I, M, XML => True));
               end if;
            end if;
         end if;
      end loop;

      Outdent;
   end XML_Print_Metrix_Vals;

   procedure XML_Print_Metrix
     (File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural)
   is
   begin
      --  Return immediately if M is for a Contract_Complexity_Eligible
      --  node, and we're not going to print.

      if Depth > 2
        and then Kind (M.Node) in Contract_Complexity_Eligible
        and then not Should_Print
          (Contract_Complexity, Metrics_To_Compute, M, Depth, XML => True)
      then
         return;
      end if;

      Indent;

      if Kind (M.Node) = Compilation_Unit_Kind then
         Put ("<file name=\1>\n", Q (File_Name_To_Print (Cmd, File_Name)));
      else
         declare
            Sloc : constant Tokens.Source_Location_Range :=
              Sloc_Range (M.Node);
         begin
            Put ("<unit name=\1 kind=\2 line=\3 col=\4>\n",
                 XML (Full_Name (Get_Def_Name (M.Node))),
                 Q (Node_Kind_String (M.Node)),
                 Q (Image (Integer (Sloc.Start_Line))),
                 Q (Image (Integer (Sloc.Start_Column))));
         end;
      end if;

      --  Print metrics for this unit

      XML_Print_Metrix_Vals
        (Metrics_To_Compute, M,
         Do_Complexity_Metrics => Kind (M.Node) /= Compilation_Unit_Kind,
         Depth => Depth);

      --  Then recursively print metrix of nested units

      for Child of M.Submetrix loop
         XML_Print_Metrix
           (File_Name, Metrics_To_Compute, Child.all, Depth + 1);
      end loop;

      --  At the file level, average complexity metrics go at the end:

      if Kind (M.Node) = Compilation_Unit_Kind then
         XML_Print_Metrix_Vals
           (Metrics_To_Compute, M,
            Do_Complexity_Metrics => M.Num_With_Complexity > 0,
            Depth => Depth);
         Put ("</file>\n");
      else
         Put ("</unit>\n");
      end if;

      Outdent;
   end XML_Print_Metrix;

   procedure Walk
     (Tool : in out Metrics_Tool'Class;
      Cmd : Command_Line;
      File_Name : String;
      CU_Node : Ada_Node;
      Metrics_To_Compute : Metrics_Set)
   is
      pragma Assert (CU_Node /= null);
      pragma Assert (Kind (CU_Node) = Compilation_Unit_Kind);
--    pragma Assert (Child_Count (F_Bodies (Compilation_Unit (CU_Node))) = 1);

      Metrix_Stack : Metrix_Vectors.Vector renames Tool.Metrix_Stack;
      --  Why don't we use Fast_Vectors????

      procedure Inc_All (Metric : Metrics_Enum; By : Metric_Int := 1);
      --  Increment all values on the stack for a given Metric

      procedure Inc_All (Metric : Metrics_Enum; By : Metric_Int := 1) is
      begin
         for M of Metrix_Stack loop
            Inc (M.Vals (Metric), By);
         end loop;
      end Inc_All;

      First_Body : constant Ada_Node :=
        Childx (F_Bodies (Compilation_Unit (CU_Node)), 0);
      Lib_Item : constant Ada_Node :=
        (case Kind (First_Body) is
           when Library_Item_Kind =>
              Ada_Node (F_Item (Library_Item (First_Body))),
           when Subunit_Kind =>
              Ada_Node (F_Body (Subunit (First_Body))),
           when others => raise Program_Error);
      --  ????????????????Could be subunit

      Node_Stack : Ada_Node_Vectors.Vector;
      --  Stack of all nodes currently being walked

      function Ancestor_Node (N : Natural) return Ada_Node;
      --  Returns the N'th ancestor of the current node. Ancestor (0) is the
      --  current node, Ancestor (1) is the parent of the current node,
      --  Ancestor (2) is the grandparent of the current node, and so on.

      function Ancestor_Node (N : Natural) return Ada_Node is
      begin
         pragma Assert (Last_Index (Node_Stack) >= N);
         return Get (Node_Stack, Last_Index (Node_Stack) - N);
      end Ancestor_Node;

      function Parent_Node return Ada_Node is (Ancestor_Node (1));
      pragma Unreferenced (Parent_Node); -- ????

      Exception_Handler_Count : Natural := 0;
      --  Number of exception handlers we are nested within. Used to
      --  suppress computing cyclomatic complexity within handlers.

      Quantified_Expr_Count : Natural := 0;
      --  Number of quantified expressions we are nested within. This is
      --  used to get around the fact that For_Loop_Spec is used for both
      --  for loops and quantified expressions. For complexity metrics, the
      --  former increments the statement metric, and the latter the
      --  expression metric.

      Expression_Function_Count : Natural := 0;
      --  Apparently, gnatmetric doesn't walk expression functions for
      --  complexity metrics.

      Loop_Count : Natural := 0;
      --  Number of loop statements we are nested within, within the
      --  current program unit. So if a loop contains a block, which
      --  contains a body, the outer loop doesn't count within that
      --  body. Used to compute the maximum loop nesting level.

      --  Maybe count all kinds, via an array indexed by kind????

      Private_Part_Count : Natural := 0;
      --  Number of private parts we are nested within. Used for contract
      --  metrics, which are only supposed to be shown for visible subprograms.

      function In_Visible_Part_1 return Boolean is
         (Last_Index (Metrix_Stack) >= 2
            and then Kind (Get (Metrix_Stack, 2).Node) in
              Package_Decl_Kind | Generic_Package_Decl_Kind
            and then
              not Is_Private (Get (Metrix_Stack, 2).Node)
            and then Private_Part_Count = 0);

      function In_Visible_Part_2 return Boolean; -- Get rid????????????????
      function In_Visible_Part_2 return Boolean is
         A : constant Boolean := Last_Index (Metrix_Stack) >= 2;
         N2 : constant Ada_Node := Get (Metrix_Stack, 2).Node;
         B : constant Boolean :=
           A and then Kind (N2) in
              Package_Decl_Kind | Generic_Package_Decl_Kind;
         D : constant Boolean := B and then
           not Is_Private (N2);
         E : constant Boolean := D and then Private_Part_Count = 0;
      begin
         return E;
      end In_Visible_Part_2;

      function In_Visible_Part return Boolean;
      function In_Visible_Part return Boolean is
      begin
         pragma Assert (In_Visible_Part_1 = In_Visible_Part_2);
         return In_Visible_Part_1;
      end In_Visible_Part;

      In_Assertion : Boolean := False;
      --  True if we are nested within an assertion (pragma Assert,
      --  or a pre/post/etc aspect). 'gnatmetric' skips such constructs.
      --  However, we need to process those, but only for the
      --  --contract-complexity metric.

      procedure Rec (Node : Ada_Node);
      --  Recursive tree walk. Rec and Gather_Metrics_And_Walk_Children are
      --  mutually recursive.

      procedure Cyclomate (Node : Ada_Node; M : in out Metrix);
      --  Compute McCabe Cyclomatic Complexity metrics. This also handles the
      --  Contract_Complexity metric, even though that's considered a "contract
      --  metric".

      procedure Gather_Contract_Metrics (Node : Ada_Node);
      --  Compute contract metrics, except for Contract_Complexity, which is
      --  handled by Cyclomate.

      procedure Gather_Syntax_Metrics (Node : Ada_Node; M : in out Metrix);
      --  Compute syntax element metrics

      procedure Gather_Metrics_And_Walk_Children (Node : Ada_Node);

      procedure Print;
      --  Print out the metrics for this file, and for all units
      --  within it.

      procedure Rec (Node : Ada_Node) is
      begin
         if Debug_Flag_V then
            Put ("-->Walk: \1\n", Short_Image (Node));
            Indent;
         end if;

         case Kind (Node) is
            when Exception_Handler_Kind =>
               Inc (Exception_Handler_Count);
            when Quantified_Expr_Kind =>
               Inc (Quantified_Expr_Count);
            when Expression_Function_Kind =>
               Inc (Expression_Function_Count);
            when Loop_Statement_Kind =>
               Inc (Loop_Count);
            when Private_Part_Kind =>
               Inc (Private_Part_Count);
            when others => null;
         end case;

         if Is_Assertion (Node) then
            pragma Assert (not In_Assertion);
            In_Assertion := True;
         end if;

         Append (Node_Stack, Node); -- push

         if Node = Lib_Item or else
           (Kind (Node) in Eligible
              and then not -- ????This part is a workaround for prot-body-stubs
                (Kind (Node) = Protected_Body_Kind
                   and then F_Body_Stub (Protected_Body (Node)) /= null))
         then
            declare
               File_M : Metrix renames Get (Metrix_Stack, 1).all;
               Parent : Metrix renames
                 Get (Metrix_Stack, Last_Index (Metrix_Stack)).all;
               M : constant Metrix_Ref :=
                 new Metrix'(Node, others => <>);
               Saved_Loop_Count : constant Natural := Loop_Count;
            begin
               Loop_Count := 0;
               Append (Metrix_Stack, M); -- push
               M.Visible := In_Visible_Part; -- must be after push
               Append (Parent.Submetrix, M);
               if Has_Complexity_Metrics (Node, Lib_Item => False) then
                  Inc (File_M.Num_With_Complexity);
               end if;
               Gather_Metrics_And_Walk_Children (Node);
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
            when Exception_Handler_Kind =>
               Dec (Exception_Handler_Count);
            when Quantified_Expr_Kind =>
               Dec (Quantified_Expr_Count);
            when Expression_Function_Kind =>
               Dec (Expression_Function_Count);
            when Loop_Statement_Kind =>
               Dec (Loop_Count);
            when Private_Part_Kind =>
               Dec (Private_Part_Count);
            when others => null;
         end case;

         if Debug_Flag_V then
            Outdent;
            Put ("<--Walk: \1\n", Short_Image (Node));
         end if;
      end Rec;

      procedure Cyclomate (Node : Ada_Node; M : in out Metrix) is
         File_M : Metrix renames Get (Metrix_Stack, 1).all;
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

         procedure Inc_Cyc (Metric : Metrics_Enum; By : Metric_Int := 1) with
           Pre => Metric in Complexity_Statement | Complexity_Expression;
         --  Increment the specified complexity metric, and also
         --  Complexity_Cyclomatic. Increment the current unit's metrics, as
         --  well as the file-level ones. However, if this is something like a
         --  subprogram declaration, we actually increment the
         --  Contract_Complexity.

         procedure Inc_Cyc (Metric : Metrics_Enum; By : Metric_Int := 1) is
         begin
            if Debug_Flag_V then
               Put ("Inc_Cyc\1 for \2 in \3\n",
                    (if By = 1 then "" else "(" & Image (By) & ")"),
                    Short_Image (Node), Short_Image (M.Node));
            end if;

            if Kind (M.Node) in Contract_Complexity_Eligible then
               Inc (M.Vals (Contract_Complexity), By);
               Inc (File_M.Vals (Contract_Complexity), By);
            elsif not In_Assertion then
               Inc (M.Vals (Metric), By);
               Inc (M.Vals (Complexity_Cyclomatic), By);
               Inc (File_M.Vals (Metric), By);
               Inc (File_M.Vals (Complexity_Cyclomatic), By);
            end if;
         end Inc_Cyc;

      --  Start of processing for Cyclomate

      begin
         --  Don't compute these metrics within exception handlers.
         --  Apparently, gnatmetric doesn't walk expression functions for
         --  complexity metrics.

         if Exception_Handler_Count > 0
           or else (False and then Expression_Function_Count > 0)
         then
            return;
         end if;

         case Kind (Node) is
            when If_Statement_Kind |
              Elsif_Statement_Part_Kind |
              While_Loop_Spec_Kind =>
               Inc_Cyc (Complexity_Statement);

            when For_Loop_Spec_Kind =>
               if Quantified_Expr_Count = 0 then
                  --  We want to increment the statement count only for real
                  --  for loops.
                  Inc_Cyc (Complexity_Statement);
                  --  ????except in some cases (see No_Static_Loop)
               end if;

            when Case_Statement_Kind =>
               Inc_Cyc (Complexity_Statement,
                 By => Child_Count (F_Case_Alts (Case_Statement (Node))) - 1);

            when Exit_Statement_Kind =>
               if F_Condition (Exit_Statement (Node)) /= null then
                  Inc_Cyc (Complexity_Statement);
               end if;

            when Select_Statement_Kind =>
               declare
                  S : constant Select_Statement := Select_Statement (Node);
                  Num_Alts : constant Metric_Int := Child_Count (F_Guards (S));
                  Num_Else : constant Metric_Int :=
                    (if F_Else_Statements (S) = null then 0 else 1);
                  Num_Abort : constant Metric_Int :=
                    (if F_Abort_Statements (S) = null then 0 else 1);
               begin
                  Inc_Cyc (Complexity_Statement,
                           By => Num_Alts + Num_Else + Num_Abort - 1);
               end;

            when Expression_Function_Kind =>
               null; -- It's already set to 1

            when Bin_Op_Kind =>
               if F_Op (Bin_Op (Node)) in Or_Else | And_Then then
                  Inc_Cyc (Complexity_Expression);
               end if;

            when If_Expr_Kind | Elsif_Expr_Part_Kind =>
               Inc_Cyc (Complexity_Expression);

            when Case_Expr_Kind =>
               Inc_Cyc (Complexity_Expression,
                        By => Child_Count (F_Cases (Case_Expr (Node))) - 1);

            when Quantified_Expr_Kind =>
               Inc_Cyc (Complexity_Expression, By => 2);

            when Loop_Statement_Kind =>
               --  Compute M.Vals (Loop_Nesting) as the maximum loop
               --  nesting level for this unit. We only set it for the
               --  innermost unit and at the file level.

               if not In_Assertion then
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

      procedure Gather_Contract_Metrics (Node : Ada_Node) is
         Vis_Decls : constant List_Ada_Node := Visible_Part (Node);

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
            Aspects : constant Aspect_Specification :=
              Get_Aspects (Basic_Decl (Subp_Decl));
         begin
            if Kind (Subp_Decl) = Expression_Function_Kind then
               --  Expression functions are considered to have
               --  contracts and postconditions.
               Has_Contracts := True;
               Has_Post := True;

            elsif Aspects /= null then
               Has_Contracts := False;
               Has_Post := False;

               declare
                  Assocs : constant List_Aspect_Assoc :=
                    F_Aspect_Assocs (Aspects);
               begin
                  for I in 0 .. Child_Count (Assocs) - 1 loop
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
         if Vis_Decls /= null then -- Shouldn't it be empty list???
            for I in 0 .. Child_Count (Vis_Decls) - 1 loop
               declare
                  Decl : constant Ada_Node := Childx (Vis_Decls, I);
                  Has_Contracts, Has_Post : Boolean;
               begin
                  if Decl.all in Basic_Subprogram_Decl_Type'Class then
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
      begin
         if Kind (Node) = Subprogram_Body_Kind then
            Inc_All (All_Subprograms);
         end if;

         if Last_Index (Metrix_Stack) = 2 then
            if Node = M.Node and then
              Kind (M.Node) in Subprogram_Decl_Kind |
                Generic_Subprogram_Decl_Kind |
                Renaming_Subprogram_Decl_Kind |
                Subprogram_Body_Kind
            then
               Inc_All (Public_Subprograms);
            end if;

            if Kind (Node) in
              Package_Decl_Kind | Generic_Package_Decl_Kind
            then
               declare
                  Vis_Decls : constant List_Ada_Node := Visible_Part (Node);
               begin
                  if Vis_Decls /= null then
                     for I in 0 .. Child_Count (Vis_Decls) - 1 loop
                        declare
                           Decl : constant Ada_Node := Childx (Vis_Decls, I);
                        begin
                           if Decl.all in Basic_Subprogram_Decl_Type'Class then
                              Inc_All (Public_Subprograms);
                           end if;
                        end;
                     end loop;
                  end if;
               end;
            end if;
         end if;
      end Gather_Syntax_Metrics;

      procedure Gather_Metrics_And_Walk_Children (Node : Ada_Node) is
         function Num_Statements
           (Node : access Ada_Node_Type'Class)
           return Natural;
         function Num_Statements
           (Node : access Ada_Node_Type'Class)
           return Natural is
         begin
            if Node = null then
               --  ????Probably these things should be empty lists, not
               --  'null'.
               return 0;
            end if;
            return Result : Natural := 0 do
               for Stm of Children (Node) loop
                  if Stm /= null
                    and then Kind (Stm) not in
                      Pragma_Node_Kind | Label_Kind | Terminate_Statement_Kind
                  then
                     Inc (Result);
                  end if;
               end loop;
            end return;
         end Num_Statements;

         M : Metrix renames
           Get (Metrix_Stack, Last_Index (Metrix_Stack)).all;

      --  Start of processing for Gather_Metrics_And_Walk_Children

      begin
         if Parents (Node).Items'Length >= 2 then
            pragma Assert
              (Parents (Node).Items (2) =
                 Get (Node_Stack, Last_Index (Node_Stack) - 2));
         end if;
         if not In_Assertion then
            if False and then -- ????????????????
              (Node.all in Statement_Type'Class
              or else
                (Kind (Node) in Identifier_Kind |
                     Prefix_Kind |
                     Call_Expr_Kind |
                     Attribute_Ref_Kind
                   and then Kind
                     (Get (Node_Stack, Last_Index (Node_Stack) - 2))
                       in Handled_Statements_Kind |
                          Case_Statement_Alternative_Kind |
                          Elsif_Statement_Part_Kind |
                          Ext_Return_Statement_Kind |
                          Accept_Statement_Kind |
                          If_Statement_Kind |
                          Loop_Statement_Kind |
                          Select_Statement_Kind))
            then
               if Debug_Flag_W then
                  Put ("Statement: \1\n", Short_Image (Node));
               end if;
               Inc_All (Statements);
            end if;

            case Kind (Node) is
               when Handled_Statements_Kind =>
                  Inc_All (Statements,
                       By => Num_Statements
                         (F_Statements
                            (Handled_Statements (Node))));
               when Exception_Handler_Kind =>
                  Inc_All (Statements,
                       By => Num_Statements
                         (F_Statements
                            (Exception_Handler (Node))));
               when Case_Statement_Alternative_Kind =>
                  Inc_All (Statements,
                       By => Num_Statements
                         (F_Statements
                            (Case_Statement_Alternative (Node))));
               when If_Statement_Kind =>
                  Inc_All (Statements,
                       By =>
                         Num_Statements
                           (F_Statements (If_Statement (Node))) +
                         Num_Statements
                           (F_Else_Statements (If_Statement (Node))));
               when Elsif_Statement_Part_Kind =>
                  Inc_All (Statements,
                       By => Num_Statements
                         (F_Statements
                            (Elsif_Statement_Part (Node))));
               when Ext_Return_Statement_Kind =>
                  if False then -- Currently uses Handled_Statements????
                     Inc_All (Statements,
                          By => Num_Statements
                            (F_Statements
                               (Ext_Return_Statement (Node))));
                  end if;
               when Accept_Statement_Kind =>
                  if False then -- Currently uses Handled_Statements????
                     Inc_All (Statements,
                          By => Num_Statements
                            (F_Statements
                               (Accept_Statement (Node))));
                  end if;
               when Loop_Statement_Kind =>
                  Inc_All (Statements,
                       By => Num_Statements
                         (F_Statements
                            (Loop_Statement (Node))));
               when Select_Statement_Kind =>
                  Inc_All (Statements,
                       By =>
                         Num_Statements
                           (F_Else_Statements (Select_Statement (Node))) +
                         Num_Statements
                           (F_Abort_Statements (Select_Statement (Node))));
               when Select_When_Part_Kind =>
                  Inc_All (Statements,
                       By => Num_Statements
                         (F_Statements
                            (Select_When_Part (Node))));
               when others => null;
            end case;

            Gather_Syntax_Metrics (Node, M);

            if Kind (Node) in
                Package_Decl_Kind | Generic_Package_Decl_Kind |
                Task_Def_Kind | Protected_Def_Kind
              and then In_Visible_Part
            then
               --  We only gather contract metrics for public subprograms

               Gather_Contract_Metrics (Node);
            end if;
         end if;

         if Has_Complexity_Metrics (M.Node, Lib_Item => False)
           or else (Kind (M.Node) in Contract_Complexity_Eligible
                      and then In_Visible_Part)
         then
            Cyclomate (Node, M);
         end if;

         for I in 1 .. Child_Count (Node) loop
            declare
               Cur_Child : constant Ada_Node := Child (Node, I - 1);
            begin
               if Cur_Child /= null then
                  --  We don't have a separate node kind for the private part,
                  --  so we have to check whether the child is the private part
                  --  here.
                  --  ????Actually, we do for packages. Probably tasks and
                  --  protecteds should be changed to work the same way.

                  if False then
                     declare
                        P : constant Boolean :=
                          Is_Private_Part (Node, Cur_Child);
                     begin
                        if P then
                           Private_Part_Count := Private_Part_Count + 1;
                        end if;

                        Rec (Cur_Child);

                        if P then
                           Private_Part_Count := Private_Part_Count - 1;
                        end if;
                     end;
                  else
                     Rec (Cur_Child);
                  end if;
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
         File_M : Metrix renames Get (Metrix_Stack, 1).all;
      begin
         if Text_Name = File_Name then
            --  Otherwise, we could overwrite the input!
            raise Program_Error with "empty suffix";
         end if;

         if Gen_Text (Cmd) then
            if not Output_To_Standard_Output then
               Create (Text, Name => Text_Name);
               Set_Output (Text);
            end if;

            Print_Metrix
              (File_Name, Metrics_To_Compute, File_M,
               Depth => 1);

            if not Output_To_Standard_Output then
               Set_Output (Standard_Output);
               Close (Text);
            end if;
         end if;

         if Gen_XML (Cmd) then
            if not Output_To_Standard_Output then
               Set_Output (Tool.XML);
            end if;
            XML_Print_Metrix
              (File_Name, Metrics_To_Compute, File_M, Depth => 1);
            if not Output_To_Standard_Output then
               Set_Output (Standard_Output);
            end if;
         end if;
      end Print;

      M : constant Metrix_Ref := new Metrix'(CU_Node, others => <>);

   --  Start of processing for Walk

   begin
      if Debug_Flag_V then
         Put ("-->Walk: \1\n", Short_Image (CU_Node));
         Indent;
      end if;

      pragma Assert (Length (Metrix_Stack) = 1);
      Append (Node_Stack, CU_Node); -- push
      Append (Metrix_Stack, M); -- push

      Gather_Metrics_And_Walk_Children (CU_Node);

      Pop (Metrix_Stack);
      Pop (Node_Stack);
      pragma Assert (Length (Metrix_Stack) = 1);
      pragma Assert (Length (Node_Stack) = 0);
      pragma Assert (M.Vals (Complexity_Cyclomatic) =
                       M.Vals (Complexity_Statement) +
                       M.Vals (Complexity_Expression));

      pragma Assert (Debug_Flag_V or else Indentation = 0);
      Print;
      pragma Assert (Debug_Flag_V or else Indentation = 0);

      if Debug_Flag_V then
         Outdent;
         Put ("<--Walk: \1\n", Short_Image (CU_Node));
      end if;

      --  ????????????????Free all the Node_Metrix.
      Destroy (Node_Stack);
   end Walk;

   ----------
   -- Init --
   ----------

   procedure Init (Tool : in out Metrics_Tool; Cmd : Command_Line) is

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

            --  Special cases

            if Arg (Cmd, Complexity_Cyclomatic) then
               Result (Complexity_Statement) := True;
               Result (Complexity_Expression) := True;
            end if;

            --  If no metrics were requested on the command line, we compute
            --  all metrics except coupling metrics. Also, at least for now,
            --  disable contract metrics, because those don't exist in
            --  gnatmetric and we're trying to be compatible.

            if Result = (Metrics_Enum => False) then
               Result := (Coupling_Metrics | Contract_Metrics => False,
                          others => True);
            end if;
         end return;
      end To_Compute;

      Metrics_To_Compute : Metrics_Set renames Tool.Metrics_To_Compute;

      Metrix_Stack : Metrix_Vectors.Vector renames Tool.Metrix_Stack;

      Xml_Name : constant String :=
        (if Arg (Cmd, Xml_File_Name) = null
           then "metrix.xml"
           else Arg (Cmd, Xml_File_Name).all);

      M : constant Metrix_Ref := new Metrix;

   --  Start of processing for Init

   begin
      --  Decide what metrics to compute. Initialize the Metrix_Stack
      --  by pushing the outermost Metrix, which is for totals for all
      --  the files together. If XML requested, create the XML file
      --  and put the first lines.

      Metrics_To_Compute := To_Compute;
      Append (Metrix_Stack, M); -- push

      if Gen_XML (Cmd) then
         if not Output_To_Standard_Output then
            Text_IO.Create (Tool.XML, Name => Xml_Name);
            Text_IO.Set_Output (Tool.XML);
         end if;
         Put ("<?xml version=\1?>\n", Q ("1.0"));
         Put ("<global>\n");
         if not Output_To_Standard_Output then
            Text_IO.Set_Output (Text_IO.Standard_Output);
         end if;
      end if;
   end Init;

   -----------
   -- Final --
   -----------

   procedure Final (Tool : in out Metrics_Tool; Cmd : Command_Line) is
      Metrics_To_Compute : Metrics_Set renames Tool.Metrics_To_Compute;
      Metrix_Stack : Metrix_Vectors.Vector renames Tool.Metrix_Stack;
      Summed : constant String :=
        "summed over " & Image (Num_File_Names (Cmd)) & " units";
      pragma Assert (Length (Metrix_Stack) = 1);
      M : Metrix renames Get (Metrix_Stack, 0).all;
   begin
      pragma Assert (M.Vals (Complexity_Cyclomatic) =
                       M.Vals (Complexity_Statement) +
                       M.Vals (Complexity_Expression));

      --  Print the totals in text form

      if Gen_Text (Cmd) then
         Print_Range
           ("Line metrics " & Summed,
            Metrics_To_Compute,
            Lines_Metrics'First, Lines_Metrics'Last, M,
            Depth => 0,
            Global => True);
         Print_Range
           ("Contract metrics " & Summed,
            Metrics_To_Compute,
            Contract_Metrics'First, Contract_Metrics'Last, M,
            Depth => 0,
            Global => True);
         Print_Range
           ("Element metrics " & Summed,
            Metrics_To_Compute,
            Syntax_Metrics'First, Syntax_Metrics'Last, M,
            Depth => 0,
            Global => True);
      end if;

      --  Print the totals in XML form

      if Gen_XML (Cmd) then
         if not Output_To_Standard_Output then
            Text_IO.Set_Output (Tool.XML);
         end if;
         XML_Print_Metrix_Vals
           (Metrics_To_Compute, M, Do_Complexity_Metrics => False, Depth => 0);
         Put ("</global>\n");
         if not Output_To_Standard_Output then
            Text_IO.Set_Output (Text_IO.Standard_Output);
            Text_IO.Close (Tool.XML);
         end if;
      end if;

      Pop (Metrix_Stack);
      Destroy (Metrix_Stack);
   end Final;

   ---------------------
   -- Per_File_Action --
   ---------------------

   procedure Per_File_Action
     (Tool : in out Metrics_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Unit : Analysis_Unit) is

      Metrics_To_Compute : Metrics_Set renames Tool.Metrics_To_Compute;

   --  Start of processing for Per_File_Action

   begin
      if Debug_Flag_V then
         Print (Unit);
      end if;

      Actions.Unit := Unit;
      Walk (Tool, Cmd, File_Name, Root (Unit), Metrics_To_Compute);
   end Per_File_Action;

   ---------------
   -- Tool_Help --
   ---------------

   procedure Tool_Help (Tool : Metrics_Tool) is
      pragma Unreferenced (Tool);
   begin
      pragma Style_Checks ("M200"); -- Allow long lines

      Put ("usage: gnatmetric [options] {filename} {-files filename} [-cargs gcc_switches]\n");
      Put (" options:\n");
      Put (" --version - Display version and exit\n");
      Put (" --help    - Display usage and exit\n");
      Put ("\n");
      Put (" -Pproject        - Use project file project. Only one such switch can be used\n");
      Put (" -U               - process all sources of the argument project\n");
      Put (" -U main          - process the closure of units rooted at unit main\n");
      Put (" -Xname=value     - specify an external reference for argument project file\n");
      Put (" --subdirs=dir    - specify subdirectory to place the result files into\n");
      Put (" --no_objects_dir - place results into current dir instead of project dir\n");
      Put (" -eL              - follow all symbolic links when processing project files\n");

      Put ("\n");
      Put (" -a    - process sources from RTL\n");
      if False then -- Disable this for now
         Put (" --incremental -- incremental processing on a per-file basis\n");
      end if;
      Put (" -jn   - n is the maximal number of processes\n");
      Put (" -v    - verbose mode\n");
      Put (" -q    - quiet mode\n");
      Put ("\n");

      Put (" -nolocal - do not compute detailed metrics for local program units\n");
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
      Put ("  -ne                     - do not consider exit statements as gotos when\n");
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
      Put ("\n");

      Put (" Syntax element metrics:\n");
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

      Put (" Coupling metrics. By default they are disabled, options below enable all or\n");
      Put (" specific coupling metrics, there is no  option to disable coupling metrics\n");
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

      Put (" output file control:\n");
      Put ("  -d=dirname     - put files with detailed metrics into 'dirname'\n");
      Put ("  -x             - generate XML output\n");
      Put ("  -xs            - generate XML output and corresponding schema file\n");
      Put ("  -nt            - do not generate output in text form, implies '-x'\n");
      Put ("  -o file-suffix - suffix for the file to put detailed metrics for\n");
      Put ("                   a source file into (file suffix should follow OS\n");
      Put ("                   file name conventions and contain '.' or '$' character)\n");
      Put ("  -og filename   - name of the file to put global metrics info into\n");
      Put ("                   (if not set, this info is sent to Stdout),\n");
      Put ("                   ignored if -nt is used\n");
      Put ("  -ox filename   - name of the file to put XML output into, implies '-x'\n");
      Put ("  -sfn           - use short source file name in output\n");
      Put ("\n");
      Put (" filename        - name of Ada source file for which metrics\n");
      Put ("                   should be computed (wildcards are allowed)\n");
      Put (" -files filename - name of the text file containing a list of Ada\n");
      Put ("                   source files for which metrics should be computed\n");

      Put (" gcc_switches    - switches to be passed to gcc called by \1\n", Tool_Names.Tool_Name);

      pragma Style_Checks ("M79");
   end Tool_Help;

end METRICS.Actions;
