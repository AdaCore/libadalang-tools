with Text_IO;

with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada;
with Ada.Characters.Handling;
with Interfaces; use type Interfaces.Unsigned_16;
with Unchecked_Deallocation;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang;     use Libadalang;
with Libadalang.Lexer;
with LAL_Extensions; use LAL_Extensions;

with LAL_UL.Common; use LAL_UL; use LAL_UL.Common;
with LAL_UL.Dbg_Out;
with LAL_UL.Formatted_Output;
with LAL_UL.String_Utilities; use LAL_UL.String_Utilities;
with LAL_UL.Tool_Names;

with ASIS_UL.Debug; use ASIS_UL.Debug;
with ASIS_UL.Vectors;

pragma Warnings (Off);
with LAL_UL.Projects;
with LAL_UL.Drivers;
pragma Warnings (On);

package body METRICS.Actions is

   Output_To_Standard_Output : Boolean renames Debug_Flag_S;

   function Parent (Node : Ada_Node) return Ada_Node is
     (Parents (Node).Items (2));

   function Image (X : Integer) return String
     renames String_Utilities.Image;

   pragma Warnings (Off);
   procedure Stop (Node : Ada_Node; S : Wide_Wide_String);
   --  For setting breakpoints in gdb

   procedure Stop (Node : Ada_Node; S : Wide_Wide_String) is
      P : constant Ada_Node_Array_Access := Parents (Node);
      use LAL_UL.Dbg_Out;
   begin
      if False then
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
   procedure Put_Child_Record (C : Child_Record);
   procedure Put_Children_Array (A : Children_Arrays.Array_Type);
   --  Debugging printouts

   procedure knd (X : Ada_Node) is
      use LAL_UL.Dbg_Out;
   begin
      Put ("\1\n", Kind (X)'Img);
   end knd;

   procedure pp (X : Ada_Node) is
      use LAL_UL.Dbg_Out;
   begin
      LAL_UL.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", (if X = null then "null" else Short_Image (X)));
--      Print (X);
   end pp;
   pragma Warnings (On);

   procedure Put_Child_Record (C : Child_Record) is
      use LAL_UL.Dbg_Out;
   begin
      case C.Kind is
         when Child =>
            Put ("Child: \1\n", Short_Image (C.Node));
         when Trivia =>
            Put ("Trivia: \1 ""\2"" \3\n",
                 C.Trivia.Kind'Img,
                 To_UTF8 (C.Trivia.Text.all),
                 Slocs.Image (C.Trivia.Sloc_Range));
      end case;
   end Put_Child_Record;

   procedure Put_Children_Array (A : Children_Arrays.Array_Type) is
      use LAL_UL.Dbg_Out;
   begin
      for I in A'Range loop
         Put ("\1: ", Image (I));
         Put_Child_Record (A (I));
      end loop;
   end Put_Children_Array;

   pragma Warnings (Off); -- ???
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   use Metrics_Flag_Switches, Metrics_Boolean_Switches,
     Metrics_String_Switches, Metrics_String_Seq_Switches;
   pragma Warnings (On);

   use LAL_UL.Formatted_Output;

   Empty_Sym : constant Symbol := Intern ("");
   Subunit_Sym : constant Symbol := Intern (" - subunit");
   Library_Item_Sym : constant Symbol := Intern (" - library item");

   Empty_CU_Sym : constant CU_Symbol := Intern ("");

   type Metrix_Ref_Array is
     array (CU_Symbol_Index range <>) of aliased Metrix_Ref;

   package CU_Metrix is new ASIS_UL.Vectors
     (CU_Symbol_Index, Metrix_Ref, Metrix_Ref_Array);
   use CU_Metrix;
   Specs : CU_Metrix.Vector;
   Bodies : CU_Metrix.Vector;
   --  Mappings from Symbols representing compilation unit full names to the
   --  spec and body of that compilation unit. Used for coupling metrics.

   function Get_CU_Access
     (A : in out CU_Metrix.Vector; S : CU_Symbol)
     return CU_Metrix.Element_Access;
   --  Return a pointer to Specs[S] or Bodies[S]

   function Get_CU_Access
     (A : in out CU_Metrix.Vector; S : CU_Symbol)
     return CU_Metrix.Element_Access
   is
      I : constant CU_Symbol_Index := Get_Symbol_Index (S);
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

   function Output_Dir (Cmd : Command_Line) return String;

   procedure Validate (M : Metrix);
   --  For testing/debugging. Check consistency of M.

   procedure Destroy (M : in out Metrix_Ref);
   --  Reclaim memory for a tree of Metrix records

   procedure Walk
     (Tool : in out Metrics_Tool'Class;
      File_Name : String;
      CU_List : Ada_Node;
      Cumulative : Cumulative_Counts_Array);

   use Ada_Node_Vectors;

   subtype Gnatmetric_Eligible is Ada_Node_Kind_Type with
     Predicate => Gnatmetric_Eligible in
       Ada_Expression_Function |
       Ada_Generic_Package_Decl |
       Ada_Package_Body |
       Ada_Package_Decl |
       Ada_Protected_Body |
       Ada_Protected_Decl |
       Ada_Protected_Type_Decl |
       Ada_Entry_Body |
       Ada_Subprogram_Body |
       Ada_Task_Body |
       Ada_Task_Decl |
       Ada_Task_Type_Decl;
   --  These are the node kinds that the gnatmetric documentation calls
   --  "eligible local units". We compute metrics for the outermost node (the
   --  compilation unit, for the file-level metrics), and the library item or
   --  subunit, as well as eligible local units. A procedure declaration, for
   --  example, has metrics only if it is the outer node. There is one
   --  exception: see Contract_Complexity below.

   subtype Contract_Complexity_Eligible is Ada_Node_Kind_Type with
     Predicate => Contract_Complexity_Eligible in
       Ada_Generic_Subprogram_Decl |
       Ada_Abstract_Subprogram_Decl |
       Ada_Null_Subprogram_Decl |
       Ada_Renaming_Subprogram_Decl |
       Ada_Subprogram_Decl;
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
      Source_File_Name : String_Ref := null)
     return Metrix_Ref with
     Pre => (Source_File_Name /= null) =
            (Node /= null and then Kind (Node) = Ada_Compilation_Unit);
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
   --  Return what kind of assertion it it, if any. Postconditions have their
   --  own metric, so we have to split that out.

   function Is_Assertion (Node : Ada_Node) return Boolean is
     (case Assertion_Kind (Node) is
        when Postcondition | Other_Assertion => True,
        when Not_An_Assertion => False);
   --  True for pragma Assert and for Pre, Post, Contract_Cases,
   --  Pre'Class, and Post'Class.

   function Has_Complexity_Metrics (Node : Ada_Node) return Boolean;
   --  True if complexity metrix should be computed for Node (assuming it's
   --  requested on the command line).

   function File_Name_To_Print
     (Cmd : Command_Line; File_Name : String) return String is
      (if Arg (Cmd, Short_File_Name_In_Output)
         then File_Name
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

   --  Below, Depth parameters are the nesting depth, starting with 0 for the
   --  global metrics.

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
   --  Lines_Average is also an average.

   procedure Print_Range
     (Name : String;
      Metrics_To_Compute : Metrics_Set;
      First, Last : Metrics_Enum;
      M : Metrix;
      Depth : Natural);
   --  Prints a range of metrics. This is needed because the metrics are
   --  printed in groups (line metrics, contract metrics, etc).  Name is the
   --  name of the group, e.g. "=== Lines metrics ===". Prints the name
   --  followed by metrics First..Last.

   procedure Print_Metrix
     (Cmd : Command_Line;
      File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural);
   --  Print the metrics for one node in text form

   procedure XML_Print_Metrix_Vals
     (Metrics_To_Compute : Metrics_Set;
      First : Metrics_Enum;
      Last : Metrics_Enum;
      M : Metrix;
      Depth : Natural);

   procedure XML_Print_Metrix
     (Cmd : Command_Line;
      File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural);
   --  Print the metrics for one node in XML form

   procedure Print_File_Metrics
     (Cmd : Command_Line;
      XML_File : in out Text_IO.File_Type;
      File_M : Metrix;
      Metrics_To_Compute : Metrics_Set);
   --  Print out the metrics for this file, and for all units
   --  within it.

   function Is_Private (Node : Ada_Node) return Boolean;
   --  True if Node is a private library unit. ???This doesn't work for
   --  bodies; we need semantic information.

   function Push_New_Metrix
     (Tool : in out Metrics_Tool'Class;
      Node : Ada_Node;
      Source_File_Name : String_Ref := null)
     return Metrix_Ref
   is
      K : constant Ada_Node_Kind_Type :=
        (if Node = null then Null_Kind else Kind (Node));

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
              Ada_Protected_Decl |
              Ada_Protected_Type_Decl |
              Ada_Task_Decl |
              Ada_Task_Type_Decl |
              Ada_Generic_Function_Instantiation |
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Procedure_Instantiation |
              Ada_Generic_Renaming_Decl |
              Ada_Package_Renaming_Decl |
              Ada_Abstract_Subprogram_Decl |
              Ada_Null_Subprogram_Decl |
              Ada_Renaming_Subprogram_Decl =>
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

            when Ada_Subprogram_Body =>
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

            when Ada_Subprogram_Decl | Ada_Generic_Subprogram_Decl =>
               if S /= null then
                  pragma Assert (B = null);
                  B := S;
                  B.Is_Spec := False;
               end if;
               S := M;
               M.Is_Spec := True;

            when others =>
               raise Program_Error with Short_Image (M.Node);
         end case;
      end Set_CU_Metrix;

   --  Start of processing for Push_New_Metrix

   begin
      return Result : constant Metrix_Ref := new Metrix (Kind => K) do
         Append (Tool.Metrix_Stack, Result); -- push

         Result.Node := Node;
         Result.Knd := Get_Fine_Kind (Node);
         Result.Sloc := (if Node = null
                           then Slocs.No_Source_Location_Range
                           else Sloc_Range (Node));
         Result.Is_Private_Lib_Unit := Is_Private (Node);
         Result.Has_Complexity_Metrics := Has_Complexity_Metrics (Node);

         if Node /= null then
            declare
               function Get_Name (N : Ada_Node) return String is
                 (To_UTF8 (Full_Name (Get_Def_Name (N))));
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
                          then Intern
                            (To_UTF8
                              (Full_Name
                                (F_Name
                                  (Subunit
                                    (F_Body (Compilation_Unit (Node)))))))
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
                       else Empty_Sym);
               end if;
            end;
         end if;

         if Source_File_Name /= null then
            Result.Source_File_Name := Source_File_Name;
         end if;

         if Node /= null
           and then Kind (Node) = Ada_Package_Body
           and then F_Statements (Package_Body (Node)) /= null
         then
            Result.Statements_Sloc :=
              Sloc_Range
                (Ada_Node
                   (F_Statements
                      (F_Statements (Package_Body (Node)))));
         end if;
      end return;
   end Push_New_Metrix;

   function Get_Outer_Unit (Node : Ada_Node) return Ada_Node is
      Lib_Item_Or_Subunit : constant Ada_Node :=
        F_Body (Compilation_Unit (Node));
   begin
      return
        (case Kind (Lib_Item_Or_Subunit) is
           when Ada_Library_Item =>
              Ada_Node (F_Item (Library_Item (Lib_Item_Or_Subunit))),
           when Ada_Subunit =>
              Ada_Node (F_Body (Subunit (Lib_Item_Or_Subunit))),
           when others => raise Program_Error);
   end Get_Outer_Unit;

   function Assertion_Kind (Node : Ada_Node) return Assertion_Enum is
      Contract_Cases : constant Wide_Wide_String := "contract_cases";
      Pre : constant Wide_Wide_String := "pre";
      Post : constant Wide_Wide_String := "post";
      Class : constant Wide_Wide_String := "class";
   begin
      case Kind (Node) is
         when Ada_Pragma_Node =>
            declare
               Pragma_Name : constant Text_Type :=
                 L_Name (F_Id (Pragma_Node (Node)));
            begin
               if Pragma_Name = "assert" then
                  return Other_Assertion;
               end if;
            end;

         when Ada_Aspect_Assoc =>
            declare
               Id : constant Expr := F_Id (Aspect_Assoc (Node));
            begin
               case Kind (Id) is
                  when Ada_Identifier =>
                     declare
                        Text : constant Text_Type := L_Name (Id);
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
                        Prefix : constant Text_Type :=
                          L_Name (F_Prefix (Attribute_Ref (Id)));
                        Attr : constant Text_Type :=
                          L_Name (F_Attribute (Attribute_Ref (Id)));
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

   function Has_Complexity_Metrics (Node : Ada_Node) return Boolean is
   begin
      if Node = null then
         return False;
      end if;

      case Kind (Node) is
         when Ada_Compilation_Unit =>
            --  At the file level, we print complexity metrics if the outermost
            --  unit has complexity metrics, or if it's a package decl, etc.

            declare
               Outer_Unit : constant Ada_Node := Get_Outer_Unit (Node);
            begin
               return Has_Complexity_Metrics (Outer_Unit)
                       or else Kind (Outer_Unit) in
                         Ada_Package_Decl | Ada_Generic_Package_Decl |
                         Ada_Package_Body | Ada_Protected_Body;
            end;

         when Ada_Expression_Function |
           Ada_Entry_Body |
           Ada_Subprogram_Body |
           Ada_Task_Body =>
            return True;

         when Ada_Package_Body =>
            --  Apparently, gnatmetric doesn't do nested package bodies if
            --  there are no statements.
            return F_Statements (Package_Body (Node)) /= null;

         when others =>
            return False;
      end case;
   end Has_Complexity_Metrics;

   function Get_Fine_Kind (Node : Ada_Node) return Fine_Kind is
   begin
      if Node = null then
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
         when Ada_Protected_Decl =>
            return Protected_Object_Knd;
         when Ada_Protected_Type_Decl =>
            return Protected_Type_Knd;
         when Ada_Entry_Body =>
            return Entry_Body_Knd;
         when Ada_Subprogram_Body =>
            declare
               R : constant Type_Expression :=
                 F_Returns (F_Subp_Spec (Subprogram_Body (Node)));
            begin
               return
                 (if R = null then Procedure_Body_Knd else Function_Body_Knd);
            end;
         when Ada_Task_Body =>
            return Task_Body_Knd;
         when Ada_Task_Decl =>
            return Task_Object_Knd;
         when Ada_Task_Type_Decl =>
            return Task_Type_Knd;

         when Ada_Generic_Function_Instantiation =>
            return Function_Instantiation_Knd;
         when Ada_Generic_Package_Instantiation =>
            return Package_Instantiation_Knd;
         when Ada_Generic_Procedure_Instantiation =>
            return Procedure_Instantiation_Knd;
         when Ada_Generic_Renaming_Decl =>
            return Generic_Package_Renaming_Knd; -- ???Or proc/func
         when Ada_Generic_Subprogram_Decl =>
            declare
               R : constant Type_Expression :=
                 F_Returns (F_Subp_Spec (Generic_Subprogram_Decl (Node)));
               --  ???R is null here even for functions
            begin
               return
                 (if R = null
                    then Generic_Procedure_Knd
                    else Generic_Function_Knd);
            end;
         when Ada_Package_Renaming_Decl =>
            return Package_Renaming_Knd;
         when Ada_Abstract_Subprogram_Decl |
             Ada_Null_Subprogram_Decl |
             Ada_Renaming_Subprogram_Decl |
             Ada_Subprogram_Decl =>
            declare
               R : constant Type_Expression :=
                 F_Returns (F_Subp_Spec (Basic_Subprogram_Decl (Node)));
            begin
               return (if R = null then Procedure_Knd else Function_Knd);
            end;

         when Ada_Expression_Function =>
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
      if Node = null or else Kind (Node) = Ada_Compilation_Unit then
         return False;
      end if;

      declare
         P : constant Ada_Node := Parent (Node);
      begin
         return Kind (P) = Ada_Library_Item
           and then F_Is_Private (Library_Item (P));
      end;
   end Is_Private;

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
         when Lines_Eol_Comment =>
            return "end-of-line comments";
         when Lines_Average =>
            return "Average lines in body";

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
      --  Don't print metrics that weren't requested on the command line (or by
      --  default).

      if not Metrics_To_Compute (Metric) then
         return False;
      end if;

      --  Don't print metrics for nested subprogram declarations
      --  unless it's Contract_Complexity, which is the only metric
      --  that applies to those.

      if Depth > 2
        and then M.Kind in Contract_Complexity_Eligible
        and then Metric /= Contract_Complexity
      then
         return False;
      end if;

      case Metric is
         when Lines |
           Lines_Code |
           Lines_Comment |
           Lines_Eol_Comment |
           Lines_Blank |
           Lines_Ratio =>
            return True;
         when Lines_Code_In_Bodies | Num_Bodies =>
            return Depth = 0;
         when Lines_Average =>
            return Depth = 0;

         when All_Subprograms =>
            return (Depth = 2
              and then M.Kind in
                      Ada_Package_Body |
                      Ada_Subprogram_Body)
              or else (XML and then Depth = 0);
         when Public_Subprograms =>
            return (Depth = 2
              and then M.Kind in
                      Ada_Package_Decl |
                      Ada_Generic_Package_Decl |
                      Ada_Subprogram_Decl |
                      Ada_Subprogram_Body |
                      --  Only if no spec???
                      Ada_Generic_Subprogram_Decl)
              or else (XML and then Depth = 0);
         when Declarations |
           Statements |
           Public_Types |
           All_Types |
           Unit_Nesting |
           Construct_Nesting |
           Param_Number =>
            return M.Kind /= Ada_Compilation_Unit;

         when Contract_Complexity =>
            return M.Kind in Contract_Complexity_Eligible and then M.Visible;
         when Contract | Post | Contract_Complete =>
            return M.Visible;

         when Complexity_Metrics =>
            return M.Has_Complexity_Metrics;

         when Coupling_Metrics =>
            return Depth = 2;
      end case;
   end Should_Print;

   function Val_To_Print
     (Metric : Metrics_Enum; M : Metrix; XML : Boolean) return String is
      type Fixed is delta 0.01 digits 8;
   begin
      if (Metric in Complexity_Metrics
            and then M.Kind = Ada_Compilation_Unit)
      or else (Metric = Lines_Average and then M.Kind = Null_Kind)
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
            --  Metric whose value is used as the numerator when
            --  computing the average. For complexity metrics, that
            --  the Metric itself, but for Lines_Average, we need to
            --  divide Lines_Code_In_Bodies by something.
            Av : constant Float :=
              Float (M.Vals (Numerator_Metric) + Adjust) / Float (Num);
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
            Img : constant String := Fixed (Comments / Code * 100.0)'Img;
         begin
            pragma Assert (Img'First = 1 and then Img (1) = ' ');
            return Img (2 .. Img'Last);
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
      Depth : Natural)
   is
   begin
      if Should_Print_Any
        (First, Last, Metrics_To_Compute, M, Depth, XML => False)
      then
         if Depth /= 0 then
            Put ("\n");
         end if;

         Put ("\1\n", Name);

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
                     Indentation_Amount : constant Natural :=
                       (if I = Lines_Average
                          then 0
                        elsif Depth = 0
                          then 2
                        elsif Depth = 1 and then First in Lines_Metrics
                          then 2
                        elsif Name = Average_Complexity_Metrics
                          then 2 * Default_Indentation_Amount
                        else Default_Indentation_Amount);
                     --  Indentation_Amount, and Tab below, are
                     --  intended to mimic some partially arbitrary
                     --  behavior of gnatmetric.
                     Tab : constant Positive :=
                       (if Depth = 0 and then I in Lines_Metrics
                          then 22
                        elsif Depth = 0 or else I in Lines_Metrics
                          then 21
                        else 26);
                  begin
                     Indent (Indentation_Amount);
                     if I = Lines_Average then -- gnatmetric puts extra line
                        Put ("\n");
                     end if;
                     Put ("\1", Metric_Name);
                     Tab_To_Column (Indentation + Tab);
                     Put (": \1\n", Val_To_Print (I, M, XML => False));
                     Outdent (Indentation_Amount);
                  end;
               end if;
            end if;
         end loop;

         if Depth = 0 and then First in Lines_Metrics then
            Put ("\n");
         end if;
      end if;
   end Print_Range;

   procedure Print_Metrix
     (Cmd : Command_Line;
      File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural)
   is
   begin
      Validate (M);
      pragma Assert (M.Node = null);

      --  Return immediately if M is for a Contract_Complexity_Eligible node,
      --  and we're not going to print. Also don't print metrics for "eligible
      --  local program units" if the -nolocal switch was given.

      if Depth > 2 then
         if M.Kind in Contract_Complexity_Eligible
           and then not Should_Print
             (Contract_Complexity, Metrics_To_Compute, M, Depth, XML => True)
         then
            return;
         end if;

         if Arg (Cmd, No_Local_Metrics) then
            return;
         end if;
      end if;

      if Depth > 2 then
         Indent;
      end if;

      if M.Kind = Ada_Compilation_Unit then
         Put ("Metrics computed for \1\n",
              File_Name_To_Print (Cmd, File_Name));
         Put ("containing \1 \2\n",
              Fine_Kind_String_For_Header (Get (M.Submetrix, 0).all),
              Str (M.Text_Name).S);
      else
         Put ("\n\1 (\2\3 at lines  \4)\n",
              Str (M.Text_Name).S,
              Fine_Kind_String (M.Knd), Str (M.LI_Sub).S,
              Lines_String (M.Sloc));
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

      if Depth > 2 then
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
         when Lines_Average =>
            return "average_lines_in_bodies";
         when Lines_Code_In_Bodies =>
            return "lines_in_bodies";
         when Num_Bodies =>
            return "num_bodies";
         when Lines_Ratio =>
            return "comment_percentage";
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
      end case;
   end XML_Metric_Name_String;

   procedure XML_Print_Metrix_Vals
     (Metrics_To_Compute : Metrics_Set;
      First : Metrics_Enum;
      Last : Metrics_Enum;
      M : Metrix;
      Depth : Natural)
   is
   begin
      Indent;

      for I in First .. Last loop
         if Should_Print (I, Metrics_To_Compute, M, Depth, XML => True) then
            if True or else M.Vals (I) /= 0 then -- ???
               Put ("<metric name=\1>\2</metric>\n",
                    Q (XML_Metric_Name_String (I)),
                    Val_To_Print (I, M, XML => True));
            end if;
         end if;
      end loop;

      Outdent;
   end XML_Print_Metrix_Vals;

   procedure XML_Print_Metrix
     (Cmd : Command_Line;
      File_Name : String;
      Metrics_To_Compute : Metrics_Set;
      M : Metrix;
      Depth : Natural)
   is
      pragma Assert (M.Node = null);

      To_Print_First : constant Metrics_Set :=
        Metrics_To_Compute and
          (if M.Kind = Ada_Compilation_Unit
             then not Complexity_Only
             else Metrics_Set'(others => True));
      --  Set of metrics to print first, before printing subtrees. Same as
      --  Metrics_To_Compute, except at the top level, we leave out complexity
      --  metrics, because they will be printed last.

      To_Print_Last : constant Metrics_Set :=
        Metrics_To_Compute and
          (if M.Kind = Ada_Compilation_Unit
               and then M.Num_With_Complexity > 0
             then Complexity_Only
             else Metrics_Set'(others => False));
      --  Set of metrics to print last, after printing subtrees. This is
      --  normally empty. At the top level it is the intersection of
      --  Metrics_To_Compute and Complexity_Only, but only if we have
      --  some with complexity metrics.

      Doing_Coupling_Metrics : constant Boolean :=
        (Metrics_To_Compute and not Coupling_Only) = Empty_Metrics_Set;
   begin
      --  Return immediately if we're doing coupling metrics, and this is not a
      --  compilation unit spec, or if M is for a Contract_Complexity_Eligible
      --  node, and we're not going to print. Also don't print metrics for
      --  "eligible local program units" if the -nolocal switch was given.

      if Depth > 2 then
         if Doing_Coupling_Metrics then
            return;
         end if;

         if M.Kind in Contract_Complexity_Eligible
           and then not Should_Print
             (Contract_Complexity, Metrics_To_Compute, M, Depth, XML => True)
         then
            return;
         end if;

         if Arg (Cmd, No_Local_Metrics) then
            return;
         end if;
      end if;

      --  Coupling metrics are only printed for specs

      if M.Kind = Ada_Compilation_Unit
        and then Doing_Coupling_Metrics
        and then not M.Is_Spec
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
        (To_Print_First, M.Vals'First, M.Vals'Last, M, Depth => Depth);

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
           (To_Print_Last, M.Vals'First, M.Vals'Last, M, Depth => Depth);
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
      Metrics_To_Compute : Metrics_Set)
   is
      Suffix : constant String :=
        (if Arg (Cmd, Output_Suffix) = null
           then ".metrix"
           else Arg (Cmd, Output_Suffix).all);
      use Text_IO;
      Text : File_Type;
      File_Name : String renames File_M.Source_File_Name.all;
      Text_File_Name : constant String :=
        Directories.Compose (Output_Dir (Cmd), File_Name & Suffix);
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
           (Cmd, File_Name, Metrics_To_Compute, File_M, Depth => 1);

         if not Output_To_Standard_Output then
            Set_Output (Standard_Output);
            Close (Text);
         end if;
      end if;

      if Gen_XML (Cmd) then
         if not Output_To_Standard_Output then
            Set_Output (XML_File);
         end if;
         XML_Print_Metrix
           (Cmd, File_Name, Metrics_To_Compute, File_M, Depth => 1);
         if not Output_To_Standard_Output then
            Set_Output (Standard_Output);
         end if;
      end if;
   end Print_File_Metrics;

   function Output_Dir (Cmd : Command_Line) return String is
   begin
      if Arg (Cmd, Output_Directory) = null then
         return "";
      else
         return Arg (Cmd, Output_Directory).all;
      end if;
   end Output_Dir;

   procedure Destroy (M : in out Metrix_Ref) is
      procedure Free is new Unchecked_Deallocation (Metrix, Metrix_Ref);
   begin
      for Index in 0 .. Last_Index (M.Submetrix) loop
         declare
            Sub : Metrix_Ref renames Get_Access (M.Submetrix, Index).all;
         begin
            Destroy (Sub);
         end;
      end loop;
      Destroy (M.Submetrix);

      Free (M);
   end Destroy;

   procedure Walk
     (Tool : in out Metrics_Tool'Class;
      File_Name : String;
      CU_List : Ada_Node;
      Cumulative : Cumulative_Counts_Array)
   is
      pragma Assert (CU_List /= null);
--      pragma Assert (Kind (CU_List) = List_Kind);
--    pragma Assert (Child_Count (CU_List) = 1);
      --  libadalang supports multiple compilation units per file,
      --  but gnatmetric does not, and lalmetric does not yet.

      Metrix_Stack : Metrix_Vectors.Vector renames Tool.Metrix_Stack;
      --  Why don't we use Fast_Vectors???

      procedure Inc_All (Metric : Metrics_Enum; By : Metric_Nat := 1);
      --  Increment all values on the stack for a given Metric

      procedure Inc_All (Metric : Metrics_Enum; By : Metric_Nat := 1) is
      begin
         for M of Metrix_Stack loop
            Inc (M.Vals (Metric), By);
         end loop;
      end Inc_All;

      --  CU_Node : constant Ada_Node := Childx (CU_List, 0);
      CU_Node : constant Ada_Node := CU_List;
      pragma Assert (Kind (CU_Node) = Ada_Compilation_Unit);
      Outer_Unit : constant Ada_Node := Get_Outer_Unit (CU_Node);

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
      pragma Unreferenced (Parent_Node); -- ???

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

      --  Maybe count all kinds, via an array indexed by kind???

      Private_Part_Count : Natural := 0;
      --  Number of private parts we are nested within. Used for contract
      --  metrics, which are only supposed to be shown for visible subprograms.

      Non_Package_Body_Count : Natural := 0;
      --  Number of bodies we are nested within, except package bodies
      --  don't count.

      function In_Visible_Part return Boolean is
         (Last_Index (Metrix_Stack) >= 2
            and then Get (Metrix_Stack, 2).Kind in
              Ada_Package_Decl | Ada_Generic_Package_Decl
            and then not Get (Metrix_Stack, 2).Is_Private_Lib_Unit
            and then Private_Part_Count = 0);
      --  True if we're within only visible parts. Note that it is possible to
      --  be in a visible part that is within a private part; we return False
      --  in that case.

      In_Assertion : Boolean := False;
      --  True if we are nested within an assertion (pragma Assert,
      --  or a pre/post/etc aspect). 'gnatmetric' skips such constructs.
      --  However, we need to process those, but only for the
      --  --contract-complexity metric.

      procedure Cyclomate (Node : Ada_Node; M : in out Metrix);
      --  Compute McCabe Cyclomatic Complexity metrics. This also handles the
      --  Contract_Complexity metric, even though that's considered a "contract
      --  metric".

      procedure Gather_Contract_Metrics (Node : Ada_Node);
      --  Compute contract metrics, except for Contract_Complexity, which is
      --  handled by Cyclomate.

      procedure Gather_Line_Metrics (Node : Ada_Node; M : in out Metrix);
      --  Compute line metrics

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
            when Ada_If_Statement |
              Ada_Elsif_Statement_Part |
              Ada_While_Loop_Spec =>
               Inc_Cyc (Complexity_Statement);

            when Ada_For_Loop_Spec =>
               if Quantified_Expr_Count = 0 then
                  --  We want to increment the statement count only for real
                  --  for loops.
                  Inc_Cyc (Complexity_Statement);
                  --  ???except in some cases (see No_Static_Loop)
               end if;

            when Ada_Case_Statement =>
               Inc_Cyc (Complexity_Statement,
                 By => Child_Count (F_Case_Alts (Case_Statement (Node))) - 1);

            when Ada_Exit_Statement =>
               if F_Condition (Exit_Statement (Node)) /= null then
                  Inc_Cyc (Complexity_Statement);
               end if;

            when Ada_Select_Statement =>
               declare
                  S : constant Select_Statement := Select_Statement (Node);
                  Num_Alts : constant Metric_Nat := Child_Count (F_Guards (S));
                  Num_Else : constant Metric_Nat :=
                    (if F_Else_Statements (S) = null then 0 else 1);
                  Num_Abort : constant Metric_Nat :=
                    (if F_Abort_Statements (S) = null then 0 else 1);
               begin
                  Inc_Cyc (Complexity_Statement,
                           By => Num_Alts + Num_Else + Num_Abort - 1);
               end;

            when Ada_Expression_Function =>
               null; -- It's already set to 1

            when Ada_Bin_Op =>
               if F_Op (Bin_Op (Node)) in Or_Else | And_Then then
                  Inc_Cyc (Complexity_Expression);
               end if;

            when Ada_If_Expr | Ada_Elsif_Expr_Part =>
               Inc_Cyc (Complexity_Expression);

            when Ada_Case_Expr =>
               Inc_Cyc (Complexity_Expression,
                        By => Child_Count (F_Cases (Case_Expr (Node))) - 1);

            when Ada_Quantified_Expr =>
               Inc_Cyc (Complexity_Expression, By => 2);

            when Ada_Loop_Statement =>
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

      procedure Gather_Line_Metrics (Node : Ada_Node; M : in out Metrix) is
         function Should_Gather_Body_Lines return Boolean;
         --  True if we should gather the Lines_Code_In_Bodies for
         --  this node. True for entry, subprogram, and task bodies,
         --  and True for package bodies if there is a statement part.

         function Should_Gather_Body_Lines return Boolean is
         begin
            case Kind (Node) is
               when Ada_Package_Body =>
                  return F_Statements (Package_Body (Node)) /= null;
               when Ada_Entry_Body | Ada_Subprogram_Body | Ada_Task_Body =>
                  return True;
               when others =>
                  return False;
            end case;
         end Should_Gather_Body_Lines;

         Global_M : Metrix renames Get (Metrix_Stack, 0).all;
      begin
         if Node = M.Node then
            declare
               use type Interfaces.Unsigned_32;
               Start : constant Interfaces.Unsigned_32 :=
                 (if Kind (Node) = Ada_Compilation_Unit
                    then 1
                    else M.Sloc.Start_Line);
               --  At the file level, we want to include all the comments and
               --  blank lines preceding the compilation unit. We should also
               --  include trailing lines, but we don't do that yet.

               Lines_Count : constant Metric_Nat :=
                 Metric_Nat (M.Sloc.End_Line - Start + 1);
            begin
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
                          First_Line => Start, Last_Line => M.Sloc.End_Line,
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

                        Inc (M.Vals (Num_Bodies));
                        Inc (Global_M.Vals (Num_Bodies));
                     end if;
                  end;
               end loop;
            end;
         end if;
      end Gather_Line_Metrics;

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
            if Kind (Subp_Decl) = Ada_Expression_Function then
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
         --  --statements

         --  Labels and terminate alternatives are classified as statements by
         --  libadalang, but they are not actually statements, so shouldn't be
         --  counted in the metric.

         if Node.all in Statement_Type'Class
           and then Node.all not in Label_Type'Class
           and then Node.all not in Terminate_Statement_Type'Class
         then
            if Debug_Flag_W then
               Put ("Statement: \1\n", Short_Image (Node));
            end if;
            Inc_All (Statements);
         end if;

         --  --all-subprograms

         if Kind (Node) = Ada_Subprogram_Body then
            Inc_All (All_Subprograms);
         end if;

         --  --public-subprograms

         if Last_Index (Metrix_Stack) = 2 then
            if Node = M.Node and then
              M.Kind in Ada_Subprogram_Decl |
                Ada_Generic_Subprogram_Decl |
                Ada_Renaming_Subprogram_Decl |
                Ada_Subprogram_Body
            then
               Inc_All (Public_Subprograms);
            end if;

            if Kind (Node) in
              Ada_Package_Decl | Ada_Generic_Package_Decl
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

      procedure Gather_Dependencies (Node : Ada_Node) is
         File_M : Metrix renames Get (Metrix_Stack, 1).all;
      begin
         case Kind (Node) is
            --  For "with P, Q;" include P and Q

            when Ada_With_Decl =>
               declare
                  Names : constant List_Name :=
                    F_Packages (With_Decl (Node));
               begin
                  for I in 0 .. Child_Count (Names) - 1 loop
                     if F_Is_Limited (With_Decl (Node)) then
                        Include
                          (File_M.Limited_Depends_On,
                           Intern
                             (To_UTF8 (Full_Name (Name (Childx (Names, I))))));
                     else
                        Include
                          (File_M.Depends_On,
                           Intern
                             (To_UTF8 (Full_Name (Name (Childx (Names, I))))));
                     end if;
                  end loop;
               end;

            --  A child unit depends on its parent

            when Ada_Generic_Function_Instantiation |
              Ada_Generic_Procedure_Instantiation |
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Renaming_Decl |
              Ada_Package_Renaming_Decl |
              Ada_Renaming_Subprogram_Decl |
              Ada_Package_Decl |
              Ada_Generic_Package_Decl |
              Ada_Subprogram_Decl |
              Ada_Subprogram_Body | -- could be acting as spec
              Ada_Generic_Subprogram_Decl =>
               declare
                  Def_Name : constant Name := Get_Def_Name (Node);
               begin
                  if Kind (Def_Name) = Ada_Prefix then
                     Include
                       (File_M.Depends_On,
                        Intern (To_UTF8
                          (Full_Name (Name (F_Prefix (Prefix (Def_Name)))))));
                  end if;
               end;

            when others => null;
         end case;
      end Gather_Dependencies;

      procedure Rec (Node : Ada_Node) is
      begin
         if Debug_Flag_V then
            Put ("-->Walk: \1\n", Short_Image (Node));
            Indent;
         end if;

         case Kind (Node) is
            when Ada_Exception_Handler =>
               Inc (Exception_Handler_Count);
            when Ada_Quantified_Expr =>
               Inc (Quantified_Expr_Count);
            when Ada_Expression_Function =>
               Inc (Expression_Function_Count);
            when Ada_Loop_Statement =>
               Inc (Loop_Count);
            when Ada_Private_Part =>
               Inc (Private_Part_Count);
            when Ada_Entry_Body | Ada_Subprogram_Body | Ada_Task_Body =>
               Inc (Non_Package_Body_Count);
            when others => null;
         end case;

         if Is_Assertion (Node) then
            pragma Assert (not In_Assertion);
            In_Assertion := True;
         end if;

         Append (Node_Stack, Node); -- push

         --  For the library item and eligible local units, we push and pop the
         --  Metrix_Stack around the call to Gather_Metrics_And_Walk_Children;
         --  otherwise we just call Gather_Metrics_And_Walk_Children.

         if Node = Outer_Unit or else Kind (Node) in Eligible then
            declare
               File_M : Metrix renames Get (Metrix_Stack, 1).all;
               Parent : Metrix renames
                 Get (Metrix_Stack, Last_Index (Metrix_Stack)).all;
               M : constant Metrix_Ref := Push_New_Metrix (Tool, Node);
               Saved_Loop_Count : constant Natural := Loop_Count;
            begin
               Loop_Count := 0;
               M.Visible := In_Visible_Part; -- must be after Push_New_Metrix
               Append (Parent.Submetrix, M);
               if M.Has_Complexity_Metrics then
                  Inc (File_M.Num_With_Complexity);
               end if;
               Gather_Metrics_And_Walk_Children (Node);
               Validate (M.all);
               M.Node := null;
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
            when Ada_Expression_Function =>
               Dec (Expression_Function_Count);
            when Ada_Loop_Statement =>
               Dec (Loop_Count);
            when Ada_Private_Part =>
               Dec (Private_Part_Count);
            when Ada_Entry_Body | Ada_Subprogram_Body | Ada_Task_Body =>
               Dec (Non_Package_Body_Count);
            when others => null;
         end case;

         if Debug_Flag_V then
            Outdent;
            Put ("<--Walk: \1\n", Short_Image (Node));
         end if;
      end Rec;

      procedure Gather_Metrics_And_Walk_Children (Node : Ada_Node) is
         M : Metrix renames
           Get (Metrix_Stack, Last_Index (Metrix_Stack)).all;

         With_Trivia : constant Children_Arrays.Array_Type :=
           Children_With_Trivia (Node);

         use type Lexer.Token_Kind;

      --  Start of processing for Gather_Metrics_And_Walk_Children

      begin
         for Trivium of With_Trivia loop
            if False and then Trivium.Kind = Trivia then
               Put_Children_Array (With_Trivia);
               Put ("----\n");
               Stop (Node, Trivium.Trivia.Text.all);
               exit;
            end if;
         end loop;
         for Trivium of With_Trivia loop
            if Trivium.Kind = Trivia then
               pragma Assert (Trivium.Trivia.Kind = Lexer.Ada_Comment);
            end if;
         end loop;

         if Parents (Node).Items'Length > 2 then
            pragma Assert
              (Parents (Node).Items (3) =
                 Get (Node_Stack, Last_Index (Node_Stack) - 2));
         end if;
         if not In_Assertion then
            Gather_Line_Metrics (Node, M);
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
         end if;

         for I in 1 .. Child_Count (Node) loop
            declare
               Cur_Child : constant Ada_Node := Child (Node, I - 1);
            begin
               if Cur_Child /= null then
                  Rec (Cur_Child);
               end if;
            end;
         end loop;
      end Gather_Metrics_And_Walk_Children;

      pragma Assert (Length (Metrix_Stack) = 1);
      Parent : Metrix renames Get (Metrix_Stack, 0).all;
      M : constant Metrix_Ref :=
        Push_New_Metrix
          (Tool, CU_Node, Source_File_Name => new String'(File_Name));

   --  Start of processing for Walk

   begin
      if Debug_Flag_V then
         Put ("-->Walk: \1\n", Short_Image (CU_Node));
         Indent;
      end if;

      Append (Node_Stack, CU_Node); -- push
      Append (Parent.Submetrix, M);

      Gather_Metrics_And_Walk_Children (CU_Node);

      M.Node := null;
      Pop (Metrix_Stack);
      Pop (Node_Stack);
      pragma Assert (Length (Metrix_Stack) = 1);
      pragma Assert (Length (Node_Stack) = 0);
      pragma Assert (M.Vals (Complexity_Cyclomatic) =
                       M.Vals (Complexity_Statement) +
                       M.Vals (Complexity_Expression));

      if Debug_Flag_V then
         Outdent;
         Put ("<--Walk: \1\n", Short_Image (CU_Node));
      end if;

      Destroy (Node_Stack);
   end Walk;

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

      Ignored : constant Metrix_Ref := Push_New_Metrix (Tool, Node => null);

   --  Start of processing for Init

   begin
      --  Decide what metrics to compute. Initialize the Metrix_Stack
      --  by pushing the outermost Metrix, which is for totals for all
      --  the files together. If XML requested, create the XML file
      --  and put the first lines.

      Metrics_To_Compute := To_Compute;

      --  Create output directory if necessary

      if Arg (Cmd, Output_Directory) /= null then
         declare
            Dir : constant String := Arg (Cmd, Output_Directory).all;
            Cannot_Create : constant String :=
              "cannot create directory '" & Dir & "'";
            use Directories;
         begin
            if Exists (Dir) then
               if Kind (Dir) /= Directory then
                  Cmd_Error (Cannot_Create & "; file already exists");
               end if;
            else
               begin
                  Create_Directory (Dir);
               exception
                  when Name_Error | Use_Error =>
                     Cmd_Error (Cannot_Create);
               end;
            end if;
         end;
      end if;
   end Init;

   -----------
   -- Final --
   -----------

   procedure Compute_Indirect_Dependencies (Global_M : Metrix) with
     Pre => Global_M.Kind = Null_Kind;
   --  Depends_On contains direct dependencies. This computes the indirect
   --  dependencies for all compilation units by walking the dependency graph.

   procedure Compute_Coupling
     (Tool : in out Metrics_Tool; Global_M : Metrix);
   --  This uses the dependency information to compute the coupling metrics.

   function Get_Spec (M : Metrix_Ref) return Metrix_Ref with
     Pre => M.Kind = Ada_Compilation_Unit;
   --  If M is a spec, return M. If it's a library unit body, return the
   --  corresponding spec. If it's a subunit, return the spec of the innermost
   --  enclosing library unit.

   procedure XML_Print_Coupling
     (Cmd : Command_Line;
      Metrics_To_Compute : Metrics_Set);
   --  Print the metrics computed by Compute_Coupling

   Metrix_For_Coupling : Metrix_Vectors.Vector;
   --  ???This is intended to mimic the ordering behavior of gnatmetric, for
   --  printing coupling metrics.

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
            if Get_Symbol_Index (Sym) > Last_Index (Specs)
              or else Specs (Get_Symbol_Index (Sym)) = null
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
               Append (Metrix_For_Coupling, M);
            end if;

            for Sym of Copy (M.Depends_On) loop
               --  Copy is necessary, because we are passing M.Depends_On to
               --  the recursive Visit, which is going to modify it, so we
               --  can't be iterating over it directly.

               Visit (Specs (Get_Symbol_Index (Sym)), M.Depends_On);
            end loop;
         end if;

         Include (Depends_On, M.CU_Name);
         Union (Depends_On, M.Depends_On);
      end Visit;

      Ignored : CU_Symbol_Sets.Set;

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
            Spec : constant Metrix_Ref := Specs (Get_Symbol_Index (M.CU_Name));
         begin
            pragma Assert (Spec /= null);
            return (if Spec = null then M else Spec);
         end;
      else
         declare
            Parent_Body : Metrix_Ref :=
              Bodies (Get_Symbol_Index (M.Subunit_Parent));
         begin
            --  The parent could be a body acting as spec, in which case it's
            --  in Specs, not Bodies.

            if Parent_Body = null then
               Parent_Body := Specs (Get_Symbol_Index (M.Subunit_Parent));
            end if;

            pragma Assert (Parent_Body /= null);
            return (if Parent_Body = null then M else Get_Spec (Parent_Body));
            --  This recursion will climb up a chain of nested subunits until
            --  it reaches a library unit, and then we'll get the spec of that
            --  library unit.
         end;
      end if;
   end Get_Spec;

   procedure Compute_Coupling
     (Tool : in out Metrics_Tool; Global_M : Metrix) is
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
               Union (S.Depends_On, B.Depends_On);
               Union (S.Limited_Depends_On, B.Limited_Depends_On);
            end;
         end if;
      end loop;

      Dump (Tool, Global_M, "After bodies:");

      --  Compute metrics for the specs

      for S of Specs loop
         if S /= null then
            declare
               pragma Assert (S.Indirect_Dependences_Computed);
               Outer_Unit : Metrix renames Get (S.Submetrix, 0).all;
               --  Outer_Unit is the outermost package spec, procedure spec,
               --  etc.
            begin
               --  Unit_Coupling_Out counts the number of things this depends
               --  on.

               Inc (Outer_Unit.Vals (Unit_Coupling_Out),
                    By => Integer (Length (S.Depends_On)) +
                          Integer (Length (S.Limited_Depends_On)));

               --  Unit_Coupling_In counts the number of things that depend on
               --  this.

               for Sym of S.Depends_On loop
                  declare
                     Dep : Metrix renames Specs (Get_Symbol_Index (Sym)).all;
                     Dep_Outer_Unit : Metrix renames
                       Get (Dep.Submetrix, 0).all;
                  begin
                     Inc (Dep_Outer_Unit.Vals (Unit_Coupling_In));
                  end;
               end loop;
               for Sym of S.Limited_Depends_On loop
                  declare
                     Dep : Metrix renames Specs (Get_Symbol_Index (Sym)).all;
                     Dep_Outer_Unit : Metrix renames
                       Get (Dep.Submetrix, 0).all;
                  begin
                     Inc (Dep_Outer_Unit.Vals (Unit_Coupling_In));
                  end;
               end loop;
            end;
         end if;
      end loop;
   end Compute_Coupling;

   procedure XML_Print_Coupling
     (Cmd : Command_Line;
      Metrics_To_Compute : Metrics_Set)
   is
      subtype Arr_Index is Natural range 0 .. Last_Index (Metrix_For_Coupling);
      subtype Arr is Metrix_Vectors.Elements_Array (Arr_Index);
      A : Arr := To_Array (Metrix_For_Coupling);

      function Lt (X, Y : Metrix_Ref) return Boolean;
      pragma Inline (Lt);

      procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort
        (Arr_Index, Metrix_Ref, Arr, "<" => Lt);

      function Lt (X, Y : Metrix_Ref) return Boolean is
         U1 : constant String := Str (X.CU_Name).S;
         U2 : constant String := Str (Y.CU_Name).S;
      begin
         return U1 < U2;
      end Lt;

   begin
      if Metrics_To_Compute = Empty_Metrics_Set then
         return;
      end if;

      Sort (A);

      Indent;
      Put ("<coupling>\n");

      for File_M of A loop
         XML_Print_Metrix
           (Cmd,
            File_M.Source_File_Name.all,
            Metrics_To_Compute,
            File_M.all,
            Depth => 1);
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
      M : Metrix_Ref := Get (Metrix_Stack, 0);

      Global : Text_IO.File_Type;
      --  File for global information in text form, if --global-file-name
      --  switch was given. By default, this information is sent to standard
      --  output.

      Xml_F_Name : constant String :=
        (if Arg (Cmd, Xml_File_Name) /= null
           then Arg (Cmd, Xml_File_Name).all
           else Directories.Compose
             (Output_Dir (Cmd),
              (if Arg (Cmd, Xml_File_Name) = null
                 then "metrix.xml"
                 else Arg (Cmd, Xml_File_Name).all)));
      --  Actually, gnatmetric seems to ignore Output_Dir for the xml

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
         Norm : constant String  := Normalize_Pathname (Xml_F_Name);
         Xml : constant String := ".xml";
         Xsd : constant String := ".xsd";
      begin
         if Has_Suffix (Norm, Suffix => Xml) then
            return Replace_String (Norm, Xml, Xsd);
         else
            return Norm & Xsd;
         end if;
      end Xsd_File_Name;

   --  Start of processing for Final

   begin
      pragma Assert (M.Vals (Complexity_Cyclomatic) =
                       M.Vals (Complexity_Statement) +
                       M.Vals (Complexity_Expression));

      --  We're done with Metrix_Stack at this point. Printing uses the tree
      --  formed by Submetrix.

      M.Node := null;
      Pop (Metrix_Stack);
      Destroy (Metrix_Stack);

      Dump (Tool, M.all, "Initial:");
      Compute_Indirect_Dependencies (M.all);
      Dump (Tool, M.all, "After Compute_Indirect_Dependencies");
      Compute_Coupling (Tool, M.all);

      if Gen_XML (Cmd) then

         --  Generate schema (XSD file), if requested

         if Arg (Cmd, Generate_XML_Schema) then
            Write_XML_Schema (Xsd_File_Name);
         end if;

         --  Put initial lines of XML

         if not Output_To_Standard_Output then
            Text_IO.Create (XML_File, Name => Xml_F_Name);
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
      end if;

      --  Print the metrics for each file in text and XML form

      for File_M of M.Submetrix loop
         pragma Assert (Debug_Flag_V or else Indentation = 0);
         Print_File_Metrics
           (Cmd, XML_File, File_M.all, Without_Coupling);
         pragma Assert (Debug_Flag_V or else Indentation = 0);
--         Destroy (File_M);
      end loop;

      --  Print the totals in text form. These go to standard output, unless
      --  --global-file-name was specified. Note that Output_Dir (Cmd) is
      --  ignored by gnatmetric for this output.

      if Gen_Text (Cmd) then
         if Arg (Cmd, Global_File_Name) /= null then
            if not Output_To_Standard_Output then
               Text_IO.Create
                 (Global, Name => Arg (Cmd, Global_File_Name).all);
               Text_IO.Set_Output (Global);
            end if;
         end if;

         Print_Range
           ("Line metrics " & Summed,
            Metrics_To_Compute,
            Lines_Metrics'First, Lines_Metrics'Last, M.all,
            Depth => 0);
         Print_Range
           ("Contract metrics " & Summed,
            Metrics_To_Compute,
            Contract_Metrics'First, Contract_Metrics'Last, M.all,
            Depth => 0);
         Print_Range
           ("Element metrics " & Summed,
            Metrics_To_Compute,
            Syntax_Metrics'First, Syntax_Metrics'Last, M.all,
            Depth => 0);

         if Arg (Cmd, Global_File_Name) /= null then
            if not Output_To_Standard_Output then
               Text_IO.Set_Output (Text_IO.Standard_Output);
               Text_IO.Close (Global);
            end if;
         end if;
      end if;

      --  Print the totals in XML form

      if Gen_XML (Cmd) then
         if not Output_To_Standard_Output then
            Text_IO.Set_Output (XML_File);
         end if;
         XML_Print_Metrix_Vals
           (Metrics_To_Compute, M.Vals'First, M.Vals'Last, M.all, Depth => 0);
         XML_Print_Coupling (Cmd, With_Coupling);
         Put ("</global>\n");
         if not Output_To_Standard_Output then
            Text_IO.Set_Output (Text_IO.Standard_Output);
            Text_IO.Close (XML_File);
         end if;
      end if;

      Destroy (M);
   end Final;

   ---------------------
   -- Per_File_Action --
   ---------------------

   procedure Per_File_Action
     (Tool : in out Metrics_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Unit : Analysis_Unit)
   is
      pragma Unreferenced (Cmd);
   begin
      if Debug_Flag_V then
         Print (Unit);
         Put ("With trivia\n");
         PP_Trivia (Unit);
      end if;

      Walk (Tool, File_Name, Root (Unit), Cumulative (Unit));
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
   end Validate;

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
