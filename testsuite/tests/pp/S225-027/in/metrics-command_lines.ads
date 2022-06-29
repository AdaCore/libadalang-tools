with Utils.Command_Lines;        use Utils.Command_Lines;
with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
package METRICS.Command_Lines is

   pragma Warnings (Off);
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;
   pragma Warnings (On);

   package Metrics_Disable is new Disable_Switches (Common_Descriptor,
      (To_All (Rep_Clauses), To_All (Debug)) & Incremental_Switches);

   --  Note: Most tools allow "--debugx" with a shorthand of "-dx".
   --  Gnatmetric, however, uses "--gnatmetric-debugx" with a shorthand
   --  of "-debugx". Therefore, we disable Common.Debug, and define
   --  Gnatmetric_Debug below.

   package Metrics_Common_String_Shorthands is new Common_String_Switches
     .Set_Shorthands
     ((Output_Directory => +"-d", others => null));
   --  Note: gnatmetric allows -d to specify the directory. This must come
   --  before the Copy_Descriptor below. This shorthand is in addition to
   --  --output-dir, as defined in Utils.Command_Lines.Common.

   package Freeze_Common is new Freeze_Descriptor (Common_Descriptor);

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   type Metrics_Flags is
     (Test,

      No_Treat_Exit_As_Goto, -- Option for Complexity_Essential

      No_Local_Metrics,

      No_Static_Loop, -- Option for Complexity_Cyclomatic

      Generate_XML_Output, Generate_XML_Schema, No_Text_Output,
      Short_File_Names, Progress_Indicator_Mode);

   package Metrics_Flag_Switches is new Flag_Switches (Descriptor,
      Metrics_Flags);

   package Metrics_Flag_Shorthands is new Metrics_Flag_Switches.Set_Shorthands
     ((Test                  => null,
       No_Treat_Exit_As_Goto => +"-ne",
       No_Local_Metrics      => +"-nolocal",
       No_Static_Loop        => null,

       Generate_XML_Output     => +"-x", Generate_XML_Schema => +"-xs",
       No_Text_Output          => +"-nt", Short_File_Names => +"-sfn",
       Progress_Indicator_Mode => +"-dd"));

   --  Metrics_Booleans serves both as switch names, and as index into
   --  arrays of metric values. The ones marked "undocumented" below
   --  are not intended to be switches, but are enabled by other
   --  switches. For example, "--complexity-cyclomatic" enables
   --  Complexity_Statement and Complexity_Expression.

   type Metrics_Booleans is
     --  Contract metrics options:

     (Contract,
      Post,
      Contract_Complete,
      Contract_Complexity,

      --  Line metrics options:

      Lines,
      Lines_Code,
      Lines_Comment,
      Lines_Eol_Comment,
      Lines_Ratio,
      Lines_Blank,
      Lines_Average, -- global

      Lines_Spark, -- not included in Lines_All

      Lines_Code_In_Bodies, -- undocumented, not included in Lines_All

      Num_Bodies, -- undocumented
      --  Lines_Code_In_Bodies is the number of code lines in various
      --  bodies, and Num_Bodies is the number of various bodies. These
      --  are used to compute Lines_Average, as the ratio of these.

      --  Syntax element metrics options:

      Public_Types,
      --  ????????Tagged_Types, -- undocumented, enabled by Public_Types

      Private_Types, -- undocumented, enabled by Public_Types

      All_Types,
      Public_Subprograms,
      All_Subprograms,
      Statements,
      Declarations,
      Logical_Source_Lines, -- undocumented, enabled by Statements+Declarations

      Unit_Nesting,
      Construct_Nesting,
      Current_Construct_Nesting, -- undocumented, not printed
      --  Current_Construct_Nesting is used in the computation of
      --  Construct_Nesting, which is the high-water mark of
      --  Current_Construct_Nesting.

      Param_Number,
      In_Parameters,
      Out_Parameters,
      In_Out_Parameters,

      Computed_Public_Types,
      Computed_All_Types,
      Computed_Public_Subprograms,
      --  The number of units for which public subprograms are safely
      --  computed

      Computed_All_Subprograms,
      --  The number of units for which all the subprograms are safely
      --  computed
      --  Copied from ASIS/tools/gnatmetric/metrics-metric_definitions.ads.
      --  I don't know what "safely computed" means, but (e.g.)
      --  Computed_All_Subprograms appears to be the number of library units
      --  with a nonzero value for All_Subprograms.
      --  See ASIS/tools/gnatmetric/metrics-compute.adb.
      --  These are undocumented.

      --  Complexity metrics options:

      Complexity_Statement, -- undocumented

      Complexity_Expression, -- undocumented

      Complexity_Cyclomatic,
      Complexity_Essential,
      Complexity_Average, -- global

      Loop_Nesting,
      Extra_Exit_Points,

      --  Coupling metrics

      Tagged_Coupling_Out,
      Hierarchy_Coupling_Out,
      Tagged_Coupling_In,
      Hierarchy_Coupling_In,
      Control_Coupling_Out,
      Control_Coupling_In,
      Unit_Coupling_Out,
      Unit_Coupling_In,

      Metrics_All, -- undocumented; turns on all metrics

      Contract_All,
      Lines_All,
      Syntax_All,
      Complexity_All,
      Coupling_All,
      XML_Config -- undocumented, --no-xml-config suppresses <config> in XML
      );
   pragma Ordered (Metrics_Booleans);
   --  Otherwise, we get bogus warnings in Metrics.Actions.

   subtype Metrics_Enum is
     Metrics_Booleans range Metrics_Booleans'First ..
         Metrics_Booleans'Pred (Metrics_All);
   subtype Contract_Metrics is
     Metrics_Enum range Contract .. Contract_Complexity;
   subtype Complexity_Metrics is
     Metrics_Enum range Complexity_Statement .. Extra_Exit_Points;
   subtype Lines_Metrics is Metrics_Enum range Lines .. Lines_Average;
   --  not Lines_Spark, Lines_Code_In_Bodies, Num_Bodies
   subtype Syntax_Metrics is
     Metrics_Enum range Public_Types .. In_Out_Parameters;
   subtype Computed_Metrics is
     Metrics_Enum range Computed_Public_Types .. Computed_All_Subprograms;
   subtype Coupling_Metrics is
     Metrics_Enum range Tagged_Coupling_Out .. Unit_Coupling_In;

   type Metrics_Set is array (Metrics_Enum) of Boolean with
     Pack;
   function Empty_Metrics_Set return Metrics_Set is (others => False);
   function All_Metrics_Set return Metrics_Set is (others => True);

   Complexity_Only : constant Metrics_Set :=
     (Complexity_Metrics => True, others => False);
   --  Set of complexity metrics

   Coupling_Only : constant Metrics_Set :=
     (Coupling_Metrics => True, others => False);
   --  Set of coupling metrics

   package Metrics_Boolean_Switches is new Boolean_Switches (Descriptor,
      Metrics_Booleans);

   package Metrics_Boolean_Shorthands is new Metrics_Boolean_Switches
     .Set_Shorthands
     --  Old metric control options are kept for upward compatibility reasons
     --  (as an undocumented feature). In the old ASIS-based version, these
     --  shorthands do not have exactly the same semantics as the long forms. For
     --  example, -lratio seems to turn on --complexity-average, whereas
     --  --lines-ratio does not. We don't need the complexity of those
     --  variations.
   --
   --  The old version also had -nocc (turns off Complexity_Cyclomatic), -noec
   --  (turns off Complexity_Essential), and -nonl (turns off Loop_Nesting).
   --  We no longer support those.

     ((Lines => +"-la", Lines_Code => +"-lcode", Lines_Comment => +"-lcomm",
       Lines_Eol_Comment  => +"-leol", Lines_Ratio => +"-lratio",
       Lines_Blank        => +"-lb", Lines_Average => +"-lav",
       Unit_Nesting => +"-enu", Statements => +"-es", Declarations => +"-ed",
       Public_Subprograms => +"-eps", All_Subprograms => +"-eas",
       Public_Types       => +"-ept", All_Types => +"-eat",
       Construct_Nesting  => +"-ec",

       --  Old coupling metric control options, kept for upward
       --  compatibility reasons (as an undocumented feature):

       Tagged_Coupling_Out    => +"--package-efferent-coupling",
       Tagged_Coupling_In     => +"--package-afferent-coupling",
       Hierarchy_Coupling_Out => +"--category-efferent-coupling",
       Hierarchy_Coupling_In  => +"--category-afferent-coupling",
       Metrics_All            => +"--all", others => null));

   package Metrics_Boolean_Defaults is new Metrics_Boolean_Switches
     .Set_Defaults
     ((XML_Config => True, others => False));

   type Metrics_Strings is (Output_Suffix, Global_File_Name, Xml_File_Name);

   package Metrics_String_Switches is new String_Switches (Descriptor,
      Metrics_Strings);

   package Metrics_String_Syntax is new Metrics_String_Switches.Set_Syntax
     ((Output_Suffix => '=', Global_File_Name => '=', Xml_File_Name => '='));

   package Metrics_String_Shorthands is new Metrics_String_Switches
     .Set_Shorthands
     ((Output_Suffix => +"-o", Global_File_Name => +"-og",
       Xml_File_Name => +"-ox"));

   type Metrics_String_Seqs is (Gnatmetric_Debug);

   package Metrics_String_Seq_Switches is new String_Seq_Switches (Descriptor,
      Metrics_String_Seqs);

   package Metrics_String_Seq_Syntax is new Metrics_String_Seq_Switches
     .Set_Syntax
     ((Gnatmetric_Debug => '!'));

   package Metrics_String_Seq_Shorthands is new Metrics_String_Seq_Switches
     .Set_Shorthands
     ((Gnatmetric_Debug => +"-debug"));

   package Freeze is new Freeze_Descriptor (Descriptor);

   pragma Warnings (Off);
   use Metrics_Flag_Switches, Metrics_Boolean_Switches,
     Metrics_String_Switches, Metrics_String_Seq_Switches;
   pragma Warnings (On);

   ----------------

   function Gen_XML (Cmd : Command_Line) return Boolean is
     (Arg (Cmd, Generate_XML_Output) or else Arg (Cmd, Generate_XML_Schema)
      or else Arg (Cmd, No_Text_Output)
      or else Present (Arg (Cmd, Xml_File_Name)));

   function Gen_Text (Cmd : Command_Line) return Boolean is
     (not Arg (Cmd, No_Text_Output));

end METRICS.Command_Lines;
