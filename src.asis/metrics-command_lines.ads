with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Common;        use LAL_UL.Common;
package METRICS.Command_Lines is

   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   package Metrics_Disable is new Disable_Switches
     (Common_Descriptor,
      (To_All (Rep_Clauses), To_All (Debug)) & Incremental_Switches);

   --  Note: Most tools allow "--debugx" with a shorthand of "-dx".
   --  Gnatmetric, however, uses "--gnatmetric-debugx" with a shorthand
   --  of "-debugx". Therefore, we disable Common.Debug, and define
   --  Gnatmetric_Debug below.

   package Metrics_Common_String_Shorthands is new Common_String_Switches
     .Set_Shorthands
     ((Output_Directory => +"-d",
       others => null));
   --  Note: gnatmetric allows -d to specify the directory. This must come
   --  before the Copy_Descriptor below. This shorthand is in addition to
   --  --output-dir, as defined in LAL_UL.Common.

   package Freeze_Common is new Freeze_Descriptor (Common_Descriptor);

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   type Metrics_Flags is
     (Test,

   --  Old metric control options, are kept for upward compatibility
   --  reasons (as non-documented feature)
   --  These really should be shorthands, but that doesn't work???
      La_Switch,
      Lcode_Switch,
      Lcomm_Switch,
      Leol_Switch,
      Lb_Switch,
      Lratio_Switch,
      Lav_Switch,
      Enu_Switch,
      Es_Switch,
      Ed_Switch,
      Eps_Switch,
      Eas_Switch,
      Ept_Switch,
      Eat_Switch,
      Ec_Switch,
      Nocc_Switch,
      Noec_Switch,
      Nonl_Switch,

      No_Treat_Exit_As_Goto, -- Option for Essential_Complexity
      No_Local_Metrics,

      No_Static_Loop, -- Option for Complexity_Cyclomatic

      Generate_XML_Output,
      Generate_XML_Schema,
      No_Text_Output,
      Short_File_Name_In_Output,
      Progress_Indicator_Mode);

   package Metrics_Flag_Switches is new Flag_Switches
     (Descriptor,
      Metrics_Flags);

   package Metrics_Flag_Shorthands is new Metrics_Flag_Switches.Set_Shorthands
     ((Test => null,

       La_Switch     => +"-la",
       Lcode_Switch  => +"-lcode",
       Lcomm_Switch  => +"-lcomm",
       Leol_Switch   => +"-leol",
       Lb_Switch     => +"-lb",
       Lratio_Switch => +"-lratio",
       Lav_Switch    => +"-lav",
       Enu_Switch    => +"-enu",
       Es_Switch     => +"-es",
       Ed_Switch     => +"-ed",
       Eps_Switch    => +"-eps",
       Eas_Switch    => +"-eas",
       Ept_Switch    => +"-ept",
       Eat_Switch    => +"-eat",
       Ec_Switch     => +"-ec",
       Nocc_Switch   => +"-nocc",
       Noec_Switch   => +"-noec",
       Nonl_Switch   => +"-nonl",

       No_Treat_Exit_As_Goto => +"-ne",
       No_Local_Metrics => +"-nolocal",

       No_Static_Loop => null,

       Generate_XML_Output       => +"-x",
       Generate_XML_Schema       => +"-xs",
       No_Text_Output            => +"-nt",
       Short_File_Name_In_Output => +"-sfn",
       Progress_Indicator_Mode   => +"-dd"));

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
      Public_Subprograms,
      All_Subprograms,
      Statements,
      Declarations,
      Public_Types,
      All_Types,
      Unit_Nesting,
      Construct_Nesting,
      Param_Number,

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

      Contract_All,
      Lines_All,
      Syntax_All,
      Complexity_All,
      Coupling_All);
   pragma Ordered (Metrics_Booleans);
   --  Otherwise, we get bogus warnings in Metrics.Actions.

   subtype Metrics_Enum is Metrics_Booleans
     range Metrics_Booleans'First .. Metrics_Booleans'Pred (Contract_All);
   subtype Metrics_All_Enum is Metrics_Booleans
     range Contract_All .. Metrics_Booleans'Last;
   subtype Contract_Metrics is Metrics_Booleans
     range Contract .. Contract_Complexity;
   subtype Complexity_Metrics is Metrics_Booleans
     range Complexity_Statement .. Extra_Exit_Points;
   subtype Lines_Metrics is Metrics_Booleans
     range Lines .. Lines_Average;
   --  not Lines_Spark, Lines_Code_In_Bodies, Num_Bodies
   subtype Syntax_Metrics is Metrics_Booleans
     range Public_Subprograms .. Param_Number;
   subtype Coupling_Metrics is Metrics_Booleans
     range Tagged_Coupling_Out .. Unit_Coupling_In;

   type Metrics_Set is array (Metrics_Enum) of Boolean with Pack;
   function Empty_Metrics_Set return Metrics_Set is (Metrics_Enum => False);

   Complexity_Only : constant Metrics_Set :=
     (Complexity_Metrics => True, others => False);
   --  Set of complexity metrics

   Coupling_Only : constant Metrics_Set :=
     (Coupling_Metrics => True, others => False);
   --  Set of coupling metrics

   package Metrics_Boolean_Switches is new Boolean_Switches
     (Descriptor,
      Metrics_Booleans);

   --  Old coupling metric control options, kept for upward
   --  compatibility reasons (as non_documented feature):

   package Metrics_Boolean_Shorthands is new
     Metrics_Boolean_Switches.Set_Shorthands
   --  Old metric control options, are kept for upward compatibility
   --  reasons (as non-documented feature)
--       ((Lines              => +"-la",
--         Lines_Code         => +"-lcode",
--         Lines_Comment      => +"-lcomm",
--         Lines_Eol_Comment  => +"-leol",
--         Lines_Ratio        => +"-lratio",
--         Lines_Blank        => +"-lb",
--         Lines_Average      => +"-lav",
--         Unit_Nesting       => +"-enu",
--         Statements         => +"-es",
--         Declarations       => +"-ed",
--         Public_Subprograms => +"-eps",
--         All_Subprograms    => +"-eas",
--         Public_Types       => +"-ept",
--         All_Types          => +"-eat",
--         Construct_Nesting  => +"-ec",
--  Above are commented out, because these shorthands do not have exactly the
--  same semantics as the long forms. For example, -lratio seems to turn on
--  --complexity-average, whereas --lines-ratio does not.

       ((Tagged_Coupling_Out    => +"--package-efferent-coupling",
         Tagged_Coupling_In     => +"--package-afferent-coupling",
         Hierarchy_Coupling_Out => +"--category-efferent-coupling",
         Hierarchy_Coupling_In  => +"--category-afferent-coupling",
         others                 => null));

   type Metrics_Strings is
     (Output_Suffix, Global_File_Name, Xml_File_Name);

   package Metrics_String_Switches is new String_Switches
     (Descriptor,
      Metrics_Strings);

   package Metrics_String_Syntax is new Metrics_String_Switches.Set_Syntax
     ((Output_Suffix    => '=',
       Global_File_Name => '=',
       Xml_File_Name    => '='));

   package Metrics_String_Shorthands is new Metrics_String_Switches
     .Set_Shorthands
     ((Output_Suffix    => +"-o",
       Global_File_Name => +"-og",
       Xml_File_Name    => +"-ox"));

   type Metrics_String_Seqs is (Gnatmetric_Debug);

   package Metrics_String_Seq_Switches is new String_Seq_Switches
     (Descriptor,
      Metrics_String_Seqs);

   package Metrics_String_Seq_Syntax is new Metrics_String_Seq_Switches
     .Set_Syntax
     ((Gnatmetric_Debug => '!'));

   package Metrics_String_Seq_Shorthands is new Metrics_String_Seq_Switches
     .Set_Shorthands
     ((Gnatmetric_Debug => +"-debug"));

   package Freeze is new Freeze_Descriptor (Descriptor);

   use Metrics_Flag_Switches, Metrics_Boolean_Switches,
     Metrics_String_Switches, Metrics_String_Seq_Switches;

   ----------------

   function Gen_XML (Cmd : Command_Line) return Boolean is
     (Arg (Cmd, Generate_XML_Output)
      or else Arg (Cmd, Generate_XML_Schema)
      or else Arg (Cmd, No_Text_Output)
      or else Arg (Cmd, Xml_File_Name) /= null);

   function Gen_Text (Cmd : Command_Line) return Boolean is
     (not Arg (Cmd, No_Text_Output));

end METRICS.Command_Lines;
