with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Common;        use LAL_UL.Common;
package METRICS.Command_Lines is

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   package Metrics_Disable is new Disable_Switches
     (Descriptor,
      (To_All (Rep_Clauses), To_All (Output_Dir), To_All (Debug)) &
      Incremental_Switches);

   --  Note: Most tools allow "--debugx" with a shorthand of "-dx".
   --  Gnatmetric, however, uses "--gnatmetric-debugx" with a shorthand
   --  of "-debugx". Therefore, we disable Common.Debug, and define
   --  Gnatmetric_Debug below.

   type Metrics_Flags is
     (Test,

   --  Old metric control options, are kept for upward compatibility
   --  reasons (as non-documented feature)
   --  ????????????????These really should be shorthands.
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

   type Metrics_Booleans is
   --  Contract metrics options:
     (Contract,
      Post,
      Contract_Complete,
      Contract_Cyclomatic,

   --  Complexity metrics options:
      Complexity_Cyclomatic,
      Complexity_Essential,
      Complexity_Average, -- global
      Loop_Nesting,
      Extra_Exit_Points,

   --  Line metrics options:
      Lines,
      Lines_Code,
      Lines_Comment,
      Lines_Eol_Comment,
      Lines_Ratio, -- ????Requires above counts?
      Lines_Blank,
      Lines_Average, -- global

   --  Syntax element metrics options:
      Declarations,
      Statements,
      Public_Subprograms,
      All_Subprograms,
      Public_Types,
      All_Types,
      Unit_Nesting,
      Construct_Nesting,
      Param_Number,

   --  Coupling metrics
      Tagged_Coupling_Out,
      Tagged_Coupling_In,
      Hierarchy_Coupling_Out,
      Hierarchy_Coupling_In,
      Unit_Coupling_Out,
      Unit_Coupling_In,
      Control_Coupling_Out,
      Control_Coupling_In,

      Contract_All,
      Complexity_All,
      Lines_All,
      Syntax_All,
      Coupling_All);

   subtype Metrics_Enum is Metrics_Booleans
     range Metrics_Booleans'First .. Metrics_Booleans'Pred (Contract_All);
   subtype Metrics_All_Enum is Metrics_Booleans
     range Contract_All .. Metrics_Booleans'Last;
   subtype Contract_Metrics is Metrics_Booleans
     range Contract .. Contract_Cyclomatic;
   subtype Complexity_Metrics is Metrics_Booleans
     range Complexity_Cyclomatic .. Extra_Exit_Points;
   subtype Lines_Metrics is Metrics_Booleans
     range Lines .. Lines_Average;
   subtype Syntax_Metrics is Metrics_Booleans
     range Declarations .. Param_Number;
   subtype Coupling_Metrics is Metrics_Booleans
     range Tagged_Coupling_Out .. Control_Coupling_In;

   type Metrics_Set is array (Metrics_Enum) of Boolean with Pack;

   package Metrics_Boolean_Switches is new Boolean_Switches
     (Descriptor,
      Metrics_Booleans);

   --  Old coupling metric control options, kept for upward
   --  compatibility reasons (as non_documented feature):

   package Metrics_Boolean_Shorthands is new
     Metrics_Boolean_Switches.Set_Shorthands
       ((Tagged_Coupling_Out    => +"--package-efferent-coupling",
         Tagged_Coupling_In     => +"--package-afferent-coupling",
         Hierarchy_Coupling_Out => +"--category-efferent-coupling",
         Hierarchy_Coupling_In  => +"--category-afferent-coupling",
         others                 => null));

   type Metrics_Strings is
     (Output_Suffix, Global_File_Name, Xml_File_Name, Output_Directory);

   package Metrics_String_Switches is new String_Switches
     (Descriptor,
      Metrics_Strings);

   package Metrics_String_Syntax is new Metrics_String_Switches.Set_Syntax
     ((Output_Suffix    => '=',
       Global_File_Name => '=',
       Xml_File_Name    => '=',
       Output_Directory => '='));

   package Metrics_String_Shorthands is new Metrics_String_Switches
     .Set_Shorthands
     ((Output_Suffix    => +"-o",
       Global_File_Name => +"-og",
       Xml_File_Name    => +"-ox",
       Output_Directory => +"-d"));

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

   Cmd : Command_Line (Descriptor'Access);

   use Metrics_Flag_Switches, Metrics_Boolean_Switches,
     Metrics_String_Switches, Metrics_String_Seq_Switches;

end METRICS.Command_Lines;
