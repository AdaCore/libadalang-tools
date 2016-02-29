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

      Treat_Exit_As_Goto, -- defaults to False????????????????
      Compute_Local_Metrics, -- defaults to False????????????????

      Generate_XML_Output,
      Generate_XML_Schema,
      No_Text_Output,
      Short_SFN_In_Output,
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

       Treat_Exit_As_Goto    => +"-ne",
       Compute_Local_Metrics => +"-nolocal",

       Generate_XML_Output     => +"-x",
       Generate_XML_Schema     => +"-xs",
       No_Text_Output          => +"-nt",
       Short_SFN_In_Output     => +"-sfn",
       Progress_Indicator_Mode => +"-dd"));

   type Metrics_Booleans is
   --  Complexity metrics options:
     (Complexity_Cyclomatic,
      Complexity_Essential,
      Complexity_Average,
      Loop_Nesting,
      Extra_Exit_Points,
      Complexity_All,
      Static_Loop,

   --  Line metrics options:
      Lines,
      Lines_Code,
      Lines_Comment,
      Lines_Eol_Comment,
      Lines_Ratio,
      Lines_Blank,
      Lines_Average,
      Lines_All,

   --  Syntax element metrics options:
      Declarations,
      Statements,
      Public_Subprograms,
      All_Subprograms,
      Public_Types,
      All_Types,
      Unit_Nesting,
      Construct_Nesting,
      Syntax_All,

   --  Coupling metrics
      Tagged_Coupling_Out,
      Tagged_Coupling_In,
      Hierarchy_Coupling_Out,
      Hierarchy_Coupling_In,
      Unit_Coupling_Out,
      Unit_Coupling_In,
      Control_Coupling_Out,
      Control_Coupling_In,

   --  Old coupling metric control options, kept for upward
   --  compatibility reasons (as non_documented feature)

      Package_Efferent_Coupling,
      Package_Afferent_Coupling,
      Category_Efferent_Coupling,
      Category_Afferent_Coupling,
      Coupling_All);

   package Metrics_Boolean_Switches is new Boolean_Switches
     (Descriptor,
      Metrics_Booleans);

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
