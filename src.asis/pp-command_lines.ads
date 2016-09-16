with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Common;        use LAL_UL.Common;
package Pp.Command_Lines is

   package Freeze_Common is new Freeze_Descriptor (Common_Descriptor);

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   package Pp_Disable is new Disable_Switches
     (Descriptor, (1 => To_All (Rep_Clauses)));

   type Pp_Flags is
     (No_Alignment,
      Alignment,
      Obsolete_A2,
      Obsolete_A3,
      Obsolete_A4,
      Obsolete_A5,
      Comments_Unchanged,
      Comments_Gnat_Indentation,
      Comments_Standard_Indentation, -- Not documented
      Comments_Gnat_Beginning,
      Comments_Fill,
      Comments_Special,
      Comments_Only,
      Pipe,
      Replace,
      Replace_Force,
      Replace_No_Backup,
      Separate_Loop_Then,
      No_Separate_Loop_Then,
      Insert_Blank_Lines,
      Preserve_Blank_Lines,
      No_End_Labels, -- Not documented
      Xml_Help, -- Not documented
      L1, L2, L3, -- Not documented
      No_Tab, -- Not documented
      Separate_Label, -- Not documented
      Separate_Stmt_Name, -- Not documented
      Test, -- Not documented
      Warnings); -- Not documented

   package Pp_Flag_Switches is new Flag_Switches
     (Descriptor,
      Pp_Flags);

   package Pp_Flag_Shorthands is new Pp_Flag_Switches.Set_Shorthands
     ((No_Alignment => +"-A0",
       Alignment => +"-A1",
       Obsolete_A2 => +"-A2",
       Obsolete_A3 => +"-A3",
       Obsolete_A4 => +"-A4",
       Obsolete_A5 => +"-A5",
       Comments_Unchanged => +"-c0",
       Comments_Gnat_Indentation => +"-c1",
       Comments_Standard_Indentation => +"-c2", -- Not documented
       Comments_Gnat_Beginning => +"-c3",
       Comments_Fill => +"-c4",
       Comments_Special => +"-c5",
       Comments_Only => null,
       Pipe => +"-pipe",
       Replace => +"-r",
       Replace_Force => +"-rf",
       Replace_No_Backup => +"-rnb",
       Separate_Loop_Then => null,
       No_Separate_Loop_Then => null,
       Insert_Blank_Lines => null,
       Preserve_Blank_Lines => null,
       No_End_Labels => +"-e", -- Not documented
       Xml_Help => +"-hx", -- Not documented
       L1 => +"-l1", L2 => +"-l2", L3 => +"-l3", -- Not documented
       No_Tab => +"-notab", -- Not documented
       Separate_Label => null, -- Not documented
       Separate_Stmt_Name => null, -- Not documented
       Test => null, -- Not documented
       Warnings => +"-w")); -- Not documented

   package Pp_Flag_Shorthands_N is new Pp_Flag_Switches.Set_Shorthands
     ((No_Tab => +"-N", -- Not documented
       others => null));

   type Attribute_Casing is
     (Attribute_Mixed_Case,
      Attribute_Lower_Case,
      Attribute_Upper_Case);

   package Attribute_Casing_Switches is new Enum_Switches
     (Descriptor,
      Attribute_Casing);

   package Attribute_Casing_Shorthands is
      new Attribute_Casing_Switches.Set_Shorthands
     ((Attribute_Lower_Case => +"-aL",
       Attribute_Upper_Case => +"-aU",
       Attribute_Mixed_Case => +"-aM"));

   type Keyword_Casing is
     (Keyword_Lower_Case,
      Keyword_Upper_Case);

   package Keyword_Casing_Switches is new Enum_Switches
     (Descriptor,
      Keyword_Casing);

   package Keyword_Casing_Shorthands is
      new Keyword_Casing_Switches.Set_Shorthands
     ((Keyword_Lower_Case => +"-kL",
       Keyword_Upper_Case => +"-kU"));

   type Name_Casing is
     (Name_Default_Case, -- "default" --> "as declared"????????????????
      Name_Lower_Case,
      Name_Upper_Case,
      Name_Mixed_Case);

   package Name_Casing_Switches is new Enum_Switches
     (Descriptor,
      Name_Casing);

   package Name_Casing_Shorthands is
      new Name_Casing_Switches.Set_Shorthands
     ((Name_Default_Case => +"-nD",
       Name_Lower_Case => +"-nL",
       Name_Upper_Case => +"-nU",
       Name_Mixed_Case => +"-nM"));

   type Enum_Casing is
     (Enum_Default_Case,
      Enum_Lower_Case,
      Enum_Upper_Case,
      Enum_Mixed_Case);

   package Enum_Casing_Switches is new Enum_Switches
     (Descriptor,
      Enum_Casing);

   package Enum_Casing_Shorthands is
      new Enum_Casing_Switches.Set_Shorthands
     ((Enum_Default_Case => +"-neD",
       Enum_Lower_Case => +"-neL",
       Enum_Upper_Case => +"-neU",
       Enum_Mixed_Case => +"-neM"));

   type Type_Casing is
     (Type_Default_Case,
      Type_Lower_Case,
      Type_Upper_Case,
      Type_Mixed_Case);

   package Type_Casing_Switches is new Enum_Switches
     (Descriptor,
      Type_Casing);

   package Type_Casing_Shorthands is
      new Type_Casing_Switches.Set_Shorthands
     ((Type_Default_Case => +"-ntD",
       Type_Lower_Case => +"-ntL",
       Type_Upper_Case => +"-ntU",
       Type_Mixed_Case => +"-ntM"));

   type Number_Casing is
     (Number_Default_Case,
      Number_Lower_Case,
      Number_Upper_Case,
      Number_Mixed_Case);

   package Number_Casing_Switches is new Enum_Switches
     (Descriptor,
      Number_Casing);

   package Number_Casing_Shorthands is
      new Number_Casing_Switches.Set_Shorthands
     ((Number_Default_Case => +"-nnD",
       Number_Lower_Case => +"-nnL",
       Number_Upper_Case => +"-nnU",
       Number_Mixed_Case => +"-nnM"));

   type Pragma_Casing is
     (Pragma_Mixed_Case,
      Pragma_Lower_Case,
      Pragma_Upper_Case);

   package Pragma_Casing_Switches is new Enum_Switches
     (Descriptor,
      Pragma_Casing);

   package Pragma_Casing_Shorthands is
      new Pragma_Casing_Switches.Set_Shorthands
     ((Pragma_Lower_Case => +"-pL",
       Pragma_Upper_Case => +"-pU",
       Pragma_Mixed_Case => +"-pM"));

   type Pp_Booleans is
     (End_Id,
      Separate_Is,
      Use_On_New_Line,
      Split_Line_Before_Op,
      Rm_Style_Spacing,
      Ff_After_Pragma_Page);

   package Pp_Boolean_Switches is new Boolean_Switches
     (Descriptor,
      Pp_Booleans);

   package Pp_Boolean_Shorthands is new
     Pp_Boolean_Switches.Set_Shorthands
       ((End_Id => null,
         Separate_Is => null,
         Use_On_New_Line => null,
         Split_Line_Before_Op => null,
         Rm_Style_Spacing => +"--RM-style-spacing",
         Ff_After_Pragma_Page => +"-ff"));

   package Pp_Boolean_Defaults is new
     Pp_Boolean_Switches.Set_Defaults
       ((End_Id => True,
         Separate_Is => True,
         Use_On_New_Line => False,
         Split_Line_Before_Op => False,
         Rm_Style_Spacing => False,
         Ff_After_Pragma_Page => False));

   type Pp_Strings is
     (File_Name_File,
      Output,
      Output_Force,
      End_Of_Line,
      Wide_Character_Encoding, -- Use Enum_Switches????
      Pp_Off,
      Pp_On);

   package Pp_String_Switches is new String_Switches
     (Descriptor,
      Pp_Strings);

   package Pp_String_Syntax is new Pp_String_Switches.Set_Syntax
     ((File_Name_File => '=',
       Output => '=',
       Output_Force => '=',
       End_Of_Line => '=',
       Wide_Character_Encoding => '!',
       Pp_Off => '=',
       Pp_On => '='));

   package Pp_String_Shorthands is new Pp_String_Switches
     .Set_Shorthands
     ((File_Name_File => null,
       Output => +"-o",
       Output_Force => +"-of",
       End_Of_Line => +"--eol",
       Wide_Character_Encoding => +"-W",
       Pp_Off => null,
       Pp_On => null));

   type Pp_String_Seqs is (Dictionary);

   package Pp_String_Seq_Switches is new String_Seq_Switches
     (Descriptor,
      Pp_String_Seqs);

   package Pp_String_Seq_Syntax is new Pp_String_Seq_Switches
     .Set_Syntax
     ((Dictionary => ':'));

   package Pp_String_Seq_Shorthands is new Pp_String_Seq_Switches
     .Set_Shorthands
     ((Dictionary => +"-D"));

   type Pp_Nats is
     (Max_Line_Length,
      Indentation,
      Indent_Continuation,
      Decimal_Grouping,
      Based_Grouping,
      Call_Threshold,
      Par_Threshold,
      Case_Threshold); -- Not documented

   package Pp_Nat_Switches is new Other_Switches
     (Descriptor,
      Pp_Nats,
      Natural,
      Natural'Image,
      Natural'Value);

   package Pp_Nat_Syntax is new Pp_Nat_Switches.Set_Syntax
     ((Max_Line_Length => '!',
       Indentation => '!',
       Indent_Continuation => '!',
       Decimal_Grouping => '=',
       Based_Grouping => '=',
       Call_Threshold => '=',
       Par_Threshold => '=',
       Case_Threshold => '!')); -- Not documented

   package Pp_Nat_Defaults is new Pp_Nat_Switches.Set_Defaults
     ((Max_Line_Length => 79,
       Indentation => 3,
       Indent_Continuation => 0,
   --  Default for Indent_Continuation is one less than Indentation, or 1.
       Decimal_Grouping => 0,
       Based_Grouping => 0,
       Call_Threshold => Natural'Last,
       Par_Threshold => Natural'Last,
       Case_Threshold => 10)); -- Not documented

   package Pp_Nat_Shorthands is new Pp_Nat_Switches.Set_Shorthands
     ((Max_Line_Length => +"-M",
       Indentation => +"-i",
       Indent_Continuation => +"-cl",
       Decimal_Grouping => null,
       Based_Grouping => null,
       Call_Threshold => null,
       Par_Threshold => null,
       Case_Threshold => +"-T")); -- Not documented

   package Freeze is new Freeze_Descriptor (Descriptor);

   use Pp_Flag_Switches,
     Pp_Boolean_Switches,
     Attribute_Casing_Switches,
     Keyword_Casing_Switches,
     Name_Casing_Switches,
     Enum_Casing_Switches,
     Type_Casing_Switches,
     Number_Casing_Switches,
     Pragma_Casing_Switches,
     Pp_String_Switches,
     Pp_Nat_Switches,
     Pp_String_Seq_Switches;

   function Alignment_Enabled (Cmd : Command_Line) return Boolean is
     ((not Arg (Cmd, Rm_Style_Spacing))
         and then (not Arg (Cmd, No_Alignment)
                     or else Arg (Cmd, Obsolete_A2)
                     or else Arg (Cmd, Obsolete_A3)
                     or else Arg (Cmd, Obsolete_A4)
                     or else Arg (Cmd, Obsolete_A5)));
   --  The old gnatpp had the ability to individually enable different kinds of
   --  alignment; the new gnatpp does not. Instead, we align if ANY alignment
   --  option is enabled; if all alignment is turned off, we don't align.

   function Comment_Filling_Enabled (Cmd : Command_Line) return Boolean is
     (not Arg (Cmd, Comments_Unchanged) and Arg (Cmd, Comments_Fill));

   function Insert_Blank_Lines (Cmd : Command_Line) return Boolean is
     (Arg (Cmd, Insert_Blank_Lines) and not Arg (Cmd, Preserve_Blank_Lines));

   function Preserve_Blank_Lines (Cmd : Command_Line) return Boolean is
     (not Arg (Cmd, Insert_Blank_Lines) and Arg (Cmd, Preserve_Blank_Lines));

   type PP_Casing is
   --  Defines the casing of identifiers and keywords generated by gnatpp
    (Lower_Case,
   --  All letters are lowercase
    Upper_Case,
   --  All letters are uppercase
    Mixed,
   --  For both defining and usage occurrences of identifiers The first letter
   --  and each letter which immediately follows the underscore are uppercase,
   --  and all the other letters are lowercase
    As_Declared);
   --  All the usage occurrences of identifiers have the same casing as
   --  defining occurrences, defining occurrences have the same casing as
   --  the corresponding defining occurrences in the argument source.

   subtype Lower_Upper_PP_Casing is PP_Casing with
     Predicate => Lower_Upper_PP_Casing in Lower_Case | Upper_Case;

   subtype Lower_Upper_Mixed_PP_Casing is PP_Casing with
     Predicate => Lower_Upper_Mixed_PP_Casing
       in Lower_Case | Upper_Case | Mixed;

   subtype Cmd_Line is LAL_UL.Command_Lines.Command_Line;

   function PP_Attribute_Casing
     (Cmd : Cmd_Line) return Lower_Upper_Mixed_PP_Casing is
      (case Attribute_Casing'(Arg (Cmd)) is
         when Attribute_Mixed_Case => Mixed,
         when Attribute_Lower_Case => Lower_Case,
         when Attribute_Upper_Case => Upper_Case);

   function PP_Keyword_Casing
     (Cmd : Cmd_Line) return Lower_Upper_PP_Casing is
      (case Keyword_Casing'(Arg (Cmd)) is
         when Keyword_Lower_Case => Lower_Case,
         when Keyword_Upper_Case => Upper_Case);

   function PP_Pragma_Casing
     (Cmd : Cmd_Line) return Lower_Upper_Mixed_PP_Casing is
      (case Pragma_Casing'(Arg (Cmd)) is
         when Pragma_Mixed_Case => Mixed,
         when Pragma_Lower_Case => Lower_Case,
         when Pragma_Upper_Case => Upper_Case);
   --  Specifies the casing of pragma names and identifiers specific to
   --  pragmas

   function PP_Name_Casing
     (Cmd : Cmd_Line) return PP_Casing is
      (case Name_Casing'(Arg (Cmd)) is
         when Name_Default_Case => As_Declared,
         when Name_Mixed_Case => Mixed,
         when Name_Lower_Case => Lower_Case,
         when Name_Upper_Case => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  names

   function PP_Enum_Casing
     (Cmd : Cmd_Line) return PP_Casing is
      (case Enum_Casing'(Arg (Cmd)) is
         when Enum_Default_Case => PP_Name_Casing (Cmd),
         when Enum_Mixed_Case => Mixed,
         when Enum_Lower_Case => Lower_Case,
         when Enum_Upper_Case => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  enumeration literals.

   function PP_Type_Casing
     (Cmd : Cmd_Line) return PP_Casing is
      (case Type_Casing'(Arg (Cmd)) is
         when Type_Default_Case => PP_Name_Casing (Cmd),
         when Type_Mixed_Case => Mixed,
         when Type_Lower_Case => Lower_Case,
         when Type_Upper_Case => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  type (and subtype???) names.

   function PP_Number_Casing
     (Cmd : Cmd_Line) return PP_Casing is
      (case Number_Casing'(Arg (Cmd)) is
         when Number_Default_Case => PP_Name_Casing (Cmd),
         when Number_Mixed_Case => Mixed,
         when Number_Lower_Case => Lower_Case,
         when Number_Upper_Case => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  named numbers names.

   function PP_Indentation (Cmd : Cmd_Line) return Positive is
     (Arg (Cmd, Indentation));

   function PP_Indent_Continuation (Cmd : Cmd_Line) return Positive is
     (if Arg (Cmd, Indent_Continuation) = 0
        then (if PP_Indentation (Cmd) > 1
                then PP_Indentation (Cmd) - 1
                else PP_Indentation (Cmd))
        else Arg (Cmd, Indent_Continuation));

end Pp.Command_Lines;
